import logging
import re
from typing import Dict, List, Tuple

import urllib3
from bs4 import BeautifulSoup, CData, NavigableString, PageElement, Tag

from khoj.database.models import Entry as DbEntry
from khoj.database.models import KhojUser
from khoj.processor.content.text_to_entries import TextToEntries
from khoj.utils.helpers import timer
from khoj.utils.rawconfig import Entry

logger = logging.getLogger(__name__)

# Elements a browser lays out as blocks. Each one starts a new line of extracted text,
# while the text of any other element stays in the line of the text around it.
HTML_BLOCK_TAGS = frozenset(
    "address article aside blockquote body caption center dd details dialog dir div dl dt"
    " fieldset figcaption figure footer form h1 h2 h3 h4 h5 h6 head header hgroup hr html"
    " legend li listing main menu nav ol optgroup option p plaintext pre search section"
    " summary table tbody td textarea tfoot th thead title tr ul xmp".split()
)
# Whitespace inside these elements is content, so their text is kept as written.
HTML_PREFORMATTED_TAGS = frozenset(("listing", "plaintext", "pre", "textarea", "xmp"))
# Inline boxes whose text never runs into the text next to them.
HTML_BOX_TAGS = frozenset(("button", "select"))
# Only these characters are whitespace in HTML. A no-break space is content.
HTML_WHITESPACE = re.compile(r"[ \t\n\r\f]+")


def html_to_text(soup: BeautifulSoup) -> str:
    "Extract the text of parsed HTML with one line per block element"
    lines: List[str] = []
    line: List[str] = []

    def end_line():
        text = HTML_WHITESPACE.sub(" ", "".join(line)).strip(" ")
        line.clear()
        if text.strip():
            lines.append(text)

    # Walk the tree with an explicit stack, so deeply nested markup cannot exhaust the recursion limit
    stack: List[Tuple[PageElement, str]] = [(soup, "visit")]
    while stack:
        node, action = stack.pop()
        if action == "end_line":
            end_line()
        elif action == "space":
            line.append(" ")
        elif isinstance(node, Tag):
            if node.name == "br":
                end_line()
            elif node.name in HTML_PREFORMATTED_TAGS:
                end_line()
                text = node.get_text().strip("\n")
                if text.strip():
                    lines.append(text)
            else:
                if node.name in HTML_BLOCK_TAGS:
                    end_line()
                    stack.append((node, "end_line"))
                elif node.name in HTML_BOX_TAGS:
                    line.append(" ")
                    stack.append((node, "space"))
                stack.extend((child, "visit") for child in reversed(node.contents))
        # Script, style, template and comment strings are subclasses and are not page text
        elif type(node) in (NavigableString, CData):
            line.append(str(node))
    end_line()
    return "\n".join(lines)


class PlaintextToEntries(TextToEntries):
    def __init__(self):
        super().__init__()

    # Define Functions
    def process(self, files: dict[str, str], user: KhojUser, regenerate: bool = False) -> Tuple[int, int]:
        deletion_file_names = set([file for file in files if files[file] == ""])
        files_to_process = set(files) - deletion_file_names
        files = {file: files[file] for file in files_to_process}

        # Extract Entries from specified plaintext files
        with timer("Extract entries from specified Plaintext files", logger):
            file_to_text_map, current_entries = PlaintextToEntries.extract_plaintext_entries(files)

        # Split entries by max tokens supported by model
        with timer("Split entries by max token size supported by model", logger):
            current_entries = self.split_entries_by_max_tokens(current_entries, max_tokens=256, raw_is_compiled=True)

        # Identify, mark and merge any new entries with previous entries
        with timer("Identify new or updated entries", logger):
            num_new_embeddings, num_deleted_embeddings = self.update_embeddings(
                user,
                current_entries,
                DbEntry.EntryType.PLAINTEXT,
                DbEntry.EntrySource.COMPUTER,
                key="compiled",
                logger=logger,
                deletion_filenames=deletion_file_names,
                regenerate=regenerate,
                file_to_text_map=file_to_text_map,
            )

        return num_new_embeddings, num_deleted_embeddings

    @staticmethod
    def extract_html_content(markup_content: str, markup_type: str):
        "Extract content from HTML"
        if markup_type == "xml":
            soup = BeautifulSoup(markup_content, "xml")
            return soup.get_text(strip=True, separator="\n")
        return html_to_text(BeautifulSoup(markup_content, "html.parser"))

    @staticmethod
    def extract_plaintext_entries(text_files: Dict[str, str]) -> Tuple[Dict, List[Entry]]:
        entries: List[str] = []
        entry_to_file_map: List[Tuple[str, str]] = []
        file_to_text_map = dict()
        for text_file in text_files:
            try:
                text_content = text_files[text_file]
                entries, entry_to_file_map = PlaintextToEntries.process_single_plaintext_file(
                    text_content, text_file, entries, entry_to_file_map
                )
                file_to_text_map[text_file] = text_content
            except Exception as e:
                logger.warning(f"Unable to read file: {text_file} as plaintext. Skipping file.")
                logger.warning(e, exc_info=True)

        # Extract Entries from specified plaintext files
        return file_to_text_map, PlaintextToEntries.convert_text_files_to_entries(entries, dict(entry_to_file_map))

    @staticmethod
    def process_single_plaintext_file(
        text_content: str,
        text_file: str,
        entries: List[str],
        entry_to_file_map: List[Tuple[str, str]],
    ) -> Tuple[List[str], List[Tuple[str, str]]]:
        if text_file.endswith(("html", "htm", "xml")):
            text_content = PlaintextToEntries.extract_html_content(text_content, text_file.split(".")[-1])
        entry_to_file_map += [(text_content, text_file)]
        entries.extend([text_content])
        return entries, entry_to_file_map

    @staticmethod
    def convert_text_files_to_entries(parsed_entries: List[str], entry_to_file_map: dict[str, str]) -> List[Entry]:
        "Convert each plaintext file into an entry"
        entries: List[Entry] = []
        for parsed_entry in parsed_entries:
            raw_filename = entry_to_file_map[parsed_entry]
            # Check if raw_filename is a URL. If so, save it as is. If not, convert it to a Path.
            if isinstance(raw_filename, str) and re.search(r"^https?://", raw_filename):
                # Escape the URL to avoid issues with special characters
                entry_filename = urllib3.util.parse_url(raw_filename).url
            else:
                entry_filename = raw_filename

            # Append base filename to compiled entry for context to model
            entries.append(
                Entry(
                    raw=parsed_entry,
                    file=f"{entry_filename}",
                    compiled=f"{entry_filename}\n{parsed_entry}",
                    heading=entry_filename,
                )
            )

        logger.debug(f"Converted {len(parsed_entries)} plaintext files to entries")
        return entries
