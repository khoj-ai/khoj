import logging
import re
from typing import Dict, List, Tuple

import urllib3.util

from khoj.database.models import Entry as DbEntry
from khoj.database.models import KhojUser
from khoj.processor.content.text_to_entries import TextToEntries
from khoj.utils.helpers import timer
from khoj.utils.rawconfig import Entry

logger = logging.getLogger(__name__)


class MarkdownToEntries(TextToEntries):
    def __init__(self):
        super().__init__()

    # Define Functions
    def process(self, files: dict[str, str], user: KhojUser, regenerate: bool = False) -> Tuple[int, int]:
        # Extract required fields from config
        deletion_file_names = set([file for file in files if files[file] == ""])
        files_to_process = set(files) - deletion_file_names
        files = {file: files[file] for file in files_to_process}

        max_tokens = 256
        # Extract Entries from specified Markdown files
        with timer("Extract entries from specified Markdown files", logger):
            file_to_text_map, current_entries = MarkdownToEntries.extract_markdown_entries(files, max_tokens)

        # Split entries by max tokens supported by model
        with timer("Split entries by max token size supported by model", logger):
            current_entries = self.split_entries_by_max_tokens(current_entries, max_tokens)

        # Identify, mark and merge any new entries with previous entries
        with timer("Identify new or updated entries", logger):
            num_new_embeddings, num_deleted_embeddings = self.update_embeddings(
                user,
                current_entries,
                DbEntry.EntryType.MARKDOWN,
                DbEntry.EntrySource.COMPUTER,
                "compiled",
                logger,
                deletion_file_names,
                regenerate=regenerate,
                file_to_text_map=file_to_text_map,
            )

        return num_new_embeddings, num_deleted_embeddings

    @staticmethod
    def extract_markdown_entries(markdown_files: Dict[str, str], max_tokens=256) -> Tuple[Dict[str, str], List[Entry]]:
        "Extract entries by heading from specified Markdown files"
        entries: List[str] = []
        entry_to_file_map: List[Tuple[str, str, int]] = []
        file_to_text_map: Dict[str, str] = dict()
        for markdown_file in markdown_files:
            try:
                markdown_content = markdown_files[markdown_file]
                entries, entry_to_file_map = MarkdownToEntries.process_single_markdown_file(
                    markdown_content, markdown_file, entries, entry_to_file_map, max_tokens, start_line=1
                )
                file_to_text_map[markdown_file] = markdown_content
            except Exception as e:
                logger.error(
                    f"Unable to process file: {markdown_file}. This file will not be indexed.\n{e}", exc_info=True
                )

        return file_to_text_map, MarkdownToEntries.convert_markdown_entries_to_maps(entries, entry_to_file_map)

    @staticmethod
    def process_single_markdown_file(
        markdown_content: str,
        markdown_file: str,
        entries: List[str],
        entry_to_file_map: List[Tuple[str, str, int]],
        max_tokens=256,
        ancestry: Dict[int, str] = {},
        start_line: int = 1,
    ) -> Tuple[List[str], List[Tuple[str, str, int]]]:
        # Prepend the markdown section's heading ancestry
        ancestry_string = "\n".join([f"{'#' * key} {ancestry[key]}" for key in sorted(ancestry.keys())])
        markdown_content_with_ancestry = f"{ancestry_string}{markdown_content}"

        # If content is small or content has no children headings, save it as a single entry
        if len(TextToEntries.tokenizer(markdown_content_with_ancestry)) <= max_tokens or not re.search(
            rf"^#{{{len(ancestry) + 1},}}\s", markdown_content, flags=re.MULTILINE
        ):
            # Create entry with line number information
            entry_with_line_info = (markdown_content_with_ancestry, markdown_file, start_line)
            entry_to_file_map += [entry_with_line_info]
            entries.extend([markdown_content_with_ancestry])
            return entries, entry_to_file_map

        # Split by next heading level present in the entry
        next_heading_level = len(ancestry)
        sections: List[str] = []
        while len(sections) < 2:
            next_heading_level += 1
            sections = re.split(rf"(\n|^)(?=[#]{{{next_heading_level}}} .+\n?)", markdown_content, flags=re.MULTILINE)

        # Recurse down each non-empty section after parsing its body, heading and ancestry
        current_line_offset = 0
        for section in sections:
            num_lines_in_section = section.count("\n")
            # Skip empty sections
            if section.strip() == "":
                current_line_offset += num_lines_in_section
                continue

            section_start_line_in_file = start_line + current_line_offset

            # Extract the section body and (when present) the heading
            current_ancestry = ancestry.copy()
            first_line = [line for line in section.split("\n") if line.strip() != ""][0]
            if re.search(rf"^#{{{next_heading_level}}} ", first_line):
                # Extract the section body without the heading
                current_section_heading, current_section_body = section.split(first_line, 1)
                current_section_body_offset = current_section_heading.count("\n")
                # Parse the section heading into current section ancestry
                current_section_title = first_line[next_heading_level:].strip()
                current_ancestry[next_heading_level] = current_section_title
                # Line number should point to the heading itself
                recursive_start_line = section_start_line_in_file + current_section_body_offset
            else:
                current_section_body = section
                recursive_start_line = section_start_line_in_file

            # Recurse down children of the current entry
            MarkdownToEntries.process_single_markdown_file(
                current_section_body,
                markdown_file,
                entries,
                entry_to_file_map,
                max_tokens,
                current_ancestry,
                start_line=recursive_start_line,
            )
            current_line_offset += num_lines_in_section

        return entries, entry_to_file_map

    @staticmethod
    def convert_markdown_entries_to_maps(
        parsed_entries: List[str], entry_to_file_map: List[Tuple[str, str, int]]
    ) -> List[Entry]:
        "Convert each Markdown entries into a dictionary"
        entries: List[Entry] = []

        # Create a mapping from parsed entry to file info
        entry_map: Dict[str, Tuple[str, int]] = {}
        for entry_info in entry_to_file_map:
            entry_content, raw_filename, start_line = entry_info
            entry_map[entry_content] = (raw_filename, start_line)

        for parsed_entry in parsed_entries:
            raw_filename, start_line = entry_map[parsed_entry]
            calculated_line = start_line if start_line > 0 else 1

            # Check if raw_filename is a URL. If so, save it as is. If not, convert it to a Path.
            if isinstance(raw_filename, str) and re.search(r"^https?://", raw_filename):
                # Escape the URL to avoid issues with special characters
                entry_filename = urllib3.util.parse_url(raw_filename).url
                uri = entry_filename
            else:
                entry_filename = raw_filename
                # Create URI with line number
                uri = f"file://{entry_filename}#line={calculated_line}"

            heading = parsed_entry.splitlines()[0] if re.search(r"^#+\s", parsed_entry) else ""
            # Append base filename to compiled entry for context to model
            # Increment heading level for heading entries and make filename as its top level heading
            prefix = f"# {entry_filename}\n#" if heading else f"# {entry_filename}\n"
            compiled_entry = f"{prefix}{parsed_entry}"
            entries.append(
                Entry(
                    compiled=compiled_entry,
                    raw=parsed_entry,
                    heading=f"{prefix}{heading}",
                    file=entry_filename,
                    uri=uri,
                )
            )

        logger.debug(f"Converted {len(parsed_entries)} markdown entries to dictionaries")

        return entries
