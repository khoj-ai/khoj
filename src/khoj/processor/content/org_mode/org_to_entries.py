import logging
import re
from pathlib import Path
from typing import Dict, List, Tuple

from khoj.database.models import Entry as DbEntry
from khoj.database.models import KhojUser
from khoj.processor.content.org_mode import orgnode
from khoj.processor.content.org_mode.orgnode import Orgnode
from khoj.processor.content.text_to_entries import TextToEntries
from khoj.utils.helpers import timer
from khoj.utils.rawconfig import Entry

logger = logging.getLogger(__name__)


class OrgToEntries(TextToEntries):
    def __init__(self):
        super().__init__()

    # Define Functions
    def process(self, files: dict[str, str], user: KhojUser, regenerate: bool = False) -> Tuple[int, int]:
        deletion_file_names = set([file for file in files if files[file] == ""])
        files_to_process = set(files) - deletion_file_names
        files = {file: files[file] for file in files_to_process}

        # Extract Entries from specified Org files
        max_tokens = 256
        with timer("Extract entries from specified Org files", logger):
            file_to_text_map, current_entries = self.extract_org_entries(files, max_tokens=max_tokens)

        with timer("Split entries by max token size supported by model", logger):
            current_entries = self.split_entries_by_max_tokens(current_entries, max_tokens=max_tokens)

        # Identify, mark and merge any new entries with previous entries
        with timer("Identify new or updated entries", logger):
            num_new_embeddings, num_deleted_embeddings = self.update_embeddings(
                user,
                current_entries,
                DbEntry.EntryType.ORG,
                DbEntry.EntrySource.COMPUTER,
                "compiled",
                logger,
                deletion_file_names,
                regenerate=regenerate,
                file_to_text_map=file_to_text_map,
            )

        return num_new_embeddings, num_deleted_embeddings

    @staticmethod
    def extract_org_entries(
        org_files: dict[str, str], index_heading_entries: bool = False, max_tokens=256
    ) -> Tuple[Dict, List[Entry]]:
        "Extract entries from specified Org files"
        file_to_text_map, entries, entry_to_file_map = OrgToEntries.extract_org_nodes(org_files, max_tokens)
        return file_to_text_map, OrgToEntries.convert_org_nodes_to_entries(
            entries, entry_to_file_map, index_heading_entries
        )

    @staticmethod
    def extract_org_nodes(
        org_files: dict[str, str], max_tokens
    ) -> Tuple[Dict, List[List[Orgnode]], Dict[Orgnode, str]]:
        "Extract org nodes from specified org files"
        entries: List[List[Orgnode]] = []
        entry_to_file_map: List[Tuple[Orgnode, str]] = []
        file_to_text_map = {}
        for org_file in org_files:
            try:
                org_content = org_files[org_file]
                entries, entry_to_file_map = OrgToEntries.process_single_org_file(
                    org_content, org_file, entries, entry_to_file_map, max_tokens
                )
                file_to_text_map[org_file] = org_content
            except Exception as e:
                logger.error(f"Unable to process file: {org_file}. Skipped indexing it.\nError; {e}", exc_info=True)

        return file_to_text_map, entries, dict(entry_to_file_map)

    @staticmethod
    def process_single_org_file(
        org_content: str,
        org_file: str,
        entries: List[List[Orgnode]],
        entry_to_file_map: List[Tuple[Orgnode, str]],
        max_tokens=256,
        ancestry: Dict[int, str] = {},
        start_line: int = 1,
    ) -> Tuple[List[List[Orgnode]], List[Tuple[Orgnode, str]]]:
        """Parse org_content from org_file into OrgNode entries

        Recurse down org file entries, one heading level at a time,
        until reach a leaf entry or the current entry tree fits max_tokens.

        Parse recursion terminating entry (trees) into (a list of) OrgNode objects.
        """
        # Prepend the org section's heading ancestry
        ancestry_string = "\n".join([f"{'*' * key} {ancestry[key]}" for key in sorted(ancestry.keys())])
        org_content_with_ancestry = f"{ancestry_string}{org_content}"

        # If content is small or content has no children headings, save it as a single entry
        # Note: This is the terminating condition for this recursive function
        if len(TextToEntries.tokenizer(org_content_with_ancestry)) <= max_tokens or not re.search(
            rf"^\*{{{len(ancestry) + 1},}}\s", org_content, re.MULTILINE
        ):
            orgnode_content_with_ancestry = orgnode.makelist(
                org_content_with_ancestry, org_file, start_line=start_line, ancestry_lines=len(ancestry)
            )
            entry_to_file_map += zip(orgnode_content_with_ancestry, [org_file] * len(orgnode_content_with_ancestry))
            entries.extend([orgnode_content_with_ancestry])
            return entries, entry_to_file_map

        # Split this entry tree into sections by the next heading level in it
        # Increment heading level until able to split entry into sections or reach max heading level
        # A successful split will result in at least 2 sections
        max_heading_level = 100
        next_heading_level = len(ancestry)
        sections: List[str] = []
        while len(sections) < 2 and next_heading_level < max_heading_level:
            next_heading_level += 1
            sections = re.split(rf"(\n|^)(?=[*]{{{next_heading_level}}} .+\n?)", org_content, flags=re.MULTILINE)

        # If unable to split entry into sections, log error and skip indexing it
        if next_heading_level == max_heading_level:
            logger.error(f"Unable to split current entry chunk: {org_content_with_ancestry[:20]}. Skip indexing it.")
            return entries, entry_to_file_map

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
            first_non_empty_line = [line for line in section.split("\n") if line.strip() != ""][0]
            # If first non-empty line is a heading with expected heading level
            if re.search(rf"^\*{{{next_heading_level}}}\s", first_non_empty_line):
                # Extract the section body without the heading
                current_section_heading, current_section_body = section.split(first_non_empty_line, 1)
                current_section_body_offset = current_section_heading.count("\n")
                # Parse the section heading into current section ancestry
                current_section_title = first_non_empty_line[next_heading_level:].strip()
                current_ancestry[next_heading_level] = current_section_title
                recursive_start_line = section_start_line_in_file + current_section_body_offset
            # Else process the section as just body text
            else:
                current_section_body = section
                recursive_start_line = section_start_line_in_file

            # Recurse down children of the current entry
            OrgToEntries.process_single_org_file(
                current_section_body,
                org_file,
                entries,
                entry_to_file_map,
                max_tokens,
                current_ancestry,
                start_line=recursive_start_line,
            )
            current_line_offset += num_lines_in_section

        return entries, entry_to_file_map

    @staticmethod
    def convert_org_nodes_to_entries(
        parsed_entries: List[List[Orgnode]],
        entry_to_file_map: Dict[Orgnode, str],
        index_heading_entries: bool = False,
    ) -> List[Entry]:
        """
        Convert OrgNode lists into list of Entry objects

        Each list of OrgNodes is a parsed parent org tree or leaf node.
        Convert each list of these OrgNodes into a single Entry.
        """
        entries: List[Entry] = []
        for entry_group in parsed_entries:
            entry_heading, entry_compiled, entry_raw = "", "", ""
            for parsed_entry in entry_group:
                if not parsed_entry.hasBody and not index_heading_entries:
                    # Ignore title notes i.e notes with just headings and empty body
                    continue

                todo_str = f"{parsed_entry.todo} " if parsed_entry.todo else ""

                # Set base level to current org-node tree's root heading level
                if not entry_heading and parsed_entry.level > 0:
                    base_level = parsed_entry.level
                # Indent entry by 1 heading level as ancestry is prepended as top level heading
                heading = f"{'*' * (parsed_entry.level - base_level + 2)} {todo_str}" if parsed_entry.level > 0 else ""
                if parsed_entry.heading:
                    heading += f"{parsed_entry.heading}."

                # Prepend ancestor headings, filename as top heading to root parent entry for context
                # Children nodes do not need ancestors trail as root parent node will have it
                if not entry_heading:
                    ancestors_trail = " / ".join(parsed_entry.ancestors) or Path(entry_to_file_map[parsed_entry])
                    heading = f"* {ancestors_trail}\n{heading}" if heading else f"* {ancestors_trail}."

                compiled = heading

                if parsed_entry.tags:
                    tags_str = " ".join(parsed_entry.tags)
                    compiled += f"\t {tags_str}."

                if parsed_entry.closed:
                    compiled += f"\n Closed on {parsed_entry.closed.strftime('%Y-%m-%d')}."

                if parsed_entry.scheduled:
                    compiled += f"\n Scheduled for {parsed_entry.scheduled.strftime('%Y-%m-%d')}."

                if parsed_entry.hasBody:
                    compiled += f"\n {parsed_entry.body}"

                uri = parsed_entry.properties.pop("LINE", None)

                # Add the sub-entry contents to the entry
                entry_compiled += compiled
                entry_raw += f"{parsed_entry}"
                if not entry_heading:
                    entry_heading = heading

            if entry_compiled:
                entries.append(
                    Entry(
                        compiled=entry_compiled,
                        raw=entry_raw,
                        heading=entry_heading,
                        file=entry_to_file_map[parsed_entry],
                        uri=uri,
                    )
                )

        return entries
