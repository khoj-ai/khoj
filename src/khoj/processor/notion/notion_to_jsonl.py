# Standard Packages
import logging

# External Packages
import requests

# Internal Packages
from khoj.utils.helpers import timer
from khoj.utils.rawconfig import Entry, NotionContentConfig
from khoj.processor.text_to_jsonl import TextToJsonl
from khoj.utils.jsonl import compress_jsonl_data
from khoj.utils.rawconfig import Entry

from enum import Enum


logger = logging.getLogger(__name__)


class NotionBlockType(Enum):
    PARAGRAPH = "paragraph"
    HEADING_1 = "heading_1"
    HEADING_2 = "heading_2"
    HEADING_3 = "heading_3"
    BULLETED_LIST_ITEM = "bulleted_list_item"
    NUMBERED_LIST_ITEM = "numbered_list_item"
    TO_DO = "to_do"
    TOGGLE = "toggle"
    CHILD_PAGE = "child_page"
    UNSUPPORTED = "unsupported"
    BOOKMARK = "bookmark"
    DIVIDER = "divider"
    PDF = "pdf"
    IMAGE = "image"
    EMBED = "embed"
    VIDEO = "video"
    FILE = "file"
    SYNCED_BLOCK = "synced_block"
    TABLE_OF_CONTENTS = "table_of_contents"
    COLUMN = "column"
    EQUATION = "equation"
    LINK_PREVIEW = "link_preview"
    COLUMN_LIST = "column_list"
    QUOTE = "quote"
    BREADCRUMB = "breadcrumb"
    LINK_TO_PAGE = "link_to_page"
    CHILD_DATABASE = "child_database"
    TEMPLATE = "template"
    CALLOUT = "callout"


class NotionToJsonl(TextToJsonl):
    def __init__(self, config: NotionContentConfig):
        super().__init__(config)
        self.config = config
        self.session = requests.Session()
        self.session.headers.update({"Authorization": f"Bearer {config.token}", "Notion-Version": "2022-02-22"})
        self.unsupported_block_types = [
            NotionBlockType.BOOKMARK.value,
            NotionBlockType.DIVIDER.value,
            NotionBlockType.CHILD_DATABASE.value,
            NotionBlockType.TEMPLATE.value,
            NotionBlockType.CALLOUT.value,
            NotionBlockType.UNSUPPORTED.value,
        ]

        self.display_block_block_types = [
            NotionBlockType.PARAGRAPH.value,
            NotionBlockType.HEADING_1.value,
            NotionBlockType.HEADING_2.value,
            NotionBlockType.HEADING_3.value,
            NotionBlockType.BULLETED_LIST_ITEM.value,
            NotionBlockType.NUMBERED_LIST_ITEM.value,
            NotionBlockType.TO_DO.value,
            NotionBlockType.TOGGLE.value,
            NotionBlockType.CHILD_PAGE.value,
            NotionBlockType.BOOKMARK.value,
            NotionBlockType.DIVIDER.value,
        ]

        self.body_params = {"page_size": 100}

    def process(self, previous_entries=[]):
        current_entries = []

        # Get all pages
        with timer("Getting all pages via search endpoint", logger=logger):
            responses = []

            while True:
                result = self.session.post(
                    "https://api.notion.com/v1/search",
                    json=self.body_params,
                ).json()
                responses.append(result)
                if result["has_more"] == False:
                    break
                else:
                    self.body_params.update({"start_cursor": result["next_cursor"]})

        for response in responses:
            with timer("Processing response", logger=logger):
                pages_or_databases = response["results"]

                # Get all pages content
                for p_or_d in pages_or_databases:
                    with timer(f"Processing {p_or_d['object']} {p_or_d['id']}", logger=logger):
                        if p_or_d["object"] == "database":
                            # TODO: Handle databases
                            continue
                        elif p_or_d["object"] == "page":
                            page_entries = self.process_page(p_or_d)
                            current_entries.extend(page_entries)

        return self.update_entries_with_ids(current_entries, previous_entries)

    def process_page(self, page):
        page_id = page["id"]
        title, content = self.get_page_content(page_id)

        if title == None or content == None:
            return []

        current_entries = []
        curr_heading = ""
        for block in content["results"]:
            block_type = block.get("type")

            if block_type == None:
                continue
            block_data = block[block_type]

            if block_data.get("rich_text") == None or len(block_data["rich_text"]) == 0:
                # There's no text to handle here.
                continue

            raw_content = ""
            if block_type in ["heading_1", "heading_2", "heading_3"]:
                # If the current block is a heading, we can consider the previous block processing completed.
                # Add it as an entry and move on to processing the next chunk of the page.
                if raw_content != "":
                    current_entries.append(
                        Entry(
                            compiled=raw_content,
                            raw=raw_content,
                            heading=title,
                            file=page["url"],
                        )
                    )
                curr_heading = block_data["rich_text"][0]["plain_text"]
            else:
                if curr_heading != "":
                    # Add the last known heading to the content for additional context
                    raw_content = self.process_heading(curr_heading)
            for text in block_data["rich_text"]:
                raw_content += self.process_text(text)

            if block.get("has_children", True):
                raw_content += "\n"
                raw_content = self.process_nested_children(
                    self.get_block_children(block["id"]), raw_content, block_type
                )

            if raw_content != "":
                current_entries.append(
                    Entry(
                        compiled=raw_content,
                        raw=raw_content,
                        heading=title,
                        file=page["url"],
                    )
                )
        return current_entries

    def process_heading(self, heading):
        return f"\n<b>{heading}</b>\n"

    def process_nested_children(self, children, raw_content, block_type=None):
        results = children["results"] if children.get("results") else []
        for child in results:
            child_type = child.get("type")
            if child_type == None:
                continue
            child_data = child[child_type]
            if child_data.get("rich_text") and len(child_data["rich_text"]) > 0:
                for text in child_data["rich_text"]:
                    raw_content += self.process_text(text, block_type)
            if child_data.get("has_children", True):
                return self.process_nested_children(self.get_block_children(child["id"]), raw_content, block_type)

        return raw_content

    def process_text(self, text, block_type=None):
        text_type = text.get("type", None)
        if text_type in self.unsupported_block_types:
            return ""
        if text.get("href", None):
            return f"<a href='{text['href']}'>{text['plain_text']}</a>"
        raw_text = text["plain_text"]
        if text_type in self.display_block_block_types or block_type in self.display_block_block_types:
            return f"\n{raw_text}\n"
        return raw_text

    def get_block_children(self, block_id):
        try:
            return self.session.get(f"https://api.notion.com/v1/blocks/{block_id}/children").json()
        except Exception as e:
            logger.error(f"Error getting children for block {block_id}: {e}")
            return {}

    def get_page(self, page_id):
        return self.session.get(f"https://api.notion.com/v1/pages/{page_id}").json()

    def get_page_children(self, page_id):
        return self.session.get(f"https://api.notion.com/v1/blocks/{page_id}/children").json()

    def get_page_content(self, page_id):
        try:
            page = self.get_page(page_id)
            content = self.get_page_children(page_id)
        except Exception as e:
            logger.error(f"Error getting page {page_id}: {e}", exc_info=True)
            return None, None
        properties = page["properties"]
        title_field = "title"
        if "Title" in properties:
            title_field = "Title"
        elif "Name" in properties:
            title_field = "Name"
        elif "Page" in properties:
            title_field = "Page"
        elif "Event" in properties:
            title_field = "Event"
        elif title_field not in properties:
            logger.error(f"Page {page_id} does not have a title field")
            return None, None
        title = page["properties"][title_field]["title"][0]["text"]["content"]
        return title, content

    def update_entries_with_ids(self, current_entries, previous_entries):
        # Identify, mark and merge any new entries with previous entries
        with timer("Identify new or updated entries", logger):
            entries_with_ids = TextToJsonl.mark_entries_for_update(
                current_entries, previous_entries, key="compiled", logger=logger
            )

        with timer("Write Notion entries to JSONL file", logger):
            # Process Each Entry from all Notion entries
            entries = list(map(lambda entry: entry[1], entries_with_ids))
            jsonl_data = TextToJsonl.convert_text_maps_to_jsonl(entries)

            # Compress JSONL formatted Data
            compress_jsonl_data(jsonl_data, self.config.compressed_jsonl)

        return entries_with_ids
