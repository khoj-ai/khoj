# Standard Packages
import logging

# External Packages
import requests

# Internal Packages
from khoj.utils.helpers import timer
from khoj.utils.rawconfig import Entry, NotionContentConfig
from khoj.processor.text_to_jsonl import TextToJsonl
from khoj.utils.jsonl import dump_jsonl, compress_jsonl_data
from khoj.utils.rawconfig import Entry


logger = logging.getLogger(__name__)


class NotionToJsonl(TextToJsonl):
    def __init__(self, config: NotionContentConfig):
        super().__init__(config)
        self.config = config
        self.session = requests.Session()
        self.session.headers.update({"Authorization": f"Bearer {config.token}", "Notion-Version": "2022-02-22"})

    def process(self, previous_entries=None):
        current_entries = []

        # Get all pages
        with timer("Getting all pages via search endpoint", logger=logger):
            responses = []

            while True:
                result = self.session.post(
                    "https://api.notion.com/v1/search",
                    json={"page_size": 100},
                ).json()
                responses.append(result)
                if result["has_more"] == False:
                    break
                else:
                    self.session.params = {"start_cursor": responses[-1]["next_cursor"]}

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
                            page_id = p_or_d["id"]
                            title, content = self.get_page_content(page_id)
                            raw_content = ""
                            curr_heading = ""
                            for block in content["results"]:
                                block_type = block.get("type", None)
                                if block_type == None:
                                    continue
                                block_data = block[block_type]
                                raw_content = ""
                                if block_data.get("rich_text", None) and len(block_data["rich_text"]) > 0:
                                    if block_type in ["heading_1", "heading_2", "heading_3"]:
                                        if raw_content != "":
                                            current_entries.append(
                                                Entry(
                                                    compiled=raw_content,
                                                    raw=raw_content,
                                                    heading=title,
                                                    file=p_or_d["url"],
                                                )
                                            )
                                        curr_heading = block_data["rich_text"][0]["plain_text"]
                                    if curr_heading != "":
                                        raw_content = curr_heading + "\n"
                                    for text in block_data["rich_text"]:
                                        raw_content += text["plain_text"]
                                        raw_content += "\n"
                                if raw_content != "":
                                    current_entries.append(
                                        Entry(
                                            compiled=raw_content,
                                            raw=raw_content,
                                            heading=title,
                                            file=p_or_d["url"],
                                        )
                                    )

        return self.update_entries_with_ids(current_entries, previous_entries)

    def get_page(self, page_id):
        return self.session.get(f"https://api.notion.com/v1/pages/{page_id}").json()

    def get_page_children(self, page_id):
        return self.session.get(f"https://api.notion.com/v1/blocks/{page_id}/children").json()

    def get_page_content(self, page_id):
        page = self.get_page(page_id)
        content = self.get_page_children(page_id)
        title = page["properties"]["Title"]["title"][0]["text"]["content"]
        return title, content

    def update_entries_with_ids(self, current_entries, previous_entries):
        # Identify, mark and merge any new entries with previous entries
        with timer("Identify new or updated entries", logger):
            if not previous_entries:
                entries_with_ids = list(enumerate(current_entries))
            else:
                entries_with_ids = TextToJsonl.mark_entries_for_update(
                    current_entries, previous_entries, key="compiled", logger=logger
                )

        with timer("Write Notion entries to JSONL file", logger):
            # Process Each Entry from all Notion entries
            entries = list(map(lambda entry: entry[1], entries_with_ids))
            jsonl_data = TextToJsonl.convert_text_maps_to_jsonl(entries)

            # Compress JSONL formatted Data
            if self.config.compressed_jsonl.suffix == ".gz":
                compress_jsonl_data(jsonl_data, self.config.compressed_jsonl)
            elif self.config.compressed_jsonl.suffix == ".jsonl":
                dump_jsonl(jsonl_data, self.config.compressed_jsonl)

        return entries_with_ids
