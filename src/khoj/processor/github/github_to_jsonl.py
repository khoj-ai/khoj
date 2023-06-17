# Standard Packages
import logging
import time
from typing import Dict, List

# External Packages
import requests

# Internal Packages
from khoj.utils.helpers import timer
from khoj.utils.rawconfig import Entry, GithubContentConfig
from khoj.processor.markdown.markdown_to_jsonl import MarkdownToJsonl
from khoj.processor.text_to_jsonl import TextToJsonl
from khoj.utils.jsonl import dump_jsonl, compress_jsonl_data


logger = logging.getLogger(__name__)


class GithubToJsonl(TextToJsonl):
    def __init__(self, config: GithubContentConfig):
        super().__init__(config)
        self.config = config
        self.repo_url = f"https://api.github.com/repos/{self.config.repo_owner}/{self.config.repo_name}"

    def process(self, previous_entries=None):
        with timer("Download markdown files from github repo", logger):
            try:
                docs = self.get_markdown_files()
            except Exception as e:
                logger.error(f"Unable to download github repo for {self.config.repo_owner}/{self.config.repo_name}")
                raise e

        logger.info(f"Found {len(docs)} documents in {self.config.repo_owner}/{self.config.repo_name}")

        with timer("Extract markdown entries from github repo", logger):
            current_entries = MarkdownToJsonl.convert_markdown_entries_to_maps(
                *GithubToJsonl.extract_markdown_entries(docs)
            )

        with timer("Extract commit messages from github repo", logger):
            current_entries += self.convert_commits_to_entries(self.get_commits())

        with timer("Split entries by max token size supported by model", logger):
            current_entries = TextToJsonl.split_entries_by_max_tokens(current_entries, max_tokens=256)

        # Identify, mark and merge any new entries with previous entries
        with timer("Identify new or updated entries", logger):
            if not previous_entries:
                entries_with_ids = list(enumerate(current_entries))
            else:
                entries_with_ids = TextToJsonl.mark_entries_for_update(
                    current_entries, previous_entries, key="compiled", logger=logger
                )

        with timer("Write markdown entries to JSONL file", logger):
            # Process Each Entry from All Notes Files
            entries = list(map(lambda entry: entry[1], entries_with_ids))
            jsonl_data = MarkdownToJsonl.convert_markdown_maps_to_jsonl(entries)

            # Compress JSONL formatted Data
            if self.config.compressed_jsonl.suffix == ".gz":
                compress_jsonl_data(jsonl_data, self.config.compressed_jsonl)
            elif self.config.compressed_jsonl.suffix == ".jsonl":
                dump_jsonl(jsonl_data, self.config.compressed_jsonl)

        return entries_with_ids

    def get_markdown_files(self):
        # set the url to get the contents of the repository
        repo_content_url = f"{self.repo_url}/git/trees/{self.config.repo_branch}"
        # set the headers to include the authentication token
        headers = {"Authorization": f"{self.config.pat_token}"}

        # get the contents of the repository
        response = requests.get(repo_content_url, headers=headers)
        contents = response.json()

        # If the rate limit is reached, wait for the reset time
        if response.status_code != 200 and response.headers.get("X-RateLimit-Remaining") == "0":
            wait_time = int(response.headers.get("X-RateLimit-Reset")) - int(time.time())
            logger.info(f"Github Rate limit reached. Waiting for {wait_time} seconds")
            time.sleep(wait_time)
            return self.get_markdown_files()

        markdown_files = []
        for item in contents["tree"]:
            # Find all markdown files in the repository
            if item["type"] == "blob" and item["path"].endswith(".md"):
                # Get text from each markdown file
                file_content_url = f'{self.repo_url}/contents/{item["path"]}'
                headers["Accept"] = "application/vnd.github.v3.raw"
                markdown_file_contents = requests.get(file_content_url, headers=headers).content.decode("utf-8")
                markdown_files += [{"content": markdown_file_contents, "path": item["path"]}]

        return markdown_files

    def get_commits(self) -> List[Dict]:
        # Get commit messages from the repository using the Github API
        headers = {"Authorization": f"{self.config.pat_token}"}
        commits_url = f"{self.repo_url}/commits"
        commits = []

        while commits_url is not None:
            # Get the next page of commits
            response = requests.get(commits_url, headers=headers)

            # If the rate limit is reached, wait for the reset time
            if response.status_code != 200 and response.headers.get("X-RateLimit-Remaining") == "0":
                wait_time = int(response.headers.get("X-RateLimit-Reset")) - int(time.time())
                logger.info(f"Github Rate limit reached. Waiting for {wait_time} seconds")
                time.sleep(wait_time)
                continue

            raw_commits = response.json()

            # Extract commit messages from the response
            for commit in raw_commits:
                commits += [{"content": commit["commit"]["message"], "path": commit["html_url"]}]

            # Get the URL for the next page of commits, if any
            commits_url = response.links.get("next", {}).get("url")

        return commits

    def convert_commits_to_entries(self, commits) -> List[Entry]:
        entries: List[Entry] = []
        for commit in commits:
            compiled = f'Commit message from {self.config.repo_owner}/{self.config.repo_name}:\n{commit["content"]}'
            entries.append(
                Entry(
                    compiled=compiled,
                    raw=f'### {commit["content"]}',
                    heading=commit["content"].split("\n")[0],
                    file=commit["path"],
                )
            )

        return entries

    @staticmethod
    def extract_markdown_entries(markdown_files):
        entries = []
        entry_to_file_map = []
        for doc in markdown_files:
            entries, entry_to_file_map = MarkdownToJsonl.process_single_markdown_file(
                doc["content"], doc["path"], entries, entry_to_file_map
            )
        return entries, dict(entry_to_file_map)
