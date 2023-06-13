import logging
from llama_index import download_loader
from khoj.utils.helpers import timer
from khoj.utils.rawconfig import GithubContentConfig
from llama_hub.github_repo import GithubRepositoryReader, GithubClient
from khoj.processor.markdown.markdown_to_jsonl import MarkdownToJsonl
from khoj.processor.text_to_jsonl import TextToJsonl
from khoj.utils.jsonl import dump_jsonl, compress_jsonl_data
from khoj.utils import state

logger = logging.getLogger(__name__)


class GithubToJsonl:
    def __init__(self, config: GithubContentConfig):
        self.config = config
        download_loader("GithubRepositoryReader")

    def process(self, previous_entries=None):
        try:
            self.initialize()
        except Exception as e:
            logger.error(
                f"Unable to initialize Github Repository Reader for {self.config.repo_owner}/{self.config.repo_name}"
            )
            raise e

        with timer("Download github repo", logger):
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

    def initialize(self):
        logger.info(f"Initializing Github Repository Reader for {self.config.repo_owner}/{self.config.repo_name}")
        github_client = GithubClient(self.config.pat_token)
        self.loader = GithubRepositoryReader(
            github_client,
            owner=self.config.repo_owner,
            repo=self.config.repo_name,
            filter_file_extensions=([".md"], GithubRepositoryReader.FilterType.INCLUDE),
            verbose=state.verbose > 1,
        )

    def get_markdown_files(self):
        return self.loader.load_data(branch=self.config.repo_branch)

    @staticmethod
    def extract_markdown_entries(markdown_files):
        entries = []
        entry_to_file_map = []
        for doc in markdown_files:
            entries, entry_to_file_map = MarkdownToJsonl.process_single_markdown_file(
                doc.get_text(), doc.extra_info.get("file_path"), entries, entry_to_file_map
            )
        return entries, dict(entry_to_file_map)
