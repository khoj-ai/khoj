import logging
import time
from typing import Any, List, Tuple

import requests

from khoj.database.models import Entry as DbEntry
from khoj.database.models import GithubConfig, KhojUser
from khoj.processor.content.markdown.markdown_to_entries import MarkdownToEntries
from khoj.processor.content.org_mode.org_to_entries import OrgToEntries
from khoj.processor.content.text_to_entries import TextToEntries
from khoj.utils.helpers import timer
from khoj.utils.rawconfig import Entry, GithubContentConfig, GithubRepoConfig

logger = logging.getLogger(__name__)


class GithubToEntries(TextToEntries):
    def __init__(self, config: GithubConfig):
        super().__init__(config)
        raw_repos = config.githubrepoconfig.all()
        repos = []
        for repo in raw_repos:
            repos.append(
                GithubRepoConfig(
                    name=repo.name,
                    owner=repo.owner,
                    branch=repo.branch,
                )
            )
        self.config = GithubContentConfig(
            pat_token=config.pat_token,
            repos=repos,
        )
        self.session = requests.Session()
        self.session.headers.update({"Authorization": f"token {self.config.pat_token}"})

    @staticmethod
    def wait_for_rate_limit_reset(response, func, *args, **kwargs):
        if response.status_code != 200 and response.headers.get("X-RateLimit-Remaining") == "0":
            wait_time = int(response.headers.get("X-RateLimit-Reset")) - int(time.time())
            logger.info(f"Github Rate limit reached. Waiting for {wait_time} seconds")
            time.sleep(wait_time)
            return func(*args, **kwargs)
        else:
            return

    def process(
        self, files: dict[str, str] = None, full_corpus: bool = True, user: KhojUser = None, regenerate: bool = False
    ) -> Tuple[int, int]:
        if self.config.pat_token is None or self.config.pat_token == "":
            logger.error(f"Github PAT token is not set. Skipping github content")
            raise ValueError("Github PAT token is not set. Skipping github content")
        current_entries = []
        for repo in self.config.repos:
            current_entries += self.process_repo(repo)

        return self.update_entries_with_ids(current_entries, user=user)

    def process_repo(self, repo: GithubRepoConfig):
        repo_url = f"https://api.github.com/repos/{repo.owner}/{repo.name}"
        repo_shorthand = f"{repo.owner}/{repo.name}"
        logger.info(f"Processing github repo {repo_shorthand}")
        with timer("Download markdown files from github repo", logger):
            try:
                markdown_files, org_files = self.get_files(repo_url, repo)
            except Exception as e:
                logger.error(f"Unable to download github repo {repo_shorthand}", exc_info=True)
                raise e

        logger.info(f"Found {len(markdown_files)} markdown files in github repo {repo_shorthand}")
        logger.info(f"Found {len(org_files)} org files in github repo {repo_shorthand}")
        current_entries = []

        with timer(f"Extract markdown entries from github repo {repo_shorthand}", logger):
            current_entries = MarkdownToEntries.convert_markdown_entries_to_maps(
                *GithubToEntries.extract_markdown_entries(markdown_files)
            )

        with timer(f"Extract org entries from github repo {repo_shorthand}", logger):
            current_entries += OrgToEntries.convert_org_nodes_to_entries(
                *GithubToEntries.extract_org_entries(org_files)
            )

        with timer(f"Split entries by max token size supported by model {repo_shorthand}", logger):
            current_entries = TextToEntries.split_entries_by_max_tokens(current_entries, max_tokens=256)

        return current_entries

    def update_entries_with_ids(self, current_entries, user: KhojUser = None):
        # Identify, mark and merge any new entries with previous entries
        with timer("Identify new or updated entries", logger):
            num_new_embeddings, num_deleted_embeddings = self.update_embeddings(
                current_entries,
                DbEntry.EntryType.GITHUB,
                DbEntry.EntrySource.GITHUB,
                key="compiled",
                logger=logger,
                user=user,
            )

        return num_new_embeddings, num_deleted_embeddings

    def get_files(self, repo_url: str, repo: GithubRepoConfig):
        # Get the contents of the repository
        repo_content_url = f"{repo_url}/git/trees/{repo.branch}"
        headers = {"Authorization": f"token {self.config.pat_token}"}
        params = {"recursive": "true"}
        response = requests.get(repo_content_url, headers=headers, params=params)
        contents = response.json()

        # Wait for rate limit reset if needed
        result = self.wait_for_rate_limit_reset(response, self.get_files, repo_url, repo)
        if result is not None:
            return result

        # Extract markdown files from the repository
        markdown_files: List[Any] = []
        org_files: List[Any] = []
        if "tree" not in contents:
            return markdown_files, org_files

        for item in contents["tree"]:
            # Find all markdown files in the repository
            if item["type"] == "blob" and item["path"].endswith(".md"):
                # Create URL for each markdown file on Github
                url_path = f'https://github.com/{repo.owner}/{repo.name}/blob/{repo.branch}/{item["path"]}'

                # Add markdown file contents and URL to list
                markdown_files += [{"content": self.get_file_contents(item["url"]), "path": url_path}]

            # Find all org files in the repository
            elif item["type"] == "blob" and item["path"].endswith(".org"):
                # Create URL for each org file on Github
                url_path = f'https://github.com/{repo.owner}/{repo.name}/blob/{repo.branch}/{item["path"]}'

                # Add org file contents and URL to list
                org_files += [{"content": self.get_file_contents(item["url"]), "path": url_path}]

        return markdown_files, org_files

    def get_file_contents(self, file_url):
        # Get text from each markdown file
        headers = {"Accept": "application/vnd.github.v3.raw"}
        response = self.session.get(file_url, headers=headers, stream=True)

        # Wait for rate limit reset if needed
        result = self.wait_for_rate_limit_reset(response, self.get_file_contents, file_url)
        if result is not None:
            return result

        content = ""
        for chunk in response.iter_content(chunk_size=2048):
            if chunk:
                try:
                    content += chunk.decode("utf-8")
                except Exception as e:
                    logger.error(f"Unable to decode chunk from {file_url}")
                    logger.error(e)

        return content

    @staticmethod
    def extract_markdown_entries(markdown_files):
        entries = []
        entry_to_file_map = []
        for doc in markdown_files:
            entries, entry_to_file_map = MarkdownToEntries.process_single_markdown_file(
                doc["content"], doc["path"], entries, entry_to_file_map
            )
        return entries, dict(entry_to_file_map)

    @staticmethod
    def extract_org_entries(org_files):
        entries = []
        entry_to_file_map = []

        for doc in org_files:
            entries, entry_to_file_map = OrgToEntries.process_single_org_file(
                doc["content"], doc["path"], entries, entry_to_file_map
            )
        return entries, dict(entry_to_file_map)
