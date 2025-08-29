import logging
import time
from typing import Dict, List, Tuple

import requests
from magika import Magika

from khoj.database.models import Entry as DbEntry
from khoj.database.models import GithubConfig, KhojUser
from khoj.processor.content.markdown.markdown_to_entries import MarkdownToEntries
from khoj.processor.content.org_mode.org_to_entries import OrgToEntries
from khoj.processor.content.plaintext.plaintext_to_entries import PlaintextToEntries
from khoj.processor.content.text_to_entries import TextToEntries
from khoj.utils.helpers import is_none_or_empty, timer
from khoj.utils.rawconfig import GithubContentConfig, GithubRepoConfig

logger = logging.getLogger(__name__)
magika = Magika()


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
        if not is_none_or_empty(self.config.pat_token):
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

    def process(self, files: dict[str, str], user: KhojUser, regenerate: bool = False) -> Tuple[int, int]:
        if is_none_or_empty(self.config.pat_token):
            logger.warning(
                "Github PAT token is not set. Private repositories cannot be indexed and lower rate limits apply."
            )
        current_entries = []
        for repo in self.config.repos:
            current_entries += self.process_repo(repo)

        return self.update_entries_with_ids(current_entries, user=user)

    def process_repo(self, repo: GithubRepoConfig):
        repo_url = f"https://api.github.com/repos/{repo.owner}/{repo.name}"
        repo_shorthand = f"{repo.owner}/{repo.name}"
        logger.info(f"Processing github repo {repo_shorthand}")
        with timer("Download files from github repo", logger):
            try:
                markdown_files, org_files, plaintext_files = self.get_files(repo_url, repo)
            except ConnectionAbortedError as e:
                logger.error(f"Github rate limit reached. Skip indexing github repo {repo_shorthand}")
                raise e
            except Exception as e:
                logger.error(f"Unable to download github repo {repo_shorthand}", exc_info=True)
                raise e

        logger.info(
            f"Found {len(markdown_files)} md, {len(org_files)} org and {len(plaintext_files)} text files in github repo {repo_shorthand}"
        )
        current_entries = []

        with timer(f"Extract markdown entries from github repo {repo_shorthand}", logger):
            current_entries = MarkdownToEntries.convert_markdown_entries_to_maps(
                *GithubToEntries.extract_markdown_entries(markdown_files)
            )

        with timer(f"Extract org entries from github repo {repo_shorthand}", logger):
            current_entries += OrgToEntries.convert_org_nodes_to_entries(
                *GithubToEntries.extract_org_entries(org_files)
            )

        with timer(f"Extract plaintext entries from github repo {repo_shorthand}", logger):
            current_entries += PlaintextToEntries.convert_text_files_to_entries(
                *GithubToEntries.extract_plaintext_entries(plaintext_files)
            )

        with timer(f"Split entries by max token size supported by model {repo_shorthand}", logger):
            current_entries = TextToEntries.split_entries_by_max_tokens(current_entries, max_tokens=256)

        return current_entries

    def update_entries_with_ids(self, current_entries, user: KhojUser = None):
        # Identify, mark and merge any new entries with previous entries
        with timer("Identify new or updated entries", logger):
            num_new_embeddings, num_deleted_embeddings = self.update_embeddings(
                user,
                current_entries,
                DbEntry.EntryType.GITHUB,
                DbEntry.EntrySource.GITHUB,
                key="compiled",
                logger=logger,
            )

        return num_new_embeddings, num_deleted_embeddings

    def get_files(self, repo_url: str, repo: GithubRepoConfig):
        # Get the contents of the repository
        repo_content_url = f"{repo_url}/git/trees/{repo.branch}"
        headers = {}
        if not is_none_or_empty(self.config.pat_token):
            headers = {"Authorization": f"token {self.config.pat_token}"}
        params = {"recursive": "true"}
        response = requests.get(repo_content_url, headers=headers, params=params)
        contents = response.json()

        # Raise exception if hit rate limit
        if response.status_code != 200 and response.headers.get("X-RateLimit-Remaining") == "0":
            raise ConnectionAbortedError("Github rate limit reached")

        # Extract markdown files from the repository
        markdown_files: List[Dict[str, str]] = []
        org_files: List[Dict[str, str]] = []
        plaintext_files: List[Dict[str, str]] = []
        if "tree" not in contents:
            return markdown_files, org_files, plaintext_files

        for item in contents["tree"]:
            # Find all markdown files in the repository
            if item["type"] == "blob" and item["path"].endswith(".md"):
                # Create URL for each markdown file on Github
                url_path = f"https://github.com/{repo.owner}/{repo.name}/blob/{repo.branch}/{item['path']}"

                # Add markdown file contents and URL to list
                markdown_files += [{"content": self.get_file_contents(item["url"]), "path": url_path}]

            # Find all org files in the repository
            elif item["type"] == "blob" and item["path"].endswith(".org"):
                # Create URL for each org file on Github
                url_path = f"https://github.com/{repo.owner}/{repo.name}/blob/{repo.branch}/{item['path']}"

                # Add org file contents and URL to list
                org_files += [{"content": self.get_file_contents(item["url"]), "path": url_path}]

            # Find, index remaining non-binary files in the repository
            elif item["type"] == "blob":
                url_path = f"https://github.com/{repo.owner}/{repo.name}/blob/{repo.branch}/{item['path']}"
                content_bytes = self.get_file_contents(item["url"], decode=False)
                content_type, content_str = None, None
                try:
                    content_type = magika.identify_bytes(content_bytes).output.group
                except Exception:
                    logger.error(f"Unable to identify content type of file at {url_path}. Skip indexing it")
                    continue

                # Add non-binary file contents and URL to list
                if content_type in ["text", "code"]:
                    try:
                        content_str = content_bytes.decode("utf-8")
                    except Exception:
                        logger.error(f"Unable to decode content of file at {url_path}. Skip indexing it")
                        continue
                    plaintext_files += [{"content": content_str, "path": url_path}]

        return markdown_files, org_files, plaintext_files

    def get_file_contents(self, file_url, decode=True):
        # Get text from each markdown file
        headers = {"Accept": "application/vnd.github.v3.raw"}
        response = self.session.get(file_url, headers=headers, stream=True)

        # Stop indexing on hitting rate limit
        if response.status_code != 200 and response.headers.get("X-RateLimit-Remaining") == "0":
            raise ConnectionAbortedError("Github rate limit reached")

        content = "" if decode else b""
        for chunk in response.iter_content(chunk_size=2048):
            if chunk:
                try:
                    content += chunk.decode("utf-8") if decode else chunk
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

    @staticmethod
    def extract_plaintext_entries(plaintext_files):
        entries = []
        entry_to_file_map = []

        for doc in plaintext_files:
            entries, entry_to_file_map = PlaintextToEntries.process_single_plaintext_file(
                doc["content"], doc["path"], entries, entry_to_file_map
            )
        return entries, dict(entry_to_file_map)
