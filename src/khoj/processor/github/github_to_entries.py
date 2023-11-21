# Standard Packages
import logging
import time
from datetime import datetime
from typing import Dict, List, Union, Tuple

# External Packages
import requests

# Internal Packages
from khoj.utils.helpers import timer
from khoj.utils.rawconfig import Entry, GithubContentConfig, GithubRepoConfig
from khoj.processor.markdown.markdown_to_entries import MarkdownToEntries
from khoj.processor.org_mode.org_to_entries import OrgToEntries
from khoj.processor.text_to_entries import TextToEntries
from khoj.database.models import Entry as DbEntry, GithubConfig, KhojUser


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

        with timer(f"Extract commit messages from github repo {repo_shorthand}", logger):
            current_entries += self.convert_commits_to_entries(self.get_commits(repo_url), repo)

        with timer(f"Extract issues from github repo {repo_shorthand}", logger):
            issue_entries = GithubToEntries.convert_issues_to_entries(
                *GithubToEntries.extract_github_issues(self.get_issues(repo_url))
            )
            current_entries += issue_entries

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
        markdown_files = []
        org_files = []
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

    def get_commits(self, repo_url: str) -> List[Dict]:
        return self._get_commits(f"{repo_url}/commits")

    def _get_commits(self, commits_url: Union[str, None]) -> List[Dict]:
        # Get commit messages from the repository using the Github API
        params = {"per_page": 100}
        commits = []

        while commits_url is not None:
            # Get the next page of commits
            response = self.session.get(commits_url, params=params, stream=True)

            # Read the streamed response into a JSON object
            content = response.json()

            # Wait for rate limit reset if needed
            result = self.wait_for_rate_limit_reset(response, self._get_commits, commits_url)
            if result is not None:
                return result

            # Extract commit messages from the response
            for commit in content:
                commits += [{"content": commit["commit"]["message"], "path": commit["html_url"]}]

            # Get the URL for the next page of commits, if any
            commits_url = response.links.get("next", {}).get("url")

        return commits

    def get_issues(self, repo_url: str) -> List[Dict]:
        return self._get_issues(f"{repo_url}/issues")

    def _get_issues(self, issues_url: Union[str, None]) -> List[Dict]:
        issues = []
        per_page = 100
        params = {"per_page": per_page, "state": "all"}

        while issues_url is not None:
            # Get the next page of issues
            response = self.session.get(issues_url, params=params, stream=True)  # type: ignore
            raw_issues = response.json()

            # Wait for rate limit reset if needed
            result = self.wait_for_rate_limit_reset(response, self._get_issues, issues_url)
            if result is not None:
                return result

            for issue in raw_issues:
                username = issue["user"]["login"]
                user_url = f"[{username}]({issue['user']['html_url']})"
                issue_content = {
                    "content": f"## [Issue {issue['number']}]({issue['html_url']}) {issue['title']}\nby {user_url}\n\n{issue['body']}",
                    "path": issue["html_url"],
                }
                issue_content["created_at"] = {issue["created_at"]}
                if issue["comments"] > 0:
                    issue_content["comments"] = self.get_comments(issue["comments_url"])
                issues += [issue_content]

            issues_url = response.links.get("next", {}).get("url")

        return issues

    def get_comments(self, comments_url: Union[str, None]) -> List[Dict]:
        # By default, the number of results per page is 30. We'll keep it as-is for now.
        comments = []
        per_page = 100
        params = {"per_page": per_page}

        while comments_url is not None:
            # Get the next page of comments
            response = self.session.get(comments_url, params=params, stream=True)
            raw_comments = response.json()

            # Wait for rate limit reset if needed
            result = self.wait_for_rate_limit_reset(response, self.get_comments, comments_url)
            if result is not None:
                return result

            for comment in raw_comments:
                created_at = datetime.strptime(comment["created_at"], "%Y-%m-%dT%H:%M:%SZ").strftime("%Y-%m-%d %H:%M")
                commenter = comment["user"]["login"]
                commenter_url = comment["user"]["html_url"]
                comment_url = comment["html_url"]
                comment_url_link = f"[{created_at}]({comment_url})"
                avatar_url = comment["user"]["avatar_url"]
                avatar = f"![{commenter}]({avatar_url})"
                comments += [
                    {
                        "content": f"### {avatar} [{commenter}]({commenter_url}) - ({comment_url_link})\n\n{comment['body']}"
                    }
                ]

            comments_url = response.links.get("next", {}).get("url")

        return comments

    def convert_commits_to_entries(self, commits, repo: GithubRepoConfig) -> List[Entry]:
        entries: List[Entry] = []
        for commit in commits:
            compiled = f'Commit message from {repo.owner}/{repo.name}:\n{commit["content"]}'
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
    def extract_github_issues(issues):
        entries = []
        entry_to_file_map = {}
        for issue in issues:
            content = issue["content"]
            if "comments" in issue:
                for comment in issue["comments"]:
                    content += "\n\n" + comment["content"]
            entries.append(content)
            entry_to_file_map[content] = {"path": issue["path"]}
        return entries, entry_to_file_map

    @staticmethod
    def convert_issues_to_entries(parsed_entries: List[str], entry_to_metadata_map: Dict[str, Dict]) -> List[Entry]:
        entries = []
        for entry in parsed_entries:
            entry_file_name = entry_to_metadata_map[entry]["path"]
            entries.append(
                Entry(
                    compiled=entry,
                    raw=entry,
                    heading=entry.split("\n")[0],
                    file=entry_file_name,
                )
            )

        return entries
