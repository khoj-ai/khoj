import logging

from khoj.database.models import GithubConfig
from khoj.processor.content.github.github_to_entries import GithubToEntries

logger = logging.getLogger(__name__)


def github_sync_task():
    """
    This module contains the GitHub sync task that runs periodically to sync
    GitHub repositories with the Khoj database.
    If the task returns False, it will stop running.
    If the task returns True, it will continue running.
    """

    logger.info("Running GitHub sync...")

    # Fetch all GitHub configurations
    configs = GithubConfig.objects.all()
    if configs:
        for config in configs:
            GithubToEntries(config).process(files={}, user=config.user, regenerate=False)

    return True
