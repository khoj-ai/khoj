"""
Folder watcher service for monitoring local folders and triggering content indexing.

Uses watchdog library to watch for file system changes and triggers re-indexing
when files are created, modified, or deleted.
"""

import logging
import os
import threading
from collections import defaultdict
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Callable, Optional, cast

from watchdog.events import (
    DirCreatedEvent,
    DirDeletedEvent,
    FileCreatedEvent,
    FileDeletedEvent,
    FileModifiedEvent,
    FileSystemEventHandler,
)
from watchdog.observers import Observer

logger = logging.getLogger(__name__)

# Supported file extensions for indexing (matching existing Khoj file types)
SUPPORTED_EXTENSIONS: set[str] = {
    # Markdown
    ".md",
    ".markdown",
    # Org mode
    ".org",
    # Plaintext
    ".txt",
    ".text",
    # PDF
    ".pdf",
    # Word documents
    ".docx",
    ".doc",
    # Images
    ".jpg",
    ".jpeg",
    ".png",
    ".webp",
}


def get_file_type_from_extension(extension: str) -> Optional[str]:
    """Map file extension to Khoj file type."""
    ext = extension.lower()
    if ext in {".md", ".markdown"}:
        return "markdown"
    elif ext == ".org":
        return "org"
    elif ext in {".txt", ".text"}:
        return "plaintext"
    elif ext == ".pdf":
        return "pdf"
    elif ext in {".docx", ".doc"}:
        return "docx"
    elif ext in {".jpg", ".jpeg", ".png", ".webp"}:
        return "image"
    return None


def is_supported_file(file_path: str) -> bool:
    """Check if a file has a supported extension for indexing."""
    ext = Path(file_path).suffix.lower()
    return ext in SUPPORTED_EXTENSIONS


class FolderEventHandler(FileSystemEventHandler):
    """
    Handles file system events for a watched folder.

    Collects events and calls the provided callback with batched changes
    to avoid excessive re-indexing.
    """

    def __init__(
        self,
        folder_path: str,
        user_id: int,
        on_change_callback: Callable[[int, str, dict], None],
        debounce_seconds: float = 2.0,
    ):
        """
        Initialize the event handler.

        Args:
            folder_path: The root folder being watched
            user_id: The user ID who owns this folder configuration
            on_change_callback: Callback function called with (user_id, folder_path, changes)
                where changes is a dict with keys 'created', 'modified', 'deleted'
                containing lists of file paths
            debounce_seconds: Time to wait before triggering callback after last event
        """
        super().__init__()
        self.folder_path = folder_path
        self.user_id = user_id
        self.on_change_callback = on_change_callback
        self.debounce_seconds = debounce_seconds

        # Pending changes to be processed
        self._pending_changes: dict[str, set[str]] = {
            "created": set(),
            "modified": set(),
            "deleted": set(),
        }
        self._lock = threading.Lock()
        self._debounce_timer: Optional[threading.Timer] = None

    def _schedule_callback(self):
        """Schedule the callback to be called after debounce period."""
        with self._lock:
            if self._debounce_timer:
                self._debounce_timer.cancel()
            self._debounce_timer = threading.Timer(
                self.debounce_seconds,
                self._trigger_callback,
            )
            self._debounce_timer.start()

    def _trigger_callback(self):
        """Trigger the callback with accumulated changes."""
        with self._lock:
            if not any(self._pending_changes.values()):
                return

            # Convert sets to lists and reset pending changes
            changes = {
                "created": list(self._pending_changes["created"]),
                "modified": list(self._pending_changes["modified"]),
                "deleted": list(self._pending_changes["deleted"]),
            }
            self._pending_changes = {
                "created": set(),
                "modified": set(),
                "deleted": set(),
            }

        # Call the callback outside the lock
        try:
            self.on_change_callback(self.user_id, self.folder_path, changes)
        except Exception as e:
            logger.error(f"Error in folder watcher callback: {e}", exc_info=True)

    def _handle_file_event(self, event_type: str, src_path: str):
        """Handle a file event by adding it to pending changes."""
        if not is_supported_file(src_path):
            return

        with self._lock:
            # If a file is created then modified, keep it as created
            # If a file is modified then deleted, just mark as deleted
            # If a file is created then deleted, remove from both
            if event_type == "deleted":
                self._pending_changes["created"].discard(src_path)
                self._pending_changes["modified"].discard(src_path)
                self._pending_changes["deleted"].add(src_path)
            elif event_type == "created":
                if src_path not in self._pending_changes["deleted"]:
                    self._pending_changes["created"].add(src_path)
            elif event_type == "modified":
                if src_path not in self._pending_changes["created"] and src_path not in self._pending_changes["deleted"]:
                    self._pending_changes["modified"].add(src_path)

        self._schedule_callback()

    def on_created(self, event):
        """Handle file/directory creation events."""
        if isinstance(event, FileCreatedEvent):
            logger.debug(f"File created: {event.src_path}")
            self._handle_file_event("created", event.src_path)
        elif isinstance(event, DirCreatedEvent):
            # Scan directory for supported files
            logger.debug(f"Directory created: {event.src_path}")
            self._scan_directory(event.src_path, "created")

    def on_modified(self, event):
        """Handle file modification events."""
        if isinstance(event, FileModifiedEvent):
            logger.debug(f"File modified: {event.src_path}")
            self._handle_file_event("modified", event.src_path)

    def on_deleted(self, event):
        """Handle file/directory deletion events."""
        if isinstance(event, FileDeletedEvent):
            logger.debug(f"File deleted: {event.src_path}")
            self._handle_file_event("deleted", event.src_path)
        elif isinstance(event, DirDeletedEvent):
            # Note: We can't scan deleted directories, so we rely on
            # individual file deletion events or the next full sync
            logger.debug(f"Directory deleted: {event.src_path}")

    def _scan_directory(self, directory_path: str, event_type: str):
        """Scan a directory for supported files and add them to pending changes."""
        try:
            for root, _dirs, files in os.walk(directory_path):
                for file in files:
                    file_path = os.path.join(root, file)
                    if is_supported_file(file_path):
                        self._handle_file_event(event_type, file_path)
        except OSError as e:
            logger.warning(f"Error scanning directory {directory_path}: {e}")

    def stop(self):
        """Stop any pending debounce timers."""
        with self._lock:
            if self._debounce_timer:
                self._debounce_timer.cancel()
                self._debounce_timer = None


class FolderWatcherService:
    """
    Service that manages file system watchers for multiple users and folders.

    This is the main entry point for the folder watching functionality.
    """

    def __init__(self):
        """Initialize the folder watcher service."""
        self._observer: Optional[Observer] = None
        self._handlers: dict[str, FolderEventHandler] = {}  # key: f"{user_id}:{folder_path}"
        self._watches: dict[str, object] = {}  # key: f"{user_id}:{folder_path}", value: watch object
        self._lock = threading.Lock()
        self._started = False
        self._on_change_callback: Optional[Callable[[int, str, dict], None]] = None

    def set_change_callback(self, callback: Callable[[int, str, dict], None]):
        """
        Set the callback to be called when file changes are detected.

        Args:
            callback: Function called with (user_id, folder_path, changes)
                where changes is a dict with keys 'created', 'modified', 'deleted'
        """
        self._on_change_callback = callback

    def start(self):
        """Start the folder watcher service."""
        with self._lock:
            if self._started:
                logger.warning("Folder watcher service already started")
                return

            self._observer = Observer()
            self._observer.start()
            self._started = True
            logger.info("Folder watcher service started")

    def stop(self):
        """Stop the folder watcher service and all watches."""
        with self._lock:
            if not self._started:
                return

            # Stop all handlers
            for handler in self._handlers.values():
                handler.stop()

            # Stop the observer
            if self._observer:
                self._observer.stop()
                self._observer.join(timeout=5.0)
                self._observer = None

            self._handlers.clear()
            self._watches.clear()
            self._started = False
            logger.info("Folder watcher service stopped")

    def add_folder(self, user_id: int, folder_path: str) -> bool:
        """
        Add a folder to be watched for a specific user.

        Args:
            user_id: The user ID who owns this folder
            folder_path: Absolute path to the folder to watch

        Returns:
            True if the folder was added successfully, False otherwise
        """
        if not self._on_change_callback:
            logger.error("Cannot add folder: no change callback set")
            return False

        key = f"{user_id}:{folder_path}"

        with self._lock:
            if not self._started:
                logger.error("Cannot add folder: service not started")
                return False

            if key in self._watches:
                logger.debug(f"Folder already being watched: {folder_path} for user {user_id}")
                return True

            if not os.path.isdir(folder_path):
                logger.error(f"Cannot watch folder: path does not exist or is not a directory: {folder_path}")
                return False

            try:
                handler = FolderEventHandler(
                    folder_path=folder_path,
                    user_id=user_id,
                    on_change_callback=self._on_change_callback,
                )
                watch = self._observer.schedule(
                    handler,
                    folder_path,
                    recursive=True,
                )
                self._handlers[key] = handler
                self._watches[key] = watch
                logger.info(f"Started watching folder: {folder_path} for user {user_id}")
                return True
            except Exception as e:
                logger.error(f"Failed to add folder watch for {folder_path}: {e}", exc_info=True)
                return False

    def remove_folder(self, user_id: int, folder_path: str) -> bool:
        """
        Remove a folder from being watched.

        Args:
            user_id: The user ID who owns this folder
            folder_path: Absolute path to the folder to stop watching

        Returns:
            True if the folder was removed successfully, False otherwise
        """
        key = f"{user_id}:{folder_path}"

        with self._lock:
            if key not in self._watches:
                logger.debug(f"Folder not being watched: {folder_path} for user {user_id}")
                return False

            try:
                handler = self._handlers.pop(key, None)
                if handler:
                    handler.stop()

                watch = self._watches.pop(key, None)
                if watch and self._observer:
                    self._observer.unschedule(watch)

                logger.info(f"Stopped watching folder: {folder_path} for user {user_id}")
                return True
            except Exception as e:
                logger.error(f"Failed to remove folder watch for {folder_path}: {e}", exc_info=True)
                return False

    def get_watched_folders(self, user_id: Optional[int] = None) -> list[tuple[int, str]]:
        """
        Get list of currently watched folders.

        Args:
            user_id: Optional user ID to filter by

        Returns:
            List of (user_id, folder_path) tuples
        """
        with self._lock:
            result = []
            for key in self._watches:
                uid, path = key.split(":", 1)
                uid = int(uid)
                if user_id is None or uid == user_id:
                    result.append((uid, path))
            return result

    def is_running(self) -> bool:
        """Check if the service is currently running."""
        return self._started


def scan_folder_for_files(folder_path: str) -> dict[str, list[str]]:
    """
    Scan a folder and return all supported files organized by type.

    Args:
        folder_path: Absolute path to the folder to scan

    Returns:
        Dictionary mapping file types to lists of absolute file paths
    """
    files_by_type: dict[str, list[str]] = defaultdict(list)

    if not os.path.isdir(folder_path):
        logger.warning(f"Cannot scan folder: not a directory: {folder_path}")
        return files_by_type

    try:
        for root, _dirs, files in os.walk(folder_path):
            for file in files:
                file_path = os.path.join(root, file)
                ext = Path(file_path).suffix.lower()
                file_type = get_file_type_from_extension(ext)
                if file_type:
                    files_by_type[file_type].append(file_path)
    except OSError as e:
        logger.error(f"Error scanning folder {folder_path}: {e}")

    return files_by_type


def collect_files_for_indexing(folder_path: str) -> dict[str, dict[str, bytes | str]]:
    """
    Collect files from a folder for indexing, reading their contents.

    Args:
        folder_path: Absolute path to the folder to scan

    Returns:
        Dictionary in the format expected by configure_content():
        {
            "markdown": {"filename": "content", ...},
            "pdf": {"filename": bytes_content, ...},
            ...
        }
    """
    files_by_type = scan_folder_for_files(folder_path)
    result: dict[str, dict[str, bytes | str]] = {
        "org": {},
        "markdown": {},
        "pdf": {},
        "plaintext": {},
        "image": {},
        "docx": {},
    }

    # Binary file types that should be read as bytes
    binary_types = {"pdf", "image", "docx"}

    for file_type, file_paths in files_by_type.items():
        if file_type not in result:
            continue

        for file_path in file_paths:
            try:
                if file_type in binary_types:
                    with open(file_path, "rb") as f:
                        result[file_type][file_path] = f.read()
                else:
                    with open(file_path, "r", encoding="utf-8", errors="replace") as f:
                        result[file_type][file_path] = f.read()
            except OSError as e:
                logger.warning(f"Failed to read file {file_path}: {e}")
            except Exception as e:
                logger.warning(f"Unexpected error reading file {file_path}: {e}")

    return result


# Global folder watcher service instance
_folder_watcher_service: Optional[FolderWatcherService] = None


def get_folder_watcher_service() -> FolderWatcherService:
    """Get or create the global folder watcher service instance."""
    global _folder_watcher_service
    if _folder_watcher_service is None:
        _folder_watcher_service = FolderWatcherService()
    return _folder_watcher_service


def process_folder_changes(user_id: int, folder_path: str, changes: dict[str, list[str]]) -> bool:
    """
    Process file changes detected by the folder watcher and update the content index.

    This is the callback function passed to the FolderWatcherService. It handles
    file create/modify/delete events by updating the search index accordingly.

    Args:
        user_id: The user ID who owns this folder configuration
        folder_path: The root folder being watched
        changes: Dict with keys 'created', 'modified', 'deleted' containing file paths

    Returns:
        True if processing succeeded, False otherwise
    """
    # Import here to avoid circular imports
    from khoj.database.adapters import EntryAdapters, LocalFolderConfigAdapters
    from khoj.database.models import KhojUser
    from khoj.routers.helpers import configure_content

    try:
        # Get the user object
        user = KhojUser.objects.filter(id=user_id).first()
        if not user:
            logger.error(f"User not found for user_id={user_id}")
            return False

        created_files = changes.get("created", [])
        modified_files = changes.get("modified", [])
        deleted_files = changes.get("deleted", [])

        logger.info(
            f"Processing folder changes for user {user_id}: "
            f"{len(created_files)} created, {len(modified_files)} modified, {len(deleted_files)} deleted"
        )

        # Handle deleted files - remove from index
        if deleted_files:
            for file_path in deleted_files:
                try:
                    deleted_count = EntryAdapters.delete_entry_by_file(user, file_path)
                    if deleted_count > 0:
                        logger.debug(f"Deleted {deleted_count} entries for file: {file_path}")
                except Exception as e:
                    logger.warning(f"Failed to delete entries for file {file_path}: {e}")

        # Handle created and modified files - index them
        files_to_index = created_files + modified_files
        if files_to_index:
            # Collect files by type for indexing
            files_by_type: dict[str, dict[str, bytes | str]] = {
                "org": {},
                "markdown": {},
                "pdf": {},
                "plaintext": {},
                "image": {},
                "docx": {},
            }

            # Binary file types that should be read as bytes
            binary_types = {"pdf", "image", "docx"}

            for file_path in files_to_index:
                ext = Path(file_path).suffix.lower()
                file_type = get_file_type_from_extension(ext)
                if not file_type or file_type not in files_by_type:
                    continue

                try:
                    if file_type in binary_types:
                        with open(file_path, "rb") as f:
                            files_by_type[file_type][file_path] = f.read()
                    else:
                        with open(file_path, "r", encoding="utf-8", errors="replace") as f:
                            files_by_type[file_type][file_path] = f.read()
                except OSError as e:
                    logger.warning(f"Failed to read file {file_path}: {e}")
                except Exception as e:
                    logger.warning(f"Unexpected error reading file {file_path}: {e}")

            # Index the files using configure_content
            # Cast to Any because configure_content's type hint is too restrictive
            # (it accepts bytes for pdf/image/docx but declares dict[str, str])
            success = configure_content(user, cast(Any, files_by_type), regenerate=False)
            if not success:
                logger.error(f"Failed to index files from folder {folder_path}")
                return False

        # Update the folder's last_synced_at timestamp
        LocalFolderConfigAdapters.update_folder_sync_time(user, folder_path)

        logger.info(f"Successfully processed folder changes for {folder_path}")
        return True

    except Exception as e:
        logger.error(f"Error processing folder changes: {e}", exc_info=True)
        return False


def sync_folder(user_id: int, folder_path: str) -> bool:
    """
    Perform a full sync of a folder by scanning all files and indexing them.

    Args:
        user_id: The user ID who owns this folder configuration
        folder_path: The folder path to sync

    Returns:
        True if sync succeeded, False otherwise
    """
    from khoj.database.adapters import LocalFolderConfigAdapters
    from khoj.database.models import KhojUser
    from khoj.routers.helpers import configure_content

    try:
        user = KhojUser.objects.filter(id=user_id).first()
        if not user:
            logger.error(f"User not found for user_id={user_id}")
            return False

        logger.info(f"Starting full sync of folder {folder_path} for user {user_id}")

        # Collect all files from the folder
        files = collect_files_for_indexing(folder_path)

        # Count total files
        total_files = sum(len(f) for f in files.values())
        logger.info(f"Found {total_files} files to index in {folder_path}")

        if total_files == 0:
            logger.info(f"No supported files found in {folder_path}")
            LocalFolderConfigAdapters.update_folder_sync_time(user, folder_path)
            return True

        # Index the files
        # Cast to Any because configure_content's type hint is too restrictive
        # (it accepts bytes for pdf/image/docx but declares dict[str, str])
        success = configure_content(user, cast(Any, files), regenerate=False)
        if not success:
            logger.error(f"Failed to index files from folder {folder_path}")
            return False

        # Update sync timestamp
        LocalFolderConfigAdapters.update_folder_sync_time(user, folder_path)

        logger.info(f"Successfully synced folder {folder_path}")
        return True

    except Exception as e:
        logger.error(f"Error syncing folder {folder_path}: {e}", exc_info=True)
        return False


def sync_user_folders(user_id: int) -> tuple[int, int]:
    """
    Sync all configured folders for a user.

    Args:
        user_id: The user ID to sync folders for

    Returns:
        Tuple of (success_count, total_count)
    """
    from khoj.database.adapters import LocalFolderConfigAdapters
    from khoj.database.models import KhojUser

    try:
        user = KhojUser.objects.filter(id=user_id).first()
        if not user:
            logger.error(f"User not found for user_id={user_id}")
            return (0, 0)

        # Check if folder sync is enabled for this user
        if not LocalFolderConfigAdapters.is_enabled(user):
            logger.debug(f"Folder sync not enabled for user {user_id}")
            return (0, 0)

        # Get all folders for the user
        folders = LocalFolderConfigAdapters.get_folders(user)
        if not folders:
            logger.debug(f"No folders configured for user {user_id}")
            return (0, 0)

        success_count = 0
        total_count = len(folders)

        for folder in folders:
            if sync_folder(user_id, folder.path):
                success_count += 1
            else:
                logger.warning(f"Failed to sync folder {folder.path} for user {user_id}")

        logger.info(f"Synced {success_count}/{total_count} folders for user {user_id}")
        return (success_count, total_count)

    except Exception as e:
        logger.error(f"Error syncing folders for user {user_id}: {e}", exc_info=True)
        return (0, 0)
