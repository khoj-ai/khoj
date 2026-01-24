---
id: first-class-folder-sync-support
title: First class folder sync support
status: in-progress
priority: high
created: 2026-01-24T12:00:00Z
updated: 2026-01-24T18:20:00.000Z
---

## Summary
Add server-side folder watching for self-hosted Khoj instances, allowing users to configure local folders in the Knowledge Base settings that are automatically monitored and indexed in real-time using Python's `watchdog` library.

## Tasks
- [x] Add `LocalFolderConfig` model (per-user parent config with enabled flag)
- [x] Add `LocalFolder` model (individual folder paths with last_synced_at)
- [x] Add database adapters for folder config CRUD operations
- [x] Create database migration for new models
- [x] Add API endpoints for folder management (list, add, remove, sync)
- [x] Implement `watchdog`-based file watcher service
- [x] Integrate file watcher with existing `configure_content()` pipeline
- [x] Initialize folder watcher on server startup
- [x] Add folder management UI to Knowledge Base settings card
- [ ] Add sync status indicator showing last indexed time per folder
- [ ] Handle file create/modify/delete events appropriately

## Files
- src/khoj/database/models/__init__.py
- src/khoj/database/adapters/__init__.py
- src/khoj/routers/api_content.py
- src/khoj/routers/helpers.py
- src/khoj/configure.py
- src/khoj/main.py
- src/interface/web/app/settings/page.tsx
- src/khoj/processor/content/folder_watcher.py (new)

## Acceptance Criteria
- Users can add/remove multiple local folders via the settings UI
- Files in configured folders are indexed automatically on server startup
- File changes (create, modify, delete) are detected and indexed in real-time
- Each folder shows its last sync timestamp in the UI
- Folder watching can be enabled/disabled per user
- Only supported file types are indexed (pdf, md, org, txt, docx, images)
- Deleting a file removes its entries from the index

## Definition of Done
When users can configure local folders in the Knowledge Base settings, and those folders are automatically watched and indexed in real-time without requiring the desktop app or manual file uploads.