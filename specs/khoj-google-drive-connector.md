# Khoj Google Drive Connector - Technical Specification

## Overview

This document specifies the implementation of a Google Drive connector for [Khoj](https://github.com/khoj-ai/khoj), enabling automatic synchronization and indexing of Google Drive documents into Khoj's personal knowledge base.

## Background

### Why Khoj + Custom Connector?

| Option | Pros | Cons |
|--------|------|------|
| **Onyx** | Native Drive support | Heavy (10-16GB RAM, 12 containers) |
| **Khoj** | Lightweight (~2-4GB), great UX | No Drive connector |
| **Custom build** | Full control | Build everything from scratch |

**Decision**: Extend Khoj with a Google Drive connector to get the best of both worlds - lightweight architecture with Drive integration.

### Khoj Architecture Overview

Khoj uses a modular connector pattern:

```
src/khoj/processor/content/
├── text_to_entries.py          # Base class for all connectors
├── notion/
│   └── notion_to_entries.py    # Cloud connector (OAuth)
├── github/
│   └── github_to_entries.py    # Cloud connector (PAT)
├── pdf/
│   └── pdf_to_entries.py       # File processor
└── ...
```

All connectors extend `TextToEntries` base class and implement `process()` method.

---

## Implementation Plan

### Files to Create

#### 1. `src/khoj/processor/content/googledrive/__init__.py`

```python
from khoj.processor.content.googledrive.googledrive_to_entries import GoogleDriveToEntries
```

#### 2. `src/khoj/processor/content/googledrive/googledrive_to_entries.py`

Main connector implementation.

```python
import logging
from typing import Tuple, List, Dict, Any
from datetime import datetime

from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request
from googleapiclient.discovery import build
from googleapiclient.http import MediaIoBaseDownload

from khoj.processor.content.text_to_entries import TextToEntries
from khoj.database.models import Entry as DbEntry, KhojUser, GoogleDriveConfig
from khoj.utils.helpers import timer

logger = logging.getLogger(__name__)

# MIME type mappings for Google Workspace files
GOOGLE_EXPORT_MIMES = {
    "application/vnd.google-apps.document": ("text/plain", ".txt"),
    "application/vnd.google-apps.spreadsheet": ("text/csv", ".csv"),
    "application/vnd.google-apps.presentation": ("text/plain", ".txt"),
}

SUPPORTED_MIMES = [
    "application/pdf",
    "text/plain",
    "text/markdown",
    "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    *GOOGLE_EXPORT_MIMES.keys(),
]


class GoogleDriveToEntries(TextToEntries):
    def __init__(self, config: GoogleDriveConfig):
        super().__init__(config)
        self.config = config
        self.credentials = self._build_credentials()
        self.service = build("drive", "v3", credentials=self.credentials)

    def _build_credentials(self) -> Credentials:
        """Build OAuth credentials from stored tokens."""
        creds = Credentials(
            token=self.config.access_token,
            refresh_token=self.config.refresh_token,
            token_uri="https://oauth2.googleapis.com/token",
            client_id=settings.GOOGLE_OAUTH_CLIENT_ID,
            client_secret=settings.GOOGLE_OAUTH_CLIENT_SECRET,
        )
        
        # Refresh if expired
        if creds.expired and creds.refresh_token:
            creds.refresh(Request())
            # Update stored tokens
            self.config.access_token = creds.token
            self.config.save()
        
        return creds

    def process(
        self, 
        files: dict[str, str], 
        user: KhojUser, 
        regenerate: bool = False
    ) -> Tuple[int, int]:
        """
        Fetch and index files from Google Drive.
        
        Returns: (num_new_embeddings, num_deleted_embeddings)
        """
        with timer("Fetched files from Google Drive", logger):
            drive_files = self._list_files()
        
        with timer("Processed Google Drive files", logger):
            current_entries = []
            for file in drive_files:
                try:
                    content = self._download_file(file)
                    if content:
                        entries = self._file_to_entries(file, content)
                        current_entries.extend(entries)
                except Exception as e:
                    logger.error(f"Error processing file {file['name']}: {e}")
        
        with timer("Split entries by max tokens", logger):
            current_entries = self.split_entries_by_max_tokens(
                current_entries, 
                max_tokens=256
            )
        
        with timer("Updated Google Drive embeddings", logger):
            num_new, num_deleted = self.update_entries_with_ids(
                current_entries, 
                user=user
            )
        
        return num_new, num_deleted

    def _list_files(self, folder_id: str = None) -> List[Dict[str, Any]]:
        """List all supported files from Drive."""
        query_parts = [
            f"mimeType='{mime}'" for mime in SUPPORTED_MIMES
        ]
        query = f"({' or '.join(query_parts)}) and trashed=false"
        
        if folder_id:
            query += f" and '{folder_id}' in parents"
        
        files = []
        page_token = None
        
        while True:
            response = self.service.files().list(
                q=query,
                spaces="drive",
                fields="nextPageToken, files(id, name, mimeType, modifiedTime, webViewLink)",
                pageToken=page_token,
                pageSize=100,
            ).execute()
            
            files.extend(response.get("files", []))
            page_token = response.get("nextPageToken")
            
            if not page_token:
                break
        
        return files

    def _download_file(self, file: Dict[str, Any]) -> str:
        """Download file content, exporting Google Workspace files."""
        mime_type = file["mimeType"]
        file_id = file["id"]
        
        if mime_type in GOOGLE_EXPORT_MIMES:
            # Export Google Workspace file
            export_mime, _ = GOOGLE_EXPORT_MIMES[mime_type]
            request = self.service.files().export_media(
                fileId=file_id, 
                mimeType=export_mime
            )
        else:
            # Download regular file
            request = self.service.files().get_media(fileId=file_id)
        
        import io
        buffer = io.BytesIO()
        downloader = MediaIoBaseDownload(buffer, request)
        
        done = False
        while not done:
            _, done = downloader.next_chunk()
        
        buffer.seek(0)
        
        # Handle binary files (PDF, DOCX)
        if mime_type == "application/pdf":
            return self._extract_pdf_text(buffer)
        elif mime_type.endswith("wordprocessingml.document"):
            return self._extract_docx_text(buffer)
        else:
            return buffer.read().decode("utf-8", errors="ignore")

    def _extract_pdf_text(self, buffer) -> str:
        """Extract text from PDF."""
        try:
            import pypdf
            reader = pypdf.PdfReader(buffer)
            return "\n".join(page.extract_text() or "" for page in reader.pages)
        except Exception as e:
            logger.error(f"PDF extraction error: {e}")
            return ""

    def _extract_docx_text(self, buffer) -> str:
        """Extract text from DOCX."""
        try:
            import docx
            doc = docx.Document(buffer)
            return "\n".join(para.text for para in doc.paragraphs)
        except Exception as e:
            logger.error(f"DOCX extraction error: {e}")
            return ""

    def _file_to_entries(self, file: Dict[str, Any], content: str) -> List[Entry]:
        """Convert file content to Entry objects."""
        from khoj.utils.rawconfig import Entry
        
        if not content.strip():
            return []
        
        return [
            Entry(
                raw=content,
                compiled=content,
                heading=file["name"],
                file=file.get("webViewLink", f"gdrive://{file['id']}"),
            )
        ]

    def update_entries_with_ids(
        self, 
        current_entries, 
        user: KhojUser = None
    ):
        """Persist entries to database."""
        return self.update_embeddings(
            user,
            current_entries,
            DbEntry.EntryType.GOOGLEDRIVE,
            DbEntry.EntrySource.GOOGLEDRIVE,
            key="compiled",
            logger=logger,
        )
```

#### 3. `src/khoj/routers/googledrive.py`

OAuth flow and API endpoints.

```python
import os
import logging
from fastapi import APIRouter, Request, HTTPException, BackgroundTasks
from starlette.responses import RedirectResponse
from starlette.authentication import requires

from google_auth_oauthlib.flow import Flow

from khoj.database.models import GoogleDriveConfig
from khoj.routers.helpers import configure_content
from khoj.utils.helpers import run_in_executor
from khoj.utils.rawconfig import SearchType

logger = logging.getLogger(__name__)

SCOPES = [
    "https://www.googleapis.com/auth/drive.readonly",
]

googledrive_router = APIRouter(prefix="/api/googledrive", tags=["googledrive"])


def get_oauth_flow(redirect_uri: str) -> Flow:
    """Create OAuth flow from environment credentials."""
    client_config = {
        "web": {
            "client_id": os.getenv("GOOGLE_OAUTH_CLIENT_ID"),
            "client_secret": os.getenv("GOOGLE_OAUTH_CLIENT_SECRET"),
            "auth_uri": "https://accounts.google.com/o/oauth2/auth",
            "token_uri": "https://oauth2.googleapis.com/token",
            "redirect_uris": [redirect_uri],
        }
    }
    return Flow.from_client_config(client_config, scopes=SCOPES, redirect_uri=redirect_uri)


@googledrive_router.get("/auth")
@requires(["authenticated"])
async def googledrive_auth(request: Request):
    """Initiate OAuth flow."""
    redirect_uri = str(request.url_for("googledrive_callback"))
    flow = get_oauth_flow(redirect_uri)
    
    authorization_url, state = flow.authorization_url(
        access_type="offline",
        include_granted_scopes="true",
        prompt="consent",  # Force consent to get refresh token
    )
    
    # Store state in session
    request.session["googledrive_oauth_state"] = state
    
    return RedirectResponse(authorization_url)


@googledrive_router.get("/auth/callback")
@requires(["authenticated"])
async def googledrive_callback(
    request: Request, 
    background_tasks: BackgroundTasks
):
    """Handle OAuth callback."""
    code = request.query_params.get("code")
    state = request.query_params.get("state")
    
    # Verify state
    stored_state = request.session.get("googledrive_oauth_state")
    if state != stored_state:
        raise HTTPException(400, "Invalid OAuth state")
    
    # Exchange code for tokens
    redirect_uri = str(request.url_for("googledrive_callback"))
    flow = get_oauth_flow(redirect_uri)
    flow.fetch_token(code=code)
    credentials = flow.credentials
    
    # Save config
    user = request.user.object
    await GoogleDriveConfig.objects.aupdate_or_create(
        user=user,
        defaults={
            "access_token": credentials.token,
            "refresh_token": credentials.refresh_token,
            "token_expiry": credentials.expiry,
        }
    )
    
    # Trigger initial sync
    background_tasks.add_task(
        run_in_executor,
        configure_content,
        user,
        {},
        False,
        SearchType.GoogleDrive,
    )
    
    return RedirectResponse("/settings?googledrive=connected")


@googledrive_router.delete("/disconnect")
@requires(["authenticated"])
async def googledrive_disconnect(request: Request):
    """Disconnect Google Drive."""
    user = request.user.object
    
    # Delete entries
    from khoj.database.adapters import EntryAdapters
    await EntryAdapters.adelete_all_entries(
        user, 
        file_source=DbEntry.EntrySource.GOOGLEDRIVE
    )
    
    # Delete config
    await GoogleDriveConfig.objects.filter(user=user).adelete()
    
    return {"status": "disconnected"}


@googledrive_router.post("/sync")
@requires(["authenticated"])
async def googledrive_sync(request: Request, background_tasks: BackgroundTasks):
    """Trigger manual sync."""
    user = request.user.object
    
    config = await GoogleDriveConfig.objects.filter(user=user).afirst()
    if not config:
        raise HTTPException(400, "Google Drive not connected")
    
    background_tasks.add_task(
        run_in_executor,
        configure_content,
        user,
        {},
        False,
        SearchType.GoogleDrive,
    )
    
    return {"status": "sync_started"}
```

### Files to Modify

#### 1. `src/khoj/database/models/__init__.py`

Add new model and entry types.

```python
# Add to Entry.EntryType choices
class EntryType(models.TextChoices):
    # ... existing types ...
    GOOGLEDRIVE = "googledrive"

# Add to Entry.EntrySource choices  
class EntrySource(models.TextChoices):
    # ... existing sources ...
    GOOGLEDRIVE = "googledrive"

# Add new model
class GoogleDriveConfig(DbBaseModel):
    """Stores Google Drive OAuth credentials per user."""
    access_token = models.CharField(max_length=2000)
    refresh_token = models.CharField(max_length=2000, null=True, blank=True)
    token_expiry = models.DateTimeField(null=True, blank=True)
    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE)
    
    # Optional: folder restriction
    folder_id = models.CharField(max_length=200, null=True, blank=True)
    
    # Sync state
    last_sync_at = models.DateTimeField(null=True, blank=True)
    sync_page_token = models.CharField(max_length=500, null=True, blank=True)

    class Meta:
        db_table = "googledrive_config"
```

#### 2. `src/khoj/database/adapters/__init__.py`

Add adapter functions.

```python
async def get_user_googledrive_config(user: KhojUser) -> GoogleDriveConfig | None:
    return await GoogleDriveConfig.objects.filter(user=user).afirst()

async def set_user_googledrive_config(
    user: KhojUser,
    access_token: str,
    refresh_token: str = None,
    token_expiry: datetime = None,
) -> GoogleDriveConfig:
    config, _ = await GoogleDriveConfig.objects.aupdate_or_create(
        user=user,
        defaults={
            "access_token": access_token,
            "refresh_token": refresh_token,
            "token_expiry": token_expiry,
        }
    )
    return config
```

#### 3. `src/khoj/utils/rawconfig.py`

Add search type.

```python
class SearchType(str, Enum):
    # ... existing types ...
    GoogleDrive = "googledrive"
```

#### 4. `src/khoj/routers/helpers.py`

Wire up connector in `configure_content()`.

```python
from khoj.processor.content.googledrive.googledrive_to_entries import GoogleDriveToEntries

def configure_content(user: KhojUser, files: dict, regenerate: bool = False, search_type: SearchType = None):
    # ... existing code ...
    
    # Add Google Drive processing
    if search_type in [None, SearchType.GoogleDrive]:
        googledrive_config = get_user_googledrive_config(user)
        if googledrive_config:
            try:
                processor = GoogleDriveToEntries(googledrive_config)
                num_new, num_deleted = processor.process({}, user, regenerate)
                logger.info(f"Google Drive: {num_new} new, {num_deleted} deleted")
            except Exception as e:
                logger.error(f"Google Drive sync failed: {e}")
```

#### 5. `src/khoj/configure.py`

Register the router.

```python
from khoj.routers.googledrive import googledrive_router

# In configure_routes():
app.include_router(googledrive_router)
```

#### 6. `src/interface/web/app/settings/page.tsx`

Add UI card (simplified).

```tsx
// Add Google Drive card similar to Notion card
<Card id="googledrive">
    <CardHeader>
        <GoogleDriveLogo className="h-8 w-8 mr-2" />
        Google Drive
        {userConfig.enabled_content_source.googledrive && (
            <CheckCircle className="h-6 w-6 ml-auto text-green-500" />
        )}
    </CardHeader>
    <CardContent>
        <p>Connect your Google Drive to index documents</p>
    </CardContent>
    <CardFooter>
        {!userConfig.enabled_content_source.googledrive ? (
            <Button onClick={() => window.location.href = "/api/googledrive/auth"}>
                Connect
            </Button>
        ) : (
            <>
                <Button onClick={() => syncContent("googledrive")}>Sync</Button>
                <Button variant="destructive" onClick={() => disconnectContent("googledrive")}>
                    Disconnect
                </Button>
            </>
        )}
    </CardFooter>
</Card>
```

---

## Database Migration

Create migration file: `src/khoj/database/migrations/XXXX_add_googledrive.py`

```python
from django.db import migrations, models
import django.db.models.deletion

class Migration(migrations.Migration):
    dependencies = [
        ('database', 'previous_migration'),
    ]

    operations = [
        migrations.CreateModel(
            name='GoogleDriveConfig',
            fields=[
                ('id', models.BigAutoField(auto_created=True, primary_key=True)),
                ('created_at', models.DateTimeField(auto_now_add=True)),
                ('updated_at', models.DateTimeField(auto_now=True)),
                ('access_token', models.CharField(max_length=2000)),
                ('refresh_token', models.CharField(max_length=2000, null=True, blank=True)),
                ('token_expiry', models.DateTimeField(null=True, blank=True)),
                ('folder_id', models.CharField(max_length=200, null=True, blank=True)),
                ('last_sync_at', models.DateTimeField(null=True, blank=True)),
                ('sync_page_token', models.CharField(max_length=500, null=True, blank=True)),
                ('user', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, to='database.khojuser')),
            ],
            options={
                'db_table': 'googledrive_config',
            },
        ),
    ]
```

---

## Environment Variables

Add to `.env`:

```bash
# Google OAuth (create at https://console.cloud.google.com/apis/credentials)
GOOGLE_OAUTH_CLIENT_ID=your-client-id.apps.googleusercontent.com
GOOGLE_OAUTH_CLIENT_SECRET=your-client-secret

# Optional: Restrict to specific folder
GOOGLE_DRIVE_FOLDER_ID=
```

---

## Implementation Phases

### Phase 1: MVP (3-4 days)

- [ ] Database model + migration
- [ ] Basic connector class (list files, download, extract text)
- [ ] OAuth flow (connect/disconnect)
- [ ] Manual sync endpoint
- [ ] Basic UI card
- [ ] Support: PDF, plain text, markdown, Google Docs

### Phase 2: Enhanced (2-3 days)

- [ ] Google Sheets support (CSV export)
- [ ] DOCX support
- [ ] Incremental sync (using Drive Changes API)
- [ ] Better error handling + retry logic
- [ ] Progress indication in UI

### Phase 3: Production Ready (2-3 days)

- [ ] Webhook support for real-time sync (optional)
- [ ] Folder selection in UI
- [ ] Shared drive support
- [ ] Rate limit handling
- [ ] Unit tests
- [ ] Documentation

---

## Dependencies

Add to `pyproject.toml`:

```toml
[project.dependencies]
# ... existing deps ...
google-auth = "^2.0"
google-auth-oauthlib = "^1.0"
google-api-python-client = "^2.0"
pypdf = "^4.0"
python-docx = "^1.0"
```

---

## Testing

### Manual Testing Checklist

1. [ ] OAuth flow completes successfully
2. [ ] Tokens are stored in database
3. [ ] File listing works
4. [ ] Google Docs exported and indexed
5. [ ] PDFs extracted and indexed
6. [ ] Search returns Drive documents
7. [ ] Disconnect removes all entries
8. [ ] Token refresh works when expired

### Unit Tests

```python
# tests/test_googledrive.py
import pytest
from unittest.mock import Mock, patch

from khoj.processor.content.googledrive.googledrive_to_entries import GoogleDriveToEntries

@pytest.fixture
def mock_drive_service():
    with patch("googleapiclient.discovery.build") as mock:
        yield mock

def test_list_files(mock_drive_service):
    # Test file listing
    pass

def test_export_google_doc(mock_drive_service):
    # Test Google Docs export
    pass

def test_process_pdf():
    # Test PDF extraction
    pass
```

---

## Upstream Contribution

### PR Strategy

1. Fork `khoj-ai/khoj`
2. Create feature branch `feature/google-drive-connector`
3. Implement in phases, commit incrementally
4. Open draft PR early for feedback
5. Add tests and documentation
6. Request review from maintainers

### PR Description Template

```markdown
## Summary
Adds Google Drive as a data source for Khoj, enabling automatic indexing of Drive documents.

## Changes
- New `GoogleDriveToEntries` connector
- OAuth flow for Drive authentication
- Support for Google Docs, Sheets, PDF, text files
- Settings UI card for connection management

## Testing
- [ ] Manual testing with personal Drive
- [ ] Unit tests for connector
- [ ] OAuth flow tested

## Related Issues
Closes #XXX (if applicable)
```

---

## References

- [Khoj GitHub](https://github.com/khoj-ai/khoj)
- [Khoj Notion Connector](https://github.com/khoj-ai/khoj/blob/master/src/khoj/processor/content/notion/notion_to_entries.py)
- [Google Drive API Python Quickstart](https://developers.google.com/drive/api/quickstart/python)
- [Google OAuth 2.0](https://developers.google.com/identity/protocols/oauth2)
- [Drive API Files.list](https://developers.google.com/drive/api/reference/rest/v3/files/list)
