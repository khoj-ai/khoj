# Personal Knowledge Base: Google Drive + RAG

## Overview

A self-hosted knowledge base system that syncs with Google Drive, processes all document types, stores embeddings in a managed database, and provides a chat interface powered by LLM APIs.

## Tech Stack

| Component | Choice | Rationale |
|-----------|--------|-----------|
| **Vector DB** | Neon (pgvector) | Managed, generous free tier, familiar SQL |
| **Embeddings** | OpenAI text-embedding-3-small | Best quality/cost ratio, 1536 dimensions |
| **LLM** | GLM-4.1 / MiniMax via OpenRouter | Essentially free, good quality |
| **Doc Processing** | Unstructured (Docker) | Handles all file types including tables |
| **RAG Framework** | LlamaIndex | Better doc handling, native Drive reader |
| **Backend** | FastAPI | OpenAI-compatible API for Open WebUI |
| **Frontend** | Open WebUI | Polished chat UI, easy to connect |
| **Deployment** | Docker Compose | Same config for local and VPS |

## Architecture

```
┌──────────────────────────────────────────────────────────────────────┐
│                              VPS                                      │
│  ┌────────────────────────────────────────────────────────────────┐  │
│  │  Caddy (reverse proxy + auto SSL)                              │  │
│  │  - :443 → open-webui:8080                                      │  │
│  │  - :443/api → api:8080                                         │  │
│  │  - :443/webhook/drive → api:8080                               │  │
│  └────────────────────────────────────────────────────────────────┘  │
│                                                                       │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────────────┐  │
│  │ Open WebUI  │  │  FastAPI    │  │  Unstructured (optional)    │  │
│  │  Chat UI    │  │  Backend    │  │  Complex doc processing     │  │
│  └─────────────┘  └──────┬──────┘  └─────────────────────────────┘  │
│                          │                                           │
└──────────────────────────┼───────────────────────────────────────────┘
                           │
       ┌───────────────────┼───────────────────┐
       │                   │                   │
       ▼                   ▼                   ▼
┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│ Google Drive│    │    Neon     │    │ OpenRouter  │
│  Push API   │    │  pgvector   │    │ GLM-4.1 /   │
│             │    │             │    │ MiniMax     │
└─────────────┘    └─────────────┘    └─────────────┘
```

## Sync Strategy

| Environment | Method | Details |
|-------------|--------|---------|
| **Local dev** | Polling or manual trigger | No public URL; use `/sync` endpoint |
| **VPS (prod)** | Drive Push Notifications | Webhook at `https://your-domain/webhook/drive` |

### Google Drive Push Notifications

Google Drive API supports push notifications via webhooks:

- **Requires HTTPS** with valid SSL certificate (no self-signed)
- **Max expiration**: 1 day for `files.watch`, 1 week for `changes.watch`
- **Must renew** channels before expiration (no auto-renewal)
- **Event types**: `sync`, `add`, `remove`, `update`, `trash`, `untrash`, `change`

Reference: https://developers.google.com/drive/api/guides/push

## Project Structure

```
personal-kb/
├── docker-compose.yml           # Production (VPS)
├── docker-compose.local.yml     # Local dev (no webhooks)
├── Caddyfile                    # Reverse proxy + SSL
├── .env.example
├── .env
│
├── api/
│   ├── Dockerfile
│   ├── requirements.txt
│   ├── main.py
│   ├── config.py
│   │
│   ├── routers/
│   │   ├── chat.py              # /v1/chat/completions
│   │   ├── ingest.py            # /ingest (manual)
│   │   ├── sync.py              # /sync (manual trigger)
│   │   └── webhook.py           # /webhook/drive (push notifications)
│   │
│   ├── services/
│   │   ├── gdrive.py            # Drive API + channel management
│   │   ├── processor.py         # File processing pipeline
│   │   ├── chunker.py           # Semantic chunking
│   │   ├── embedder.py          # OpenAI embeddings
│   │   ├── retriever.py         # Hybrid search
│   │   └── rag.py               # RAG chain with LlamaIndex
│   │
│   ├── db/
│   │   ├── connection.py
│   │   ├── models.py
│   │   └── schema.sql
│   │
│   └── tasks/
│       ├── scheduler.py         # APScheduler for channel renewal
│       └── jobs.py              # Scheduled job definitions
│
└── scripts/
    ├── setup_db.py
    ├── initial_sync.py
    └── register_webhook.py      # Register Drive push channel
```

## Database Schema

```sql
-- Enable extensions
CREATE EXTENSION IF NOT EXISTS vector;
CREATE EXTENSION IF NOT EXISTS pg_trgm;

-- Documents: metadata about source files
CREATE TABLE documents (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    drive_file_id VARCHAR(255) UNIQUE NOT NULL,
    name VARCHAR(1024) NOT NULL,
    mime_type VARCHAR(255),
    drive_modified_time TIMESTAMPTZ,
    file_hash VARCHAR(64),
    processing_status VARCHAR(50) DEFAULT 'pending',
    error_message TEXT,
    metadata JSONB DEFAULT '{}',
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

-- Chunks: processed text segments with embeddings
CREATE TABLE chunks (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    document_id UUID REFERENCES documents(id) ON DELETE CASCADE,
    content TEXT NOT NULL,
    chunk_index INTEGER NOT NULL,
    token_count INTEGER,
    embedding vector(1536),
    metadata JSONB DEFAULT '{}',
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Indexes for chunks
CREATE INDEX idx_chunks_document ON chunks(document_id);
CREATE INDEX idx_chunks_embedding ON chunks 
    USING hnsw (embedding vector_cosine_ops)
    WITH (m = 16, ef_construction = 64);
CREATE INDEX idx_chunks_content_trgm ON chunks 
    USING gin (content gin_trgm_ops);

-- Conversations: chat history for context
CREATE TABLE conversations (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    session_id VARCHAR(255) NOT NULL,
    role VARCHAR(20) NOT NULL,
    content TEXT NOT NULL,
    sources JSONB,
    created_at TIMESTAMPTZ DEFAULT NOW()
);
CREATE INDEX idx_conversations_session ON conversations(session_id, created_at);

-- Sync state: track Drive sync progress
CREATE TABLE sync_state (
    id INTEGER PRIMARY KEY DEFAULT 1,
    page_token TEXT,
    last_sync_at TIMESTAMPTZ,
    CONSTRAINT single_row CHECK (id = 1)
);
INSERT INTO sync_state (id) VALUES (1);

-- Webhook channels for Drive push notifications
CREATE TABLE webhook_channels (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    channel_id VARCHAR(255) UNIQUE NOT NULL,
    resource_id VARCHAR(255) NOT NULL,
    expiration BIGINT NOT NULL,
    is_active BOOLEAN DEFAULT true,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Processed webhook messages (for deduplication)
CREATE TABLE webhook_messages (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    channel_id VARCHAR(255) NOT NULL,
    message_number INTEGER NOT NULL,
    resource_state VARCHAR(50),
    processed_at TIMESTAMPTZ DEFAULT NOW(),
    UNIQUE(channel_id, message_number)
);
CREATE INDEX idx_webhook_messages_channel ON webhook_messages(channel_id);
```

## Configuration

```python
# config.py
from pydantic_settings import BaseSettings

class Settings(BaseSettings):
    # Database
    DATABASE_URL: str
    
    # Google Drive
    GOOGLE_CREDENTIALS_PATH: str = "credentials/google.json"
    DRIVE_FOLDER_ID: str | None = None  # None = entire Drive
    DRIVE_WEBHOOK_TOKEN: str
    
    # APIs
    OPENROUTER_API_KEY: str
    OPENAI_API_KEY: str  # For embeddings
    
    # App
    BASE_URL: str = "http://localhost:8080"
    SYNC_MODE: str = "polling"  # "polling" or "webhook"
    POLLING_INTERVAL_MINUTES: int = 15
    
    # RAG
    CHUNK_SIZE: int = 512
    CHUNK_OVERLAP: int = 50
    RETRIEVAL_TOP_K: int = 5
    
    # LLM
    LLM_MODEL: str = "zhipu-ai/glm-4-plus"
    
    class Config:
        env_file = ".env"
```

## File Type Handling

| File Type | Processing Method | Notes |
|-----------|-------------------|-------|
| Google Docs | Export as HTML → parse | Preserves formatting |
| Google Sheets | Export as CSV → parse rows | Each row as chunk option |
| PDF (text) | Unstructured `partition_pdf` | Tables extracted |
| PDF (scanned) | Unstructured with OCR | `hi_res` strategy |
| DOCX | Unstructured `partition_docx` | Tables/lists preserved |
| PPTX | Unstructured `partition_pptx` | Slide-by-slide |
| Plain text | Direct read | Sentence chunking |
| Markdown | Direct read | Section-aware chunking |
| Images | Deferred (Phase 2) | GPT-4V description |

## Key Implementation Details

### Hybrid Search

```python
async def hybrid_search(query: str, k: int = 5) -> list[ChunkResult]:
    query_embedding = await embedder.embed(query)
    
    results = await db.execute("""
        WITH vector_search AS (
            SELECT id, content, metadata,
                   1 - (embedding <=> $1::vector) AS vector_score,
                   ROW_NUMBER() OVER (ORDER BY embedding <=> $1::vector) AS vector_rank
            FROM chunks
            ORDER BY embedding <=> $1::vector
            LIMIT $3
        ),
        keyword_search AS (
            SELECT id, content, metadata,
                   similarity(content, $2) AS keyword_score,
                   ROW_NUMBER() OVER (ORDER BY similarity(content, $2) DESC) AS keyword_rank
            FROM chunks
            WHERE content % $2
            ORDER BY similarity(content, $2) DESC
            LIMIT $3
        )
        SELECT COALESCE(v.id, k.id) AS id,
               COALESCE(v.content, k.content) AS content,
               COALESCE(v.metadata, k.metadata) AS metadata,
               COALESCE(1.0/(60 + v.vector_rank), 0) + 
               COALESCE(1.0/(60 + k.keyword_rank), 0) AS rrf_score
        FROM vector_search v
        FULL OUTER JOIN keyword_search k ON v.id = k.id
        ORDER BY rrf_score DESC
        LIMIT $4
    """, query_embedding, query, k * 2, k)
    
    return [ChunkResult(**r) for r in results]
```

### Webhook Endpoint

```python
@router.post("/webhook/drive")
async def drive_webhook(
    request: Request,
    background_tasks: BackgroundTasks,
    x_goog_channel_id: str = Header(...),
    x_goog_resource_state: str = Header(...),
    x_goog_resource_id: str = Header(...),
    x_goog_message_number: int = Header(...),
    x_goog_channel_token: Optional[str] = Header(None),
    x_goog_changed: Optional[str] = Header(None),
):
    # Verify channel token
    if x_goog_channel_token != settings.DRIVE_WEBHOOK_TOKEN:
        raise HTTPException(403, "Invalid token")
    
    # Handle sync message
    if x_goog_resource_state == "sync":
        return {"status": "ok"}
    
    # Dedupe by message number
    if await db.is_message_processed(x_goog_channel_id, x_goog_message_number):
        return {"status": "already_processed"}
    
    # Queue processing in background
    background_tasks.add_task(
        process_drive_change,
        resource_state=x_goog_resource_state,
        resource_id=x_goog_resource_id,
        changed=x_goog_changed,
    )
    
    return {"status": "accepted"}
```

### Channel Management

```python
class DriveChannelManager:
    CHANNEL_TTL = timedelta(days=6)  # Renew before 7-day max
    
    async def register_changes_channel(self, start_page_token: str) -> dict:
        channel_id = str(uuid.uuid4())
        expiration = int((datetime.utcnow() + self.CHANNEL_TTL).timestamp() * 1000)
        
        response = self.drive_service.changes().watch(
            pageToken=start_page_token,
            body={
                "id": channel_id,
                "type": "web_hook",
                "address": f"{settings.BASE_URL}/webhook/drive",
                "token": settings.DRIVE_WEBHOOK_TOKEN,
                "expiration": expiration,
            }
        ).execute()
        
        await db.save_channel(
            channel_id=response["id"],
            resource_id=response["resourceId"],
            expiration=response["expiration"],
        )
        
        return response
    
    async def renew_channel_if_needed(self):
        channel = await db.get_active_channel()
        if not channel:
            return await self.register_changes_channel()
        
        expires_at = datetime.fromtimestamp(channel.expiration / 1000)
        if expires_at - datetime.utcnow() < timedelta(days=1):
            await self.stop_channel(channel.channel_id, channel.resource_id)
            page_token = await db.get_sync_token()
            return await self.register_changes_channel(page_token)
```

## Implementation Phases

### Phase 1: Infrastructure (Day 1)
- [ ] Neon DB setup + schema
- [ ] GCP project + OAuth credentials
- [ ] OpenRouter API key
- [ ] Project scaffolding

### Phase 2: Core Processing (Days 2-3)
- [ ] Google Drive reader (folder or entire Drive, configurable)
- [ ] File type routing + processing
- [ ] Chunking + embedding
- [ ] Database storage

### Phase 3: Retrieval & RAG (Days 4-5)
- [ ] Hybrid search implementation
- [ ] RAG chain with GLM-4.1/MiniMax
- [ ] OpenAI-compatible API endpoint

### Phase 4: Frontend + Local Dev (Day 6)
- [ ] Docker Compose (local)
- [ ] Open WebUI integration
- [ ] Manual `/sync` endpoint for local testing

### Phase 5: Webhook + VPS Deployment (Days 7-8)
- [ ] Caddy setup with SSL
- [ ] Webhook endpoint implementation
- [ ] Channel registration + renewal scheduler
- [ ] Full Docker Compose for VPS
- [ ] Initial sync + webhook registration

### Phase 6: Polish (Day 9)
- [ ] Error handling + retries
- [ ] Logging + monitoring
- [ ] Documentation

## Cost Estimate (Monthly)

| Service | Free Tier | Est. Usage | Est. Cost |
|---------|-----------|------------|-----------|
| Neon | 0.5GB + 100 compute hrs | Light usage | $0 |
| OpenAI Embeddings | - | ~1M tokens/mo | ~$0.02 |
| OpenRouter (GLM-4.1) | Free credits | Moderate | ~$0-5 |
| VPS (optional) | - | Small instance | ~$5-10 |
| **Total** | | | **~$5-15/mo** |

## Environment Comparison

| Component | Local Dev | VPS Production |
|-----------|-----------|----------------|
| **Sync** | Manual `/sync` or polling | Drive Push Notifications |
| **SSL** | Not needed | Caddy auto-SSL |
| **Scheduler** | Optional | Runs channel renewal |
| **URL** | localhost:3000 | your-domain.com |

## References

- [Google Drive Push Notifications](https://developers.google.com/drive/api/guides/push)
- [LlamaIndex Google Drive Reader](https://docs.llamaindex.ai/en/stable/examples/data_connectors/GoogleDriveDemo/)
- [Neon pgvector](https://neon.tech/docs/extensions/pgvector)
- [Open WebUI](https://github.com/open-webui/open-webui)
- [Unstructured](https://github.com/Unstructured-IO/unstructured)
