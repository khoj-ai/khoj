# Khoj Optimization - Quick Implementation Guide

## Files to Create/Modify

### 1. RAG Performance (5 files)

**Create:**
- `src/khoj/utils/query_cache.py` - LRU query embedding cache
- `src/khoj/utils/metrics.py` - Prometheus metrics

**Modify:**
- `src/khoj/processor/embeddings.py` - Add async embedding methods
- `src/khoj/search_type/text_search.py` - Add cache integration
- `src/khoj/database/migrations/XXXX_add_hnsw_index.py` - HNSW index migration

### 2. Decentralized Access (4 files)

**Create:**
- `src/khoj/processor/providers/base.py` - Provider abstraction
- `src/khoj/processor/providers/ollama.py` - Native Ollama integration
- `config/providers.yml` - Provider configuration

**Modify:**
- `src/khoj/database/models/__init__.py` - Add provider types

### 3. Scalability (5 files)

**Create:**
- `src/khoj/tasks/celery_app.py` - Celery configuration
- `src/khoj/tasks/indexing.py` - Distributed indexing tasks
- `src/khoj/utils/distributed_lock.py` - Redis-based locks
- `src/khoj/processor/content/incremental_sync.py` - Delta sync engine
- `docker-compose.distributed.yml` - Distributed deployment

---

## Quick Start Commands

```bash
# 1. Profile current embedding performance
python -c "
from khoj.processor.embeddings import EmbeddingsModel
import time
model = EmbeddingsModel('thenlper/gte-small')
docs = ['test ' * 100] * 1000
start = time.time()
model.embed_documents(docs)
print(f'{len(docs)} docs: {time.time()-start:.2f}s')
"

# 2. Check PostgreSQL vector index status
psql -c "SELECT indexname, indexdef FROM pg_indexes WHERE tablename = 'entry';"

# 3. Test Ollama connection
curl http://localhost:11434/api/tags

# 4. Start Celery worker (after implementation)
celery -A khoj.tasks.celery_app worker -Q indexing,embedding -c 4 -l info
```

---

## Environment Variables

```bash
# .env additions for optimizations

# Async embedding
EMBEDDING_BATCH_SIZE=100
EMBEDDING_MAX_CONCURRENT=10
QUERY_CACHE_SIZE=5000
QUERY_CACHE_TTL=3600

# Self-hosted providers
OLLAMA_BASE_URL=http://localhost:11434
OLLAMA_DEFAULT_MODEL=llama3.1:8b
OLLAMA_EMBEDDING_MODEL=nomic-embed-text

# Distributed indexing
CELERY_BROKER_URL=redis://localhost:6379/0
CELERY_RESULT_BACKEND=redis://localhost:6379/1
REDIS_LOCK_URL=redis://localhost:6379/2

# PostgreSQL
DATABASE_MAX_CONNECTIONS=20
DATABASE_POOL_SIZE=10
```

---

## Dependencies to Add

```toml
# pyproject.toml additions

[project.dependencies]
# Async HTTP
aiohttp = ">=3.9.0"

# Caching
redis = ">=5.0.0"

# Task queue
celery = ">=5.3.0"
kombu = ">=5.3.0"

# Metrics
prometheus-client = ">=0.19.0"
```

---

## Migration Path

1. **Week 1**: Query cache + async embedding (P0)
2. **Week 2**: Ollama native provider (P1)
3. **Week 3**: Celery task queue + Redis locks (P1)
4. **Week 4**: Incremental sync + HNSW index (P2)

---

## Success Metrics

| Metric | Before | After | Target |
|--------|--------|-------|--------|
| Query latency (p50) | 150ms | 15ms | <20ms |
| Query latency (p99) | 500ms | 50ms | <100ms |
| Indexing 1K docs | 7.5min | 45s | <60s |
| Cache hit rate | 0% | 90% | >85% |
| Self-hosted support | 1 provider | 6+ providers | Full coverage |
