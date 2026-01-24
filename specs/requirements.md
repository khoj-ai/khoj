# Personal Knowledge Base - Requirements Document

## 1. Problem Statement

A single user (technical/academic background) has accumulated documents across Google Drive including notes, research papers, PDFs, and project documentation. Current pain points:

| Problem | Impact |
|---------|--------|
| **Can't find documents** | Drive search is inadequate, hard to remember file names/locations |
| **Information is scattered** | Same topics spread across multiple files |
| **No content querying** | Can search titles but can't ask questions about content |
| **Manual summarization** | Must read entire docs to extract key points |
| **Context loss** | Forget what's in older documents over time |

## 2. Goals

Build a personal knowledge base that enables:

1. **Find information faster** - Quickly locate specific documents or facts
2. **Synthesize knowledge** - Connect ideas across documents, summarize, explain
3. **Reduce context switching** - Single interface to query all documents

## 3. User Stories

| ID | As a user, I want to... | So that... | Priority |
|----|-------------------------|------------|----------|
| US-1 | Ask natural language questions | I can find information without remembering exact terms or file names | High |
| US-2 | See which documents answers came from | I can verify information and dive deeper if needed | High |
| US-3 | Get summaries of topics across multiple docs | I don't have to manually read and synthesize | High |
| US-4 | Do quick fact lookups | I can find specific dates, decisions, definitions fast | Medium |
| US-5 | Use it as a research assistant | I can pull relevant prior work when writing | Medium |
| US-6 | Review past learning | I can refresh knowledge on topics I've studied | Low |

## 4. Functional Requirements

### 4.1 Document Ingestion

| ID | Requirement | Priority |
|----|-------------|----------|
| FR-1 | System shall sync documents from Google Drive | High |
| FR-2 | System shall support entire Drive or specific folder(s) as scope | Medium |
| FR-3 | System shall process: Google Docs, Sheets, PDFs, DOCX, plain text, markdown | High |
| FR-4 | System shall automatically detect and process new/updated files | High |
| FR-5 | System shall handle deleted files (remove from knowledge base) | Medium |

### 4.2 Querying

| ID | Requirement | Priority |
|----|-------------|----------|
| FR-6 | System shall accept natural language questions | High |
| FR-7 | System shall return answers synthesized from document content | High |
| FR-8 | System shall always cite source document(s) for answers | High |
| FR-9 | System shall support both semantic (meaning) and keyword (exact term) search | Medium |

### 4.3 Response Behavior

| ID | Requirement | Priority |
|----|-------------|----------|
| FR-10 | When relevant documents exist, system shall answer based on them | High |
| FR-11 | When no relevant documents found, system shall explicitly state this | High |
| FR-12 | System may offer to answer from general knowledge with clear disclaimer | Low |
| FR-13 | System may ask clarifying questions to help narrow search | Low |

### 4.4 Interface

| ID | Requirement | Priority |
|----|-------------|----------|
| FR-14 | System shall provide a chat-based interface | High |
| FR-15 | System shall maintain conversation context within a session | Medium |

## 5. Non-Functional Requirements

### 5.1 Accuracy

| ID | Requirement | Priority |
|----|-------------|----------|
| NFR-1 | Answers based on documents must be factually accurate to source material (near-zero hallucination tolerance) | High |
| NFR-2 | General knowledge answers (if enabled) may have normal LLM accuracy with disclaimer | Low |

### 5.2 Privacy & Security

| ID | Requirement | Priority |
|----|-------------|----------|
| NFR-3 | Documents remain in user's control (self-hosted processing) | High |
| NFR-4 | No document content sent to third parties except for embeddings/LLM APIs | Medium |
| NFR-5 | API keys and credentials stored securely | High |

### 5.3 Performance

| ID | Requirement | Priority |
|----|-------------|----------|
| NFR-6 | Query response time under 10 seconds for typical questions | Medium |
| NFR-7 | New/updated documents indexed within reasonable time | Medium |

### 5.4 Scalability

| ID | Requirement | Priority |
|----|-------------|----------|
| NFR-8 | Support hundreds to low thousands of documents (typical academic/technical user) | High |

### 5.5 Resources

| ID | Requirement | Priority |
|----|-------------|----------|
| NFR-9 | Should run on modest hardware (4-8 GB RAM) | High |
| NFR-10 | Should be deployable locally and on small VPS | Medium |

### 5.6 Maintenance

| ID | Requirement | Priority |
|----|-------------|----------|
| NFR-11 | Leverage existing open-source project to reduce maintenance burden | High |
| NFR-12 | Updates should not require manual data migration | Medium |

## 6. Constraints

| Constraint | Description |
|------------|-------------|
| Single user | No multi-tenancy required |
| Google Drive only | Sole document source (initially) |
| Budget | Minimal ongoing costs preferred (free tiers where possible) |
| Images/charts | Processing deferred to later phase |

## 7. Solution Decision

### 7.1 Options Evaluated

| Option | Pros | Cons | Verdict |
|--------|------|------|---------|
| **Onyx** | Native Drive, 40+ connectors, enterprise features | Heavy (10-16GB RAM, 12 containers), overkill for single user | Rejected |
| **Khoj** | Lightweight (2-4GB), great UX, Obsidian integration, active community | No Google Drive connector | **Selected with extension** |
| **PrivateGPT** | Privacy-focused | Stale development, security issues, no Drive | Rejected |
| **Quivr** | Good RAG library | Pivoted to SDK, no standalone app | Rejected |
| **Custom build** | Full control | Most effort, all maintenance on user | Backup option |

### 7.2 Selected Approach

**Extend Khoj with a Google Drive connector**

Rationale:
- Khoj is lightweight and well-architected
- Active community and development
- Modular connector pattern makes extension straightforward
- Can contribute upstream for community benefit
- Lower maintenance than full custom build

### 7.3 Resource Comparison

| Aspect | Onyx | Khoj (selected) |
|--------|------|-----------------|
| RAM (idle) | 10-16 GB | 2-4 GB |
| Containers | 12 | 1-2 |
| Setup | Complex | Simple |
| Google Drive | Native | **Custom connector** |

## 8. Existing Solutions Considered

### 8.1 Commercial/SaaS

| Solution | Google Drive | Notes |
|----------|--------------|-------|
| Google NotebookLM | Native | Limited to 50 sources, not self-hosted |
| ChatGPT + plugins | Via plugins | Requires subscription, not self-hosted |
| Mem.ai | Partial | SaaS only |

### 8.2 Open Source

| Solution | Google Drive | Self-hosted | Stars | Status |
|----------|--------------|-------------|-------|--------|
| Onyx | Yes | Yes | 17k | Active but heavy |
| Khoj | No (upload only) | Yes | 32k | Active, lightweight |
| PrivateGPT | No | Yes | 57k | Declining |
| Quivr | No | SDK only | 39k | Pivoted |
| AnythingLLM | No | Yes | 54k | Active |

## 9. Success Criteria

| Criterion | Measurement |
|-----------|-------------|
| Can ask a question and get accurate answer with source citation | Manual testing |
| Finds documents that Drive search would miss | Comparative testing |
| Can synthesize information across multiple documents | Manual testing |
| Updates when Drive content changes | Sync verification |
| Response time acceptable for interactive use | < 10 seconds |
| Runs on target hardware | 4-8 GB RAM machine |

## 10. Out of Scope (Phase 1)

- Multi-user support
- Other data sources (email, Slack, etc.)
- Image/diagram understanding
- Voice interface
- Mobile app
- Real-time collaboration
- Webhook-based instant sync (polling acceptable initially)

## 11. Implementation Reference

See: [specs/khoj-google-drive-connector.md](./khoj-google-drive-connector.md)

## 12. Glossary

| Term | Definition |
|------|------------|
| RAG | Retrieval-Augmented Generation - technique to ground LLM responses in retrieved documents |
| Embedding | Vector representation of text for semantic similarity search |
| Vector DB | Database optimized for storing and searching embeddings |
| Chunking | Splitting documents into smaller segments for processing |
| Hybrid search | Combining semantic (vector) and keyword search |
| Connector | Component that fetches data from an external source |
