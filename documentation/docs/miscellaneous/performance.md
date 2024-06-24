---
sidebar_position: 2
---

# Performance

Here are some top-level performance metrics for Khoj. These are rough estimates and will vary based on your hardware and data.

### Search performance

- Semantic search using the bi-encoder is fairly fast at \<100 ms across all content types
- Reranking using the cross-encoder is slower at \<2s on 15 results. Tweak `top_k` to tradeoff speed for accuracy of results
- Filters in query (e.g. by file, word or date) usually add \<20ms to query latency

### Indexing performance

- Indexing is more strongly impacted by the size of the source data
- Indexing 100K+ line corpus of notes takes about 10 minutes
- Indexing 4000+ images takes about 15 minutes and more than 8Gb of RAM
- Note: *It should only take this long on the first run* as the index is incrementally updated

### Miscellaneous

- Testing done on a Mac M1 and a \>100K line corpus of notes
- Search, indexing on a GPU has not been tested yet
