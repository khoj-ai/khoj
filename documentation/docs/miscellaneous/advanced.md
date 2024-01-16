---
sidebar_position: 3
---

# Advanced Usage

### Search across Different Languages (Self-Hosting)
To search for notes in multiple, different languages, you can use a [multi-lingual model](https://www.sbert.net/docs/pretrained_models.html#multi-lingual-models).<br />
For example, the [paraphrase-multilingual-MiniLM-L12-v2](https://huggingface.co/sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2) supports [50+ languages](https://www.sbert.net/docs/pretrained_models.html#:~:text=we%20used%20the%20following%2050%2B%20languages), has good search quality and speed. To use it:
1. Manually update the search config in server's admin settings page. Go to [the search config](http://localhost:42110/server/admin/database/searchmodelconfig/). Either create a new one, if none exists, or update the existing one. Set the bi_encoder to `sentence-transformers/multi-qa-MiniLM-L6-cos-v1` and the cross_encoder to `cross-encoder/ms-marco-MiniLM-L-6-v2`.
2. Regenerate your content index from all the relevant clients. This step is very important, as you'll need to re-encode all your content with the new model.

### Query Filters

Use structured query syntax to filter entries from your knowledge based used by search results or chat responses.

- **Word Filter**: Get entries that include/exclude a specified term
  - Entries that contain term_to_include: `+"term_to_include"`
  - Entries that contain term_to_exclude: `-"term_to_exclude"`
- **Date Filter**: Get entries containing dates in YYYY-MM-DD format from specified date (range)
  - Entries from April 1st 1984: `dt:"1984-04-01"`
  - Entries after March 31st 1984: `dt>="1984-04-01"`
  - Entries before April 2nd 1984 : `dt<="1984-04-01"`
- **File Filter**: Get entries from a specified file
  - Entries from incoming.org file: `file:"incoming.org"`
- Combined Example
  - `what is the meaning of life? file:"1984.org" dt>="1984-01-01" dt<="1985-01-01" -"big" -"brother"`
  - Adds all filters to the natural language query. It should return entries
    - from the file *1984.org*
    - containing dates from the year *1984*
    - excluding words *"big"* and *"brother"*
    - that best match the natural language query *"what is the meaning of life?"*
