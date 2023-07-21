## Khoj Search
- **Khoj via Obsidian**
  - Click the *Khoj search* icon 🔎 on the [Ribbon](https://help.obsidian.md/User+interface/Workspace/Ribbon) or Search for *Khoj: Search* in the [Command Palette](https://help.obsidian.md/Plugins/Command+palette)
- **Khoj via Emacs**
  - Run `M-x khoj <user-query>`
- **Khoj via Web**
  - Open <http://localhost:42110/> directly
- **Khoj via API**
  - See the Khoj FastAPI [Swagger Docs](http://localhost:42110/docs), [ReDocs](http://localhost:42110/redocs)

### Query Filters

Use structured query syntax to filter the natural language search results
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

## Details
1. Your query is used to retrieve the most relevant notes, if any, using Khoj search
2. These notes, the last few messages and associated metadata is passed to ChatGPT along with your query for a response


## Performance

### Query performance

- Semantic search using the bi-encoder is fairly fast at \<50 ms
- Reranking using the cross-encoder is slower at \<2s on 15 results. Tweak `top_k` to tradeoff speed for accuracy of results
- Filters in query (e.g by file, word or date) usually add \<20ms to query latency

### Indexing performance

- Indexing is more strongly impacted by the size of the source data
- Indexing 100K+ line corpus of notes takes about 10 minutes
- Indexing 4000+ images takes about 15 minutes and more than 8Gb of RAM
- Note: *It should only take this long on the first run* as the index is incrementally updated

### Miscellaneous

- Testing done on a Mac M1 and a \>100K line corpus of notes
- Search, indexing on a GPU has not been tested yet

## Advanced Usage

### Use OpenAI Models for Search
#### Setup
1. Set `encoder-type`, `encoder` and `model-directory` under `asymmetric` and/or `symmetric` `search-type` in your `khoj.yml`[^1]:
   ```diff
      asymmetric:
   -    encoder: "sentence-transformers/multi-qa-MiniLM-L6-cos-v1"
   +    encoder: text-embedding-ada-002
   +    encoder-type: khoj.utils.models.OpenAI
        cross-encoder: "cross-encoder/ms-marco-MiniLM-L-6-v2"
   -    encoder-type: sentence_transformers.SentenceTransformer
   -    model_directory: "~/.khoj/search/asymmetric/"
   +    model-directory: null
   ```
2. [Setup your OpenAI API key in Khoj](#set-your-openai-api-key-in-khoj)
3. Restart Khoj server to generate embeddings. It will take longer than with offline models.

#### Warnings
  This configuration *uses an online model*
  - It will **send all notes to OpenAI** to generate embeddings
  - **All queries will be sent to OpenAI** when you search with Khoj
  - You will be **charged by OpenAI** based on the total tokens processed
  - It *requires an active internet connection* to search and index
