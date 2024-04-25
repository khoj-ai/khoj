---
sidebar_position: 3
---

# Advanced Usage

## Search across Different Languages (Self-Hosting)
To search for notes in multiple, different languages, you can use a [multi-lingual model](https://www.sbert.net/docs/pretrained_models.html#multi-lingual-models).<br />
For example, the [paraphrase-multilingual-MiniLM-L12-v2](https://huggingface.co/sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2) supports [50+ languages](https://www.sbert.net/docs/pretrained_models.html#:~:text=we%20used%20the%20following%2050%2B%20languages), has good search quality and speed. To use it:
1. Manually update the search config in server's admin settings page. Go to [the search config](http://localhost:42110/server/admin/database/searchmodelconfig/). Either create a new one, if none exists, or update the existing one. Set the bi_encoder to `sentence-transformers/multi-qa-MiniLM-L6-cos-v1` and the cross_encoder to `mixedbread-ai/mxbai-rerank-xsmall-v1`.
2. Regenerate your content index from all the relevant clients. This step is very important, as you'll need to re-encode all your content with the new model.

Note: If you use a search model that expects a prefix (e.g [mixedbread-ai/mxbai-embed-large-v1](https://huggingface.co/mixedbread-ai/mxbai-embed-large-v1)) to the query (or docs) string before encoding. Update the `bi_encoder_query_encode_config` field with `{prompt: <prefix-prompt>}`. Eg. `{prompt: "Represent this query for searching documents"}`. You can pass a valid JSON object that the SentenceTransformer `encode` function accepts

## Query Filters

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

## Use OpenAI compatible LLM API Server (Self Hosting)
Use this if you want to use non-standard, open or commercial, local or hosted LLM models for Khoj chat
1. Setup your desired chat LLM by installing an OpenAI compatible LLM API Server like [LiteLLM](https://docs.litellm.ai/docs/proxy/quick_start), [llama-cpp-python](https://github.com/abetlen/llama-cpp-python?tab=readme-ov-file#openai-compatible-web-server)
2. Set environment variable `OPENAI_API_BASE="<url-of-your-llm-server>"` before starting Khoj
3. Add ChatModelOptions with `model-type` `OpenAI`, and `chat-model` to anything (e.g `gpt-3.5-turbo`) during [Config](/get-started/setup#3-configure)
   - *(Optional)* Set the `tokenizer` and `max-prompt-size` relevant to the actual chat model you're using

#### Sample Setup using LiteLLM and Mistral API

```shell
# Install LiteLLM
pip install litellm[proxy]

# Start LiteLLM and use Mistral tiny via Mistral API
export MISTRAL_API_KEY=<MISTRAL_API_KEY>
litellm --model mistral/mistral-tiny --drop_params

# Set OpenAI API Base to LiteLLM server URL and start Khoj
export OPENAI_API_BASE='http://localhost:8000'
khoj --anonymous-mode
```
