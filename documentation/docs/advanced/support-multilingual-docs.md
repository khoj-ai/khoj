# Support Multilingual Docs
Khoj uses an embedding model to understand documents. Multilingual embedding models improve the search quality for documents not in English. This affects both search and chat with docs experiences across Khoj.

To improve search and chat quality for non-english documents you can use a [multilingual model](https://www.sbert.net/docs/pretrained_models.html#multi-lingual-models).<br />
For example, the [paraphrase-multilingual-MiniLM-L12-v2](https://huggingface.co/sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2) supports [50+ languages](https://www.sbert.net/docs/pretrained_models.html#:~:text=we%20used%20the%20following%2050%2B%20languages), has decent search quality and speed for a consumer machine.
To use it:
1. Open [the search config](http://localhost:42110/server/admin/database/searchmodelconfig/) on your server's admin settings page. Either create a new search model, if none exists, or update the existing one. For example,
   - Set the `bi_encoder` field to `sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2`
   - Set the `cross_encoder` field to `mixedbread-ai/mxbai-rerank-xsmall-v1`
2. Regenerate your content index from all the relevant clients. This step is very important, as you'll need to re-encode all your content with the new model.

:::info[Note]
Modern search/embedding model like [mixedbread-ai/mxbai-embed-large-v1](https://huggingface.co/mixedbread-ai/mxbai-embed-large-v1) expect a prefix to the query (or docs) string to improve encoding. Update the `bi_encoder_query_encode_config` field of your [embedding model](http://localhost:42110/server/admin/database/searchmodelconfig/) with `{prompt: <prefix-prompt>}` to improve the search quality of these models.

E.g. `{prompt: "Represent this query for searching documents"}`. You can pass any valid JSON object that the SentenceTransformer `encode` function accepts

:::
