# Rapid-MLX

:::info
This is only helpful for self-hosted users. If you're using [Khoj Cloud](https://app.khoj.dev), you're limited to our first-party models.
:::

[Rapid-MLX](https://github.com/raullenchai/Rapid-MLX) runs local models on Apple Silicon Macs and exposes an OpenAI-compatible API. You can use it with Khoj for chat and document embeddings.

## Setup

1. Install Rapid-MLX on your Mac:
   ```bash
   pip install rapid-mlx
   ```
2. Start a chat model and an embedding model:
   ```bash
   rapid-mlx serve qwen3.5-4b-4bit --embedding-model embeddinggemma-2-4bit
   ```
   The server listens on port `8000` by default. To serve only the chat model, omit `--embedding-model embeddinggemma-2-4bit`.
3. Create a new [AI Model API](http://localhost:42110/server/admin/database/aimodelapi/add/) on your Khoj admin panel.
   - **Name**: `Rapid-MLX`
   - **Api Key**: `rapid-mlx` (a placeholder; Rapid-MLX does not require an API key)
   - **Api Base Url**: `http://127.0.0.1:8000/v1`
4. Create a new [Chat Model](http://localhost:42110/server/admin/database/chatmodel/add) on your Khoj admin panel.
   - **Name**: `mlx-community/Qwen3.5-4B-MLX-4bit` (replace with the model ID served by Rapid-MLX if you use another model)
   - **Model Type**: `Openai`
   - **Ai Model Api**: *the Rapid-MLX AI Model API you created in step 3*
   - **Max prompt size**: `20000` (adjust within your model's context limit)
   - **Tokenizer**: *leave blank*
5. Go to [your config](http://localhost:42110/settings) and select the model you just created in the chat model dropdown.

Create each Chat Model manually. Khoj does not automatically discover models for an AI Model API named `Rapid-MLX`.

:::info
The URLs above assume Khoj and Rapid-MLX run directly on the same Mac. If Khoj runs in Docker, use `http://host.docker.internal:8000/v1` for the API base URL and embeddings endpoint, and ensure the Rapid-MLX server is reachable from the container.
:::

## Document embeddings

1. Open the [SearchModelConfig](http://localhost:42110/server/admin/database/searchmodelconfig/) page on your Khoj admin panel and edit your search model config.
   - **Name**: `default` (ensure this is the only search model config named `default`)
   - **Bi encoder**: `embeddinggemma-2-4bit`
   - **Embeddings inference endpoint type**: `Openai`
   - **Embeddings inference endpoint**: `http://127.0.0.1:8000/v1`
   - **Embeddings inference endpoint api key**: `rapid-mlx`
2. Save the config and restart Khoj.
3. Re-index your documents when switching embedding models, then use [Search](/features/search) or chat with your documents.

Use the API base URL for the embeddings endpoint; Khoj appends `/embeddings` itself. Keep your existing cross-encoder settings: Khoj's reranker adapter does not support Rapid-MLX's `/v1/rerank` endpoint.

See the [search configuration guide](/features/search#setup-self-hosting) for confidence threshold tuning.
