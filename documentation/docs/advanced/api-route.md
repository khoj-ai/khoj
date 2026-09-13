# API Route
:::info
This is only helpful for self-hosted users. If you're using [Khoj Cloud](https://app.khoj.dev), you're limited to our first-party models.
:::

:::info
Khoj can use any OpenAI API compatible provider to access commercial LLMs via API. Using [API Route](https://www.api-route.com) provides unified access to top-tier models (Claude 3.7 Sonnet, GPT-4o, DeepSeek, and more) through a single reliable, high-speed OpenAI-compatible gateway.
:::

[API Route](https://www.api-route.com) is an AI routing gateway that unifies access to leading large language models with an OpenAI-compatible API format.

Integrating API Route with Khoj allows you to empower your personal AI second brain with models like Claude 3.7 Sonnet, Claude 3.5 Sonnet, GPT-4o, and DeepSeek-V3/R1.

## Setup via Web Admin UI

1. Get your API Key from [API Route](https://www.api-route.com).
2. Create a new [AI Model API](http://localhost:42110/server/admin/database/aimodelapi/add/) on your Khoj admin panel:
   - **Name**: `API Route`
   - **Api Key**: `your API Route key`
   - **Api Base Url**: `https://global.api-route.com/v1`
3. Create a new [Chat Model](http://localhost:42110/server/admin/database/chatmodel/add) on your Khoj admin panel:
   - **Name**: `claude-3-7-sonnet-20250219` (or `gpt-4o`, `deepseek-chat`, etc.)
   - **Model Type**: `Openai`
   - **Ai Model Api**: *The API Route AI Model API created in step 2*
   - **Max prompt size**: `60000` (or appropriate context window for your chosen model)
   - **Vision enabled**: Check if using a multimodal model (such as `claude-3-7-sonnet-20250219` or `gpt-4o`)
   - **Tokenizer**: *Leave blank*
4. Go to [Settings](http://localhost:42110/settings) and select the model you just configured in the chat model dropdown.

## Setup via Environment Variables

When launching Khoj server, you can configure API Route directly via environment variables:

```bash
export OPENAI_BASE_URL="https://global.api-route.com/v1"
export OPENAI_API_KEY="your-api-route-key"
# Or:
# export APIROUTE_API_KEY="your-api-route-key"

khoj
```

Khoj will automatically recognize `API Route` as the provider and query available models from the endpoint.
