# OrcaRouter
:::info
This is only helpful for self-hosted users. If you're using [Khoj Cloud](https://app.khoj.dev), you're limited to our first-party models.
:::

[OrcaRouter](https://www.orcarouter.ai) is a unified OpenAI-compatible API that routes requests across 150+ chat models from OpenAI, Anthropic, Google, DeepSeek, xAI, Qwen, Kimi, MiniMax, Z-AI and more behind a single API key.

It also exposes a virtual `orcarouter/auto` router that picks an upstream model per-request based on configurable strategies (cheapest, balanced, quality, adaptive, gated-adaptive). See the [routing console](https://www.orcarouter.ai/console/routing) for strategy details.

Using OrcaRouter with Khoj lets you experiment with many chat models from a single account and key, including the latest frontier models, without managing separate provider credentials.

## Setup
1. Create an account at [orcarouter.ai](https://www.orcarouter.ai) and generate an API key.
2. Create a new [AI Model API](http://localhost:42110/server/admin/database/aimodelapi/add) on your Khoj admin panel
   - **Name**: `orcarouter`
   - **Api Key**: *your OrcaRouter API key*
   - **Api Base Url**: `https://api.orcarouter.ai/v1`
3. Create a new [Chat Model](http://localhost:42110/server/admin/database/chatmodel/add) on your Khoj admin panel.
   - **Name**: `openai/gpt-5` (or any model ID from the [OrcaRouter catalog](https://www.orcarouter.ai/models), e.g. `anthropic/claude-opus-4.7`, `google/gemini-3-flash-preview`, `deepseek/deepseek-v4-pro`, or `orcarouter/auto` for adaptive routing)
   - **Model Type**: `Openai`
   - **Ai Model Api**: *the orcarouter Ai Model API you created in step 2*
   - **Max prompt size**: `20000` (replace with the max prompt size of your chosen model)
   - **Tokenizer**: *Do not set for OpenAI, Anthropic, Gemini and similar API-based models*
4. Go to [your config](http://localhost:42110/settings) and select the model you just created in the chat model dropdown.

That's it! You can repeat step 3 to register additional models from the OrcaRouter catalog.

:::tip
The full model list and pricing is available at [orcarouter.ai/models](https://www.orcarouter.ai/models). API and routing reference: [docs.orcarouter.ai](https://docs.orcarouter.ai).
:::
