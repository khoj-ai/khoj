---
sidebar_position: 1
---

# Use OpenAI Proxy
:::info
This is only helpful for self-hosted users. If you're using [Khoj Cloud](https://app.khoj.dev), you're limited to our first-party models.
:::

:::info
Khoj natively supports local LLMs [available on HuggingFace in GGUF format](https://huggingface.co/models?library=gguf). Using an OpenAI API proxy with Khoj maybe useful for ease of setup, trying new models or using commercial LLMs via API.
:::

Khoj can use any OpenAI API compatible server including local providers like [Ollama](/advanced/ollama), [LMStudio](/advanced/lmstudio) and [LiteLLM](/advanced/litellm) and commercial providers like [HuggingFace](https://huggingface.co/docs/api-inference/tasks/chat-completion#using-the-api), [OpenRouter](https://openrouter.ai/docs/quick-start) etc.
Configuring this allows you to use non-standard, open or commercial, local or hosted LLM models for Khoj.

Combine them with Khoj can turn your favorite LLM into an AI agent. Allowing you to chat with your docs, find answers from the internet, build custom agents and run automations.

For specific integrations, see our [Ollama](/advanced/ollama), [LMStudio](/advanced/lmstudio) and [LiteLLM](/advanced/litellm) setup docs. For general instructions to setup Khoj with an OpenAI API proxy see below.

## General Setup

1. Start your preferred OpenAI API compatible app locally or get API keys from commercial AI model providers.
3. Create a new [API Model API](http://localhost:42110/server/admin/database/aimodelapi/add) on your Khoj admin panel
   - **Name**: `any name`
   - **Api Key**: `any string`
   - **Api Base Url**: *The URL of your Openai Compatible API*
3. Create a new [Chat Model](http://localhost:42110/server/admin/database/chatmodel/add) on your Khoj admin panel.
   - **Name**: `llama3` (replace with the name of your local model)
   - **Model Type**: `Openai`
   - **Ai Model Api**: *The AI Model API you created in step 2*
   - **Max prompt size**: `2000` (replace with the max prompt size of your model)
   - **Tokenizer**: *Do not set for OpenAI, mistral, llama3 based models*
4. Go to [your config](http://localhost:42110/settings) and select the model you just created in the chat model dropdown.

## Example: Fikra API

Fikra API (https://fikraapi.co.ke) is an OpenAI-compatible hosted inference endpoint serving DeepSeek, GLM, Kimi, StepFun and Qwen models.

In the steps above, use:

- **Api Base Url**: `https://api.fikraapi.co.ke/v1`
- **Api Key**: your Fikra API key from https://fikraapi.co.ke
- For the Chat Model's **Name**, use `deepseek-v4.1-flash` (model ids come from `https://api.fikraapi.co.ke/v1/models`)

Models on the endpoint: DeepSeek V4.1 Flash ($0.045 per 1M input / $0.09 per 1M output, 1M context; the 1017 build is $0.05/$0.11), GLM 5.3 ($1.05/$3.35), GLM 5.3 Flash ($0.155/$0.49), GLM 5.2 ($0.75/$1.65), Kimi K3 ($2.10/$10.50), StepFun Step 5 (preview), Qwen 3.8 27B ($0.045/$0.35, cached input $0.10), MiMo V2.6 Pro, Nemotron 3.5 Super VL. Per-token USD with $1 minimum top-up; M-Pesa and card payments. Prices verified 2026-10-19 at [fikraapi.co.ke/pricing](https://fikraapi.co.ke/pricing).
