---
sidebar_position: 2
---

# DaoXE

:::info
This is only helpful for self-hosted users. If you're using [Khoj Cloud](https://app.khoj.dev), you're limited to our first-party models.
:::

:::info
Khoj natively supports local LLMs [available on HuggingFace in GGUF format](https://huggingface.co/models?library=gguf). Using an OpenAI API compatible gateway with Khoj is useful when you want hosted multi-model access without wiring each vendor SDK separately.
:::

[DaoXE](https://daoxe.com) is a multi-model multi-protocol API gateway. It exposes OpenAI-compatible Chat Completions at `https://daoxe.com/v1` (and also OpenAI Responses plus Anthropic Messages for other clients). Khoj talks to DaoXE through the standard OpenAI-compatible path.

This page is a community contribution from a DaoXE maintainer. DaoXE is not affiliated with Khoj. DaoXE is not available in mainland China. Prefer live model IDs from your DaoXE account (`GET /v1/models` or the [pricing/catalog page](https://daoxe.com/pricing)) over hardcoded examples.

## Setup

1. Create an API key in your [DaoXE dashboard](https://daoxe.com/dashboard).
2. Create a new [API Model API](http://localhost:42110/server/admin/database/aimodelapi/add) on your Khoj admin panel:
   - **Name**: `daoxe`
   - **Api Key**: your DaoXE API key
   - **Api Base Url**: `https://daoxe.com/v1`
3. Create a new [Chat Model](http://localhost:42110/server/admin/database/chatmodel/add) on your Khoj admin panel:
   - **Name**: an exact model ID currently available to your DaoXE account
   - **Model Type**: `Openai`
   - **Ai Model Api**: the DaoXE AI Model API you created in step 2
   - **Max prompt size**: set to a value appropriate for that model
   - **Tokenizer**: leave unset for most OpenAI-compatible chat models
4. Go to [your config](http://localhost:42110/settings) and select the chat model you just created.

## Notes

- Do not commit real API keys.
- Model IDs are account-scoped; copy them from your DaoXE catalog rather than guessing names from third-party blogs.
- For a generic OpenAI-compatible proxy (Ollama, LM Studio, LiteLLM, OpenRouter, etc.), see [Use OpenAI Proxy](/advanced/use-openai-proxy).
- Related client notes live in the [DaoXE CLIENT_SETUP guide](https://github.com/seven7763/DaoXE-AI/blob/main/CLIENT_SETUP.md).
