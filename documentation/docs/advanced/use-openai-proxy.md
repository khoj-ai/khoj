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
Configuring this allows you to use non-standard, open or commercial, local or hosted LLM models for Khoj

Combine them with Khoj can turn your favorite LLM into an AI agent. Allowing you to chat with your docs, find answers from the internet, build custom agents and run automations.

For specific integrations, see our [Ollama](/advanced/ollama), [LMStudio](/advanced/lmstudio) and [LiteLLM](/advanced/litellm) setup docs. For general instructions to setup Khoj with an OpenAI API proxy see below.

## General Setup

1. Start your preferred OpenAI API compatible app locally or get API keys from commercial AI model providers.
3. Create a new [API Model API](http://localhost:42110/server/admin/database/aimodelapi/add) on your Khoj admin panel
   - Name: `any name`
   - Api Key: `any string`
   - Api Base Url: **URL of your Openai Proxy API**
3. Create a new [Chat Model](http://localhost:42110/server/admin/database/chatmodel/add) on your Khoj admin panel.
   - Name: `llama3` (replace with the name of your local model)
   - Model Type: `Openai`
   - Openai Config: `<the proxy config you created in step 2>`
   - Max prompt size: `2000` (replace with the max prompt size of your model)
   - Tokenizer: *Do not set for OpenAI, mistral, llama3 based models*
4. Go to [your config](http://localhost:42110/settings) and select the model you just created in the chat model dropdown.
