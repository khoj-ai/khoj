# EmpirioLabs
:::info
This is only helpful for self-hosted users. If you're using [Khoj Cloud](https://app.khoj.dev), you're limited to our first-party models.
:::

:::info
Khoj natively supports local LLMs [available on HuggingFace in GGUF format](https://huggingface.co/models?library=gguf). Using an OpenAI API proxy with Khoj maybe useful for ease of setup, trying new models or using commercial LLMs via API.
:::

[EmpirioLabs](https://empiriolabs.ai) exposes an OpenAI compatible API that gives you access to many frontier open and commercial LLMs behind a single API key. This provides a standardized API to interact with a wide range of models without managing a separate provider for each one.

Using EmpirioLabs with Khoj makes it possible to turn any of these LLMs into your personal AI agent.

## Setup
1. Create an account on [EmpirioLabs](https://empiriolabs.ai) and generate an API key from the [API keys page](https://platform.empiriolabs.ai/dashboard/api-keys)
2. Create a new [AI Model API](http://localhost:42110/server/admin/database/aimodelapi/add) on your Khoj admin panel
   - **Name**: `empiriolabs`
   - **Api Key**: `your EmpirioLabs API key`
   - **Api Base Url**: `https://api.empiriolabs.ai/v1`
3. Create a new [Chat Model](http://localhost:42110/server/admin/database/chatmodel/add) on your Khoj admin panel.
   - **Name**: `qwen3-7-plus` (replace with any [model](https://docs.empiriolabs.ai) you want to use, e.g. `deepseek-v4-pro`, `glm-5-1`, `kimi-k2-7-code`)
   - **Model Type**: `Openai`
   - **Ai Model Api**: *the empiriolabs AI Model API you created in step 2*
   - **Max prompt size**: `20000` (replace with the max prompt size of your chosen model)
   - **Tokenizer**: *Do not set for OpenAI, Mistral, Llama3 based models*
4. Go to [your config](http://localhost:42110/settings) and select the model you just created in the chat model dropdown.
