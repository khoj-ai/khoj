# LiteLLM
:::info
This is only helpful for self-hosted users. If you're using [Khoj Cloud](https://app.khoj.dev), you're limited to our first-party models.
:::

:::info
Khoj natively supports local LLMs [available on HuggingFace in GGUF format](https://huggingface.co/models?library=gguf). Using an OpenAI API proxy with Khoj maybe useful for ease of setup, trying new models or using commercial LLMs via API.
:::

[LiteLLM](https://docs.litellm.ai/docs/proxy/quick_start) exposes an OpenAI compatible API that proxies requests to other LLM API services. This provides a standardized API to interact with both open-source and commercial LLMs.

Using LiteLLM with Khoj makes it possible to turn any LLM behind an API into your personal AI agent.

## Setup
1. Install LiteLLM
   ```bash
   pip install litellm[proxy]
   ```
2. Start LiteLLM and use Mistral tiny via Mistral API
   ```
   export MISTRAL_API_KEY=<MISTRAL_API_KEY>
   litellm --model mistral/mistral-tiny --drop_params
   ```
3. Create a new [API Model API](http://localhost:42110/server/admin/database/aimodelapi/add) on your Khoj admin panel
   - Name: `proxy-name`
   - Api Key: `any string`
   - Api Base Url: **URL of your Openai Proxy API**
4. Create a new [Chat Model](http://localhost:42110/server/admin/database/chatmodel/add) on your Khoj admin panel.
   - Name: `llama3.1` (replace with the name of your local model)
   - Model Type: `Openai`
   - Openai Config: `<the proxy config you created in step 3>`
   - Max prompt size: `20000` (replace with the max prompt size of your model)
   - Tokenizer: *Do not set for OpenAI, Mistral, Llama3 based models*
5. Go to [your config](http://localhost:42110/settings) and select the model you just created in the chat model dropdown.
