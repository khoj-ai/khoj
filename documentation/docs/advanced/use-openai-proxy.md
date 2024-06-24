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

Khoj can use any OpenAI API compatible server including [Ollama](#ollama), [LMStudio](#lm-studio) and [LiteLLM](#litellm).
Configuring this allows you to use non-standard, open or commercial, local or hosted LLM models for Khoj

Combine them with Khoj can turn your favorite LLM into an AI agent. Allowing you to chat with your docs, find answers from the internet, build custom agents and run automations.

## Ollama
Ollama allows you to run [many popular open-source LLMs](https://ollama.com/library) locally from your terminal.
For folks comfortable with the terminal, Ollama's terminal based flows can ease setup and management of chat models.

Ollama exposes a local [OpenAI API compatible server](https://github.com/ollama/ollama/blob/main/docs/openai.md#models). This makes it possible to use chat models from Ollama to create your personal AI agents with Khoj.

### Setup

1. Setup Ollama: https://ollama.com/
2. Start your preferred model with Ollama. For example,
    ```bash
    ollama run llama3
    ```
3. Create a new [OpenAI Processor Conversation Config](http://localhost:42110/server/admin/database/openaiprocessorconversationconfig/add) on your Khoj admin panel
   - Name: `ollama`
   - Api Key: `any string`
   - Api Base Url: `http://localhost:11434/v1/` (default for Ollama)
4. Create a new [Chat Model Option](http://localhost:42110/server/admin/database/chatmodeloptions/add) on your Khoj admin panel.
   - Name: `llama3` (replace with the name of your local model)
   - Model Type: `Openai`
   - Openai Config: `<the ollama config you created in step 3>`
   - Max prompt size: `1000` (replace with the max prompt size of your model)
5. Create a new [Server Chat Setting](http://localhost:42110/server/admin/database/serverchatsettings/add/) on your Khoj admin panel
   - Default model: `<name of chat model option you created in step 4>`
   - Summarizer model: `<name of chat model option you created in step 4>`
6. Go to [your config](http://localhost:42110/config) and select the model you just created in the chat model dropdown.

That's it! You should now be able to chat with your Ollama model from Khoj. If you want to add additional models running on Ollama, repeat step 6 for each model.


## LM Studio
[LM Studio](https://lmstudio.ai/) is a desktop app to chat with open-source LLMs on your local machine. LM Studio provides a neat interface for folks comfortable with a GUI.

LM Studio can also expose an [OpenAI API compatible server](https://lmstudio.ai/docs/local-server). This makes it possible to turn chat models from LM Studio into your personal AI agents with Khoj.

### Setup
1. Install [LM Studio](https://lmstudio.ai/) and download your preferred Chat Model
2. Go to the Server Tab on LM Studio, Select your preferred Chat Model and Click the green Start Server button
3. Create a new [OpenAI Processor Conversation Config](http://localhost:42110/server/admin/database/openaiprocessorconversationconfig/add) on your Khoj admin panel
   - Name: `proxy-name`
   - Api Key: `any string`
   - Api Base Url: `http://localhost:1234/v1/` (default for LMStudio)
4. Create a new [Chat Model Option](http://localhost:42110/server/admin/database/chatmodeloptions/add) on your Khoj admin panel.
   - Name: `llama3` (replace with the name of your local model)
   - Model Type: `Openai`
   - Openai Config: `<the proxy config you created in step 3>`
   - Max prompt size: `2000` (replace with the max prompt size of your model)
   - Tokenizer: *Do not set for OpenAI, mistral, llama3 based models*
5. Create a new [Server Chat Setting](http://localhost:42110/server/admin/database/serverchatsettings/add/) on your Khoj admin panel
   - Default model: `<name of chat model option you created in step 4>`
   - Summarizer model: `<name of chat model option you created in step 4>`
6. Go to [your config](http://localhost:42110/config) and select the model you just created in the chat model dropdown.

## LiteLLM
[LiteLLM](https://docs.litellm.ai/docs/proxy/quick_start) exposes an OpenAI compatible API that proxies requests to other LLM API services. This provides a standardized API to interact with both open-source and commercial LLMs.

Using LiteLLM with Khoj makes it possible to turn any LLM behind an API into your personal AI agent.

### Setup
1. Install LiteLLM
   ```bash
   pip install litellm[proxy]
   ```
2. Start LiteLLM and use Mistral tiny via Mistral API
   ```
   export MISTRAL_API_KEY=<MISTRAL_API_KEY>
   litellm --model mistral/mistral-tiny --drop_params
   ```
3. Create a new [OpenAI Processor Conversation Config](http://localhost:42110/server/admin/database/openaiprocessorconversationconfig/add) on your Khoj admin panel
   - Name: `proxy-name`
   - Api Key: `any string`
   - Api Base Url: **URL of your Openai Proxy API**
4. Create a new [Chat Model Option](http://localhost:42110/server/admin/database/chatmodeloptions/add) on your Khoj admin panel.
   - Name: `llama3` (replace with the name of your local model)
   - Model Type: `Openai`
   - Openai Config: `<the proxy config you created in step 3>`
   - Max prompt size: `2000` (replace with the max prompt size of your model)
   - Tokenizer: *Do not set for OpenAI, mistral, llama3 based models*
5. Create a new [Server Chat Setting](http://localhost:42110/server/admin/database/serverchatsettings/add/) on your Khoj admin panel
   - Default model: `<name of chat model option you created in step 4>`
   - Summarizer model: `<name of chat model option you created in step 4>`
6. Go to [your config](http://localhost:42110/config) and select the model you just created in the chat model dropdown.


## General

1. Start your preferred OpenAI API compatible app
3. Create a new [OpenAI Processor Conversation Config](http://localhost:42110/server/admin/database/openaiprocessorconversationconfig/add) on your Khoj admin panel
   - Name: `proxy-name`
   - Api Key: `any string`
   - Api Base Url: **URL of your Openai Proxy API**
4. Create a new [Chat Model Option](http://localhost:42110/server/admin/database/chatmodeloptions/add) on your Khoj admin panel.
   - Name: `llama3` (replace with the name of your local model)
   - Model Type: `Openai`
   - Openai Config: `<the proxy config you created in step 3>`
   - Max prompt size: `2000` (replace with the max prompt size of your model)
   - Tokenizer: *Do not set for OpenAI, mistral, llama3 based models*
5. Create a new [Server Chat Setting](http://localhost:42110/server/admin/database/serverchatsettings/add/) on your Khoj admin panel
   - Default model: `<name of chat model option you created in step 4>`
   - Summarizer model: `<name of chat model option you created in step 4>`
6. Go to [your config](http://localhost:42110/config) and select the model you just created in the chat model dropdown.
