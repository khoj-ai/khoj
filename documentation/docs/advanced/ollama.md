# Ollama
:::info
This is only helpful for self-hosted users. If you're using [Khoj Cloud](https://app.khoj.dev), you're limited to our first-party models.
:::

:::info
Khoj natively supports local LLMs [available on HuggingFace in GGUF format](https://huggingface.co/models?library=gguf). Using an OpenAI API proxy with Khoj maybe useful for ease of setup, trying new models or using commercial LLMs via API.
:::

Ollama allows you to run [many popular open-source LLMs](https://ollama.com/library) locally from your terminal.
For folks comfortable with the terminal, Ollama's terminal based flows can ease setup and management of chat models.

Ollama exposes a local [OpenAI API compatible server](https://github.com/ollama/ollama/blob/main/docs/openai.md#models). This makes it possible to use chat models from Ollama to create your personal AI agents with Khoj.

## Setup

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
6. Go to [your config](http://localhost:42110/configure) and select the model you just created in the chat model dropdown.

That's it! You should now be able to chat with your Ollama model from Khoj. If you want to add additional models running on Ollama, repeat step 6 for each model.
