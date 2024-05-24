# Ollama / Khoj

You can run your own open source models locally with Ollama and use them with Khoj.

:::info[Ollama Integration]
This is only going to be helpful for self-hosted users. If you're using [Khoj Cloud](https://app.khoj.dev), you're limited to our first-party models.
:::

Khoj supports any OpenAI-API compatible server, which includes [Ollama](http://ollama.ai/). Ollama allows you to start a local server with [several popular open-source LLMs](https://ollama.com/library) directly on your own computer. Combined with Khoj, you can chat with these LLMs and use them to search your notes and documents.

While Khoj also supports local-hosted LLMs downloaded from Hugging Face, the Ollama integration is particularly useful for its ease of setup and multi-model support, especially if you're already using Ollama.

## Setup

1. Setup Ollama: https://ollama.com/
2. Start your preferred model with Ollama. For example,
    ```bash
    ollama run llama3
    ```
3. Go to Khoj settings at [OpenAI Processor Conversation Config](http://localhost:42110/server/admin/database/openaiprocessorconversationconfig/)
4. Create a new config.
   - Name: `ollama`
   - Api Key: `any string`
   - Api Base Url: `http://localhost:11434/v1/` (default for Ollama)
5. Go to [Chat Model Options](http://localhost:42110/server/admin/database/chatmodeloptions/)
6. Create a new config.
   - Name: `llama3` (replace with the name of your local model)
   - Model Type: `Openai`
   - Openai Config: `<the ollama config you created in step 4>`
   - Max prompt size: `1000` (replace with the max prompt size of your model)
7. Go to [your config](http://localhost:42110/config) and select the model you just created in the chat model dropdown.

That's it! You should now be able to chat with your Ollama model from Khoj. If you want to add additional models running on Ollama, repeat step 6 for each model.
