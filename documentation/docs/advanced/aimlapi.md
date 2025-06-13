# AI/ML API  
:::info  
This integration works for both self-hosted and cloud-hosted deployments. Simply configure your AI/ML API credentials and endpoint in Khoj, and you’ll have instant access to 300+ models—including Deepseek, Gemini, ChatGPT—backed by enterprise-grade rate limits and uptime.  
:::

AI/ML API provides a unified REST interface to over 300 cutting-edge models. Point your client at our endpoint, supply your API key, and start using any model in our catalog—no GCP setup required.

## Setup

1. **Obtain your API Key**  
   - Sign in to the AI/ML API dashboard at [website](https://aimlapi.com/app/?utm_source=khoj&utm_medium=github&utm_campaign=integration)  
   - Copy your secret API key.

2. **Create an “API Model API” in Khoj**  
   Go to **Admin → Database → AIModelAPI → Add** and enter:  
   - **Name**: `AI/ML API`  
   - **API Key**: your secret key from step 1  
   - **API Base URL**:  
     ```
     https://api.aimlapi.com/v1
     ```

3. **Add a Chat Model**  
   Go to **Admin → Database → ChatModel → Add** and configure:  
   - **Name**: any model identifier from our catalog (e.g. `chatgpt-3.5-turbo`, `gemini-pro-v1`)  
   - **Model Type**: choose `OpenAI-compatible`  
   - **AI Model API**: select the **AI/ML API** entry you created  
   - **Max prompt size**: set according to the model’s limits (e.g. `8192`)  
   - **Tokenizer**: leave blank  

4. **Select your Chat Model**  
   In **Settings**, choose your new AI/ML API chat model and start chatting!

## Troubleshooting & Tips

- **Authentication Errors?**  
  Ensure you pasted the API key correctly and didn’t include extra spaces or line breaks.

- **Rate Limit Exceeded?**  
  Check your dashboard for usage metrics. Contact support if you need higher throughput.

- **Unsupported Model Name?**  
  Verify the exact model ID in our [Model Explorer.](https://docs.aimlapi.com/?utm_source=khoj&utm_medium=github&utm_campaign=integration#model-list)

- **Environment Variables**  
  For security, store your API key in an environment variable and reference it in your Khoj config:  
  ```bash
  export AIMLAPI_KEY="xxxxxxxxxxxxxxxxxxxx"

If you hit any snags, join our Discord for real-time help: [AI/ML API](https://discord.gg/fgCwSRvazH)
