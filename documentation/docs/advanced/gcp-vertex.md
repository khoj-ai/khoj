# Google Vertex AI
:::info
This is only helpful for self-hosted users. If you're using [Khoj Cloud](https://app.khoj.dev), you can directly use any of the pre-configured AI models.
:::

Khoj can use Google's Gemini and Anthropic's Claude family of AI models from [Vertex AI](https://cloud.google.com/vertex-ai) on Google Cloud. Explore Anthropic and Gemini AI models available on Vertex AI's [Model Garden](https://console.cloud.google.com/vertex-ai/model-garden).

## Setup
1. Follow [these instructions](https://cloud.google.com/vertex-ai/generative-ai/docs/partner-models/use-claude#before_you_begin) to use models on GCP Vertex AI.
2. Create [Service Account](https://console.cloud.google.com/apis/credentials/serviceaccountkey) credentials.
   - Download the credentials keyfile in json format.
   - Base64 encode the credentials json keyfile. For example by running the following command from your terminal:
     `base64 -i <service_account_credentials_keyfile.json>`
3. Create a new [API Model API](http://localhost:42110/server/admin/database/aimodelapi/add) on your Khoj admin panel.
   - **Name**: `Google Vertex` (or whatever friendly name you prefer).
   - **Api Key**: `base64 encoded json keyfile` from step 2.
   - **Api Base Url**: `https://{MODEL_GCP_REGION}-aiplatform.googleapis.com/v1/projects/{YOUR_GCP_PROJECT_ID}`
     - MODEL_GCP_REGION: A region the AI model is available in. For example `us-east5` works for [Claude](https://cloud.google.com/vertex-ai/generative-ai/docs/partner-models/use-claude#regions).
     - YOUR_GCP_PROJECT_ID: Get your project id from the [Google cloud dashboard](https://console.cloud.google.com/home/dashboard)
4. Create a new [Chat Model](http://localhost:42110/server/admin/database/chatmodel/add) on your Khoj admin panel.
   - **Name**: `claude-3-7-sonnet@20250219`. Any Claude or Gemini model on Vertex's Model Garden should work.
   - **Model Type**: `Anthropic` or `Google`
   - **Ai Model API**: *the Google Vertex Ai Model API you created in step 3*
   - **Max prompt size**: `60000` (replace with the max prompt size of your model)
   - **Tokenizer**: *Do not set*
5. Select the chat model on [your settings page](http://localhost:42110/settings) and start a conversation.

##  Troubleshooting & gcp AI Tips

-  Permission Denied?
  Ensure your service account has the `Vertex AI User` role and that the API is enabled in your GCP project.

-  Region Errors?
  Double-check that the model you're trying to use is supported in your selected region. Some Claude or Gemini models are restricted to specific zones like `us-east5` or `us-central1`.

-  Prompt Size Limitations
  The "Max prompt size" should align with the limits defined in the model documentation. Exceeding it can silently fail or truncate inputs.

-  Testing the API Key
  Before adding it to Khoj, you can verify that your key works by making a simple curl request to Vertex AI. This helps debug auth issues early.

-  Use Environment Variables
  For better security, consider using environment variables to manage sensitive keys and inject them at runtime during base64 encoding.

If you encounter any issues, the [Khoj Discord](https://discord.gg/BDgyabRM6e) is a great place to ask for help!
