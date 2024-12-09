# Admin Panel
> Describes the Khoj settings configurable via the admin panel

By default, you admin panel is available at `http://localhost:42110/server/admin/`. You can access the admin panel by logging in with your admin credentials (this would be your `KHOJ_ADMIN_EMAIL` and `KHOJ_ADMIN_PASSWORD`). The admin panel allows you to configure various settings for your Khoj server.

## App Settings
### Agents
Add all the agents you want to use for your different use-cases like Writer, Researcher, Therapist etc.
- `Personality`: This is a prompt to tell the chat model how to tune the personality of the agent.
- `Chat model`: The chat model to use for the agent.
- `Name`: The name of the agent. This field helps give the agent a unique identity across the app.
- `Avatar`: Url to the agents profile picture. It help give the agent a unique visual identity across the app.
- `Style color`, `Style icon`: These fields help give the agent a unique, visually identifiable identity across the app.
- `Slug`: This is the agent name to use in urls.
- `Public`: Check this if the agent is expected to be visible to all users on this Khoj server.
- `Managed by admin`: Check this if the agent is managed by admin, not by any user.
- `Creator`: The user who created the agent.
- `Tools`: The list of tools available to this agent. Tools include notes, image, online. This field is not currently configurable and only supports all tools (i.e `["*"]`)

### Chat Model Options
Add all the chat models you want to try, use and switch between for your different use-cases. For each chat model you add:
- `Chat model`: The name of an [OpenAI](https://platform.openai.com/docs/models), [Anthropic](https://docs.anthropic.com/en/docs/about-claude/models#model-names), [Gemini](https://cloud.google.com/vertex-ai/generative-ai/docs/learn/models#gemini-models) or [Offline](https://huggingface.co/models?pipeline_tag=text-generation&library=gguf) chat model.
- `Model type`: The chat model provider like `OpenAI`, `Offline`.
- `Vision enabled`: Set to `true` if your model supports vision. This is currently only supported for vision capable OpenAI models like `gpt-4o`
- `Max prompt size`, `Subscribed max prompt size`: These are optional fields. They are used to truncate the context to the maximum context size that can be passed to the model. This can help with accuracy and cost-saving.<br />
- `Tokenizer`: This is an optional field. It is used to accurately count tokens and truncate context passed to the chat model to stay within the models max prompt size.
  ![example configuration for chat model options](/img/example_chatmodel_option.png)

### Server Chat Settings
The server chat settings are used as:
1. The default chat models for subscribed (`Advanced` field) and unsubscribed (`Default` field) users.
2. The chat model for all intermediate steps like intent detection, web search etc. during chat response generation.

If a server chat setting is not added the first ChatModelOption in your config is used as the default chat model.

To add a server chat setting:
- Set your preferred default chat models in the `Default` fields of your [ServerChatSettings](http://localhost:42110/server/admin/database/serverchatsettings/)
- The `Advanced` field doesn't need to be set when self-hosting. When unset, the `Default` chat model is used for all users and the intermediate steps.


### AI Model API
These settings configure APIs to interact with AI models.
For each AI Model API you [add](http://localhost:42110/server/admin/database/aimodelapi/add):
- `Api key`: Set to your [OpenAI](https://platform.openai.com/api-keys), [Anthropic](https://console.anthropic.com/account/keys) or [Gemini](https://aistudio.google.com/app/apikey) API keys.
- `Name`: Give the configuration any friendly name like `OpenAI`, `Gemini`, `Anthropic`.
- `Api base url`: Set the API base URL. This is only relevant to set if you're using another OpenAI-compatible proxy server like [Ollama](/advanced/ollama) or [LMStudio](/advanced/lmstudio).
  ![example configuration for ai model api](/img/example_openai_processor_config.png)

### Search Model Configs
Search models are used to generate vector embeddings of your documents for natural language search and chat. You can choose any [embeddings models on HuggingFace](https://huggingface.co/models?pipeline_tag=sentence-similarity) to try, use for your to create vector embeddings of your documents for natural language search and chat.

<img src="/img/example_search_model_admin_settings.png" alt="Example Search Model Settings" style={{width: 500}} />

### Text to Image Model Options
Add text to image generation models with these settings. Khoj currently supports text to image models available via OpenAI, Stability or Replicate API
- `api-key`: Set to your OpenAI, Stability or Replicate API key
- `model`: Set the model name available over the selected model provider
- `model-type`: Set to the appropiate model provider
- `openai-config`: For image generation models available via OpenAI (compatible) API you can set the appropriate OpenAI Processor Conversation Settings instead of specifying the `api-key` field above

### Speech to Text Model Options
Add speech to text models with these settings. Khoj currently only supports whisper speech to text model via OpenAI API or Offline

### Voice Model Options
Add text to speech models with these settings. Khoj currently supports models from [ElevenLabs](https://elevenlabs.io/).

## User Data
- Users, Entrys, Conversations, Subscriptions, Github configs, Notion configs, User search configs, User conversation configs, User voice configs

## Miscellaneous Data
- Process Locks: Persistent Locks for Automations
- Client Applications:

Client applications allow you to setup third party applications that can query your Khoj server using a client application ID + secret. The secret would go in a bearer token.
