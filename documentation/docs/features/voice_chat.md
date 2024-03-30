# Voice

You can talk to Khoj using your voice. Khoj will respond to your queries using the same models as the chat feature. You can use voice chat on the web, Desktop, and Obsidian apps. Click on the little mic icon to send your voice message to Khoj. It will send back what it heard via text. You'll have some time to edit it before sending it, if required. Try it at https://app.khoj.dev/.

:::info[Voice Response]
Khoj doesn't yet respond with voice, but it will send back a text response. Let us know if you're interested in voice responses at team at khoj.dev.
:::

## Setup (Self-Hosting)

Voice chat will automatically be configured when you initialize the application. The default configuration will run locally. If you want to use the OpenAI whisper API for voice chat, you can set it up by following these steps:

1. Setup your OpenAI API key. See instructions [here](/get-started/setup#2-configure).
2. Create a new configuration at http://localhost:42110/server/admin/database/speechtotextmodeloptions/. We recommend the value `whisper-1` and model type `Openai`.
