# Voice

You can talk to Khoj using your voice. Khoj will respond to your queries using the same models as the chat feature. You can use voice chat on the web, Desktop, and Obsidian apps.

![Voice Chat](https://assets.khoj.dev/speech_to_text_demo.gif)

Click on the little mic icon to send your voice message to Khoj. It will send back what it heard via text. You can edit the message before sending it, if required. Try it at https://app.khoj.dev/.

## Voice Response

If you send a voice message, Khoj will automatically respond back with a voice message.
You can also click on the speaker icon next to any message to hear it out loud. The voice response feature is available only on the web view right now.

![Speaker Icon](/img/text_to_speech.png)

## Setup (Self-Hosting)

Voice chat will automatically be configured when you initialize the application. The default configuration will run locally. If you want to use the OpenAI whisper API for voice chat, you can set it up by following these steps:

1. Setup your OpenAI API key. See instructions [here](/get-started/setup#2-configure).
2. Create a new configuration at http://localhost:42110/server/admin/database/speechtotextmodeloptions/. We recommend the value `whisper-1` and model type `Openai`.

If you want to use the Text to Speech feature, you can set it up by following these steps:

1. Setup your account on [ElevenLabs.io](https://elevenlabs.io/).
2. Configure your API key in your environment variables with the key `ELEVEN_LABS_API_KEY`.
2. (Optional) Create a new [Voice model option](http://localhost:42110/server/admin/database/voicemodeloption/) with a specific voice ID from whichever voice you want to use. You can explore the options [here](https://elevenlabs.io/app/voice-library).
