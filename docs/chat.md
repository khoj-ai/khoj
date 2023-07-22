### Khoj Chat
#### Overview
- Creates a personal assistant for you to inquire and engage with your notes
- Uses [ChatGPT](https://openai.com/blog/chatgpt) and [Khoj search](/#/search). [Offline chat](https://github.com/khoj-ai/khoj/issues/201) is coming soon.
- Supports multi-turn conversations with the relevant notes for context
- Shows reference notes used to generate a response

!> **Warning**: This will enable Khoj to send your query and note(s) to OpenAI for processing

#### Setup
- Get your [OpenAI API Key](https://platform.openai.com/account/api-keys)
- Add your OpenAI API to Khoj by using either of the two options below:

  - Open your [Khoj settings](http://localhost:42110/config/processor/conversation), add your OpenAI API key, and click *Save*. Then go to your [Khoj settings](http://localhost:42110/config) and click `Configure`. This will refresh Khoj with your OpenAI API key.

  - Set `openai-api-key` field under `processor.conversation` section in your `khoj.yml` @ `~/.khoj/khoj.yml` to your [OpenAI API key](https://beta.openai.com/account/api-keys) and restart khoj:
    ```diff
    processor:
      conversation:
    -    openai-api-key: # "YOUR_OPENAI_API_KEY"
    +    openai-api-key: sk-aaaaaaaaaaaaaaaaaaaaaaaahhhhhhhhhhhhhhhhhhhhhhhh
        model: "text-davinci-003"
        conversation-logfile: "~/.khoj/processor/conversation/conversation_logs.json"
    ```

#### Use
1. Open [/chat](http://localhost:42110/chat)
2. Type your queries and see response by Khoj from your notes

#### Demo
![](./assets/khoj_chat_on_web.png ':size=400px')

### Details
1. Your query is used to retrieve the most relevant notes, if any, using Khoj search
2. These notes, the last few messages and associated metadata is passed to ChatGPT along with your query for a response
