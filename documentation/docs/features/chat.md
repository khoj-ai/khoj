---
sidebar_position: 2
---

# Chat

You can configure Khoj to chat with you about anything. When relevant, it'll use any notes or documents you shared with it to respond.

### Overview
- Creates a personal assistant for you to inquire and engage with your notes
- You can choose to use Online or Offline Chat depending on your requirements
- Supports multi-turn conversations with the relevant notes for context
- Shows reference notes used to generate a response

### Setup (Self-Hosting)
#### Offline Chat
Offline chat stays completely private and works without internet using open-source models.

> **System Requirements**:
>  - Minimum 8 GB RAM. Recommend **16Gb VRAM**
>  - Minimum **5 GB of Disk** available
>  - A CPU supporting [AVX or AVX2 instructions](https://en.wikipedia.org/wiki/Advanced_Vector_Extensions) is required
>  - A Mac M1+ or [Vulcan supported GPU](https://vulkan.gpuinfo.org/) should significantly speed up chat response times

1. Open your [Khoj offline settings](http://localhost:42110/server/admin/database/offlinechatprocessorconversationconfig/) and click *Enable* on the Offline Chat configuration.
2. Open your [Chat model options](http://localhost:42110/server/admin/database/chatmodeloptions/) and add a new option for the offline chat model you want to use. Make sure to use `Offline` as its type. We currently only support offline models that use the [Llama chat prompt](https://replicate.com/blog/how-to-prompt-llama#wrap-user-input-with-inst-inst-tags) format. We recommend using `mistral-7b-instruct-v0.1.Q4_0.gguf`.


:::tip[Note]
Offline chat is not supported for a multi-user scenario. The host machine will encounter segmentation faults if multiple users try to use offline chat at the same time.
:::

#### Online Chat
Online chat requires internet to use ChatGPT but is faster, higher quality and less compute intensive.

:::danger[Warning]
This will enable Khoj to send your chat queries and query relevant notes to OpenAI for processing.
:::

1. Get your [OpenAI API Key](https://platform.openai.com/account/api-keys)
2. Open your [Khoj Online Chat settings](http://localhost:42110/server/admin/database/openaiprocessorconversationconfig/). Add a new setting with your OpenAI API key, and click *Save*. Only one configuration will be used, so make sure that's the only one you have.
3. Open your [Chat model options](http://localhost:42110/server/admin/database/chatmodeloptions/) and add a new option for the OpenAI chat model you want to use. Make sure to use `OpenAI` as its type.

### Use
1. Open Khoj Chat
    - **On Web**: Open [/chat](https://app.khoj.dev/chat) in your web browser
    - **On Obsidian**: Search for *Khoj: Chat* in the [Command Palette](https://help.obsidian.md/Plugins/Command+palette)
    - **On Emacs**: Run `M-x khoj <user-query>`
2. Enter your queries to chat with Khoj. Use [slash commands](#commands) and [query filters](./advanced.md#query-filters) to change what Khoj uses to respond

![](/img/khoj_chat_on_web.png ':size=400px')

#### Details
1. Your query is used to retrieve the most relevant notes, if any, using Khoj search
2. These notes, the last few messages and associated metadata is passed to the enabled chat model along with your query to generate a response

#### Commands
Slash commands allows you to change what Khoj uses to respond to your query
- **/notes**: Limit chat to only respond using your notes, not just Khoj's general world knowledge as reference
- **/general**: Limit chat to only respond using Khoj's general world knowledge, not using your notes as reference
- **/default**: Allow chat to respond using your notes or it's general knowledge as reference. It's the default behavior when no slash command is used
- **/online**: Use online information and incorporate it in the prompt to the LLM to send you a response.
- **/image**: Generate an image in response to your query.
- **/help**: Use /help to get all available commands and general information about Khoj
