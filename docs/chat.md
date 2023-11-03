### Khoj Chat
#### Overview
- Creates a personal assistant for you to inquire and engage with your notes
- You can choose to use Online or Offline Chat depending on your requirements
- Supports multi-turn conversations with the relevant notes for context
- Shows reference notes used to generate a response

### Setup
#### Offline Chat
Offline chat stays completely private and works without internet. But it is slower, lower quality and more compute intensive.

> **System Requirements**:
>  - Minimum 8 GB RAM. Recommend **16Gb VRAM**
>  - Minimum **5 GB of Disk** available
>  - A CPU supporting [AVX or AVX2 instructions](https://en.wikipedia.org/wiki/Advanced_Vector_Extensions) is required
>  - A Mac M1+ or [Vulcan supported GPU](https://vulkan.gpuinfo.org/) should significantly speed up chat response times

- Open your [Khoj settings](http://localhost:42110/config/) and click *Enable* on the Offline Chat card

![Configure offline chat](https://user-images.githubusercontent.com/6413477/257021364-8a2029f5-dc21-4de8-9af9-9ba6100d695c.mp4 ':include :type=mp4')

#### Online Chat
Online chat requires internet to use ChatGPT but is faster, higher quality and less compute intensive.

!> **Warning**: This will enable Khoj to send your chat queries and query relevant notes to OpenAI for processing

1. Get your [OpenAI API Key](https://platform.openai.com/account/api-keys)
2. Open your [Khoj Online Chat settings](http://localhost:42110/config/processor/conversation), add your OpenAI API key, and click *Save*. Then go to your [Khoj settings](http://localhost:42110/config) and click `Configure`. This will refresh Khoj with your OpenAI API key.

![Configure online chat](https://user-images.githubusercontent.com/6413477/256998908-ac26e55e-13a2-45fb-9348-3b90a62f7687.mp4 ':include :type=mp4')


### Use
1. Open Khoj Chat
    - **On Web**: Open [/chat](http://localhost:42110/chat) in your web browser
    - **On Obsidian**: Search for *Khoj: Chat* in the [Command Palette](https://help.obsidian.md/Plugins/Command+palette)
    - **On Emacs**: Run `M-x khoj <user-query>`
2. Enter your queries to chat with Khoj. Use [slash commands](#commands) and [query filters](./advanced.md#query-filters) to change what Khoj uses to respond

![](./assets/khoj_chat_on_web.png ':size=400px')

#### Details
1. Your query is used to retrieve the most relevant notes, if any, using Khoj search
2. These notes, the last few messages and associated metadata is passed to the enabled chat model along with your query to generate a response

#### Commands
Slash commands allows you to change what Khoj uses to respond to your query
- **/notes**: Limit chat to only respond using your notes, not just Khoj's general world knowledge as reference
- **/general**: Limit chat to only respond using Khoj's general world knowledge, not using your notes as reference
- **/default**: Allow chat to respond using your notes or it's general knowledge as reference. It's the default behavior when no slash command is used
- **/help**: Use /help to get all available commands and general information about Khoj
