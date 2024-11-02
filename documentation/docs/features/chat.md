---
sidebar_position: 2
---

# Chat

You can configure Khoj to chat with you about anything. When relevant, it'll use any notes or documents you shared with it to respond. It acts as an excellent research assistant, search engine, or personal tutor.

<img src="https://assets.khoj.dev/vision_chat_example.png" alt="Chat on Web" style={{width: '400px'}}/>

### Overview
- Creates a personal assistant for you to inquire and engage with your notes or online information as needed
- You can choose to use Online or Offline Chat depending on your requirements
- Supports multi-turn conversations with the relevant notes for context
- Shows reference notes used to generate a response

### Setup (Self-Hosting)
See [the setup guide](/get-started/setup.mdx) to configure your chat models.

### Use
1. Open Khoj Chat
    - **On Web**: Open [/chat](https://app.khoj.dev/chat) in your web browser
    - **On Obsidian**: Search for *Khoj: Chat* in the [Command Palette](https://help.obsidian.md/Plugins/Command+palette)
    - **On Emacs**: Run `M-x khoj <user-query>`
2. Enter your queries to chat with Khoj. Use [slash commands](#commands) and [query filters](/miscellaneous/advanced#query-filters) to change what Khoj uses to respond


#### Details
1. Your query is used to retrieve the most relevant notes, if any, using Khoj search using RAG.
2. These notes, the last few messages and associated metadata is passed to the enabled chat model along with your query to generate a response

#### Conversation File Filters
You can use conversation file filters to limit the notes used in the chat response. To do so, use the left panel in the web UI. Alternatively, you can also use [query filters](/miscellaneous/advanced#query-filters) to limit the notes used in the chat response.

<img src="/img/file_filters_conversation.png" alt="Conversation File Filter" style={{width: '400px'}}/>

#### Commands
Slash commands allows you to change what Khoj uses to respond to your query
- **/notes**: Limit chat to only respond using your notes, not just Khoj's general world knowledge as reference
- **/general**: Limit chat to only respond using Khoj's general world knowledge, not using your notes as reference
- **/default**: Allow chat to respond using your notes or it's general knowledge as reference. It's the default behavior when no slash command is used
- **/online**: Use online information and incorporate it in the prompt to the LLM to send you a response.
- **/image**: Generate an image in response to your query.
- **/help**: Use /help to get all available commands and general information about Khoj
- **/summarize**: Can be used to summarize 1 selected file filter for that conversation. Refer to [File Summarization](summarization) for details.
- **/diagram**: Generate a diagram in response to your query. This is built on [Excalidraw](https://excalidraw.com/).
- **/code**: Generate and run very simple Python code snippets. Refer to [Code Execution](code_execution) for details.
- **/research**: Go deeper in a topic for more accurate, in-depth responses.
