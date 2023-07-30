### Khoj Chat
#### Overview
- Creates a personal assistant for you to inquire and engage with your notes
- You can choose to use Online or Offline Chat depending on your requirements
- Supports multi-turn conversations with the relevant notes for context
- Shows reference notes used to generate a response

### Setup
#### Offline Chat
Offline chat works without internet but it is slower, lower quality and more compute intensive.

!> **Warning**: This will download a 3Gb+ Llama v2 chat model which can take some time

- Open your [Khoj settings](http://localhost:42110/config/), click *Enable* on the Offline Chat card

![Configure offline chat](https://user-images.githubusercontent.com/6413477/257021364-8a2029f5-dc21-4de8-9af9-9ba6100d695c.mp4 ':include :type=mp4')

#### Online Chat
Online chat requires internet to use ChatGPT but is faster, higher quality and less compute intensive.

!> **Warning**: This will enable Khoj to send your chat queries and notes to OpenAI for processing

1. Get your [OpenAI API Key](https://platform.openai.com/account/api-keys)
2. Open your [Khoj Online Chat settings](http://localhost:42110/config/processor/conversation), add your OpenAI API key, and click *Save*. Then go to your [Khoj settings](http://localhost:42110/config) and click `Configure`. This will refresh Khoj with your OpenAI API key.

![Configure online chat](https://user-images.githubusercontent.com/6413477/256998908-ac26e55e-13a2-45fb-9348-3b90a62f7687.mp4 ':include :type=mp4')


### Use
1. Open [/chat](http://localhost:42110/chat)
2. Type your queries and see Khoj respond using your notes as reference

![](./assets/khoj_chat_on_web.png ':size=400px')

#### Details
1. Your query is used to retrieve the most relevant notes, if any, using Khoj search
2. These notes, the last few messages and associated metadata is passed to the enabled chat model along with your query to generate a response
