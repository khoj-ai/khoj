---
sidebar_position: 1
---

# Desktop

> Query your Second Brain from your machine

Use the Desktop app to chat and search with Khoj.
You can also share your files, folders with Khoj using the app.
Khoj will keep these files in sync to provide contextual responses when you search or chat.

## Features
- **Chat**
  - **Faster answers**: Find answers quickly, from your private notes or the public internet
  - **Assisted creativity**: Smoothly weave across retrieving answers and generating content
  - **Iterative discovery**: Iteratively explore and re-discover your notes
  - **Quick access**: Use [Khoj Mini](/features/khoj_mini) on the desktop to quickly pull up a mini chat module for quicker answers
- **Search**
  - **Natural**: Advanced natural language understanding using Transformer based ML Models
  - **Incremental**: Incremental search for a fast, search-as-you-type experience

## Setup
:::info[Self Hosting]
If you are self-hosting the Khoj server, update the *Settings* page on the Khoj Desktop app to:
- Set the `Khoj URL` field to your Khoj server URL. By default, use `http://127.0.0.1:42110`.
- Do not set the `Khoj API Key` field if your Khoj server runs in anonymous mode. For example, `khoj --anonymous-mode`
:::


1. Install the [Khoj Desktop app](https://khoj.dev/downloads) for your OS
2. Generate an API key on the [Khoj Web App](https://app.khoj.dev/settings#clients)
3. Set your Khoj API Key on the *Settings* page of the Khoj Desktop app
4. [Optional] Add any files, folders you'd like Khoj to be aware of on the *Settings* page and Click *Save*.
   These files and folders will be automatically kept in sync for you

## Interface
| Chat | Search |
|:----:|:------:|
| ![](/img/khoj_chat_on_desktop.png) | ![](/img/khoj_search_on_desktop.png) |
