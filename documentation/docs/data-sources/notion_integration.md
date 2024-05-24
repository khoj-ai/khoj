# Notion Integration

The Notion integration allows you to search/chat with your Notion workspaces. [Notion](https://notion.so/) is a platform people use for taking notes, especially for collaboration.

Go to https://app.khoj.dev/config to connect your Notion workspace(s) to Khoj.

![notion_integration](https://assets.khoj.dev/notion_integration.gif)


## Self-Hosted Setup

1. Go to https://www.notion.so/my-integrations and create a new integration called Khoj to get an API key.
![setup_new_integration](https://github.com/khoj-ai/khoj/assets/65192171/b056e057-d4dc-47dc-aad3-57b59a22c68b)
3. Share all the workspaces that you want to integrate with the Khoj integration you just made in the previous step
![enable_workspace](https://github.com/khoj-ai/khoj/assets/65192171/98290303-b5b8-4cb0-b32c-f68c6923a3d0)
4. In the first step, you generated an API key. Use the newly generated API Key in your Khoj settings, by default at http://localhost:42110/config/content-source/notion. Click `Save`.
5. Click `Configure` in http://localhost:42110/config to index your Notion workspace(s).

That's it! You should be ready to start searching and chatting. Make sure you've configured your [chat settings](/get-started/setup#2-configure).
