---
sidebar_position: 4
---

# Agents

You can use agents to setup custom system prompts with Khoj. The server host can setup their own agents, which are accessible to all users. You can see ours at https://app.khoj.dev/agents.

![Demo](/img/agents_page_full.png)

## Creating an Agent (Self-Hosted)

Go to `server/admin/database/agent` on your server and click `Add Agent` to create a new one. You have to set it to `public` in order for it to be accessible to all the users on your server. To limit access to a specific user, do not set the `public` flag and add the user in the `Creator` field.

Set your custom prompt in the `personality` field.
