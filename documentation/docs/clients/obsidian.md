---
sidebar_position: 3
---

# Obsidian

> Query your Second Brain from Obsidian

![demo](https://assets.khoj.dev/obsidian_khoj_side_panel_pak_telemedicine.gif)

## Features
- **Chat**
  - **Faster answers**: Find answers quickly, from your private notes or the public internet
  - **Assisted creativity**: Smoothly weave across retrieving answers and generating content
  - **Iterative discovery**: Iteratively explore and re-discover your notes
- **Search**
  - **Natural**: Advanced natural language understanding using Transformer based ML Models
  - **Incremental**: Incremental search for a fast, search-as-you-type experience
- **Similar**
  - **Discover**: Find similar notes to the current one

## Setup
:::info[Self Hosting]
If you are self-hosting the Khoj server, update the Khoj Obsidian plugin settings step below:
- Set the `Khoj URL` field to your Khoj server URL. By default, use `http://127.0.0.1:42110`.
- Do not set the `Khoj API Key` field if your Khoj server runs in anonymous mode. For example, `khoj --anonymous-mode`
:::

1. Open [Khoj](https://obsidian.md/plugins?id=khoj) from the *Community plugins* tab in Obsidian settings panel
2. Click *Install*, then *Enable* on the Khoj plugin page in Obsidian
3. Generate an API key on the [Khoj Web App](https://app.khoj.dev/settings#clients)
4. Set your Khoj API Key in the Khoj plugin settings on Obsidian
5. (Optional) Click `Force Sync` in the Khoj plugin settings on Obsidian to immediately sync your Obsidian vault.
    <br />By default, your Obsidian vault is automatically synced periodically.

See the official [Obsidian Plugin Docs](https://help.obsidian.md/Extending+Obsidian/Community+plugins) for more details on installing Obsidian plugins.

## Use
### Chat
Click the *Khoj chat* icon 💬 on the [Ribbon](https://help.obsidian.md/User+interface/Workspace/Ribbon) or run *Khoj: Chat* from the [Command Palette](https://help.obsidian.md/Plugins/Command+palette) and ask questions in a natural, conversational style.<br />
E.g. *"When did I file my taxes last year?"*

See [Khoj Chat](/features/chat) for more details

### Find Similar Notes
To see other notes similar to the current one, run *Khoj: Find Similar Notes* from the [Command Palette](https://help.obsidian.md/Plugins/Command+palette)

### Search
Run *Khoj: Search* from the [Command Palette](https://help.obsidian.md/Plugins/Command+palette)

See [Khoj Search](/features/search) for more details. Use [query filters](/miscellaneous/advanced#query-filters) to limit entries to search

[search_demo](https://user-images.githubusercontent.com/6413477/218801155-cd67e8b4-a770-404a-8179-d6b61caa0f93.mp4 ':include :type=mp4')

## Upgrade
  1. Open *Community plugins* tab in Obsidian settings
  2. Click the *Check for updates* button
  3. Click the *Update* button next to Khoj, if available

## Troubleshooting
  - Open the Khoj plugin settings pane, to configure Khoj
  - Toggle Enable/Disable Khoj, if setting changes have not applied
  - Click *Update* button to force index to refresh, if results are failing or stale
