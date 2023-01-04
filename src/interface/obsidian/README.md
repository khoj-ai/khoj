# Khoj Obsidian 🦅
> Natural language search for your Obsidian notes using [Khoj](https://github.com/debanjum/khoj)

## Table of Contents

- [Features](#Features)
- [Demo](#Demo)
  - [Description](#Description)
  - [Interface](#Interface)
- [Setup](#Setup)
  - [Setup Backend](#1-Setup-Backend)
  - [Setup Plugin](#2-Setup-Plugin)
- [Use](#Use)
  - [Query Filters](#Query-filters)
- [Upgrade](#Upgrade)
  - [Upgrade Backend](#1-Upgrade-Backend)
  - [Upgrade Plugin](#2-Upgrade-Plugin)
- [Troubleshoot](#Troubleshoot)
- [Implementation](#Implementation)

## Features
- **Natural**: Advanced natural language understanding using Transformer based ML Models
- **Local**: Your personal data stays local. All search, indexing is done on your machine[\*](https://github.com/debanjum/khoj#miscellaneous)
- **Incremental**: Incremental search for a fast, search-as-you-type experience

## Demo
https://user-images.githubusercontent.com/6413477/210486007-36ee3407-e6aa-4185-8a26-b0bfc0a4344f.mp4

### Description
1. Install Khoj via `pip` and start Khoj backend in non-gui mode
2. Install Khoj plugin via Community Plugins settings pane on Obsidian app
3. Check the new Khoj plugin settings
4. Let Khoj backend index the markdown files in the current Vault
5. Open Khoj plugin on Obsidian via Search button on Left Pane
6. Search \"*Announce plugin to folks*\" in the [Obsidian Plugin docs](https://marcus.se.net/obsidian-plugin-docs/)
7. Jump to the [search result](https://marcus.se.net/obsidian-plugin-docs/publishing/submit-your-plugin)

### Interface
![](https://github.com/debanjum/khoj/blob/master/src/interface/obsidian/docs/khoj_obsidian_screenshot_0.1.0.png)

## Setup
### 1. Setup Backend

```shell
pip install khoj-assistant && khoj --no-gui
```
## 2. Setup Plugin
  1. Open *Community plugins* tab in Obsidian settings panel
  2. Click Browse and Search for *Khoj*
  3. Click *Install*, after that click *Enable* on the Khoj plugin

See [official docs](https://help.obsidian.md/Advanced+topics/Community+plugins#Discover+and+install+community+plugins) for details

## Use
Click the *Khoj search* icon 🔎 on the [Ribbon](https://help.obsidian.md/User+interface/Workspace/Ribbon) or Search for *Khoj: Search* in the [Command Palette](https://help.obsidian.md/Plugins/Command+palette)

### Query Filters
Use structured query syntax to filter the natural language search results
- **Word Filter**: Get entries that include/exclude a specified term
  - Entries that contain term_to_include: `+"term_to_include"`
  - Entries that contain term_to_exclude: `-"term_to_exclude"`
- **Date Filter**: Get entries containing dates in YYYY-MM-DD format from specified date (range)
  - Entries from April 1st 1984: `dt:"1984-04-01"`
  - Entries after March 31st 1984: `dt>="1984-04-01"`
  - Entries before April 2nd 1984 : `dt<="1984-04-01"`
- **File Filter**: Get entries from a specified file
  - Entries from incoming.org file: `file:"incoming.org"`
- Combined Example
  - `what is the meaning of life? file:"1984.org" dt>="1984-01-01" dt<="1985-01-01" -"big" -"brother"`
  - Adds all filters to the natural language query. It should return entries
    - from the file *1984.org*
    - containing dates from the year *1984*
    - excluding words *"big"* and *"brother"*
    - that best match the natural language query *"what is the meaning of life?"*

## Upgrade
### 1. Upgrade Backend
  ```shell
  pip install --upgrade khoj-assistant
  ```
### 2. Upgrade Plugin
  1. Open *Community plugins* tab in Obsidian settings
  2. Click the *Check for updates* button
  3. Click the *Update* button next to Khoj, if available

## Troubleshooting
  1. Open the Khoj plugin settings pane, in case you need to configure Khoj
  2. Toggle Enable/Disable Khoj, in case settings changes have not applied

## Implementation
The plugin implements the following functionality to search your notes with Khoj:
- [X] Open the Khoj search modal via left ribbon icon or the *Khoj: Search* command
- [X] Render results as Markdown preview to improve readability
- [X] Configure Khoj via the plugin setting tab on the settings page
  - Set Obsidian Vault to Index with Khoj. Defaults to all markdown files in current Vault
  - Set URL of Khoj backend
  - Set Number of Search Results to show in Search Modal
- [ ] Allow user to trigger reranking of result to improve search quality
