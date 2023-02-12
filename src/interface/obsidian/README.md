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
  - [Search](#search)
  - [Find Similar Notes](#find-similar-notes)
- [Upgrade](#Upgrade)
  - [Upgrade Backend](#1-Upgrade-Backend)
  - [Upgrade Plugin](#2-Upgrade-Plugin)
- [Troubleshoot](#Troubleshoot)
- [Visualize Codebase](#Visualize-Codebase)
- [Implementation](#Implementation)

## Features
- **Natural**: Advanced natural language understanding using Transformer based ML Models
- **Local**: Your personal data stays local. All search, indexing is done on your machine[\*](https://github.com/debanjum/khoj#miscellaneous)
- **Incremental**: Incremental search for a fast, search-as-you-type experience

## Demo
https://user-images.githubusercontent.com/6413477/210486007-36ee3407-e6aa-4185-8a26-b0bfc0a4344f.mp4

<details><summary>Description</summary>

1. Install Khoj via `pip` and start Khoj backend in non-gui mode
2. Install Khoj plugin via Community Plugins settings pane on Obsidian app
3. Check the new Khoj plugin settings
4. Wait for Khoj backend to index markdown files in the current Vault
5. Open Khoj plugin on Obsidian via Search button on Left Pane
6. Search \"*Announce plugin to folks*\" in the [Obsidian Plugin docs](https://marcus.se.net/obsidian-plugin-docs/)
7. Jump to the [search result](https://marcus.se.net/obsidian-plugin-docs/publishing/submit-your-plugin)

</details>

### Interface
![](https://github.com/debanjum/khoj/blob/master/src/interface/obsidian/docs/khoj_on_obsidian_0.2.5.png?)

## Setup
- *Make sure [python](https://realpython.com/installing-python/) and [pip](https://pip.pypa.io/en/stable/installation/) are installed on your machine*
- *Ensure you follow the ordering of the setup steps. Install the plugin after starting the khoj backend. This allows the plugin to configure the khoj backend*

### 1. Setup Backend
Open terminal/cmd and run below command to install and start the khoj backend
```shell
pip install khoj-assistant && khoj --no-gui
```
### 2. Setup Plugin
  1. Open [Khoj](https://obsidian.md/plugins?id=khoj) from the *Community plugins* tab in Obsidian settings panel
  2. Click *Install*, then *Enable* on the Khoj plugin page in Obsidian

See [official Obsidian plugin docs](https://help.obsidian.md/Extending+Obsidian/Community+plugins) for details

## Use
### Search
Click the *Khoj search* icon 🔎 on the [Ribbon](https://help.obsidian.md/User+interface/Workspace/Ribbon) or run *Khoj: Search* from the [Command Palette](https://help.obsidian.md/Plugins/Command+palette)

*Note: Ensure the khoj server is running in the background before searching. Execute `khoj --no-gui` in your terminal if it is not already running*

<details><summary>Query Filters</summary>

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

</details>

### Find Similar Notes
To see other notes similar to the current one, run *Khoj: Find Similar Notes* from the [Command Palette](https://help.obsidian.md/Plugins/Command+palette)

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
  - Open the Khoj plugin settings pane, to configure Khoj
  - Toggle Enable/Disable Khoj, if setting changes have not applied
  - Click *Update* button to force index to refresh, if results are failing or stale

## Current Limitations
- The plugin loads the index of only one vault at a time.<br/>
  So notes across multiple vaults **cannot** be searched at the same time

## Visualize Codebase
<img src="https://github.com/debanjum/khoj/blob/master/src/interface/obsidian/docs/khoj_obsidian_codebase_visualization_0.2.1.png" width="700" />

## Implementation
The plugin implements the following functionality to search your notes with Khoj:
- [X] Open the Khoj search modal via left ribbon icon or the *Khoj: Search* command
- [X] Render results as Markdown preview to improve readability
- [X] Configure Khoj via the plugin setting tab on the settings page
  - Set Obsidian Vault to Index with Khoj. Defaults to all markdown files in current Vault
  - Set URL of Khoj backend
  - Set Number of Search Results to show in Search Modal
- [X] Allow reranking of result to improve search quality
- [X] Allow Finding notes similar to current note being viewed
