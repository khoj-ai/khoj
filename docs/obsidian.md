<h1><img src="./assets/khoj-logo-sideways.svg" width="200" alt="Khoj Logo">Obsidian</h1>

> An AI personal assistant for your Digital Brain in Obsidian

## Features
- **Search**
  - **Natural**: Advanced natural language understanding using Transformer based ML Models
  - **Local**: Your personal data stays local. All search and indexing is done on your machine. *Unlike chat which requires access to GPT.*
  - **Incremental**: Incremental search for a fast, search-as-you-type experience
- **Chat**
  - **Faster answers**: Find answers faster and with less effort than search
  - **Iterative discovery**: Iteratively explore and (re-)discover your notes
  - **Assisted creativity**: Smoothly weave across answers retrieval and content generation

## Interface
![](./assets/khoj_search_on_obsidian.png ':size=400px')
![](./assets/khoj_chat_on_obsidian.png ':size=400px')


## Setup
- *Make sure [python](https://realpython.com/installing-python/) and [pip](https://pip.pypa.io/en/stable/installation/) are installed on your machine*
- *Ensure you follow the ordering of the setup steps. Install the plugin after starting the khoj backend. This allows the plugin to configure the khoj backend*

### 1. Setup Backend
Open terminal/cmd and run below command to install and start the khoj backend
- On Linux/MacOS
  ```shell
  python -m pip install khoj-assistant && khoj
  ```

- On Windows
  ```shell
  py -m pip install khoj-assistant && khoj
  ```

### 2. Setup Plugin
  1. Open [Khoj](https://obsidian.md/plugins?id=khoj) from the *Community plugins* tab in Obsidian settings panel
  2. Click *Install*, then *Enable* on the Khoj plugin page in Obsidian
  3. [Optional] To enable Khoj Chat, set your [OpenAI API key](https://platform.openai.com/account/api-keys) in the Khoj plugin settings

See [official Obsidian plugin docs](https://help.obsidian.md/Extending+Obsidian/Community+plugins) for details

## Use
### Chat
Run *Khoj: Chat* from the [Command Palette](https://help.obsidian.md/Plugins/Command+palette) and ask questions in a natural, conversational style.<br />
E.g "When did I file my taxes last year?"

Notes:
- *Using Khoj Chat will result in query relevant notes being shared with OpenAI for ChatGPT to respond.*
- *To use Khoj Chat, ensure you've set your [OpenAI API key](https://platform.openai.com/account/api-keys) in the Khoj plugin settings.*

See [Khoj Chat](/chat) for more details

### Search
Click the *Khoj search* icon 🔎 on the [Ribbon](https://help.obsidian.md/User+interface/Workspace/Ribbon) or run *Khoj: Search* from the [Command Palette](https://help.obsidian.md/Plugins/Command+palette)

*Note: Ensure the khoj server is running in the background before searching. Execute `khoj` in your terminal if it is not already running*

[search_demo](https://user-images.githubusercontent.com/6413477/218801155-cd67e8b4-a770-404a-8179-d6b61caa0f93.mp4 ':include :type=mp4')

#### Query Filters

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

## Demo
### Search Demo
[demo](https://github-production-user-asset-6210df.s3.amazonaws.com/6413477/240061700-3e33d8ea-25bb-46c8-a3bf-c92f78d0f56b.mp4 ':include :type=mp4')

#### Description

1. Install Khoj via `pip` and start Khoj backend
    ```shell
    python -m pip install khoj-assistant && khoj
    ```
2. Install Khoj plugin via Community Plugins settings pane on Obsidian app
    - Check the new Khoj plugin settings
    - Wait for Khoj backend to index markdown, PDF files in the current Vault
    - Open Khoj plugin on Obsidian via Search button on Left Pane
    - Search \"*Announce plugin to folks*\" in the [Obsidian Plugin docs](https://marcus.se.net/obsidian-plugin-docs/)
    - Jump to the [search result](https://marcus.se.net/obsidian-plugin-docs/publishing/submit-your-plugin)


## Troubleshooting
  - Open the Khoj plugin settings pane, to configure Khoj
  - Toggle Enable/Disable Khoj, if setting changes have not applied
  - Click *Update* button to force index to refresh, if results are failing or stale

## Current Limitations
- The plugin loads the index of only one vault at a time.<br/>
  So notes across multiple vaults **cannot** be searched at the same time
