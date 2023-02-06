# Khoj 🦅
[![build](https://github.com/debanjum/khoj/actions/workflows/build.yml/badge.svg)](https://github.com/debanjum/khoj/actions/workflows/build.yml)
[![test](https://github.com/debanjum/khoj/actions/workflows/test.yml/badge.svg)](https://github.com/debanjum/khoj/actions/workflows/test.yml)
[![publish](https://github.com/debanjum/khoj/actions/workflows/publish.yml/badge.svg)](https://github.com/debanjum/khoj/actions/workflows/publish.yml)

*A natural language search engine for your personal notes, transactions and images*

**Supported Plugins**

[![Khoj on Obsidian](https://img.shields.io/badge/Obsidian-%23483699.svg?style=for-the-badge&logo=obsidian&logoColor=white)](https://github.com/debanjum/khoj/tree/master/src/interface/obsidian#readme)
[![Khoj on Emacs](https://img.shields.io/badge/Emacs-%237F5AB6.svg?&style=for-the-badge&logo=gnu-emacs&logoColor=white)](https://github.com/debanjum/khoj/tree/master/src/interface/emacs#readme)

## Table of Contents

- [Features](#Features)
- [Demos](#Demos)
  - [Khoj in Obsidian](#khoj-in-obsidian)
  - [Khoj in Emacs, Browser](#khoj-in-emacs-browser)
  - [Interfaces](#Interfaces)
- [Architecture](#Architecture)
- [Setup](#Setup)
  - [Install](#1-Install)
  - [Configure](#2-Configure)
  - [Run](#3-Run)
- [Use](#Use)
  - [Interfaces](#Interfaces-1)
  - [Query Filters](#Query-filters)
- [Upgrade](#Upgrade)
  - [Khoj Server](#upgrade-khoj-server)
  - [Khoj.el](#upgrade-khoj-on-emacs)
  - [Khoj Obsidian](#upgrade-khoj-on-obsidian)
- [Uninstall Khoj](#uninstall-khoj)
- [Troubleshoot](#Troubleshoot)
- [Advanced Usage](#advanced-usage)
  - [Access Khoj on Mobile](#access-khoj-on-mobile)
  - [Chat with Notes](#chat-with-notes)
  - [Use OpenAI Models for Search](#use-openai-models-for-search)
  - [Search across Different Languages](#search-across-different-languages)
- [Miscellaneous](#Miscellaneous)
  - [Setup OpenAI API key in Khoj](#set-your-openai-api-key-in-khoj)
  - [Beta API](#beta-api)
- [Performance](#Performance)
  - [Query Performance](#Query-performance)
  - [Indexing Performance](#Indexing-performance)
  - [Miscellaneous](#Miscellaneous-1)
- [Development](#Development)
  - [Visualize Codebase](#visualize-codebase)
  - [Setup](#Setup)
    - [Using Pip](#Using-Pip)
    - [Using Docker](#Using-Docker)
    - [Using Conda](#Test)
  - [Test](#Test)
- [Credits](#Credits)

## Features

- **Natural**: Advanced natural language understanding using Transformer based ML Models
- **Local**: Your personal data stays local. All search, indexing is done on your machine[\*](https://github.com/debanjum/khoj#beta-api)
- **Incremental**: Incremental search for a fast, search-as-you-type experience
- **Pluggable**: Modular architecture makes it easy to plug in new data sources, frontends and ML models
- **Multiple Sources**: Search your Org-mode and Markdown notes, Beancount transactions and Photos
- **Multiple Interfaces**: Search using a [Web Browser](./src/interface/web/index.html), [Emacs](./src/interface/emacs/khoj.el) or the [API](http://localhost:8000/docs)

## Demos
### Khoj in Obsidian
https://user-images.githubusercontent.com/6413477/210486007-36ee3407-e6aa-4185-8a26-b0bfc0a4344f.mp4

<details><summary>Description</summary>

- Install Khoj via `pip` and start Khoj backend in non-gui mode
- Install Khoj plugin via Community Plugins settings pane on Obsidian app
- Check the new Khoj plugin settings
- Let Khoj backend index the markdown files in the current Vault
- Open Khoj plugin on Obsidian via Search button on Left Pane
- Search \"*Announce plugin to folks*\" in the [Obsidian Plugin docs](https://marcus.se.net/obsidian-plugin-docs/)
- Jump to the [search result](https://marcus.se.net/obsidian-plugin-docs/publishing/submit-your-plugin)
</details>

### Khoj in Emacs, Browser
https://user-images.githubusercontent.com/6413477/184735169-92c78bf1-d827-4663-9087-a1ea194b8f4b.mp4

<details><summary>Description</summary>

- Install Khoj via pip
- Start Khoj app
- Add this readme and [khoj.el readme](https://github.com/debanjum/khoj/tree/master/src/interface/emacs) as org-mode for Khoj to index
- Search \"*Setup editor*\" on the Web and Emacs. Re-rank the results for better accuracy
- Top result is what we are looking for, the [section to Install Khoj.el on Emacs](https://github.com/debanjum/khoj/tree/master/src/interface/emacs#2-Install-Khojel)
</details>

<details><summary>Analysis</summary>

- The results do not have any words used in the query
  - *Based on the top result it seems the re-ranking model understands that Emacs is an editor?*
- The results incrementally update as the query is entered
- The results are re-ranked, for better accuracy, once user hits enter
</details>

### Interfaces

![](https://github.com/debanjum/khoj/blob/master/docs/interfaces.png?)

## Architecture

![](https://github.com/debanjum/khoj/blob/master/docs/khoj_architecture.png?)

## Setup
These are the general setup instructions for Khoj.

- Check the [Khoj.el Readme](https://github.com/debanjum/khoj/tree/master/src/interface/emacs#Setup) to setup Khoj with Emacs
- Check the [Khoj Obsidian Readme](https://github.com/debanjum/khoj/tree/master/src/interface/obsidian#Setup) to setup Khoj with Obsidian<br />
  Its simpler as it can skip the configure step below.

### 1. Install

```shell
pip install khoj-assistant
```

### 2. Start App

```shell
khoj
```

### 3. Configure

1. Enable content types and point to files to search in the First Run Screen that pops up on app start
2. Click `Configure` and wait. The app will download ML models and index the content for search

## Use
### Interfaces

- **Khoj via Obsidian**
  - [Install](https://github.com/debanjum/khoj/tree/master/src/interface/obsidian#2-Setup-Plugin) the Khoj Obsidian plugin
  - Click the *Khoj search* icon 🔎 on the [Ribbon](https://help.obsidian.md/User+interface/Workspace/Ribbon) or Search for *Khoj: Search* in the [Command Palette](https://help.obsidian.md/Plugins/Command+palette)
- **Khoj via Emacs**
  - [Install](https://github.com/debanjum/khoj/tree/master/src/interface/emacs#installation) [khoj.el](./src/interface/emacs/khoj.el)
  - Run `M-x khoj <user-query>`
- **Khoj via Web**
  - Open <http://localhost:8000/> via desktop interface or directly
- **Khoj via API**
  - See the Khoj FastAPI [Swagger Docs](http://localhost:8000/docs), [ReDocs](http://localhost:8000/redocs)

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
### Upgrade Khoj Server
```shell
pip install --upgrade khoj-assistant
```

### Upgrade Khoj on Emacs
- Use your Emacs Package Manager to Upgrade
- See [khoj.el readme](https://github.com/debanjum/khoj/tree/master/src/interface/emacs#Upgrade) for details

### Upgrade Khoj on Obsidian
- Upgrade via the Community plugins tab on the settings pane in the Obsidian app
- See the [khoj plugin readme](https://github.com/debanjum/khoj/tree/master/src/interface/obsidian#2-Setup-Plugin) for details

## Uninstall Khoj
1. (Optional) Hit `Ctrl-C` in the terminal running the khoj server to stop it
2. Delete the khoj directory in your home folder (i.e `~/.khoj` on Linux, Mac or `C:\Users\<your-username>\.khoj` on Windows)
3. Uninstall the khoj server with `pip uninstall khoj-assistant`
4. (Optional) Uninstall khoj.el or the khoj obsidian plugin in the standard way on Emacs, Obsidian

## Troubleshoot

#### Install fails while building Tokenizer dependency
- **Details**: `pip install khoj-assistant` fails while building the `tokenizers` dependency. Complains about Rust.
- **Fix**: Install Rust to build the tokenizers package. For example on Mac run:
    ```shell
    brew install rustup
    rustup-init
    source ~/.cargo/env
    ```
- **Refer**: [Issue with Fix](https://github.com/debanjum/khoj/issues/82#issuecomment-1241890946) for more details

#### Search starts giving wonky results
- **Fix**: Open [/api/update?force=true](http://localhost:8000/api/update?force=true)[^2] in browser to regenerate index from scratch
- **Note**: *This is a fix for when you percieve the search results have degraded. Not if you think they've always given wonky results*

#### Khoj in Docker errors out with \"Killed\" in error message
- **Fix**: Increase RAM available to Docker Containers in Docker Settings
- **Refer**: [StackOverflow Solution](https://stackoverflow.com/a/50770267), [Configure Resources on Docker for Mac](https://docs.docker.com/desktop/mac/#resources)

#### Khoj errors out complaining about Tensors mismatch or null
- **Mitigation**: Disable `image` search using the desktop GUI

## Advanced Usage
### Access Khoj on Mobile
1. [Setup Khoj](#Setup) on your personal server. This can be any always-on machine, i.e an old computer, RaspberryPi(?) etc
2. [Install](https://tailscale.com/kb/installation/) [Tailscale](tailscale.com/) on your personal server and phone
3. Open the Khoj web interface of the server from your phone browser.<br /> It should be `http://tailscale-ip-of-server:8000` or `http://name-of-server:8000` if you've setup [MagicDNS](https://tailscale.com/kb/1081/magicdns/)
4. Click the [Add to Homescreen](https://developer.mozilla.org/en-US/docs/Web/Progressive_web_apps/Add_to_home_screen) button
5. Enjoy exploring your notes, transactions and images from your phone!

![](https://github.com/debanjum/khoj/blob/master/docs/khoj_pwa_android.png?)

### Chat with Notes
#### Overview
- Provides a chat interface to inquire and engage with your notes
- Chat Types:
  - **Summarize**: Pulls the most relevant note from your notes and summarizes it
  - **Chat**: Also does general chat. It guesses whether to give a general response or search, summarizes from your note. <br />
    E.g *"how was your day?"* will give a general response. But *When did I go surfing?* should give a response from your notes
- **Note**: *Your query and top note from search result will be sent to OpenAI for processing*

#### Use
1. [Setup your OpenAI API key in Khoj](#set-your-openai-api-key-in-khoj)
2. Open [/chat?t=summarize](http://localhost:8000/chat?t=summarize)[^2]
3. Type your queries, see summarized response by Khoj from your notes

#### Demo
![](https://github.com/debanjum/khoj/blob/master/docs/khoj_chat_web_interface.png?)

### Use OpenAI Models for Search
#### Setup
1. Set `encoder-type`, `encoder` and `model-directory` under `asymmetric` and/or `symmetric` `search-type` in your `khoj.yml`[^1]:
   ```diff
      asymmetric:
   -    encoder: "sentence-transformers/multi-qa-MiniLM-L6-cos-v1"
   +    encoder: text-embedding-ada-002
   +    encoder-type: src.utils.models.OpenAI
        cross-encoder: "cross-encoder/ms-marco-MiniLM-L-6-v2"
   -    encoder-type: sentence_transformers.SentenceTransformer
   -    model_directory: "~/.khoj/search/asymmetric/"
   +    model-directory: null
   ```
2. [Setup your OpenAI API key in Khoj](#set-your-openai-api-key-in-khoj)
3. Restart Khoj server to generate embeddings. It will take longer than with offline models.

#### Warnings
  This configuration *uses an online model*
  - It will **send all notes to OpenAI** to generate embeddings
  - **All queries will be sent to OpenAI** when you search with Khoj
  - You will be **charged by OpenAI** based on the total tokens processed
  - It *requires an active internet connection* to search and index

### Search across Different Languages
  To search for notes in multiple, different languages, you can use a [multi-lingual model](https://www.sbert.net/docs/pretrained_models.html#multi-lingual-models).<br />
  For example, the [paraphrase-multilingual-MiniLM-L12-v2](https://huggingface.co/sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2) supports [50+ languages](https://www.sbert.net/docs/pretrained_models.html#:~:text=we%20used%20the%20following%2050%2B%20languages), has good search quality and speed. To use it:
  1. Manually update `search-type > asymmetric > encoder` to `sentence-transformer/paraphrase-multilingual-MiniLM-L12-v2` in your `~/.khoj/khoj.yml` file for now. See diff of `khoj.yml` below for illustration:
  ```diff
   asymmetric:
- encoder: "sentence-transformers/multi-qa-MiniLM-L6-cos-vi"
+ encoder: "sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2"
     cross-encoder: "cross-encoder/ms-marco-MiniLM-L-6-v2"
     model_directory: "~/.khoj/search/asymmetric/"
  ```

  2. Regenerate your content index. For example, by opening [\<khoj-url\>/api/update?t=force](http://localhost:8000/api/update?t=force)

## Miscellaneous
### Set your OpenAI API key in Khoj
If you want, Khoj can be configured to use OpenAI for search and chat.<br />
Add your OpenAI API to Khoj by using either of the two options below:
 - Open the Khoj desktop GUI, add your [OpenAI API key](https://beta.openai.com/account/api-keys) and click *Configure*
   Ensure khoj is started without the `--no-gui` flag. Check your system tray to see if Khoj 🦅 is minimized there.
 - Set `openai-api-key` field under `processor.conversation` section in your `khoj.yml`[^1] to your [OpenAI API key](https://beta.openai.com/account/api-keys) and restart khoj:
    ```diff
    processor:
      conversation:
    -    openai-api-key: # "YOUR_OPENAI_API_KEY"
    +    openai-api-key: sk-aaaaaaaaaaaaaaaaaaaaaaaahhhhhhhhhhhhhhhhhhhhhhhh
        model: "text-davinci-003"
        conversation-logfile: "~/.khoj/processor/conversation/conversation_logs.json"
    ```

**Warning**: *This will enable khoj to send your query and note(s) to OpenAI for processing*

### Beta API
- The beta [chat](http://localhost:8000/api/beta/chat), [summarize](http://localhost:8000/api/beta/summarize) and [search](http://localhost:8000/api/beta/search) API endpoints use [OpenAI API](https://openai.com/api/)
- They are disabled by default
- To use them:
  1. [Setup your OpenAI API key in Khoj](#set-your-openai-api-key-in-khoj)
  2. Interact with them from the [Khoj Swagger docs](http://locahost:8000/docs)[^2]


## Performance

### Query performance

- Semantic search using the bi-encoder is fairly fast at \<50 ms
- Reranking using the cross-encoder is slower at \<2s on 15 results. Tweak `top_k` to tradeoff speed for accuracy of results
- Filters in query (e.g by file, word or date) usually add \<20ms to query latency

### Indexing performance

- Indexing is more strongly impacted by the size of the source data
- Indexing 100K+ line corpus of notes takes about 10 minutes
- Indexing 4000+ images takes about 15 minutes and more than 8Gb of RAM
- Note: *It should only take this long on the first run* as the index is incrementally updated

### Miscellaneous

- Testing done on a Mac M1 and a \>100K line corpus of notes
- Search, indexing on a GPU has not been tested yet

## Development
### Visualize Codebase

*[Interactive Visualization](https://mango-dune-07a8b7110.1.azurestaticapps.net/?repo=debanjum%2Fkhoj)*

![](https://github.com/debanjum/khoj/blob/master/docs/khoj_codebase_visualization_0.2.1.png?)

### Setup
#### Using Pip
##### 1. Install

```shell
git clone https://github.com/debanjum/khoj && cd khoj
python3 -m venv .venv && source .venv/bin/activate
pip install -e .
```

##### 2. Configure

- Copy the `config/khoj_sample.yml` to `~/.khoj/khoj.yml`
- Set `input-files` or `input-filter` in each relevant `content-type` section of `~/.khoj/khoj.yml`
  - Set `input-directories` field in `image` `content-type` section
- Delete `content-type` and `processor` sub-section(s) irrelevant for your use-case

##### 3. Run

```shell
khoj -vv
```
Load ML model, generate embeddings and expose API to query notes, images, transactions etc specified in config YAML

##### 4. Upgrade

```shell
# To Upgrade To Latest Stable Release
# Maps to the latest tagged version of khoj on master branch
pip install --upgrade khoj-assistant

# To Upgrade To Latest Pre-Release
# Maps to the latest commit on the master branch
pip install --upgrade --pre khoj-assistant

# To Upgrade To Specific Development Release.
# Useful to test, review a PR.
# Note: khoj-assistant is published to test PyPi on creating a PR
pip install -i https://test.pypi.org/simple/ khoj-assistant==0.1.5.dev57166025766
```

#### Using Docker
##### 1. Clone

```shell
git clone https://github.com/debanjum/khoj && cd khoj
```

##### 2. Configure

- **Required**: Update [docker-compose.yml](./docker-compose.yml) to mount your images, (org-mode or markdown) notes and beancount directories
- **Optional**: Edit application configuration in [khoj_docker.yml](./config/khoj_docker.yml)

##### 3. Run

```shell
docker-compose up -d
```

*Note: The first run will take time. Let it run, it\'s mostly not hung, just generating embeddings*

##### 4. Upgrade

```shell
docker-compose build --pull
```

#### Using Conda
##### 1. Install Dependencies
- [Install Conda](https://docs.conda.io/projects/conda/en/latest/user-guide/install/index.html)

##### 2. Install Khoj
```shell
git clone https://github.com/debanjum/khoj && cd khoj
conda env create -f config/environment.yml
conda activate khoj
python3 -m pip install pyqt6  # As conda does not support pyqt6 yet
```

##### 3. Configure
- Copy the `config/khoj_sample.yml` to `~/.khoj/khoj.yml`
- Set `input-files` or `input-filter` in each relevant `content-type` section of `~/.khoj/khoj.yml`
  - Set `input-directories` field in `image` `content-type` section
- Delete `content-type`, `processor` sub-sections irrelevant for your use-case

##### 4. Run
```shell
python3 -m src.main -vv
```
  Load ML model, generate embeddings and expose API to query notes, images, transactions etc specified in config YAML

##### 5. Upgrade
```shell
cd khoj
git pull origin master
conda deactivate khoj
conda env update -f config/environment.yml
conda activate khoj
```

### Test
```shell
pytest
```

## Credits

- [Multi-QA MiniLM Model](https://huggingface.co/sentence-transformers/multi-qa-MiniLM-L6-cos-v1), [All MiniLM Model](https://huggingface.co/sentence-transformers/all-MiniLM-L6-v2) for Text Search. See [SBert Documentation](https://www.sbert.net/examples/applications/retrieve_rerank/README.html)
- [OpenAI CLIP Model](https://github.com/openai/CLIP) for Image Search. See [SBert Documentation](https://www.sbert.net/examples/applications/image-search/README.html)
- Charles Cave for [OrgNode Parser](http://members.optusnet.com.au/~charles57/GTD/orgnode.html)
- [Org.js](https://mooz.github.io/org-js/) to render Org-mode results on the Web interface
- [Markdown-it](https://github.com/markdown-it/markdown-it) to render Markdown results on the Web interface


[^1]: Default Khoj config file @ `~/.khoj/khoj.yml`

[^2]: Default Khoj url @ http://localhost:8000
