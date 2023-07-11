<h1><img src="src/khoj/interface/web/assets/icons/khoj-logo-sideways.svg" width="330" alt="Khoj Logo"></h1>

[![test](https://github.com/khoj-ai/khoj/actions/workflows/test.yml/badge.svg)](https://github.com/khoj-ai/khoj/actions/workflows/test.yml)
[![dockerize](https://github.com/khoj-ai/khoj/actions/workflows/dockerize.yml/badge.svg)](https://github.com/khoj-ai/khoj/pkgs/container/khoj)
[![pypi](https://github.com/khoj-ai/khoj/actions/workflows/pypi.yml/badge.svg)](https://pypi.org/project/khoj-assistant/)

*An AI personal assistant for your digital brain*

**Supported Plugins**

[![Khoj on Obsidian](https://img.shields.io/badge/Obsidian-%23483699.svg?style=for-the-badge&logo=obsidian&logoColor=white)](https://github.com/khoj-ai/khoj/tree/master/src/interface/obsidian#readme)
[![Khoj on Emacs](https://img.shields.io/badge/Emacs-%237F5AB6.svg?&style=for-the-badge&logo=gnu-emacs&logoColor=white)](https://github.com/khoj-ai/khoj/tree/master/src/interface/emacs#readme)

## Table of Contents

- [Features](#Features)
- [Demos](#Demos)
  - [Khoj in Obsidian](#khoj-in-obsidian)
  - [Khoj in Emacs, Browser](#khoj-in-emacs-browser)
  - [Interfaces](#Interfaces)
- [Architecture](#Architecture)
- [Setup](#Setup)
  - [Install](#1-Install)
  - [Run](#2-Run)
  - [Configure](#3-Configure)
  - [Install Plugins](#4-install-interface-plugins)
- [Use](#Use)
  - [Khoj Search](#Khoj-search)
  - [Khoj Chat](#Khoj-chat)
- [Upgrade](#Upgrade)
  - [Khoj Server](#upgrade-khoj-server)
  - [Khoj.el](#upgrade-khoj-on-emacs)
  - [Khoj Obsidian](#upgrade-khoj-on-obsidian)
- [Uninstall](#uninstall)
- [Troubleshoot](#Troubleshoot)
- [Advanced Usage](#advanced-usage)
  - [Access Khoj on Mobile](#access-khoj-on-mobile)
  - [Use OpenAI Models for Search](#use-openai-models-for-search)
  - [Search across Different Languages](#search-across-different-languages)
  - [Boostrap Khoj Search for Offline Usage Later](#bootstrap-khoj-search-for-offline-usage-later)
- [Miscellaneous](#Miscellaneous)
  - [Setup OpenAI API key in Khoj](#set-your-openai-api-key-in-khoj)
  - [GPT API](#gpt-api)
- [Performance](#Performance)
  - [Query Performance](#Query-performance)
  - [Indexing Performance](#Indexing-performance)
  - [Miscellaneous](#Miscellaneous-1)
- [Development](#Development)
  - [Visualize Codebase](#visualize-codebase)
  - [Create Release](#create-khoj-release)
  - [Setup](#Setup)
    - [Using Pip](#Using-Pip)
    - [Using Docker](#Using-Docker)
  - [Validate](#Validate)
- [Credits](#Credits)

## Features
- **Search**
  - **Local**: Your personal data stays local. All search and indexing is done on your machine. *Unlike chat which requires access to GPT.*
  - **Incremental**: Incremental search for a fast, search-as-you-type experience
- **Chat**
  - **Faster answers**: Find answers faster, smoother than search. No need to manually scan through your notes to find answers.
  - **Iterative discovery**: Iteratively explore and (re-)discover your notes
  - **Assisted creativity**: Smoothly weave across answers retrieval and content generation
- **General**
  - **Natural**: Advanced natural language understanding using Transformer based ML Models
  - **Pluggable**: Modular architecture makes it easy to plug in new data sources, frontends and ML models
  - **Multiple Sources**: Index your Org-mode and Markdown notes, PDF files, Github repositories, and Photos
  - **Multiple Interfaces**: Interact from your [Web Browser](./src/khoj/interface/web/index.html), [Emacs](./src/interface/emacs/khoj.el) or [Obsidian](./src/interface/obsidian/)

## Demos
### Khoj in Obsidian
https://github.com/khoj-ai/khoj/assets/6413477/3e33d8ea-25bb-46c8-a3bf-c92f78d0f56b

<details><summary>Description</summary>

1. Install Khoj via `pip` and start Khoj backend in a terminal (Run `khoj`)
    ```
    python -m pip install khoj-assistant
    khoj
    ```
2. Install Khoj plugin via Community Plugins settings pane on Obsidian app
    - Check the new Khoj plugin settings
    - Let Khoj backend index the markdown, pdf, Github markdown files in the current Vault
    - Open Khoj plugin on Obsidian via Search button on Left Pane
    - Search \"*Announce plugin to folks*\" in the [Obsidian Plugin docs](https://marcus.se.net/obsidian-plugin-docs/)
    - Jump to the [search result](https://marcus.se.net/obsidian-plugin-docs/publishing/submit-your-plugin)
</details>

### Khoj in Emacs, Browser
https://user-images.githubusercontent.com/6413477/184735169-92c78bf1-d827-4663-9087-a1ea194b8f4b.mp4

<details><summary>Description</summary>

- Install Khoj via pip
- Start Khoj app
- Add this readme and [khoj.el readme](https://github.com/khoj-ai/khoj/tree/master/src/interface/emacs) as org-mode for Khoj to index
- Search \"*Setup editor*\" on the Web and Emacs. Re-rank the results for better accuracy
- Top result is what we are looking for, the [section to Install Khoj.el on Emacs](https://github.com/khoj-ai/khoj/tree/master/src/interface/emacs#2-Install-Khojel)
</details>

<details><summary>Analysis</summary>

- The results do not have any words used in the query
  - *Based on the top result it seems the re-ranking model understands that Emacs is an editor?*
- The results incrementally update as the query is entered
- The results are re-ranked, for better accuracy, once user hits enter
</details>

### Interfaces

![](https://github.com/khoj-ai/khoj/blob/master/docs/interfaces.png?)

## Architecture

![](https://github.com/khoj-ai/khoj/blob/master/docs/khoj_architecture.png?)

## Setup
These are the general setup instructions for Khoj.

- Make sure [python](https://realpython.com/installing-python/) and [pip](https://pip.pypa.io/en/stable/installation/) are installed on your machine
- Check the [Khoj.el Readme](https://github.com/khoj-ai/khoj/tree/master/src/interface/emacs#Setup) to setup Khoj with Emacs<br />
  Its simpler as it can skip the server *install*, *run* and *configure* step below.
- Check the [Khoj Obsidian Readme](https://github.com/khoj-ai/khoj/tree/master/src/interface/obsidian#Setup) to setup Khoj with Obsidian<br />
  Its simpler as it can skip the *configure* step below.

### 1. Install
Run the following command in your terminal to install the Khoj backend.

- On Linux/MacOS
  ```shell
  python -m pip install khoj-assistant
  ```

- On Windows
  ```shell
  py -m pip install khoj-assistant
  ```

### 2. Run

Run the following commmand from your terminal to start the Khoj backend and open Khoj in your browser.

```shell
khoj --gui
```

Note: To start Khoj automatically in the background use [Task scheduler](https://www.windowscentral.com/how-create-automated-task-using-task-scheduler-windows-10) on Windows or [Cron](https://en.wikipedia.org/wiki/Cron) on Mac, Linux (e.g with `@reboot khoj`)

### 3. Configure
1. Set `File`, `Folder` and hit `Save` in each Plugins you want to enable for Search on the Khoj config page
2. Add your OpenAI API key to Chat Feature settings if you want to use Chat
3. Click `Configure` and wait. The app will download ML models and index the content for search and (optionally) chat

### 4. Install Interface Plugins
Khoj exposes a web interface to search, chat and configure by default.<br />
The optional steps below allow using Khoj from within an existing application like Obsidian or Emacs.

- **Khoj Obsidian**:<br />
[Install](https://github.com/khoj-ai/khoj/tree/master/src/interface/obsidian#2-Setup-Plugin) the Khoj Obsidian plugin

- **Khoj Emacs**:<br />
[Install](https://github.com/khoj-ai/khoj/tree/master/src/interface/emacs#2-Install-Khojel) khoj.el

## Use
### Khoj Search
- **Khoj via Obsidian**
  - Click the *Khoj search* icon 🔎 on the [Ribbon](https://help.obsidian.md/User+interface/Workspace/Ribbon) or Search for *Khoj: Search* in the [Command Palette](https://help.obsidian.md/Plugins/Command+palette)
- **Khoj via Emacs**
  - Run `M-x khoj <user-query>`
- **Khoj via Web**
  - Open <http://localhost:42110/> directly
- **Khoj via API**
  - See the Khoj FastAPI [Swagger Docs](http://localhost:42110/docs), [ReDocs](http://localhost:42110/redocs)

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

### Khoj Chat
#### Overview
- Creates a personal assistant for you to inquire and engage with your notes
- Uses [ChatGPT](https://openai.com/blog/chatgpt) and [Khoj search](#khoj-search)
- Supports multi-turn conversations with the relevant notes for context
- Shows reference notes used to generate a response
- **Note**: *Your query and top notes from khoj search will be sent to OpenAI for processing*

#### Setup
- [Setup your OpenAI API key in Khoj](#set-your-openai-api-key-in-khoj)

#### Use
1. Open [/chat](http://localhost:42110/chat)[^2]
2. Type your queries and see response by Khoj from your notes

#### Demo
![](https://github.com/khoj-ai/khoj/blob/master/docs/khoj_chat_web_interface.png?)

### Details
1. Your query is used to retrieve the most relevant notes, if any, using Khoj search
2. These notes, the last few messages and associated metadata is passed to ChatGPT along with your query for a response

## Upgrade
### Upgrade Khoj Server
```shell
pip install --upgrade khoj-assistant
```

*Note: To upgrade to the latest pre-release version of the khoj server run below command*
```shell
# Maps to the latest commit on the master branch
pip install --upgrade --pre khoj-assistant
```

### Upgrade Khoj on Emacs
- Use your Emacs Package Manager to Upgrade
- See [khoj.el readme](https://github.com/khoj-ai/khoj/tree/master/src/interface/emacs#Upgrade) for details

### Upgrade Khoj on Obsidian
- Upgrade via the Community plugins tab on the settings pane in the Obsidian app
- See the [khoj plugin readme](https://github.com/khoj-ai/khoj/tree/master/src/interface/obsidian#2-Setup-Plugin) for details

## Uninstall
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
- **Refer**: [Issue with Fix](https://github.com/khoj-ai/khoj/issues/82#issuecomment-1241890946) for more details

#### Search starts giving wonky results
- **Fix**: Open [/api/update?force=true](http://localhost:42110/api/update?force=true)[^2] in browser to regenerate index from scratch
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
3. Open the Khoj web interface of the server from your phone browser.<br /> It should be `http://tailscale-ip-of-server:42110` or `http://name-of-server:42110` if you've setup [MagicDNS](https://tailscale.com/kb/1081/magicdns/)
4. Click the [Add to Homescreen](https://developer.mozilla.org/en-US/docs/Web/Progressive_web_apps/Add_to_home_screen) button
5. Enjoy exploring your notes, documents and images from your phone!

![](https://github.com/khoj-ai/khoj/blob/master/docs/khoj_pwa_android.png?)

### Use OpenAI Models for Search
#### Setup
1. Set `encoder-type`, `encoder` and `model-directory` under `asymmetric` and/or `symmetric` `search-type` in your `khoj.yml`[^1]:
   ```diff
      asymmetric:
   -    encoder: "sentence-transformers/multi-qa-MiniLM-L6-cos-v1"
   +    encoder: text-embedding-ada-002
   +    encoder-type: khoj.utils.models.OpenAI
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
  1. Manually update `search-type > asymmetric > encoder` to `paraphrase-multilingual-MiniLM-L12-v2` in your `~/.khoj/khoj.yml` file for now. See diff of `khoj.yml` below for illustration:
  ```diff
   asymmetric:
- encoder: "sentence-transformers/multi-qa-MiniLM-L6-cos-v1"
+ encoder: "paraphrase-multilingual-MiniLM-L12-v2"
     cross-encoder: "cross-encoder/ms-marco-MiniLM-L-6-v2"
     model_directory: "~/.khoj/search/asymmetric/"
  ```

  2. Regenerate your content index. For example, by opening [\<khoj-url\>/api/update?t=force](http://localhost:42110/api/update?t=force)

### Bootstrap Khoj Search for Offline Usage later

  You can bootstrap Khoj pre-emptively to run on machines that do not have internet access. An example use-case would be to run Khoj on an air-gapped machine.
  Note: *Only search can currently run in fully offline mode, not chat.*

  - With Internet
    1. Manually download the [asymmetric text](https://huggingface.co/sentence-transformers/multi-qa-MiniLM-L6-cos-v1), [symmetric text](https://huggingface.co/sentence-transformers/all-MiniLM-L6-v2)and [image search](https://huggingface.co/sentence-transformers/clip-ViT-B-32) models from HuggingFace
    2. Pip install khoj (and dependencies) in an associated virtualenv. E.g `python -m venv .venv && source .venv/bin/activate && pip install khoj-assistant`
  - Without Internet
    1. Copy each of the search models into their respective folders, `asymmetric`, `symmetric` and `image` under the `~/.khoj/search/` directory on the air-gapped machine
    2. Copy the khoj virtual environment directory onto the air-gapped machine, activate the environment and start and khoj as normal. E.g `source .venv/bin/activate && khoj`


## Miscellaneous
### Set your OpenAI API key in Khoj
If you want, Khoj can be configured to use OpenAI for search and chat.<br />
Add your OpenAI API to Khoj by using either of the two options below:
 - Open your [Khoj settings](http://localhost:42110/config/processor/conversation), add your OpenAI API key, and click *Save*. Then go to your [Khoj settings](http://localhost:42110/config) and click `Configure`. This will refresh Khoj with your OpenAI API key.
 - Set `openai-api-key` field under `processor.conversation` section in your `khoj.yml`[^1] to your [OpenAI API key](https://beta.openai.com/account/api-keys) and restart khoj:
    ```diff
    processor:
      conversation:
    -    openai-api-key: # "YOUR_OPENAI_API_KEY"
    +    openai-api-key: sk-aaaaaaaaaaaaaaaaaaaaaaaahhhhhhhhhhhhhhhhhhhhhhhh
        model: "text-davinci-003"
        conversation-logfile: "~/.khoj/processor/conversation/conversation_logs.json"
    ```

**Warning**: *This will enable Khoj to send your query and note(s) to OpenAI for processing*

### GPT API
- The [chat](http://localhost:42110/api/chat), [answer](http://localhost:42110/api/beta/answer) and [search](http://localhost:42110/api/beta/search) API endpoints use [OpenAI API](https://openai.com/api/)
- They are disabled by default
- To use them:
  1. [Setup your OpenAI API key in Khoj](#set-your-openai-api-key-in-khoj)
  2. Interact with them from the [Khoj Swagger docs](http://locahost:42110/docs)[^2]

### Index Github Repository for Search, Chat
The Khoj Github plugin can index issues, commit messages and markdown, org-mode and PDF files from any repositories you have access to. This allows you to chat or search with these repositories. Get answers, resolve issues or just explore a repo with the help of your AI personal assistant.

See the [Khoj FAQ](https://faq.khoj.dev) for a demo of Khoj search and chat. It makes the Khoj github repo available for exploring.

Note: *Khoj will ignore code files in the repository for now as the default AI model used works best with natural language text, not code.*

#### Setup Khoj Github plugin
1. Get a [pat token](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) with `repo` and `read:org` scopes in the classic flow.
2. Configure Khoj settings to include the `owner` and `repo_name`. The `owner` will be the organization name if the repo is in an organization. The `repo_name` will be the name of the repository. Optionally, you can also supply a branch name. If no branch name is supplied, the `master` branch will be used.

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

![](https://github.com/khoj-ai/khoj/blob/master/docs/khoj_codebase_visualization_0.2.1.png?)

### Create Khoj Release
Follow the steps below to [release](https://github.com/debanjum/khoj/releases/) Khoj. This will create a stable release of Khoj on [Pypi](https://pypi.org/project/khoj-assistant/), [Melpa](https://stable.melpa.org/#%252Fkhoj) and [Obsidian](https://obsidian.md/plugins?id%253Dkhoj). It will also create desktop apps of Khoj and attach them to the latest release.

1. Create and tag release commit by running the bump_version script. The release commit sets version number in required metadata files.
  ```shell
  ./scripts/bump_version.sh -c "<release_version>"
  ```
2. Push commit and then the tag to trigger the release workflow to create Release with auto generated release notes.
  ```shell
  git push origin master  # push release commit to khoj repository
  git push origin <release_version>  # push release tag to khoj repository
  ```
3. [Optional] Update the Release Notes to highlight new features, fixes and updates
### Setup
#### Using Pip
##### 1. Install

```shell
# Get Khoj Code
git clone https://github.com/khoj-ai/khoj && cd khoj

# Create, Activate Virtual Environment
python3 -m venv .venv && source .venv/bin/activate

# Install Khoj for Development
pip install -e .[dev]
```

##### 2. Run
1. Start Khoj
   ```shell
   khoj -vv
   ```
2. Configure Khoj
   - **Via the Settings UI**: Add files, directories to index the [Khoj settings](http://localhost:42110/config) UI once Khoj has started up. Once you've saved all your settings, click `Configure`.
   - **Manually**:
     - Copy the `config/khoj_sample.yml` to `~/.khoj/khoj.yml`
     - Set `input-files` or `input-filter` in each relevant `content-type` section of `~/.khoj/khoj.yml`
       - Set `input-directories` field in `image` `content-type` section
     - Delete `content-type` and `processor` sub-section(s) irrelevant for your use-case
     - Restart khoj

  Note: Wait after configuration for khoj to Load ML model, generate embeddings and expose API to query notes, images, documents etc specified in config YAML

#### Using Docker
##### 1. Clone

```shell
git clone https://github.com/khoj-ai/khoj && cd khoj
```

##### 2. Configure

- **Required**: Update [docker-compose.yml](./docker-compose.yml) to mount your images, (org-mode or markdown) notes, PDFs and Github repositories
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

### Validate
#### Before Make Changes
1. Install Git Hooks for Validation
   ```shell
   pre-commit install -t pre-push -t pre-commit
   ```
   - This ensures standard code formatting fixes and other checks run automatically on every commit and push
   - Note 1: If [pre-commit](https://pre-commit.com/#intro) didn't already get installed, [install it](https://pre-commit.com/#install) via `pip install pre-commit`
   - Note 2: To run the pre-commit changes manually, use `pre-commit run --hook-stage manual --all` before creating PR

#### Before Creating PR

1. Run Tests. If you get an error complaining about a missing `fast_tokenizer_file`, follow the solution [in this Github issue](https://github.com/UKPLab/sentence-transformers/issues/1659).
   ```shell
   pytest
   ```

2. Run MyPy to check types
   ```shell
   mypy --config-file pyproject.toml
   ```

#### After Creating PR
- Automated [validation workflows](.github/workflows) run for every PR.

  Ensure any issues seen by them our fixed

- Test the python packge created for a PR
  1. Download and extract the zipped `.whl` artifact generated from the pypi workflow run for the PR.
  2. Install (in your virtualenv) with `pip install /path/to/download*.whl>`
  3. Start and use the application to see if it works fine


## Credits

- [Multi-QA MiniLM Model](https://huggingface.co/sentence-transformers/multi-qa-MiniLM-L6-cos-v1), [All MiniLM Model](https://huggingface.co/sentence-transformers/all-MiniLM-L6-v2) for Text Search. See [SBert Documentation](https://www.sbert.net/examples/applications/retrieve_rerank/README.html)
- [OpenAI CLIP Model](https://github.com/openai/CLIP) for Image Search. See [SBert Documentation](https://www.sbert.net/examples/applications/image-search/README.html)
- Charles Cave for [OrgNode Parser](http://members.optusnet.com.au/~charles57/GTD/orgnode.html)
- [Org.js](https://mooz.github.io/org-js/) to render Org-mode results on the Web interface
- [Markdown-it](https://github.com/markdown-it/markdown-it) to render Markdown results on the Web interface


[^1]: Default Khoj config file @ `~/.khoj/khoj.yml`

[^2]: Default Khoj url @ http://localhost:42110
