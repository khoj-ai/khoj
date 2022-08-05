# Khoj 🦅
[![build](https://github.com/debanjum/khoj/actions/workflows/build.yml/badge.svg)](https://github.com/debanjum/khoj/actions/workflows/build.yml)
[![test](https://github.com/debanjum/khoj/actions/workflows/test.yml/badge.svg)](https://github.com/debanjum/khoj/actions/workflows/test.yml)
[![publish](https://github.com/debanjum/khoj/actions/workflows/publish.yml/badge.svg)](https://github.com/debanjum/khoj/actions/workflows/publish.yml)

*A natural language search engine for your personal notes, transactions and images*

## Table of Contents

- [Features](#Features)
- [Demo](#Demo)
  - [Description](#Description)
  - [Analysis](#Analysis)
- [Architecture](#Architecture)
- [Setup](#Setup)
  - [Install](#1-Install)
  - [Configure](#2-Configure)
  - [Run](#3-Run)
- [Use](#Use)
- [Upgrade](#Upgrade)
- [Troubleshoot](#Troubleshoot)
- [Miscellaneous](#Miscellaneous)
- [Performance](#Performance)
  - [Query Performance](#Query-performance)
  - [Indexing Performance](#Indexing-performance)
  - [Miscellaneous](#Miscellaneous-1)
- [Development](#Development)
  - [Setup](#Setup)
    - [Using Pip](#Using-Pip)
    - [Using Docker](#Using-Docker)
    - [Using Conda](#Test)
  - [Test](#Test)
- [Credits](#Credits)

## Features

- **Natural**: Advanced Natural language understanding using Transformer based ML Models
- **Local**: Your personal data stays local. All search, indexing is done on your machine[\*](https://github.com/debanjum/khoj#miscellaneous)
- **Incremental**: Incremental search for a fast, search-as-you-type experience
- **Pluggable**: Modular architecture makes it easy to plug in new data sources, frontends and ML models
- **Multiple Sources**: Search your Org-mode and Markdown notes, Beancount transactions and Photos
- **Multiple Interfaces**: Search using a [Web Browser](./src/interface/web/index.html), [Emacs](./src/interface/emacs/khoj.el) or the [API](http://localhost:8000/docs)

## Demo

<https://user-images.githubusercontent.com/6413477/181664862-31565b0a-0e64-47e1-a79a-599dfc486c74.mp4>

### Description

- User searches for \"*Setup editor*\"
- The demo looks for the most relevant section in this readme and the [khoj.el readme](https://github.com/debanjum/khoj/tree/master/src/interface/emacs)
- Top result is what we are looking for, the [section to Install Khoj.el on Emacs](https://github.com/debanjum/khoj/tree/master/src/interface/emacs#installation)

### Analysis

- The results do not have any words used in the query
  - *Based on the top result it seems the re-ranking model understands that Emacs is an editor?*
- The results incrementally update as the query is entered
- The results are re-ranked, for better accuracy, once user is idle

## Architecture

![](https://github.com/debanjum/khoj/blob/master/docs/khoj_architecture.png)

## Setup
### 1. Install
   ``` shell
   pip install khoj-assistant
   ```

### 2. Configure
   - Set `input-files` or `input-filter` in each relevant `content-type` section of [khoj_sample.yml](./config/khoj_sample.yml)
     - Set `input-directories` field in `content-type.image` section
   - Delete `content-type`, `processor` sub-sections irrelevant for your use-case

### 3. Run
   ``` shell
   khoj config/khoj_sample.yml -vv
   ```
   Loads ML model, generates embeddings and exposes API to search notes, images, transactions etc specified in config YAML

## Use

- **Khoj via Web**
  - Open <http://localhost:8000/>
- **Khoj via Emacs**
  - [Install](https://github.com/debanjum/khoj/tree/master/src/interface/emacs#installation) [khoj.el](./src/interface/emacs/khoj.el)
  - Run `M-x khoj <user-query>`
- **Khoj via API**
  - See [Khoj FastAPI Docs](http://localhost:8000/docs), [Khoj FastAPI ReDocs](http://localhost:8000/redocs)

## Upgrade
``` shell
pip install --upgrade khoj-assistant
```

## Troubleshoot

- Symptom: Errors out complaining about Tensors mismatch, null etc
  - Mitigation: Delete `content-type` > `image` section from `khoj_sample.yml`

- Symptom: Errors out with \"Killed\" in error message in Docker
  - Fix: Increase RAM available to Docker Containers in Docker Settings
  - Refer: [StackOverflow Solution](https://stackoverflow.com/a/50770267), [Configure Resources on Docker for Mac](https://docs.docker.com/desktop/mac/#resources)

## Miscellaneous

- The experimental [chat](localhost:8000/chat) API endpoint uses the [OpenAI API](https://openai.com/api/)
    - It is disabled by default
    - To use it add your `openai-api-key` to config.yml

## Performance

### Query performance

- Semantic search using the bi-encoder is fairly fast at \<5 ms
- Reranking using the cross-encoder is slower at \<2s on 15 results. Tweak `top_k` to tradeoff speed for accuracy of results
- Applying explicit filters is very slow currently at \~6s. This is because the filters are rudimentary. Considerable speed-ups can be achieved using indexes etc

### Indexing performance

- Indexing is more strongly impacted by the size of the source data
- Indexing 100K+ line corpus of notes takes 6 minutes
- Indexing 4000+ images takes about 15 minutes and more than 8Gb of RAM
- Once <https://github.com/debanjum/khoj/issues/36> is implemented, it should only take this long on first run

### Miscellaneous

- Testing done on a Mac M1 and a \>100K line corpus of notes
- Search, indexing on a GPU has not been tested yet

## Development
### Setup
#### Using Pip
##### 1. Install
   ``` shell
   git clone https://github.com/debanjum/khoj && cd khoj
   python -m venv .venv && source .venv/bin/activate
   pip install
   ```
##### 2. Configure
   - Set `input-files` or `input-filter` in each relevant `content-type` section of `khoj_sample.yml`
     - Set `input-directories` field in `image` `content-type` section
   - Delete `content-type`, `processor` sub-sections irrelevant for your use-case

##### 3. Run
   ``` shell
   khoj config/khoj_sample.yml -vv
   ```
   Load ML model, generate embeddings and expose API to query notes, images, transactions etc specified in config YAML

##### 4. Upgrade

```shell
# To Upgrade To Latest Stable Release
pip install --upgrade khoj-assistant

# To Upgrade To Latest Pre-Release
pip install --upgrade --pre khoj-assistant

# To Upgrade To Specific Development Release
pip install -r testpypi khoj-assistant==0.1.5.dev491659577806
```

#### Using Docker
##### 1. Clone

``` shell
git clone https://github.com/debanjum/khoj && cd khoj
```

##### 2. Configure

- **Required**: Update [docker-compose.yml](./docker-compose.yml) to mount your images, (org-mode or markdown) notes and beancount directories
- **Optional**: Edit application configuration in [khoj_docker.yml](./config/khoj_docker.yml)

##### 3. Run

``` shell
docker-compose up -d
```

*Note: The first run will take time. Let it run, it\'s mostly not hung, just generating embeddings*

##### 4. Upgrade

``` shell
docker-compose build --pull
```

#### Using Conda
##### 1. Install Dependencies
   - [Install Conda](https://docs.conda.io/projects/conda/en/latest/user-guide/install/index.html) \[Required\]
   - Install Exiftool \[Optional\]
     ``` shell
     sudo apt -y install libimage-exiftool-perl
     ```

##### 2. Install Khoj
   ``` shell
   git clone https://github.com/debanjum/khoj && cd khoj
   conda env create -f config/environment.yml
   conda activate khoj
   ```

##### 3. Configure
   - Set `input-files` or `input-filter` in each relevant `content-type` section of `khoj_sample.yml`
     - Set `input-directories` field in `image` `content-type` section
   - Delete `content-type`, `processor` sub-sections irrelevant for your use-case

##### 4. Run
   ``` shell
   python3 -m src.main config/khoj_sample.yml -vv
   ```
   Load ML model, generate embeddings and expose API to query notes, images, transactions etc specified in config YAML

##### 5. Upgrade
``` shell
cd khoj
git pull origin master
conda deactivate khoj
conda env update -f config/environment.yml
conda activate khoj
```

### Test

``` shell
pytest
```

## Credits

- [Multi-QA MiniLM Model](https://huggingface.co/sentence-transformers/multi-qa-MiniLM-L6-cos-v1), [All MiniLM Model](https://huggingface.co/sentence-transformers/all-MiniLM-L6-v2) for Text Search. See [SBert Documentation](https://www.sbert.net/examples/applications/retrieve_rerank/README.html)
- [OpenAI CLIP Model](https://github.com/openai/CLIP) for Image Search. See [SBert Documentation](https://www.sbert.net/examples/applications/image-search/README.html)
- Charles Cave for [OrgNode Parser](http://members.optusnet.com.au/~charles57/GTD/orgnode.html)
- [Org.js](https://mooz.github.io/org-js/) to render Org-mode results on the Web interface
- [Markdown-it](https://github.com/markdown-it/markdown-it) to render Markdown results on the Web interface
- Sven Marnach for [PyExifTool](https://github.com/smarnach/pyexiftool/blob/master/exiftool.py)
