<h1><img src="./assets/khoj-logo-sideways.svg" width="200" alt="Khoj Logo"></h1>

[![test](https://github.com/khoj-ai/khoj/actions/workflows/test.yml/badge.svg)](https://github.com/khoj-ai/khoj/actions/workflows/test.yml)
[![dockerize](https://github.com/khoj-ai/khoj/actions/workflows/dockerize.yml/badge.svg)](https://github.com/khoj-ai/khoj/pkgs/container/khoj)
[![pypi](https://github.com/khoj-ai/khoj/actions/workflows/pypi.yml/badge.svg)](https://pypi.org/project/khoj-assistant/)

# Khoj
*An AI personal assistant for your digital brain*

Welcome to the Docs! This is the best place to get started with Khoj. Check out our [Github](https://github.com/khoj-ai/khoj) to dive straight into the code.

Khoj gives you lightning fast, offline search on your personal machine and gives you the power to talk to your notes.

**Supported Plugins**

[![Khoj on Obsidian](https://img.shields.io/badge/Obsidian-%23483699.svg?style=for-the-badge&logo=obsidian&logoColor=white)](https://github.com/khoj-ai/khoj/tree/master/src/interface/obsidian#readme)
[![Khoj on Emacs](https://img.shields.io/badge/Emacs-%237F5AB6.svg?&style=for-the-badge&logo=gnu-emacs&logoColor=white)](https://github.com/khoj-ai/khoj/tree/master/src/interface/emacs#readme)

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

## Install
[Click here](./setup.md) for full setup instructions.

## Supported data sources
- markdown*
- org-mode*
- pdf*
- images*
- github
- notion

\* These data sources are offline only.

If you're using Github or Notion, you can get on a waitlist for [Khoj Cloud](https://khoj.dev).

## Credits

- [Multi-QA MiniLM Model](https://huggingface.co/sentence-transformers/multi-qa-MiniLM-L6-cos-v1), [All MiniLM Model](https://huggingface.co/sentence-transformers/all-MiniLM-L6-v2) for Text Search. See [SBert Documentation](https://www.sbert.net/examples/applications/retrieve_rerank/README.html)
- [OpenAI CLIP Model](https://github.com/openai/CLIP) for Image Search. See [SBert Documentation](https://www.sbert.net/examples/applications/image-search/README.html)
- Charles Cave for [OrgNode Parser](http://members.optusnet.com.au/~charles57/GTD/orgnode.html)
- [Org.js](https://mooz.github.io/org-js/) to render Org-mode results on the Web interface
- [Markdown-it](https://github.com/markdown-it/markdown-it) to render Markdown results on the Web interface


[^1]: Default Khoj config file @ `~/.khoj/khoj.yml`

[^2]: Default Khoj url @ http://localhost:42110
