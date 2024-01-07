---
sidebar_position: 2
---

# Demos

Check out a couple of demos and screenshots of Khoj in action.

### Screenshots

| Web | Obsidian | Emacs |
|:---:|:--------:|:-----:|
| ![](/img/khoj_search_on_web.png ':size=300px') | ![](/img/khoj_search_on_obsidian.png ':size=300px') | ![](/img/khoj_search_on_emacs.png ':size=300px') |
| ![](/img/khoj_chat_on_web.png ':size=300px') | ![](/img/khoj_chat_on_obsidian.png ':size=300px') | ![](/img/khoj_chat_on_emacs.png ':size=400px') |


### Videos
#### Khoj in Obsidian
[Link to Video](https://github-production-user-asset-6210df.s3.amazonaws.com/6413477/240061700-3e33d8ea-25bb-46c8-a3bf-c92f78d0f56b.mp4)

##### Installation

1. Install Khoj via `pip` and start Khoj backend in a terminal (Run `khoj`)
    ```bash
    python -m pip install khoj-assistant
    khoj
    ```
2. Install Khoj plugin via Community Plugins settings pane on Obsidian app
    - Check the new Khoj plugin settings
    - Let Khoj backend index the markdown, pdf, Github markdown files in the current Vault
    - Open Khoj plugin on Obsidian via Search button on Left Pane
    - Search \"*Announce plugin to folks*\" in the [Obsidian Plugin docs](https://marcus.se.net/obsidian-plugin-docs/)
    - Jump to the [search result](https://marcus.se.net/obsidian-plugin-docs/publishing/submit-your-plugin)

#### Khoj in Emacs, Browser
[Link to Video](https://user-images.githubusercontent.com/6413477/184735169-92c78bf1-d827-4663-9087-a1ea194b8f4b.mp4)

##### Installation

- Install Khoj via pip
- Start Khoj app
- Add this readme and [khoj.el readme](https://github.com/khoj-ai/khoj/tree/master/src/interface/emacs) as org-mode for Khoj to index
- Search \"*Setup editor*\" on the Web and Emacs. Re-rank the results for better accuracy
- Top result is what we are looking for, the [section to Install Khoj.el on Emacs](https://github.com/khoj-ai/khoj/tree/master/src/interface/emacs#2-Install-Khojel)

##### Analysis

- The results do not have any words used in the query
  - *Based on the top result it seems the re-ranking model understands that Emacs is an editor?*
- The results incrementally update as the query is entered
- The results are re-ranked, for better accuracy, once user hits enter
