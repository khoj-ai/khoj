<h1><img src="./assets/khoj-logo-sideways-500.png" width="200" alt="Khoj Logo">Emacs</h1>

> An AI personal assistance for your digital brain

<img src="https://stable.melpa.org/packages/khoj-badge.svg" width="150" alt="Melpa Stable Badge">
<img src="https://melpa.org/packages/khoj-badge.svg" width="150" alt="Melpa Badge">

<img src="https://github.com/khoj-ai/khoj/actions/workflows/build_khoj_el.yml/badge.svg" width="150" alt="Build Badge">
<img src="https://github.com/khoj-ai/khoj/actions/workflows/test_khoj_el.yml/badge.svg" width="150" alt="Test Badge">


## Features
- **Search**
  - **Natural**: Advanced natural language understanding using Transformer based ML Models
  - **Local**: Your personal data stays local. All search, indexing is done on your machine*
  - **Incremental**: Incremental search for a fast, search-as-you-type experience
- **Chat**
  - **Faster answers**: Find answers faster than search
  - **Iterative discovery**: Iteratively explore and (re-)discover your notes
  - **Assisted creativity**: Smoothly weave across answer retrieval and content generation

## Interface
#### Search
![khoj search on emacs](./assets/khoj_search_on_emacs.png ':size=400px')

#### Chat
![khoj chat on emacs](./assets/khoj_chat_on_emacs.png ':size=400px')

## Setup
- *Make sure [python](https://realpython.com/installing-python/) and [pip](https://pip.pypa.io/en/stable/installation/) are installed on your machine*

- *khoj.el attempts to automatically install, start and configure the khoj server.*
  If this fails, follow [these instructions](/setup) to manually setup the khoj server.

### Direct Install
```elisp
M-x package-install khoj
```

### Minimal Install
Add below snippet to your Emacs config file.
Indexes your org-agenda files, by default.

```elisp
  ;; Install Khoj Package from MELPA Stable
  (use-package khoj
    :ensure t
    :pin melpa-stable
    :bind ("C-c s" . 'khoj))
```

- Note: Install `khoj.el` from MELPA (instead of MELPA Stable) if you installed the pre-release version of khoj
  - That is, use `:pin melpa` to install khoj.el in above snippet if khoj server was installed with `--pre` flag, i.e `pip install --pre khoj-assistant`
  - Else use `:pin melpa-stable` to install khoj.el in above snippet if khoj was installed with `pip install khoj-assistant`
  - This ensures both khoj.el and khoj app are from the same version (git tagged or latest)

### Standard Install
  Add below snippet to your Emacs config file.
  Indexes the specified org files, directories. Sets up OpenAI API key for Khoj Chat

```elisp
;; Install Khoj Package from MELPA Stable
(use-package khoj
  :ensure t
  :pin melpa-stable
  :bind ("C-c s" . 'khoj)
  :config (setq khoj-org-directories '("~/docs/org-roam" "~/docs/notes")
                khoj-org-files '("~/docs/todo.org" "~/docs/work.org")
                khoj-openai-api-key "YOUR_OPENAI_API_KEY")) ; required to enable chat
```

### With [Straight.el](https://github.com/raxod502/straight.el)
Add below snippet to your Emacs config file.
Indexes the specified org files, directories. Sets up OpenAI API key for Khoj Chat

```elisp
  ;; Install Khoj Package using Straight.el
  (use-package khoj
    :after org
    :straight (khoj :type git :host github :repo "khoj-ai/khoj" :files (:defaults "src/interface/emacs/khoj.el"))
    :bind ("C-c s" . 'khoj)
    :config (setq khoj-org-directories '("~/docs/org-roam" "~/docs/notes")
                  khoj-org-files '("~/docs/todo.org" "~/docs/work.org")
                  khoj-openai-api-key "YOUR_OPENAI_API_KEY" ; required to enable chat)
  ```

## Use
### Search
1. Hit  `C-c s s` (or `M-x khoj RET s`) to open khoj search

2. Enter your query in natural language

  e.g "What is the meaning of life?", "My life goals for 2023"

### Chat
1. Hit `C-c s c` (or `M-x khoj RET c`) to open khoj chat

2. Ask questions in a natural, conversational style

  E.g "When did I file my taxes last year?"

  See [Khoj Chat](/#/chat) for more details

### Find Similar Entries
This feature finds entries similar to the one you are currently on.
1. Move cursor to the org-mode entry, markdown section or text paragraph you want to find similar entries for
2. Hit `C-c s f` (or `M-x khoj RET f`) to find similar entries

### Advanced Usage
- Add [query filters](https://github.com/khoj-ai/khoj/#query-filters) during search to narrow down results further

  e.g `What is the meaning of life? -"god" +"none" dt>"last week"`

- Use `C-c C-o 2` to open the current result at cursor in its source org file
  - This calls `M-x org-open-at-point` on the current entry and opens the second link in the entry.
  - The second link is the entries [org-id](https://orgmode.org/manual/Handling-Links.html#FOOT28), if set, or the heading text.
    The first link is the line number of the entry in the source file. This link is less robust to file changes.
  - Note: If you have [speed keys](https://orgmode.org/manual/Speed-Keys.html) enabled, `o 2` will also work

### Khoj Menu
![](./assets/khoj_emacs_menu.png)
Hit `C-c s` (or `M-x khoj`) to open the khoj menu above. Then:
- Hit `t` until you preferred content type is selected in the khoj menu

  `Content Type` specifies the content to perform `Search`, `Update` or `Find Similar` actions on
- Hit `n` twice and then enter number of results you want to see

  `Results Count` is used by the `Search` and `Find Similar` actions
- Hit `-f u` to `force` update the khoj content index

  The `Force Update` switch is only used by the `Update` action

## Upgrade
### Upgrade Khoj Backend
```bash
pip install --upgrade khoj-assistant
```
### Upgrade Khoj.el
Use your Emacs package manager to upgrade `khoj.el`

- For `khoj.el` from MELPA
  - Method 1
    - Run `M-x package-list-packages` to list all packages
    - Press `U` on `khoj` to mark it for upgrade
    - Press `x` to execute the marked actions
  - Method 2
    - Run `M-x package-refresh-content`
    - Run `M-x package-reinstall khoj`

- For `khoj.el` from Straight
  - Run `M-x straight-pull-package khoj`
