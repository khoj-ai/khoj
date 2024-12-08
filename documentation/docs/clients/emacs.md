---
sidebar_position: 2
---

# Emacs

<img src="https://stable.melpa.org/packages/khoj-badge.svg" width="130" alt="Melpa Stable Badge" />
<img src="https://melpa.org/packages/khoj-badge.svg" width="150" alt="Melpa Badge" />

<img src="https://github.com/khoj-ai/khoj/actions/workflows/build_khoj_el.yml/badge.svg" width="150" alt="Build Badge" />
<img src="https://github.com/khoj-ai/khoj/actions/workflows/test_khoj_el.yml/badge.svg" width="150" alt="Test Badge" />

<br />
<br />
> Query your Second Brain from Emacs

## Features
- **Chat**
  - **Faster answers**: Find answers quickly, from your private notes or the public internet
  - **Assisted creativity**: Smoothly weave across retrieving answers and generating content
  - **Iterative discovery**: Iteratively explore and re-discover your notes
- **Search**
  - **Natural**: Advanced natural language understanding using Transformer based ML Models
  - **Incremental**: Incremental search for a fast, search-as-you-type experience

## Interface

| Search | Chat |
|:------:|:----:|
| ![khoj search on emacs](/img/khoj_search_on_emacs.png) | ![khoj chat on emacs](/img/khoj_chat_on_emacs.png) |

## Setup
:::info[Self Hosting]
If you are self-hosting the Khoj server modify the install steps below:
- Set `khoj-server-url` to your Khoj server URL. By default, use `http://127.0.0.1:42110`.
- Do not set `khoj-api-key` if your Khoj server runs in anonymous mode. For example, `khoj --anonymous-mode`
:::

1. Generate an API key on the [Khoj Web App](https://app.khoj.dev/settings#clients)
2. Add below snippet to your Emacs config file, usually at `~/.emacs.d/init.el`


#### **Direct Install**
*Khoj will index your org-agenda files, by default*

```elisp
;; Install Khoj.el
M-x package-install khoj

; Set your Khoj API key
(setq khoj-api-key "YOUR_KHOJ_CLOUD_API_KEY")
(setq khoj-server-url "https://app.khoj.dev")
```

#### **Minimal Install**
*Khoj will index your org-agenda files, by default*

```elisp
;; Install Khoj client from MELPA Stable
(use-package khoj
  :ensure t
  :pin melpa-stable
  :bind ("C-c s" . 'khoj)
  :config (setq khoj-api-key "YOUR_KHOJ_CLOUD_API_KEY"
                khoj-server-url "https://app.khoj.dev"))
```

#### **Standard Install**
*Configures the specified org files, directories to be indexed by Khoj*

```elisp
;; Install Khoj client from MELPA Stable
(use-package khoj
  :ensure t
  :pin melpa-stable
  :bind ("C-c s" . 'khoj)
  :config (setq khoj-api-key "YOUR_KHOJ_CLOUD_API_KEY"
                khoj-server-url "https://app.khoj.dev"
                khoj-index-directories '("~/docs/org-roam" "~/docs/notes")
                khoj-index-files '("~/docs/todo.org" "~/docs/work.org")))
```

#### **Straight.el**
*Configures the specified org files, directories to be indexed by Khoj*

```elisp
;; Install Khoj client using Straight.el
(use-package khoj
  :after org
  :straight (khoj :type git :host github :repo "khoj-ai/khoj" :files (:defaults "src/interface/emacs/khoj.el"))
  :bind ("C-c s" . 'khoj)
  :config (setq khoj-api-key "YOUR_KHOJ_CLOUD_API_KEY"
                khoj-server-url "https://app.khoj.dev"
                khoj-org-directories '("~/docs/org-roam" "~/docs/notes")
                khoj-org-files '("~/docs/todo.org" "~/docs/work.org")))
```

## Use
### Search
See [Khoj Search](/features/search) for details
1. Hit  `C-c s s` (or `M-x khoj RET s`) to open khoj search
2. Enter your query in natural language<br/>
  E.g. *"What is the meaning of life?"*, *"My life goals for 2023"*

### Chat
See [Khoj Chat](/features/chat) for details
1. Hit `C-c s c` (or `M-x khoj RET c`) to open khoj chat
2. Ask questions in a natural, conversational style<br/>
  E.g. *"When did I file my taxes last year?"*

### Find Similar Entries
This feature finds entries similar to the one you are currently on.
1. Move cursor to the org-mode entry, markdown section or text paragraph you want to find similar entries for
2. Hit `C-c s f` (or `M-x khoj RET f`) to find similar entries

### Advanced Usage
- Add [query filters](/miscellaneous/advanced#query-filters) during search to narrow down results further
  e.g. `What is the meaning of life? -"god" +"none" dt>"last week"`

- Use `C-c C-o 2` to open the current result at cursor in its source org file
  - This calls `M-x org-open-at-point` on the current entry and opens the second link in the entry.
  - The second link is the entries [org-id](https://orgmode.org/manual/Handling-Links.html#FOOT28), if set, or the heading text.
    The first link is the line number of the entry in the source file. This link is less robust to file changes.
  - Note: If you have [speed keys](https://orgmode.org/manual/Speed-Keys.html) enabled, `o 2` will also work

### Khoj Menu
![](/img/khoj_emacs_menu.png)
Hit `C-c s` (or `M-x khoj`) to open the khoj menu above. Then:
- Hit `t` until you preferred content type is selected in the khoj menu
  `Content Type` specifies the content to perform `Search`, `Update` or `Find Similar` actions on
- Hit `n` twice and then enter number of results you want to see
  `Results Count` is used by the `Search` and `Find Similar` actions
- Hit `-f u` to `force` update the khoj content index
  The `Force Update` switch is only used by the `Update` action

## Upgrade
Use your Emacs package manager to upgrade `khoj.el`
<!-- tabs:start -->

#### **With MELPA**
1. Run `M-x package-refresh-content`
2. Run `M-x package-reinstall khoj`

#### **With Straight.el**
- Run `M-x straight-pull-package khoj`

<!-- tabs:end -->
