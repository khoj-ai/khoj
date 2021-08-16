Semantic Search
===
> Provide natural language search on user personal content like notes, images using ML models

All data is processed locally. User can interface with semantic-search app via [Emacs](https://github.com/debanjum/emacs-semantic-search), API or Commandline

Dependencies
----
  - Python3
  - [Miniconda](https://docs.conda.io/en/latest/miniconda.html#latest-miniconda-installer-links)

Install
---
  ```sh
  git clone https://github.com/debanjum/semantic-search && cd semantic-search
  conda env create -f environment.yml
  conda activate semantic-search
  ```

Setup
---
  Generate compressed JSONL from specified org-mode files
  ```sh
  python3 processor/org-mode/org-to-jsonl.py \
  --org-files "Schedule.org" "Incoming.org" \
  --org-directory "~/Notes" \
  --jsonl-file ".data/notes.jsonl" \
  --compress \
  --verbose
  ```

Run
---
  Load ML model, generate embeddings and expose API interface to run user queries on above org-mode files
  ```sh
  python3 main.py -j .data/notes.jsonl.gz -e .data/notes_embeddings.pt
  ```
    
Use
---
  - *Calls Semantic Search via Emacs*
    - `M-x semantic-search "<user-query>"`
    - `C-c C-s`
  
  - *Call Semantic Search via API*
    - `GET` [http://localhost:8000/search?q="What is the meaning of life"](http://localhost:8000/search?q=%22what%20is%20the%20meaning%20of%20life%22)
  
  - *Call Semantic Search via Python Script Directly*
    ```sh
    python3 search_types/asymmetric.py \
    -j .data/notes.jsonl.gz \
    -e .data/notes_embeddings.pt \
    -n 5 \
    --verbose \
    --interactive
    ```

Acknowledgments
--
- Charles Cave for [OrgNode Parser](http://members.optusnet.com.au/~charles57/GTD/orgnode.html)
