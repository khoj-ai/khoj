Semantic Search
===
> Provide natural language search on user personal content like notes, images using ML models

All data is processed locally. User can interface with semantic-search app via [Emacs](./interface/emacs/semantic-search.el), API or Commandline

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
  --input-files ~/Notes/Schedule.org ~/Notes/Incoming.org \
  --output-file .notes.jsonl.gz \
  --verbose
  ```

Run
---
  Load ML model, generate embeddings and expose API interface to run user queries on above org-mode files
  ```sh
  python3 main.py \
  --compressed-jsonl .notes.jsonl.gz \
  --embeddings .notes_embeddings.pt \
  --verbose
  ```
    
Use
---
  - *Semantic Search via Emacs*
    - [Install](https://github.com/debanjum/semantic-search/tree/master/interface/emacs#installation) [semantic-search.el](./interface/emacs/semantic-search.el)
    - Run `M-x semantic-search "<user-query>"` or Call `C-c C-s`
  
  - *Call Semantic Search via API*
    - `GET` [http://localhost:8000/search?q="What is the meaning of life"](http://localhost:8000/search?q=%22what%20is%20the%20meaning%20of%20life%22)
  
  - *Call Semantic Search via Python Script Directly*
    ```sh
    python3 search_types/asymmetric.py \
    --compressed-jsonl .notes.jsonl.gz \
    --embeddings .notes_embeddings.pt \
    --results-count 5 \
    --verbose \
    --interactive
    ```

Acknowledgments
--
- [MiniLM Model](https://huggingface.co/sentence-transformers/msmarco-MiniLM-L-6-v3) for Asymmetric Text Search. See [SBert Documentation](https://www.sbert.net/examples/applications/retrieve_rerank/README.html)
- [OpenAI CLIP Model](https://github.com/openai/CLIP) for Image Search. See [SBert Documentation](https://www.sbert.net/examples/applications/image-search/README.html) 
- Charles Cave for [OrgNode Parser](http://members.optusnet.com.au/~charles57/GTD/orgnode.html)
