# Main Readme
> Allow natural language search, chat with your documents using transformer based models

This is a test markdown file with multiple, nested child entries.

## Dependencies

- Python3
- [Miniconda](https://docs.conda.io/en/latest/miniconda.html#latest-miniconda-installer-links)

## Installation

```bash
pip install khoj
```

## Run
  Load ML model, generate embeddings and expose API to query specified org-mode files

  ```shell
  python3 main.py --input-files ~/Notes/Schedule.org ~/Notes/Incoming.org --verbose
  ```

## Use

### **Khoj via API**
- Query: `GET` [http://localhost:42110/api/search?q="What is the meaning of life"](http://localhost:42110/api/search?q=%22what%20is%20the%20meaning%20of%20life%22)
- Update Index: `GET` [http://localhost:42110/api/update](http://localhost:42110/api/update)
- [Khoj API Docs](http://localhost:42110/docs)

### *Khoj via Web*

- Open browser to http://localhost:42110
- Enter query in search box

## Acknowledgments

- [MiniLM Model](https://huggingface.co/sentence-transformers/multi-qa-MiniLM-L6-cos-v1) for Asymmetric Text Search. See (SBert Documentation)[https://www.sbert.net/examples/applications/retrieve_rerank/README.html]
- [OpenAI CLIP Model](https://github.com/openai/CLIP) for Image Search. See [SBert Documentation](https://www.sbert.net/examples/applications/image-search/README.html)
