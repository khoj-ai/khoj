
## Advanced Usage
### Search across Different Languages
To search for notes in multiple, different languages, you can use a [multi-lingual model](https://www.sbert.net/docs/pretrained_models.html#multi-lingual-models).<br />
For example, the [paraphrase-multilingual-MiniLM-L12-v2](https://huggingface.co/sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2) supports [50+ languages](https://www.sbert.net/docs/pretrained_models.html#:~:text=we%20used%20the%20following%2050%2B%20languages), has good search quality and speed. To use it:
1. Manually update `search-type > asymmetric > encoder` to `paraphrase-multilingual-MiniLM-L12-v2` in your `~/.khoj/khoj.yml` file for now. See diff of `khoj.yml` below for illustration:

    ```diff
    asymmetric:
    -  encoder: sentence-transformers/multi-qa-MiniLM-L6-cos-v1
    +  encoder: paraphrase-multilingual-MiniLM-L12-v2
      cross-encoder: cross-encoder/ms-marco-MiniLM-L-6-v2
      model_directory: "~/.khoj/search/asymmetric/"
    ```

2. Regenerate your content index. For example, by opening [\<khoj-url\>/api/update?t=force](http://localhost:42110/api/update?t=force)

### Access Khoj on Mobile
1. [Setup Khoj](/#/setup) on your personal server. This can be any always-on machine, i.e an old computer, RaspberryPi(?) etc
2. [Install](https://tailscale.com/kb/installation/) [Tailscale](tailscale.com/) on your personal server and phone
3. Open the Khoj web interface of the server from your phone browser.<br /> It should be `http://tailscale-ip-of-server:42110` or `http://name-of-server:42110` if you've setup [MagicDNS](https://tailscale.com/kb/1081/magicdns/)
4. Click the [Add to Homescreen](https://developer.mozilla.org/en-US/docs/Web/Progressive_web_apps/Add_to_home_screen) button
5. Enjoy exploring your notes, documents and images from your phone!

![](./assets/khoj_pwa_android.png?)

### Use OpenAI Models for Search
#### Setup
1. Set `encoder-type`, `encoder` and `model-directory` under `asymmetric` and/or `symmetric` `search-type` in your `khoj.yml` (at `~/.khoj/khoj.yml`):
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
2. [Setup your OpenAI API key in Khoj](/#/chat?id=setup)
3. Restart Khoj server to generate embeddings. It will take longer than with the offline search models.

#### Warnings
  This configuration *uses an online model*
  - It will **send all notes to OpenAI** to generate embeddings
  - **All queries will be sent to OpenAI** when you search with Khoj
  - You will be **charged by OpenAI** based on the total tokens processed
  - It *requires an active internet connection* to search and index

### Bootstrap Khoj Search for Offline Usage later

You can bootstrap Khoj pre-emptively to run on machines that do not have internet access. An example use-case would be to run Khoj on an air-gapped machine.
Note: *Only search can currently run in fully offline mode, not chat.*

- With Internet
  1. Manually download the [asymmetric text](https://huggingface.co/sentence-transformers/multi-qa-MiniLM-L6-cos-v1), [symmetric text](https://huggingface.co/sentence-transformers/all-MiniLM-L6-v2) and [image search](https://huggingface.co/sentence-transformers/clip-ViT-B-32) models from HuggingFace
  2. Pip install khoj (and dependencies) in an associated virtualenv. E.g `python -m venv .venv && source .venv/bin/activate && pip install khoj-assistant`
- Without Internet
  1. Copy each of the search models into their respective folders, `asymmetric`, `symmetric` and `image` under the `~/.khoj/search/` directory on the air-gapped machine
  2. Copy the khoj virtual environment directory onto the air-gapped machine, activate the environment and start and khoj as normal. E.g `source .venv/bin/activate && khoj`
