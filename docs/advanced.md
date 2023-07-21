
## Advanced Usage
### Access Khoj on Mobile
1. [Setup Khoj](#Setup) on your personal server. This can be any always-on machine, i.e an old computer, RaspberryPi(?) etc
2. [Install](https://tailscale.com/kb/installation/) [Tailscale](tailscale.com/) on your personal server and phone
3. Open the Khoj web interface of the server from your phone browser.<br /> It should be `http://tailscale-ip-of-server:42110` or `http://name-of-server:42110` if you've setup [MagicDNS](https://tailscale.com/kb/1081/magicdns/)
4. Click the [Add to Homescreen](https://developer.mozilla.org/en-US/docs/Web/Progressive_web_apps/Add_to_home_screen) button
5. Enjoy exploring your notes, documents and images from your phone!

![](https://github.com/khoj-ai/khoj/blob/master/docs/khoj_pwa_android.png?)

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

!> **Warning**: This will enable Khoj to send your query and note(s) to OpenAI for processing
