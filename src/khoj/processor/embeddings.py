import os
from typing import List

import requests
import tqdm
from sentence_transformers import CrossEncoder, SentenceTransformer
from torch import nn

from khoj.utils.helpers import get_device
from khoj.utils.rawconfig import SearchResponse

HUGGINGFACE_API_URL = os.getenv("HUGGINGFACE_INFERENCE_API_URL")


class EmbeddingsModel:
    def __init__(self, model_name: str = "thenlper/gte-small"):
        self.encode_kwargs = {"normalize_embeddings": True}
        self.model_kwargs = {"device": get_device()}
        self.model_name = model_name
        self.embeddings_model = SentenceTransformer(self.model_name, **self.model_kwargs)
        self.huggingface_api_key = os.getenv("HUGGINGFACE_API_KEY")

    def embed_query(self, query):
        if self.huggingface_api_key is not None:
            target_url = f"{HUGGINGFACE_API_URL}"
            payload = {"inputs": [query]}
            headers = {"Authorization": f"Bearer {self.huggingface_api_key}", "Content-Type": "application/json"}
            response = requests.post(target_url, json=payload, headers=headers)
            return response.json()["embeddings"][0]
        return self.embeddings_model.encode([query], show_progress_bar=False, **self.encode_kwargs)[0]

    def embed_documents(self, docs):
        if self.huggingface_api_key is not None:
            target_url = f"{HUGGINGFACE_API_URL}"
            # break up the docs payload in chunks of 1000 to avoid hitting rate limits
            with tqdm.tqdm(total=len(docs)) as pbar:
                for i in range(0, len(docs), 1000):
                    payload = {"inputs": docs[i : i + 1000]}
                    headers = {
                        "Authorization": f"Bearer {self.huggingface_api_key}",
                        "Content-Type": "application/json",
                    }
                    response = requests.post(target_url, json=payload, headers=headers)
                    try:
                        response.raise_for_status()
                    except requests.exceptions.HTTPError as e:
                        print(f"Error: {e}")
                        print(f"Response: {response.json()}")
                        raise e
                    if i == 0:
                        embeddings = response.json()["embeddings"]
                    else:
                        embeddings += response.json()["embeddings"]
                    pbar.update(1000)
            return embeddings
        return self.embeddings_model.encode(docs, show_progress_bar=True, **self.encode_kwargs).tolist()


class CrossEncoderModel:
    def __init__(self, model_name: str = "cross-encoder/ms-marco-MiniLM-L-6-v2"):
        self.model_name = model_name
        self.cross_encoder_model = CrossEncoder(model_name=self.model_name, device=get_device())

    def predict(self, query, hits: List[SearchResponse], key: str = "compiled"):
        cross_inp = [[query, hit.additional[key]] for hit in hits]
        cross_scores = self.cross_encoder_model.predict(cross_inp, activation_fct=nn.Sigmoid())
        return cross_scores
