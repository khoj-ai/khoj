from typing import List

from sentence_transformers import SentenceTransformer, CrossEncoder
from torch import nn

from khoj.utils.helpers import get_device
from khoj.utils.rawconfig import SearchResponse


class EmbeddingsModel:
    def __init__(self, model_name: str = "thenlper/gte-small"):
        self.encode_kwargs = {"normalize_embeddings": True}
        self.model_kwargs = {"device": get_device()}
        self.model_name = model_name
        self.embeddings_model = SentenceTransformer(self.model_name, **self.model_kwargs)

    def embed_query(self, query):
        return self.embeddings_model.encode([query], show_progress_bar=False, **self.encode_kwargs)[0]

    def embed_documents(self, docs):
        return self.embeddings_model.encode(docs, show_progress_bar=True, **self.encode_kwargs).tolist()


class CrossEncoderModel:
    def __init__(self, model_name: str = "cross-encoder/ms-marco-MiniLM-L-6-v2"):
        self.model_name = model_name
        self.cross_encoder_model = CrossEncoder(model_name=self.model_name, device=get_device())

    def predict(self, query, hits: List[SearchResponse], key: str = "compiled"):
        cross_inp = [[query, hit.additional[key]] for hit in hits]
        cross_scores = self.cross_encoder_model.predict(cross_inp, activation_fct=nn.Sigmoid())
        return cross_scores
