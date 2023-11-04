from typing import List

from sentence_transformers import SentenceTransformer, CrossEncoder

from khoj.utils.helpers import get_device
from khoj.utils.rawconfig import SearchResponse


class EmbeddingsModel:
    def __init__(self):
        self.model_name = "thenlper/gte-small"
        self.encode_kwargs = {"normalize_embeddings": True}
        model_kwargs = {"device": get_device()}
        self.embeddings_model = SentenceTransformer(self.model_name, **model_kwargs)

    def embed_query(self, query):
        return self.embeddings_model.encode([query], show_progress_bar=False, **self.encode_kwargs)[0]

    def embed_documents(self, docs):
        return self.embeddings_model.encode(docs, show_progress_bar=True, **self.encode_kwargs).tolist()


class CrossEncoderModel:
    def __init__(self):
        self.model_name = "cross-encoder/ms-marco-MiniLM-L-6-v2"
        self.cross_encoder_model = CrossEncoder(model_name=self.model_name, device=get_device())

    def predict(self, query, hits: List[SearchResponse]):
        cross__inp = [[query, hit.additional["compiled"]] for hit in hits]
        cross_scores = self.cross_encoder_model.predict(cross__inp)
        return cross_scores
