from typing import List

import torch
from langchain.embeddings import HuggingFaceEmbeddings
from sentence_transformers import CrossEncoder

from khoj.utils.rawconfig import SearchResponse


class EmbeddingsModel:
    def __init__(self):
        self.model_name = "thenlper/gte-small"
        encode_kwargs = {"normalize_embeddings": True, "show_progress_bar": True}

        if torch.cuda.is_available():
            # Use CUDA GPU
            device = torch.device("cuda:0")
        elif torch.backends.mps.is_available():
            # Use Apple M1 Metal Acceleration
            device = torch.device("mps")
        else:
            device = torch.device("cpu")

        model_kwargs = {"device": device}
        self.embeddings_model = HuggingFaceEmbeddings(
            model_name=self.model_name, encode_kwargs=encode_kwargs, model_kwargs=model_kwargs
        )

    def embed_query(self, query):
        return self.embeddings_model.embed_query(query)

    def embed_documents(self, docs):
        return self.embeddings_model.embed_documents(docs)


class CrossEncoderModel:
    def __init__(self):
        self.model_name = "cross-encoder/ms-marco-MiniLM-L-6-v2"

        if torch.cuda.is_available():
            # Use CUDA GPU
            device = torch.device("cuda:0")

        elif torch.backends.mps.is_available():
            # Use Apple M1 Metal Acceleration
            device = torch.device("mps")

        else:
            device = torch.device("cpu")

        self.cross_encoder_model = CrossEncoder(model_name=self.model_name, device=device)

    def predict(self, query, hits: List[SearchResponse]):
        cross__inp = [[query, hit.additional["compiled"]] for hit in hits]
        cross_scores = self.cross_encoder_model.predict(cross__inp)
        return cross_scores
