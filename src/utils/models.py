# External Packages
import openai
import torch
from tqdm import trange

# Internal Packages
from src.utils.state import processor_config


class OpenAI:
    def __init__(self, model_name, device=None):
        self.model_name = model_name
        openai.api_key = processor_config.conversation.openai_api_key
        self.embedding_dimensions = 1536  # Default to embedding dimensions of text-embedding-ada-002 model

    def encode(self, entries, device=None, **kwargs):
        embedding_tensors = []
        for index in trange(0, len(entries)):
            try:
                response = openai.Embedding.create(input=entries[index], model=self.model_name)
                embedding_tensors += [torch.tensor(response.data[0].embedding, device=device)]
                self.embedding_dimensions = len(response.data[0].embedding)  # Set embedding dimensions to this model's
            except Exception as e:
                print(f"Failed to encode entry {index} of length: {len(entries[index])}\n\n{entries[index][:1000]}...\n\n{e}")
                embedding_tensors += [torch.zeros(self.embedding_dimensions, device=device)]
        return torch.stack(embedding_tensors)


