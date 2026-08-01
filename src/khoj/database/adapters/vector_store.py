import math
from typing import Optional, List

from django.db.models import QuerySet, Q
from pgvector.django import CosineDistance
from torch import Tensor

from khoj.database.models import KhojUser, Agent, UserMemory


class PgVectorStore:
    "\""
    A PostgreSQL/pgvector backed store for managing and querying vector embeddings.
    Provides a standardized interface for interacting with vector fields.
    "\""

    def __init__(self, user: KhojUser, agent: Optional[Agent] = None):
        "\""
        Initialize the vector store for a specific user and optionally a specific agent.
        "\""
        self.user = user
        self.agent = agent

    def add(self, raw_text: str, embedding: Tensor, search_model=None) -> UserMemory:
        "\""
        Add a new memory with its embedding to the store.
        "\""
        memory = UserMemory.objects.create(
            user=self.user,
            agent=self.agent,
            raw=raw_text,
            embeddings=embedding,
            search_model=search_model,
        )
        return memory

    def search(
        self, query_embedding: Tensor, top_k: int = 10, max_distance: float = math.inf
    ) -> QuerySet[UserMemory]:
        "\""
        Search the vector store for the closest embeddings using Cosine Distance.
        "\""
        # Base filter by user
        owner_filter = Q(user=self.user)
        
        # If agent is specified, narrow it down
        if self.agent:
            owner_filter &= Q(agent=self.agent)

        relevant_memories = (
            UserMemory.objects.filter(owner_filter)
            .annotate(distance=CosineDistance("embeddings", query_embedding))
            .filter(distance__lte=max_distance)
            .order_by("distance")
        )

        return relevant_memories[:top_k]

    def delete(self, memory_id: int) -> bool:
        "\""
        Delete a specific memory from the store by ID.
        Returns True if deleted, False if not found or unauthorized.
        "\""
        deleted_count, _ = UserMemory.objects.filter(
            id=memory_id, user=self.user
        ).delete()
        return deleted_count > 0
