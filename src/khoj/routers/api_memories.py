import json
import logging
from typing import Optional

from asgiref.sync import sync_to_async
from fastapi import APIRouter, Request
from fastapi.responses import Response
from pydantic import BaseModel
from starlette.authentication import requires

from khoj.database.adapters import UserMemoryAdapters
from khoj.database.models import UserMemory

api_memories = APIRouter()
logger = logging.getLogger(__name__)


@api_memories.get("")
@requires(["authenticated"])
async def get_memories(
    request: Request,
    client: Optional[str] = None,
):
    """Get all memories for the authenticated user"""
    user = request.user.object

    memories = UserMemory.objects.filter(user=user)
    all_memories = await sync_to_async(list)(memories)

    # Convert memories to a list of dictionaries
    formatted_memories = [
        {
            "id": memory.id,
            "raw": memory.raw,
            "created_at": memory.created_at.isoformat(),
        }
        for memory in all_memories
    ]

    return Response(content=json.dumps(formatted_memories), media_type="application/json", status_code=200)


@api_memories.delete("/{memory_id}")
@requires(["authenticated"])
async def delete_memory(
    request: Request,
    memory_id: int,
    client: Optional[str] = None,
):
    """Delete a specific memory by ID"""
    user = request.user.object

    # Verify memory belongs to user before deleting
    memory = await UserMemory.objects.filter(id=memory_id, user=user).afirst()
    if not memory:
        return Response(
            content=json.dumps({"error": "Memory not found"}), media_type="application/json", status_code=404
        )

    await memory.adelete()

    return Response(status_code=204)


class UpdateMemoryBody(BaseModel):
    """Request model for updating a memory"""

    raw: str


@api_memories.put("/{memory_id}")
@requires(["authenticated"])
async def update_memory(
    request: Request,
    body: UpdateMemoryBody,
    memory_id: int,
    client: Optional[str] = None,
):
    """Update a specific memory's content"""
    user = request.user.object

    # Get the memory and verify it belongs to the user
    memory = await UserMemory.objects.filter(id=memory_id, user=user).afirst()
    if not memory:
        return Response(
            content=json.dumps({"error": "Memory not found"}), media_type="application/json", status_code=404
        )

    new_content = body.raw
    if not new_content:
        return Response(
            content=json.dumps({"error": "Missing required field 'raw'"}),
            media_type="application/json",
            status_code=400,
        )

    await memory.adelete()

    # Create a new memory with the updated content
    new_memory = await UserMemoryAdapters.save_memory(
        user=user,
        memory=new_content,
    )

    return Response(
        content=json.dumps(
            {
                "id": new_memory.id,
                "raw": new_memory.raw,
            }
        ),
        media_type="application/json",
        status_code=200,
    )
