from types import SimpleNamespace
from unittest.mock import AsyncMock

import pytest

from khoj.routers import helpers
from khoj.utils.helpers import ConversationCommand


@pytest.mark.anyio
async def test_tool_selection_fallback_returns_conversation_commands(monkeypatch):
    monkeypatch.setattr(helpers.EntryAdapters, "auser_has_entries", AsyncMock(return_value=True))
    monkeypatch.setattr(
        helpers,
        "send_message_to_model_wrapper",
        AsyncMock(return_value=SimpleNamespace(text='{"source": [], "output": "text"}')),
    )
    agent = SimpleNamespace(
        input_tools=["general"],
        output_modes=["image"],
        personality=None,
        slug="custom-agent",
        chat_model=object(),
    )

    selected = await helpers.aget_data_sources_and_output_format("summarize this", [], object(), agent=agent)
    commands = selected["sources"] + [selected["output"]]

    assert commands == [ConversationCommand.General, ConversationCommand.Image]
    assert [command.value for command in commands] == ["general", "image"]
