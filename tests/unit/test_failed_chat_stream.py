import asyncio
import json
from types import SimpleNamespace
from unittest.mock import AsyncMock

import pytest
from fastapi import Request, WebSocket
from fastapi.websockets import WebSocketState
from starlette.authentication import AuthCredentials
from starlette.datastructures import Headers

from khoj.processor.conversation.utils import ResponseWithThought
from khoj.routers import api_chat
from khoj.routers.helpers import ChatEvent, CommonQueryParamsClass
from khoj.utils.rawconfig import ChatRequestBody


@pytest.fixture
def chat_dependencies(monkeypatch):
    agent = SimpleNamespace(slug="test-agent")
    conversation = SimpleNamespace(
        id="conversation", agent=agent, messages=[], pop_message=AsyncMock(return_value=None)
    )
    monkeypatch.setattr(
        api_chat.ConversationAdapters, "aget_conversation_by_user", AsyncMock(return_value=conversation)
    )
    monkeypatch.setattr(api_chat.ConversationAdapters, "ais_memory_enabled", AsyncMock(return_value=False))
    monkeypatch.setattr(api_chat.AgentAdapters, "aget_default_agent", AsyncMock(return_value=agent))
    monkeypatch.setattr(api_chat, "is_ready_to_chat", AsyncMock())
    monkeypatch.setattr(api_chat, "aget_user_name", AsyncMock(return_value="Test"))
    monkeypatch.setattr(api_chat.conversation_command_rate_limiter, "update_and_check_if_valid", AsyncMock())
    save = AsyncMock()
    monkeypatch.setattr(api_chat, "save_to_conversation_log", save)
    monkeypatch.setattr(api_chat, "update_telemetry_state", lambda **kwargs: None)
    return save


@pytest.mark.asyncio
@pytest.mark.parametrize("failure", ["before", "partial", "setup", "success"])
@pytest.mark.parametrize("transport", ["http", "websocket"])
@pytest.mark.parametrize("stream", [False, True])
async def test_provider_failure_finishes_chat_and_stops_monitor(
    monkeypatch, chat_dependencies, failure, transport, stream
):
    monitor_started = asyncio.Event()
    monitor_finished = asyncio.Event()

    async def receive():
        monitor_started.set()
        try:
            await asyncio.Event().wait()
        finally:
            monitor_finished.set()

    monitor_tasks = []
    create_task = asyncio.create_task

    def track_task(coro):
        task = create_task(coro)
        if "monitor_disconnection" in coro.__qualname__:
            monitor_tasks.append(task)
        return task

    monkeypatch.setattr(api_chat.asyncio, "create_task", track_task)
    if transport == "http":
        request = Request({"type": "http", "auth": AuthCredentials(), "headers": []}, receive)
    else:
        request = WebSocket({"type": "websocket", "auth": AuthCredentials(), "headers": []}, receive, AsyncMock())
        request.client_state = WebSocketState.CONNECTED
    user = SimpleNamespace(id=1, username="test")
    scope = SimpleNamespace(object=user, client_app=None)

    async def provider():
        if failure in ("partial", "success"):
            yield ResponseWithThought(text="Partial answer")
        if failure != "success":
            raise ConnectionError("provider offline")

    async def generate(*args, **kwargs):
        if transport == "http":
            await monitor_started.wait()
        else:
            await asyncio.sleep(0)
        if failure == "setup":
            raise ConnectionError("provider offline")
        return provider(), {}

    monkeypatch.setattr(api_chat, "agenerate_chat_response", generate)
    events = []
    try:
        async for event in api_chat.event_generator(
            ChatRequestBody(q="/general hello", stream=stream),
            scope,
            CommonQueryParamsClass(client="test"),
            Headers(),
            request,
        ):
            events.append(event)
        await asyncio.sleep(0)
        if transport == "http":
            assert monitor_finished.is_set()
        assert len(monitor_tasks) == 1
        assert monitor_tasks[0].done()
        metadata = [json.loads(event) for event in events if event.startswith('{"type"')]
        assert metadata[-1]["type"] == ChatEvent.END_RESPONSE.value
        assert sum(event["type"] == ChatEvent.END_LLM_RESPONSE.value for event in metadata) == 1
        if failure == "success":
            assert chat_dependencies.await_count == 1
        else:
            assert any("could not complete" in event for event in events)
            chat_dependencies.assert_not_called()
    finally:
        tasks = [task for task in asyncio.all_tasks() if "monitor_disconnection" in task.get_coro().__qualname__]
        for task in tasks:
            task.cancel()
        await asyncio.gather(*tasks, return_exceptions=True)


@pytest.mark.asyncio
@pytest.mark.parametrize("shutdown", ["close", "cancel"])
async def test_consumer_shutdown_closes_provider_and_disconnect_monitor(monkeypatch, chat_dependencies, shutdown):
    provider_closed = asyncio.Event()
    provider_waiting = asyncio.Event()

    async def receive():
        await asyncio.Event().wait()

    async def provider():
        try:
            yield ResponseWithThought(text="Partial answer")
            provider_waiting.set()
            await asyncio.Event().wait()
        finally:
            provider_closed.set()

    monkeypatch.setattr(api_chat, "agenerate_chat_response", AsyncMock(return_value=(provider(), {})))
    request = Request({"type": "http", "auth": AuthCredentials(), "headers": []}, receive)
    scope = SimpleNamespace(object=SimpleNamespace(id=1, username="test"), client_app=None)
    response = api_chat.event_generator(
        ChatRequestBody(q="/general hello", stream=True),
        scope,
        CommonQueryParamsClass(client="test"),
        Headers(),
        request,
    )
    async for event in response:
        if event == "Partial answer":
            break
    monitors = [task for task in asyncio.all_tasks() if "monitor_disconnection" in task.get_coro().__qualname__]
    assert len(monitors) == 1
    if shutdown == "close":
        await response.aclose()
    else:

        async def consume_remaining():
            async for _ in response:
                pass

        task = asyncio.create_task(consume_remaining())
        await asyncio.wait_for(provider_waiting.wait(), timeout=2)
        task.cancel()
        with pytest.raises(asyncio.CancelledError):
            await task
    assert provider_closed.is_set()
    assert monitors[0].done()
    chat_dependencies.assert_not_called()
