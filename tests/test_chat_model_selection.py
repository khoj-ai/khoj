import pytest
from asgiref.sync import sync_to_async

from khoj.database.adapters import ConversationAdapters
from khoj.database.models import ChatModel, ServerChatSettings
from khoj.utils import state
from tests.helpers import ChatModelFactory, UserFactory


@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_aget_default_chat_model_honors_chat_default_when_chat_advanced_unset():
    """chat_default must be used by aget_default_chat_model for subscribed users.

    Regression test for https://github.com/khoj-ai/khoj/issues/1282
    On self-hosted deployments (billing disabled) every user is treated as
    subscribed. The async model-selection path only ever returned chat_advanced
    for subscribed users and never fell back to chat_default, so chat_default was
    dead config. The synchronous get_default_chat_model already falls back to
    chat_default, so the async path was inconsistent with it.
    """
    prev_billing_enabled = state.billing_enabled
    prev_anonymous_mode = state.anonymous_mode
    state.billing_enabled = False
    state.anonymous_mode = False
    try:
        # Clean slate so the "first created chat model" fallback is deterministic.
        await ServerChatSettings.objects.all().adelete()
        await ChatModel.objects.all().adelete()

        user = await sync_to_async(UserFactory)(username="chat-default-test-user")
        # Create a decoy model first so it has the lowest primary key. On the
        # buggy path aget_default_chat_model returns this decoy (the first
        # chat model) instead of chat_default.
        decoy_model = await sync_to_async(ChatModelFactory)(name="decoy-model")
        default_model = await sync_to_async(ChatModelFactory)(name="chat-default-model")
        assert decoy_model.pk < default_model.pk

        # Only chat_default is configured; chat_advanced is unset.
        await sync_to_async(ServerChatSettings.objects.create)(chat_default=default_model)

        chat_model = await ConversationAdapters.aget_default_chat_model(user=user)

        assert chat_model.pk == default_model.pk
        assert chat_model.name == "chat-default-model"
    finally:
        state.billing_enabled = prev_billing_enabled
        state.anonymous_mode = prev_anonymous_mode


@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_aget_default_chat_model_prefers_chat_advanced_when_set():
    """chat_advanced keeps priority over chat_default when it is configured.

    Server admin preference is preserved: chat_advanced is returned for
    subscribed (self-hosted) users whenever it is set.
    """
    prev_billing_enabled = state.billing_enabled
    prev_anonymous_mode = state.anonymous_mode
    state.billing_enabled = False
    state.anonymous_mode = False
    try:
        await ServerChatSettings.objects.all().adelete()
        await ChatModel.objects.all().adelete()

        user = await sync_to_async(UserFactory)(username="chat-advanced-test-user")
        advanced_model = await sync_to_async(ChatModelFactory)(name="chat-advanced-model")
        default_model = await sync_to_async(ChatModelFactory)(name="chat-default-model")
        await sync_to_async(ServerChatSettings.objects.create)(
            chat_default=default_model, chat_advanced=advanced_model
        )

        chat_model = await ConversationAdapters.aget_default_chat_model(user=user)

        assert chat_model.pk == advanced_model.pk
        assert chat_model.name == "chat-advanced-model"
    finally:
        state.billing_enabled = prev_billing_enabled
        state.anonymous_mode = prev_anonymous_mode
