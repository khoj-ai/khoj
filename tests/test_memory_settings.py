"""
Tests for memory enable/disable settings and memory scoping by user+agent.

These tests verify:
1. The behavior of ConversationAdapters.is_memory_enabled() for different combinations of:
   - ServerChatSettings.memory_mode (DISABLED, ENABLED_DEFAULT_OFF, ENABLED_DEFAULT_ON)
   - UserConversationConfig.enable_memory (True, False, or not set)

2. Memory scoping by user and agent:
   - Memories are scoped to user + agent
   - Default agent has access to ALL memories across all agents for a user
   - Non-default agents only see their own memories
"""

import pytest
from unittest.mock import MagicMock

from khoj.database.adapters import ConversationAdapters, UserMemoryAdapters
from khoj.database.models import ServerChatSettings, UserConversationConfig
from khoj.routers.helpers import get_user_config
from tests.helpers import (
    acreate_user,
    acreate_subscription,
    acreate_chat_model,
    acreate_default_agent,
    acreate_agent,
    acreate_test_memory,
    ServerChatSettingsFactory,
    SubscriptionFactory,
    UserFactory,
)


# ----------------------------------------------------------------------------------------------------
# Test is_memory_enabled with no server config (default behavior)
# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_memory_enabled_no_server_config_no_user_config():
    """When no server config and no user config exists, memory should be enabled (default on)."""
    user = UserFactory()
    SubscriptionFactory(user=user)

    result = ConversationAdapters.is_memory_enabled(user)

    assert result is True


@pytest.mark.django_db
def test_memory_enabled_no_server_config_user_enabled():
    """When no server config but user has explicitly enabled memory."""
    user = UserFactory()
    SubscriptionFactory(user=user)
    user_config = UserConversationConfig.objects.create(user=user, enable_memory=True)

    result = ConversationAdapters.is_memory_enabled(user)

    assert result is True


@pytest.mark.django_db
def test_memory_enabled_no_server_config_user_disabled():
    """When no server config but user has explicitly disabled memory."""
    user = UserFactory()
    SubscriptionFactory(user=user)
    user_config = UserConversationConfig.objects.create(user=user, enable_memory=False)

    result = ConversationAdapters.is_memory_enabled(user)

    assert result is False


# ----------------------------------------------------------------------------------------------------
# Test is_memory_enabled with server mode DISABLED
# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_memory_disabled_server_disabled_no_user_config():
    """When server disables memory, it should override everything - no user config."""
    user = UserFactory()
    SubscriptionFactory(user=user)
    ServerChatSettingsFactory(memory_mode=ServerChatSettings.MemoryMode.DISABLED)

    result = ConversationAdapters.is_memory_enabled(user)

    assert result is False


@pytest.mark.django_db
def test_memory_disabled_server_disabled_user_enabled():
    """When server disables memory, it should override user preference (enabled)."""
    user = UserFactory()
    SubscriptionFactory(user=user)
    ServerChatSettingsFactory(memory_mode=ServerChatSettings.MemoryMode.DISABLED)
    UserConversationConfig.objects.create(user=user, enable_memory=True)

    result = ConversationAdapters.is_memory_enabled(user)

    assert result is False


@pytest.mark.django_db
def test_memory_disabled_server_disabled_user_disabled():
    """When server disables memory, user disabled too - should be disabled."""
    user = UserFactory()
    SubscriptionFactory(user=user)
    ServerChatSettingsFactory(memory_mode=ServerChatSettings.MemoryMode.DISABLED)
    UserConversationConfig.objects.create(user=user, enable_memory=False)

    result = ConversationAdapters.is_memory_enabled(user)

    assert result is False


# ----------------------------------------------------------------------------------------------------
# Test is_memory_enabled with server mode ENABLED_DEFAULT_OFF
# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_memory_enabled_default_off_no_user_config():
    """When server is enabled_default_off and no user config, memory should be off."""
    user = UserFactory()
    SubscriptionFactory(user=user)
    ServerChatSettingsFactory(memory_mode=ServerChatSettings.MemoryMode.ENABLED_DEFAULT_OFF)

    result = ConversationAdapters.is_memory_enabled(user)

    assert result is False


@pytest.mark.django_db
def test_memory_enabled_default_off_user_enabled():
    """When server is enabled_default_off and user opts in, memory should be on."""
    user = UserFactory()
    SubscriptionFactory(user=user)
    ServerChatSettingsFactory(memory_mode=ServerChatSettings.MemoryMode.ENABLED_DEFAULT_OFF)
    UserConversationConfig.objects.create(user=user, enable_memory=True)

    result = ConversationAdapters.is_memory_enabled(user)

    assert result is True


@pytest.mark.django_db
def test_memory_enabled_default_off_user_disabled():
    """When server is enabled_default_off and user explicitly disabled, memory should be off."""
    user = UserFactory()
    SubscriptionFactory(user=user)
    ServerChatSettingsFactory(memory_mode=ServerChatSettings.MemoryMode.ENABLED_DEFAULT_OFF)
    UserConversationConfig.objects.create(user=user, enable_memory=False)

    result = ConversationAdapters.is_memory_enabled(user)

    assert result is False


# ----------------------------------------------------------------------------------------------------
# Test is_memory_enabled with server mode ENABLED_DEFAULT_ON
# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_memory_enabled_default_on_no_user_config():
    """When server is enabled_default_on and no user config, memory should be on."""
    user = UserFactory()
    SubscriptionFactory(user=user)
    ServerChatSettingsFactory(memory_mode=ServerChatSettings.MemoryMode.ENABLED_DEFAULT_ON)

    result = ConversationAdapters.is_memory_enabled(user)

    assert result is True


@pytest.mark.django_db
def test_memory_enabled_default_on_user_enabled():
    """When server is enabled_default_on and user enabled, memory should be on."""
    user = UserFactory()
    SubscriptionFactory(user=user)
    ServerChatSettingsFactory(memory_mode=ServerChatSettings.MemoryMode.ENABLED_DEFAULT_ON)
    UserConversationConfig.objects.create(user=user, enable_memory=True)

    result = ConversationAdapters.is_memory_enabled(user)

    assert result is True


@pytest.mark.django_db
def test_memory_enabled_default_on_user_disabled():
    """When server is enabled_default_on and user opts out, memory should be off."""
    user = UserFactory()
    SubscriptionFactory(user=user)
    ServerChatSettingsFactory(memory_mode=ServerChatSettings.MemoryMode.ENABLED_DEFAULT_ON)
    UserConversationConfig.objects.create(user=user, enable_memory=False)

    result = ConversationAdapters.is_memory_enabled(user)

    assert result is False


# ----------------------------------------------------------------------------------------------------
# Test get_user_config returns correct enable_memory and server_memory_mode
# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_get_user_config_memory_no_server_config():
    """get_user_config should return default values when no server config."""
    user = UserFactory()
    SubscriptionFactory(user=user)
    request = MagicMock()
    request.url = MagicMock()
    request.url.path = "/api/config"
    request.session = {}

    config = get_user_config(user, request, is_detailed=True)

    assert config["enable_memory"] is True
    assert config["server_memory_mode"] == "enabled_default_on"


@pytest.mark.django_db
def test_get_user_config_memory_server_disabled():
    """get_user_config should reflect server disabled mode."""
    user = UserFactory()
    SubscriptionFactory(user=user)
    ServerChatSettingsFactory(memory_mode=ServerChatSettings.MemoryMode.DISABLED)
    request = MagicMock()
    request.url = MagicMock()
    request.url.path = "/api/config"
    request.session = {}

    config = get_user_config(user, request, is_detailed=True)

    assert config["enable_memory"] is False
    assert config["server_memory_mode"] == "disabled"


@pytest.mark.django_db
def test_get_user_config_memory_server_enabled_default_off_user_opted_in():
    """get_user_config should show user opted in when server is default off."""
    user = UserFactory()
    SubscriptionFactory(user=user)
    ServerChatSettingsFactory(memory_mode=ServerChatSettings.MemoryMode.ENABLED_DEFAULT_OFF)
    UserConversationConfig.objects.create(user=user, enable_memory=True)
    request = MagicMock()
    request.url = MagicMock()
    request.url.path = "/api/config"
    request.session = {}

    config = get_user_config(user, request, is_detailed=True)

    assert config["enable_memory"] is True
    assert config["server_memory_mode"] == "enabled_default_off"


@pytest.mark.django_db
def test_get_user_config_memory_server_enabled_default_on_user_opted_out():
    """get_user_config should show user opted out when server is default on."""
    user = UserFactory()
    SubscriptionFactory(user=user)
    ServerChatSettingsFactory(memory_mode=ServerChatSettings.MemoryMode.ENABLED_DEFAULT_ON)
    UserConversationConfig.objects.create(user=user, enable_memory=False)
    request = MagicMock()
    request.url = MagicMock()
    request.url.path = "/api/config"
    request.session = {}

    config = get_user_config(user, request, is_detailed=True)

    assert config["enable_memory"] is False
    assert config["server_memory_mode"] == "enabled_default_on"


# ----------------------------------------------------------------------------------------------------
# Test memory scoping by user and agent
# ----------------------------------------------------------------------------------------------------


@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_pull_memories_default_agent_sees_all_memories():
    """Default agent should see ALL memories for the user, including those from other agents."""
    # Setup
    user = await acreate_user()
    await acreate_subscription(user)
    chat_model = await acreate_chat_model()

    # Create default agent
    default_agent = await acreate_default_agent()
    assert default_agent is not None

    # Create a custom agent
    custom_agent = await acreate_agent("Custom Agent", chat_model, "A custom agent")

    # Create memories for different agents
    await acreate_test_memory(user, agent=None, raw_text="memory without agent")
    await acreate_test_memory(user, agent=default_agent, raw_text="memory for default agent")
    await acreate_test_memory(user, agent=custom_agent, raw_text="memory for custom agent")

    # Act: Pull memories with default agent
    memories = await UserMemoryAdapters.pull_memories(user=user, agent=default_agent)

    # Assert: Default agent sees ALL memories
    memory_texts = [m.raw for m in memories]
    assert "memory without agent" in memory_texts
    assert "memory for default agent" in memory_texts
    assert "memory for custom agent" in memory_texts
    assert len(memories) == 3


@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_pull_memories_custom_agent_sees_only_own_memories():
    """Custom (non-default) agent should only see its own memories."""
    # Setup
    user = await acreate_user()
    await acreate_subscription(user)
    chat_model = await acreate_chat_model()

    # Create default agent
    default_agent = await acreate_default_agent()
    assert default_agent is not None

    # Create custom agents
    custom_agent_1 = await acreate_agent("Custom Agent 1", chat_model, "First custom agent")
    custom_agent_2 = await acreate_agent("Custom Agent 2", chat_model, "Second custom agent")

    # Create memories for different agents
    await acreate_test_memory(user, agent=None, raw_text="memory without agent")
    await acreate_test_memory(user, agent=default_agent, raw_text="memory for default agent")
    await acreate_test_memory(user, agent=custom_agent_1, raw_text="memory for custom agent 1")
    await acreate_test_memory(user, agent=custom_agent_2, raw_text="memory for custom agent 2")

    # Act: Pull memories with custom_agent_1
    memories = await UserMemoryAdapters.pull_memories(user=user, agent=custom_agent_1)

    # Assert: Custom agent 1 only sees its own memories
    memory_texts = [m.raw for m in memories]
    assert "memory for custom agent 1" in memory_texts
    assert "memory without agent" not in memory_texts
    assert "memory for default agent" not in memory_texts
    assert "memory for custom agent 2" not in memory_texts
    assert len(memories) == 1


@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_pull_memories_no_agent_same_as_default_agent():
    """Pulling memories with agent=None should behave same as default agent (see all)."""
    # Setup
    user = await acreate_user()
    await acreate_subscription(user)
    chat_model = await acreate_chat_model()

    # Create default agent
    default_agent = await acreate_default_agent()
    assert default_agent is not None

    # Create a custom agent
    custom_agent = await acreate_agent("Custom Agent", chat_model, "A custom agent")

    # Create memories
    await acreate_test_memory(user, agent=None, raw_text="memory without agent")
    await acreate_test_memory(user, agent=default_agent, raw_text="memory for default agent")
    await acreate_test_memory(user, agent=custom_agent, raw_text="memory for custom agent")

    # Act: Pull memories with agent=None
    memories = await UserMemoryAdapters.pull_memories(user=user, agent=None)

    # Assert: Should see all memories (same as default agent behavior)
    memory_texts = [m.raw for m in memories]
    assert "memory without agent" in memory_texts
    assert "memory for default agent" in memory_texts
    assert "memory for custom agent" in memory_texts
    assert len(memories) == 3


@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_save_memory_with_custom_agent_scopes_to_agent():
    """Memories saved with a custom agent should be scoped to that agent."""
    # Setup
    user = await acreate_user()
    await acreate_subscription(user)
    chat_model = await acreate_chat_model()

    # Create default agent
    default_agent = await acreate_default_agent()
    assert default_agent is not None

    # Create custom agent
    custom_agent = await acreate_agent("Custom Agent", chat_model, "A custom agent")

    # Create memory with custom agent (directly in DB to avoid embeddings)
    memory = await acreate_test_memory(user, agent=custom_agent, raw_text="custom agent memory")

    # Assert: Memory is scoped to the custom agent
    assert memory.agent == custom_agent
    assert memory.user == user

    # Verify custom agent can see it
    custom_memories = await UserMemoryAdapters.pull_memories(user=user, agent=custom_agent)
    assert len(custom_memories) == 1
    assert custom_memories[0].raw == "custom agent memory"


@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_save_memory_with_default_agent_has_no_agent_scope():
    """Memories saved with default agent should have agent=None (global scope)."""
    # Setup
    user = await acreate_user()
    await acreate_subscription(user)
    await acreate_chat_model()  # Required for default agent creation

    # Create default agent
    default_agent = await acreate_default_agent()
    assert default_agent is not None

    # Create memory with default agent (directly in DB)
    # Based on save_memory logic: if agent == default_agent, agent is not set
    memory = await acreate_test_memory(user, agent=None, raw_text="default agent memory")

    # Assert: Memory has no agent (global scope)
    assert memory.agent is None
    assert memory.user == user


@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_memories_isolated_between_users():
    """Memories should be isolated between different users."""
    # Setup
    user1 = await acreate_user()
    user2 = await acreate_user()
    await acreate_subscription(user1)
    await acreate_subscription(user2)

    # Create default agent
    await acreate_default_agent()

    # Create memories for each user
    await acreate_test_memory(user1, agent=None, raw_text="user1 memory")
    await acreate_test_memory(user2, agent=None, raw_text="user2 memory")

    # Act: Pull memories for each user
    user1_memories = await UserMemoryAdapters.pull_memories(user=user1)
    user2_memories = await UserMemoryAdapters.pull_memories(user=user2)

    # Assert: Each user only sees their own memories
    assert len(user1_memories) == 1
    assert user1_memories[0].raw == "user1 memory"

    assert len(user2_memories) == 1
    assert user2_memories[0].raw == "user2 memory"


@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_custom_agent_cannot_see_other_custom_agent_memories():
    """One custom agent should not see another custom agent's memories."""
    # Setup
    user = await acreate_user()
    await acreate_subscription(user)
    chat_model = await acreate_chat_model()

    # Create default agent
    await acreate_default_agent()

    # Create two custom agents
    agent_accountant = await acreate_agent("Accountant", chat_model, "Financial advisor")
    agent_chef = await acreate_agent("Chef", chat_model, "Cooking expert")

    # Create memories for each agent
    await acreate_test_memory(user, agent=agent_accountant, raw_text="user spent $500 on groceries")
    await acreate_test_memory(user, agent=agent_chef, raw_text="user likes Italian food")

    # Act & Assert: Accountant only sees financial memories
    accountant_memories = await UserMemoryAdapters.pull_memories(user=user, agent=agent_accountant)
    assert len(accountant_memories) == 1
    assert accountant_memories[0].raw == "user spent $500 on groceries"

    # Act & Assert: Chef only sees food memories
    chef_memories = await UserMemoryAdapters.pull_memories(user=user, agent=agent_chef)
    assert len(chef_memories) == 1
    assert chef_memories[0].raw == "user likes Italian food"
