# tests/test_agents.py
import os

import pytest
from asgiref.sync import sync_to_async

from khoj.database.adapters import AgentAdapters
from khoj.database.models import Agent, ChatModel, Entry, KhojUser
from khoj.routers.api import execute_search
from khoj.utils.helpers import get_absolute_path
from tests.helpers import ChatModelFactory


def test_create_default_agent(default_user: KhojUser):
    ChatModelFactory()

    agent = AgentAdapters.create_default_agent(default_user)
    assert agent is not None
    assert agent.input_tools == []
    assert agent.output_modes == []
    assert agent.privacy_level == Agent.PrivacyLevel.PUBLIC
    assert agent.managed_by_admin == True


@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_create_or_update_agent(default_user: KhojUser, default_openai_chat_model_option: ChatModel):
    new_agent = await AgentAdapters.aupdate_agent(
        default_user,
        "Test Agent",
        "Test Personality",
        Agent.PrivacyLevel.PRIVATE,
        "icon",
        "color",
        default_openai_chat_model_option.name,
        [],
        [],
        [],
    )
    assert new_agent is not None
    assert new_agent.name == "Test Agent"
    assert new_agent.privacy_level == Agent.PrivacyLevel.PRIVATE
    assert new_agent.creator == default_user


@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_create_or_update_agent_with_knowledge_base(
    default_user2: KhojUser, default_openai_chat_model_option: ChatModel, chat_client
):
    full_filename = get_absolute_path("tests/data/markdown/having_kids.markdown")
    new_agent = await AgentAdapters.aupdate_agent(
        default_user2,
        "Test Agent",
        "Test Personality",
        Agent.PrivacyLevel.PRIVATE,
        "icon",
        "color",
        default_openai_chat_model_option.name,
        [full_filename],
        [],
        [],
    )
    entries = await sync_to_async(list)(Entry.objects.filter(agent=new_agent))
    file_names = set()
    for entry in entries:
        file_names.add(entry.file_path)

    assert new_agent is not None
    assert new_agent.name == "Test Agent"
    assert new_agent.privacy_level == Agent.PrivacyLevel.PRIVATE
    assert new_agent.creator == default_user2
    assert len(entries) > 0
    assert full_filename in file_names
    assert len(file_names) == 1


@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_create_or_update_agent_with_knowledge_base_and_search(
    default_user2: KhojUser, default_openai_chat_model_option: ChatModel, chat_client
):
    full_filename = get_absolute_path("tests/data/markdown/having_kids.markdown")
    new_agent = await AgentAdapters.aupdate_agent(
        default_user2,
        "Test Agent",
        "Test Personality",
        Agent.PrivacyLevel.PRIVATE,
        "icon",
        "color",
        default_openai_chat_model_option.name,
        [full_filename],
        [],
        [],
    )

    search_result = await execute_search(user=default_user2, q="having kids", agent=new_agent)

    assert len(search_result) == 5


@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_agent_with_knowledge_base_and_search_not_creator(
    default_user2: KhojUser, default_openai_chat_model_option: ChatModel, chat_client, default_user3: KhojUser
):
    full_filename = get_absolute_path("tests/data/markdown/having_kids.markdown")
    new_agent = await AgentAdapters.aupdate_agent(
        default_user2,
        "Test Agent",
        "Test Personality",
        Agent.PrivacyLevel.PUBLIC,
        "icon",
        "color",
        default_openai_chat_model_option.name,
        [full_filename],
        [],
        [],
    )

    search_result = await execute_search(user=default_user3, q="having kids", agent=new_agent)

    assert len(search_result) == 5


@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_agent_with_knowledge_base_and_search_not_creator_and_private(
    default_user2: KhojUser, default_openai_chat_model_option: ChatModel, chat_client, default_user3: KhojUser
):
    full_filename = get_absolute_path("tests/data/markdown/having_kids.markdown")
    new_agent = await AgentAdapters.aupdate_agent(
        default_user2,
        "Test Agent",
        "Test Personality",
        Agent.PrivacyLevel.PRIVATE,
        "icon",
        "color",
        default_openai_chat_model_option.name,
        [full_filename],
        [],
        [],
    )

    search_result = await execute_search(user=default_user3, q="having kids", agent=new_agent)

    assert len(search_result) == 0


@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_agent_with_knowledge_base_and_search_not_creator_and_private_accessible_to_none(
    default_user2: KhojUser, default_openai_chat_model_option: ChatModel, chat_client
):
    full_filename = get_absolute_path("tests/data/markdown/having_kids.markdown")
    new_agent = await AgentAdapters.aupdate_agent(
        default_user2,
        "Test Agent",
        "Test Personality",
        Agent.PrivacyLevel.PRIVATE,
        "icon",
        "color",
        default_openai_chat_model_option.name,
        [full_filename],
        [],
        [],
    )

    search_result = await execute_search(user=None, q="having kids", agent=new_agent)

    assert len(search_result) == 5


@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_multiple_agents_with_knowledge_base_and_users(
    default_user2: KhojUser, default_openai_chat_model_option: ChatModel, chat_client, default_user3: KhojUser
):
    full_filename = get_absolute_path("tests/data/markdown/having_kids.markdown")
    new_agent = await AgentAdapters.aupdate_agent(
        default_user2,
        "Test Agent",
        "Test Personality",
        Agent.PrivacyLevel.PUBLIC,
        "icon",
        "color",
        default_openai_chat_model_option.name,
        [full_filename],
        [],
        [],
    )

    full_filename2 = get_absolute_path("tests/data/markdown/Namita.markdown")
    new_agent2 = await AgentAdapters.aupdate_agent(
        default_user2,
        "Test Agent 2",
        "Test Personality",
        Agent.PrivacyLevel.PUBLIC,
        "icon",
        "color",
        default_openai_chat_model_option.name,
        [full_filename2],
        [],
        [],
    )

    search_result = await execute_search(user=default_user3, q="having kids", agent=new_agent2)
    search_result2 = await execute_search(user=default_user3, q="Namita", agent=new_agent2)

    assert len(search_result) == 0
    assert len(search_result2) == 1
