import json
from types import SimpleNamespace

import pytest
from starlette.requests import Request

from khoj.database.adapters import AgentAdapters
from khoj.database.models import Agent
from khoj.routers.api_agents import all_agents
from khoj.utils import state


@pytest.fixture
def agent_visibility_cases(default_user, default_user3, default_openai_chat_model_option):
    def create_agent(name, privacy_level, creator=default_user, **kwargs):
        return Agent.objects.create(
            name=name,
            privacy_level=privacy_level,
            creator=creator,
            chat_model=default_openai_chat_model_option,
            **kwargs,
        )

    return {
        "default": create_agent(
            AgentAdapters.DEFAULT_AGENT_NAME,
            Agent.PrivacyLevel.PUBLIC,
            creator=None,
            slug=AgentAdapters.DEFAULT_AGENT_SLUG,
        ),
        "public": create_agent("Public agent", Agent.PrivacyLevel.PUBLIC),
        "private": create_agent("Private agent", Agent.PrivacyLevel.PRIVATE),
        "protected": create_agent("Unlisted agent", Agent.PrivacyLevel.PROTECTED),
        "hidden": create_agent("Hidden agent", Agent.PrivacyLevel.PRIVATE, is_hidden=True),
        "hidden_public": create_agent("Hidden public agent", Agent.PrivacyLevel.PUBLIC, is_hidden=True),
        "admin_private": create_agent("Private admin agent", Agent.PrivacyLevel.PRIVATE, creator=None),
        "other_private": create_agent("Other user's agent", Agent.PrivacyLevel.PRIVATE, creator=default_user3),
    }


@pytest.mark.parametrize("billing_enabled", [False, True], ids=["self-hosted", "cloud"])
@pytest.mark.parametrize("viewer", ["owner", "other", "anonymous"])
def test_agent_listing_respects_deployment_and_privacy(
    monkeypatch, default_user, default_user3, agent_visibility_cases, billing_enabled, viewer
):
    monkeypatch.setattr(state, "billing_enabled", billing_enabled)
    users = {"owner": default_user, "other": default_user3, "anonymous": None}
    agents = agent_visibility_cases
    expected = {agents["default"].pk}
    if not billing_enabled or viewer == "owner":
        expected.add(agents["public"].pk)
    if viewer == "owner":
        expected.update([agents["private"].pk, agents["protected"].pk])
    elif viewer == "other":
        expected.add(agents["other_private"].pk)

    listed_agents = list(AgentAdapters.get_all_accessible_agents(users[viewer]))

    assert {agent.pk for agent in listed_agents} == expected
    assert len(listed_agents) == len(expected)


@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
@pytest.mark.parametrize("billing_enabled", [False, True], ids=["self-hosted", "cloud"])
@pytest.mark.parametrize("authenticated", [False, True], ids=["anonymous", "other-user"])
async def test_agent_list_response_includes_shared_public_agents(
    monkeypatch, default_user3, agent_visibility_cases, billing_enabled, authenticated
):
    monkeypatch.setattr(state, "billing_enabled", billing_enabled)
    request = Request(
        {
            "type": "http",
            "user": SimpleNamespace(is_authenticated=authenticated, object=default_user3, client_app=None),
        }
    )
    agents = agent_visibility_cases
    expected = {agents["default"].slug}
    if not billing_enabled:
        expected.add(agents["public"].slug)
    if authenticated:
        expected.add(agents["other_private"].slug)

    response = await all_agents(request, None)
    listed_agents = json.loads(response.body)

    assert response.status_code == 200
    assert {agent["slug"] for agent in listed_agents} == expected
    assert listed_agents[0]["slug"] == AgentAdapters.DEFAULT_AGENT_SLUG
