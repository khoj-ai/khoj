import pytest
from apscheduler.schedulers.background import BackgroundScheduler
from django_apscheduler.jobstores import DjangoJobStore
from fastapi.testclient import TestClient

from khoj.utils import state
from tests.helpers import AiModelApiFactory, ChatModelFactory, get_chat_api_key


@pytest.fixture(autouse=True)
def setup_scheduler():
    state.scheduler = BackgroundScheduler()
    state.scheduler.add_jobstore(DjangoJobStore(), "default")
    state.scheduler.start()
    yield
    state.scheduler.shutdown()


def create_test_automation(client: TestClient) -> str:
    """Helper function to create a test automation and return its ID."""
    state.anonymous_mode = True
    ChatModelFactory(
        name="gemini-2.0-flash", model_type="google", ai_model_api=AiModelApiFactory(api_key=get_chat_api_key("google"))
    )
    params = {
        "q": "test automation",
        "crontime": "0 0 * * *",
    }
    response = client.post("/api/automation", params=params)
    assert response.status_code == 200
    return response.json()["id"]


@pytest.mark.django_db(transaction=True)
def test_create_automation(client: TestClient):
    """Test that creating an automation works as expected."""
    # Arrange
    state.anonymous_mode = True
    ChatModelFactory(
        name="gemini-2.0-flash", model_type="google", ai_model_api=AiModelApiFactory(api_key=get_chat_api_key("google"))
    )
    params = {
        "q": "test automation",
        "crontime": "0 0 * * *",
    }

    # Act
    response = client.post("/api/automation", params=params)

    # Assert
    assert response.status_code == 200
    response_json = response.json()
    assert response_json["scheduling_request"] == "test automation"
    assert response_json["crontime"] == "0 0 * * *"


@pytest.mark.django_db(transaction=True)
@pytest.mark.skipif(get_chat_api_key("google") is None, reason="Requires GEMINI_API_KEY to be set")
def test_get_automations(client: TestClient):
    """Test that getting a list of automations works."""
    automation_id = create_test_automation(client)

    # Act
    response = client.get("/api/automation")

    # Assert
    assert response.status_code == 200
    automations = response.json()
    assert isinstance(automations, list)
    assert len(automations) > 0
    assert any(a["id"] == automation_id for a in automations)


@pytest.mark.django_db(transaction=True)
@pytest.mark.skipif(get_chat_api_key("google") is None, reason="Requires GEMINI_API_KEY to be set")
def test_delete_automation(client: TestClient):
    """Test that deleting an automation works."""
    automation_id = create_test_automation(client)

    # Act
    response = client.delete(f"/api/automation?automation_id={automation_id}")

    # Assert
    assert response.status_code == 200

    # Verify it's gone
    response = client.get("/api/automation")
    assert response.status_code == 200
    automations = response.json()
    assert not any(a["id"] == automation_id for a in automations)


@pytest.mark.django_db(transaction=True)
@pytest.mark.skipif(get_chat_api_key("google") is None, reason="Requires GEMINI_API_KEY to be set")
def test_edit_automation(client: TestClient):
    """Test that editing an automation works."""
    automation_id = create_test_automation(client)

    edit_params = {
        "automation_id": automation_id,
        "q": "edited automation",
        "crontime": "0 1 * * *",
        "subject": "edited subject",
        "timezone": "UTC",
    }

    # Act
    response = client.put("/api/automation", params=edit_params)

    # Assert
    if response.status_code != 200:
        print(response.text)
    assert response.status_code == 200
    edited_automation = response.json()
    assert edited_automation["scheduling_request"] == "edited automation"
    assert edited_automation["crontime"] == "0 1 * * *"
    assert edited_automation["subject"] == "edited subject"


@pytest.mark.django_db(transaction=True)
@pytest.mark.skipif(get_chat_api_key("google") is None, reason="Requires GEMINI_API_KEY to be set")
def test_trigger_automation(client: TestClient):
    """Test that triggering an automation works."""
    automation_id = create_test_automation(client)

    # Act
    response = client.post(f"/api/automation/trigger?automation_id={automation_id}")

    # Assert
    assert response.status_code == 200
    # NOTE: We are not testing the execution of the triggered job itself,
    # as that would require a more complex test setup with mocking.
    # A 200 response is sufficient to indicate that the trigger was received.
    assert response.text == "Automation triggered"
