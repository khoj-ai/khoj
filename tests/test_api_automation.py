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


def _setup_chat_model():
    state.anonymous_mode = True
    ChatModelFactory(
        name="gemini-2.5-flash",
        model_type="google",
        ai_model_api=AiModelApiFactory(api_key=get_chat_api_key("google")),
    )


def create_test_automation(client: TestClient, q="test automation", crontime="0 0 * * *") -> str:
    _setup_chat_model()
    response = client.post("/api/automation", params={"q": q, "crontime": crontime})
    assert response.status_code == 200
    return response.json()["id"]


@pytest.mark.django_db(transaction=True)
def test_create_automation(client: TestClient):
    _setup_chat_model()
    response = client.post("/api/automation", params={"q": "test automation", "crontime": "0 0 * * *"})
    assert response.status_code == 200
    data = response.json()
    assert data["scheduling_request"] == "test automation"
    assert data["crontime"] == "0 0 * * *"


@pytest.mark.django_db(transaction=True)
@pytest.mark.skipif(get_chat_api_key("google") is None, reason="Requires GEMINI_API_KEY to be set")
def test_get_automations(client: TestClient):
    automation_id = create_test_automation(client)
    response = client.get("/api/automation")
    assert response.status_code == 200
    automations = response.json()
    assert any(a["id"] == automation_id for a in automations)


@pytest.mark.django_db(transaction=True)
@pytest.mark.skipif(get_chat_api_key("google") is None, reason="Requires GEMINI_API_KEY to be set")
def test_delete_automation(client: TestClient):
    automation_id = create_test_automation(client)
    assert client.delete(f"/api/automation?automation_id={automation_id}").status_code == 200
    automations = client.get("/api/automation").json()
    assert not any(a["id"] == automation_id for a in automations)


@pytest.mark.django_db(transaction=True)
@pytest.mark.skipif(get_chat_api_key("google") is None, reason="Requires GEMINI_API_KEY to be set")
def test_edit_automation(client: TestClient):
    automation_id = create_test_automation(client)
    params = {
        "automation_id": automation_id,
        "q": "edited automation",
        "crontime": "0 1 * * *",
        "subject": "edited subject",
        "timezone": "UTC",
    }
    response = client.put("/api/automation", params=params)
    assert response.status_code == 200
    data = response.json()
    assert data["scheduling_request"] == "edited automation"
    assert data["crontime"] == "0 1 * * *"
    assert data["subject"] == "edited subject"


@pytest.mark.django_db(transaction=True)
@pytest.mark.skipif(get_chat_api_key("google") is None, reason="Requires GEMINI_API_KEY to be set")
def test_trigger_automation(client: TestClient):
    automation_id = create_test_automation(client)
    response = client.post(f"/api/automation/trigger?automation_id={automation_id}")
    assert response.status_code == 200
    assert response.text == "Automation triggered"
