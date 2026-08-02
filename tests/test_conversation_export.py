import json

import pytest

from khoj.database.adapters import ConversationAdapters
from khoj.database.models import Conversation

EXPORT_BATCH_SIZE = 10


def export_all_conversations(user, limit=EXPORT_BATCH_SIZE):
    """Walk the export adapter the way the web client's export loop does.

    Pages until a page shorter than the requested limit comes back, bounded by a max
    offset so a regression that keeps returning full pages fails instead of hanging.
    """
    exported = []
    max_offset = Conversation.objects.filter(user=user).count() + 2 * limit
    offset = 0
    while offset <= max_offset:
        page = ConversationAdapters.get_all_conversations_for_export(user, offset=offset, limit=limit)
        exported.extend(page)
        if len(page) < limit:
            break
        offset += limit
    return exported


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_export_conversations_across_pages(default_user):
    # Arrange
    for index in range(25):
        Conversation.objects.create(user=default_user, title=f"conv-{index:02d}")

    # Act
    titles = [conversation["title"] for conversation in export_all_conversations(default_user)]

    # Assert
    assert len(titles) == 25, f"Expected 25 conversations, exported {len(titles)}"
    assert len(set(titles)) == 25, "Export contains duplicate conversations"
    assert set(titles) == {f"conv-{index:02d}" for index in range(25)}


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_export_order_unaffected_by_conversation_update(default_user):
    """Conversations written to mid-export must not shift rows across page boundaries."""
    # Arrange
    for index in range(25):
        Conversation.objects.create(user=default_user, title=f"conv-{index:02d}")
    before = [conversation["title"] for conversation in export_all_conversations(default_user)]

    # Act: touch a conversation to bump its auto_now updated_at, as a concurrent write would
    stale_conversation = Conversation.objects.filter(user=default_user, title="conv-00").first()
    stale_conversation.save()
    after = [conversation["title"] for conversation in export_all_conversations(default_user)]

    # Assert
    assert before == after, "Export order shifted after a conversation was updated"
    assert len(set(after)) == 25, "Export contains duplicate conversations after an update"


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_export_endpoint_paginates_with_offset_and_limit(client, default_user):
    # Arrange
    headers = {"Authorization": "Bearer kk-secret"}
    for index in range(15):
        Conversation.objects.create(user=default_user, title=f"conv-{index:02d}")

    # Act
    first = client.get("/api/chat/export?offset=0&limit=10", headers=headers)
    second = client.get("/api/chat/export?offset=10&limit=10", headers=headers)

    # Assert
    assert first.status_code == 200 and second.status_code == 200
    first_titles = [conversation["title"] for conversation in json.loads(first.content)]
    second_titles = [conversation["title"] for conversation in json.loads(second.content)]
    assert len(first_titles) == 10 and len(second_titles) == 5
    assert not set(first_titles) & set(second_titles), "Export endpoint returned overlapping pages"


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_export_endpoint_rejects_out_of_range_pagination(client):
    # Arrange
    headers = {"Authorization": "Bearer kk-secret"}

    # Act, Assert: negative offsets and limits would raise on the queryset slice, huge limits
    # would load every conversation into memory at once. Reject them at the API boundary.
    for query in ["offset=-1", "limit=0", "limit=-5", "limit=101", "limit=1000000"]:
        response = client.get(f"/api/chat/export?{query}", headers=headers)
        assert response.status_code == 422, f"Expected 422 for {query}, got {response.status_code}"

    # Act, Assert: the accepted bounds still work
    for query in ["", "offset=0&limit=1", "offset=0&limit=100"]:
        response = client.get(f"/api/chat/export?{query}", headers=headers)
        assert response.status_code == 200, f"Expected 200 for {query}, got {response.status_code}"
