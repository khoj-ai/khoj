import pytest
from unittest.mock import MagicMock, patch

from khoj.database.models import KhojUser, NotionConfig
from khoj.processor.content.notion.notion_to_entries import NotionToEntries

@pytest.fixture
def mock_notion_responses():
    """Mock the responses from the Notion API."""
    return {
        "search": {
            "results": [{"object": "database", "id": "mock_db_1"}],
            "has_more": False
        },
        "query_db": {
            "results": [
                {
                    "object": "page",
                    "id": "mock_row_1",
                    "url": "https://notion.so/mock_row_1",
                    "properties": {
                        "Name": {"type": "title", "title": [{"plain_text": "Buy Groceries"}]},
                        "Status": {"type": "select", "select": {"name": "To Do"}},
                        "Due Date": {"type": "date", "date": {"start": "2024-01-01"}}
                    }
                }
            ],
            "has_more": False
        },
        "page_content": {
            "results": [] # Empty blocks for this test
        }
    }

@pytest.mark.django_db(transaction=True)
def test_process_notion_database(mock_notion_responses, default_user: KhojUser):
    # 1. Setup mock config
    config = NotionConfig(token="mock_token")
    processor = NotionToEntries(config)

    # 2. Mock the requests.Session methods
    with patch("khoj.processor.content.notion.notion_to_entries.requests.Session.post") as mock_post, \
         patch("khoj.processor.content.notion.notion_to_entries.requests.Session.get") as mock_get:
        
        # Configure the mock POST to return search or DB query results based on URL
        def side_effect_post(url, **kwargs):
            mock_resp = MagicMock()
            if "search" in url:
                mock_resp.json.return_value = mock_notion_responses["search"]
            elif "query" in url:
                mock_resp.json.return_value = mock_notion_responses["query_db"]
            return mock_resp
            
        # Configure the mock GET to return page blocks
        def side_effect_get(url, **kwargs):
            mock_resp = MagicMock()
            if "children" in url:
                mock_resp.json.return_value = mock_notion_responses["page_content"]
            else:
                # Page metadata request
                mock_resp.json.return_value = mock_notion_responses["query_db"]["results"][0]
            return mock_resp

        mock_post.side_effect = side_effect_post
        mock_get.side_effect = side_effect_get

        # 3. Act: run the processor
        # Note: We need to mock update_entries_with_ids since we are only testing the extraction part here, 
        # or rely on the actual method if the embeddings mock is already set up in the conftest. 
        # Let's mock the update_entries_with_ids to avoid triggering embedding models in a simple test.
        with patch.object(processor, 'update_entries_with_ids') as mock_update:
            mock_update.return_value = (1, 0)
            added, deleted = processor.process(files={}, user=default_user)

            # 4. Assert
            assert added > 0
            assert mock_post.call_count >= 2 # One for search, one for DB query
