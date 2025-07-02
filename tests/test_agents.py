# tests/test_agents.py
import asyncio
from collections import Counter

import pytest
from asgiref.sync import sync_to_async

from khoj.database.adapters import AgentAdapters
from khoj.database.models import Agent, ChatModel, Entry, FileObject, KhojUser
from khoj.routers.helpers import execute_search
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


@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_large_knowledge_base_atomic_update(
    default_user2: KhojUser, default_openai_chat_model_option: ChatModel, chat_client_with_large_kb
):
    """
    The test simulates the scenario where lots of files are synced to an agent's knowledge base,
    and verifies that all files are properly added atomically.
    """
    # The chat_client_with_large_kb fixture has already created and indexed 200 files
    # Get the files that are already in the user's knowledge base
    user_file_objects = await sync_to_async(list)(FileObject.objects.filter(user=default_user2, agent=None))

    # Verify we have the expected large knowledge base from the fixture
    assert len(user_file_objects) >= 150, f"Expected at least 150 files from fixture, got {len(user_file_objects)}"

    # Extract file paths for agent creation
    available_files = [fo.file_name for fo in user_file_objects]
    files_to_test = available_files  # Use all available files for the stress test

    # Create initial agent with smaller set
    initial_files = files_to_test[:20]
    agent = await AgentAdapters.aupdate_agent(
        default_user2,
        "Large KB Agent",
        "Test agent with large knowledge base",
        Agent.PrivacyLevel.PRIVATE,
        "icon",
        "color",
        default_openai_chat_model_option.name,
        initial_files,
        [],
        [],
    )

    # Verify initial state
    initial_entries = await sync_to_async(list)(Entry.objects.filter(agent=agent))
    initial_entries_count = len(initial_entries)

    # Now perform the stress test: update with ALL 180 files at once
    # This is where partial sync issues would occur without atomic transactions
    updated_agent = await AgentAdapters.aupdate_agent(
        default_user2,
        "Large KB Agent Updated",  # Change name to trigger update
        "Test agent with large knowledge base - updated",
        Agent.PrivacyLevel.PRIVATE,
        "icon",
        "color",
        default_openai_chat_model_option.name,
        files_to_test,  # ALL files at once
        [],
        [],
    )

    # Verify atomic update completed successfully
    final_entries = await sync_to_async(list)(Entry.objects.filter(agent=updated_agent))
    final_file_objects = await sync_to_async(list)(FileObject.objects.filter(agent=updated_agent))

    # Critical assertions that would fail with partial sync issues
    expected_file_count = len(files_to_test)
    actual_file_count = len(final_file_objects)

    assert actual_file_count == expected_file_count, (
        f"Partial sync detected! Expected {expected_file_count} files, "
        f"but got {actual_file_count}. This indicates non-atomic update."
    )

    # Verify all files are properly represented
    file_paths_in_db = {fo.file_name for fo in final_file_objects}
    expected_file_paths = set(files_to_test)

    missing_files = expected_file_paths - file_paths_in_db
    assert not missing_files, f"Missing files in knowledge base: {missing_files}"

    # Verify entries were created (should have significantly more than initial)
    assert len(final_entries) > initial_entries_count, "Should have more entries after update"

    # With 180 files, we should have many entries (each file creates multiple entries)
    assert (
        len(final_entries) >= expected_file_count
    ), f"Expected at least {expected_file_count} entries, got {len(final_entries)}"

    # Verify no partial state - all entries should correspond to the final file set
    entry_file_paths = {entry.file_path for entry in final_entries}

    # All file objects should have corresponding entries
    assert file_paths_in_db.issubset(
        entry_file_paths
    ), "All file objects should have corresponding entries - atomic update verification"

    # Additional stress test: verify referential integrity
    # Count entries per file to ensure no partial file processing
    entries_per_file = Counter(entry.file_path for entry in final_entries)

    # Ensure every file has at least one entry (no files were partially processed and lost)
    files_without_entries = set(files_to_test) - set(entries_per_file.keys())
    assert not files_without_entries, f"Files with no entries (partial sync): {files_without_entries}"

    # Test that search works with the updated knowledge base
    search_result = await execute_search(user=default_user2, q="test", agent=updated_agent)
    assert len(search_result) > 0, "Should be able to search the updated knowledge base"


@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_concurrent_agent_updates_atomicity(
    default_user2: KhojUser, default_openai_chat_model_option: ChatModel, chat_client_with_large_kb
):
    """
    Test that concurrent updates to the same agent don't result in partial syncs.
    This simulates the race condition that could occur with non-atomic updates.
    """
    # The chat_client_with_large_kb fixture has already created and indexed 200 files
    # Get the files that are already in the user's knowledge base
    user_file_objects = await sync_to_async(list)(FileObject.objects.filter(user=default_user2, agent=None))

    # Extract file paths for agent creation
    available_files = [fo.file_name for fo in user_file_objects]
    test_files = available_files  # Use all available files for the stress test

    # Create initial agent
    agent = await AgentAdapters.aupdate_agent(
        default_user2,
        "Concurrent Test Agent",
        "Test concurrent updates",
        Agent.PrivacyLevel.PRIVATE,
        "icon",
        "color",
        default_openai_chat_model_option.name,
        test_files[:10],
        [],
        [],
    )

    # Create two concurrent update tasks with different file sets
    async def update_agent_with_files(file_subset, name_suffix, delay=0):
        if delay > 0:
            await asyncio.sleep(delay)
        return await AgentAdapters.aupdate_agent(
            default_user2,
            f"Concurrent Test Agent {name_suffix}",
            f"Test concurrent updates {name_suffix}",
            Agent.PrivacyLevel.PRIVATE,
            "icon",
            "color",
            default_openai_chat_model_option.name,
            file_subset,
            [],
            [],
        )

    # Run concurrent updates
    initial_split_size = 20
    large_split_size = 80
    try:
        results = await asyncio.gather(
            update_agent_with_files(test_files[initial_split_size : initial_split_size + large_split_size], "Second"),
            update_agent_with_files(test_files[:initial_split_size], "First"),
            return_exceptions=True,
        )

        # At least one should succeed with atomic updates
        successful_updates = [r for r in results if not isinstance(r, Exception)]
        assert len(successful_updates) >= 1, "At least one concurrent update should succeed"

        # Verify the final state is consistent
        final_agent = successful_updates[0]
        final_file_objects = await sync_to_async(list)(FileObject.objects.filter(agent=final_agent))
        final_entries = await sync_to_async(list)(Entry.objects.filter(agent=final_agent))

        # The agent should have a consistent state (all files from the successful update)
        assert len(final_file_objects) == large_split_size, "Should have file objects after concurrent update"
        assert len(final_entries) >= large_split_size, "Should have entries after concurrent update"

        # Verify referential integrity
        entry_file_paths = {entry.file_path for entry in final_entries}
        file_object_paths = {fo.file_name for fo in final_file_objects}

        # All entries should have corresponding file objects
        assert entry_file_paths.issubset(
            file_object_paths
        ), "All entries should have corresponding file objects - indicates atomic update worked"

    except Exception as e:
        # If we get database integrity errors, that's actually expected behavior
        # with proper atomic transactions - they should fail cleanly rather than
        # allowing partial updates
        assert (
            "database" in str(e).lower() or "integrity" in str(e).lower()
        ), f"Expected database/integrity error with concurrent updates, got: {e}"
