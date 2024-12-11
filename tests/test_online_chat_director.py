import os
import urllib.parse

import pytest
from freezegun import freeze_time

from khoj.database.models import Agent, Entry, KhojUser
from khoj.processor.conversation import prompts
from tests.helpers import ConversationFactory, generate_chat_history, get_chat_api_key

# Initialize variables for tests
api_key = get_chat_api_key()
if api_key is None:
    pytest.skip(
        reason="Set OPENAI_API_KEY environment variable to run tests below. Get OpenAI API key from https://platform.openai.com/account/api-keys",
        allow_module_level=True,
    )


# Helpers
# ----------------------------------------------------------------------------------------------------
def create_conversation(message_list, user, agent=None):
    # Generate conversation logs
    conversation_log = generate_chat_history(message_list)

    # Update Conversation Metadata Logs in Database
    return ConversationFactory(user=user, conversation_log=conversation_log, agent=agent)


# Tests
# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
@pytest.mark.django_db(transaction=True)
def test_chat_with_no_chat_history_or_retrieved_content(chat_client):
    # Act
    response = chat_client.post(f"/api/chat", json={"q": "Hello, my name is Testatron. Who are you?"})
    response_message = response.json()["response"]

    # Assert
    expected_responses = ["Khoj", "khoj"]
    assert response.status_code == 200
    assert any([expected_response in response_message for expected_response in expected_responses]), (
        "Expected assistants name, [K|k]hoj, in response but got: " + response_message
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
@pytest.mark.django_db(transaction=True)
def test_chat_with_online_content(chat_client):
    # Act
    q = "/online give me the link to paul graham's essay how to do great work"
    response = chat_client.post(f"/api/chat?", json={"q": q})
    response_message = response.json()["response"]

    # Assert
    expected_responses = [
        "paulgraham.com/greatwork.html",
        "paulgraham.com/hwh.html",
    ]
    assert response.status_code == 200
    assert any(
        [expected_response in response_message for expected_response in expected_responses]
    ), f"Expected links: {expected_responses}. Actual response: {response_message}"


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
@pytest.mark.django_db(transaction=True)
def test_chat_with_online_webpage_content(chat_client):
    # Act
    q = "/online how many firefighters were involved in the great chicago fire and which year did it take place?"
    response = chat_client.post(f"/api/chat", json={"q": q})
    response_message = response.json()["response"]

    # Assert
    expected_responses = ["185", "1871", "horse"]
    assert response.status_code == 200
    assert any(
        [expected_response in response_message for expected_response in expected_responses]
    ), f"Expected links: {expected_responses}. Actual response: {response_message}"


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
def test_answer_from_chat_history(chat_client, default_user2: KhojUser):
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 1st April 1984.", []),
    ]
    create_conversation(message_list, default_user2)

    # Act
    response = chat_client.post(f"/api/chat", json={"q": "What is my name?"})
    response_message = response.content.decode("utf-8")

    # Assert
    expected_responses = ["Testatron", "testatron"]
    assert response.status_code == 200
    assert any([expected_response in response_message for expected_response in expected_responses]), (
        "Expected [T|t]estatron in response but got: " + response_message
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
def test_answer_from_currently_retrieved_content(chat_client, default_user2: KhojUser):
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        (
            "When was I born?",
            "You were born on 1st April 1984.",
            ["Testatron was born on 1st April 1984 in Testville."],
        ),
    ]
    create_conversation(message_list, default_user2)

    # Act
    response = chat_client.post(f"/api/chat", json={"q": "Where was Xi Li born?"})
    response_message = response.json()["response"]

    # Assert
    assert response.status_code == 200
    assert "Fujiang" in response_message


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
def test_answer_from_chat_history_and_previously_retrieved_content(chat_client_no_background, default_user2: KhojUser):
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        (
            "When was I born?",
            "You were born on 1st April 1984.",
            ["Testatron was born on 1st April 1984 in Testville."],
        ),
    ]
    create_conversation(message_list, default_user2)

    # Act
    response = chat_client_no_background.post(f"/api/chat", json={"q": "Where was I born?"})
    response_message = response.json()["response"]

    # Assert
    assert response.status_code == 200
    # 1. Infer who I am from chat history
    # 2. Infer I was born in Testville from previously retrieved notes
    assert "Testville" in response_message


# ----------------------------------------------------------------------------------------------------
@pytest.mark.xfail(AssertionError, reason="Chat director not capable of answering this question yet")
@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
def test_answer_from_chat_history_and_currently_retrieved_content(chat_client, default_user2: KhojUser):
    # Arrange
    message_list = [
        ("Hello, my name is Xi Li. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 1st April 1984.", []),
    ]
    create_conversation(message_list, default_user2)

    # Act
    response = chat_client.post(f"/api/chat", json={"q": "Where was I born?"})
    response_message = response.json()["response"]

    # Assert
    assert response.status_code == 200
    # Inference in a multi-turn conversation
    # 1. Infer who I am from chat history
    # 2. Search for notes about when <my_name_from_chat_history> was born
    # 3. Extract where I was born from currently retrieved notes
    assert "Fujiang" in response_message


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
def test_no_answer_in_chat_history_or_retrieved_content(chat_client, default_user2: KhojUser):
    "Chat director should say don't know as not enough contexts in chat history or retrieved to answer question"
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 1st April 1984.", []),
    ]
    create_conversation(message_list, default_user2)

    # Act
    response = chat_client.post(f"/api/chat", json={"q": "Where was I born?"})
    response_message = response.json()["response"]

    # Assert
    expected_responses = [
        "don't know",
        "do not know",
        "no information",
        "do not have",
        "don't have",
        "where were you born?",
        "where you were born?",
    ]

    assert response.status_code == 200
    assert any([expected_response in response_message.lower() for expected_response in expected_responses]), (
        "Expected chat director to say they don't know in response, but got: " + response_message
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
def test_answer_using_general_command(chat_client, default_user2: KhojUser):
    # Arrange
    query = "/general Where was Xi Li born?"
    message_list = []
    create_conversation(message_list, default_user2)

    # Act
    response = chat_client.post(f"/api/chat", json={"q": query, "stream": True})
    response_message = response.content.decode("utf-8")

    # Assert
    assert response.status_code == 200
    assert "Fujiang" not in response_message


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
def test_answer_from_retrieved_content_using_notes_command(chat_client, default_user2: KhojUser):
    # Arrange
    query = "/notes Where was Xi Li born?"
    message_list = []
    create_conversation(message_list, default_user2)

    # Act
    response = chat_client.post(f"/api/chat", json={"q": query})
    response_message = response.json()["response"]

    # Assert
    assert response.status_code == 200
    assert "Fujiang" in response_message


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
def test_answer_not_known_using_notes_command(chat_client_no_background, default_user2: KhojUser):
    # Arrange
    query = "/notes Where was Testatron born?"
    message_list = []
    create_conversation(message_list, default_user2)

    # Act
    response = chat_client_no_background.post(f"/api/chat", json={"q": query})
    response_message = response.json()["response"]

    # Assert
    assert response.status_code == 200
    assert response_message == prompts.no_entries_found.format()


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
def test_summarize_one_file(chat_client, default_user2: KhojUser):
    message_list = []
    conversation = create_conversation(message_list, default_user2)
    # post "Xi Li.markdown" file to the file filters
    file_list = (
        Entry.objects.filter(user=default_user2, file_source="computer")
        .distinct("file_path")
        .values_list("file_path", flat=True)
    )
    # pick the file that has "Xi Li.markdown" in the name
    summarization_file = ""
    for file in file_list:
        if "Birthday Gift for Xiu turning 4.markdown" in file:
            summarization_file = file
            break
    assert summarization_file != ""

    response = chat_client.post(
        "api/chat/conversation/file-filters",
        json={"filename": summarization_file, "conversation_id": str(conversation.id)},
    )
    query = "/summarize"
    response = chat_client.post(f"/api/chat", json={"q": query, "conversation_id": str(conversation.id)})
    response_message = response.json()["response"]
    # Assert
    assert response_message != ""
    assert response_message != "No files selected for summarization. Please add files using the section on the left."


@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
def test_summarize_extra_text(chat_client, default_user2: KhojUser):
    message_list = []
    conversation = create_conversation(message_list, default_user2)
    # post "Xi Li.markdown" file to the file filters
    file_list = (
        Entry.objects.filter(user=default_user2, file_source="computer")
        .distinct("file_path")
        .values_list("file_path", flat=True)
    )
    # pick the file that has "Xi Li.markdown" in the name
    summarization_file = ""
    for file in file_list:
        if "Birthday Gift for Xiu turning 4.markdown" in file:
            summarization_file = file
            break
    assert summarization_file != ""

    response = chat_client.post(
        "api/chat/conversation/file-filters",
        json={"filename": summarization_file, "conversation_id": str(conversation.id)},
    )
    query = "/summarize tell me about Xiu"
    response = chat_client.post(f"/api/chat", json={"q": query, "conversation_id": str(conversation.id)})
    response_message = response.json()["response"]
    # Assert
    assert response_message != ""
    assert response_message != "No files selected for summarization. Please add files using the section on the left."


@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
def test_summarize_multiple_files(chat_client, default_user2: KhojUser):
    message_list = []
    conversation = create_conversation(message_list, default_user2)
    # post "Xi Li.markdown" file to the file filters
    file_list = (
        Entry.objects.filter(user=default_user2, file_source="computer")
        .distinct("file_path")
        .values_list("file_path", flat=True)
    )

    response = chat_client.post(
        "api/chat/conversation/file-filters", json={"filename": file_list[0], "conversation_id": str(conversation.id)}
    )
    response = chat_client.post(
        "api/chat/conversation/file-filters", json={"filename": file_list[1], "conversation_id": str(conversation.id)}
    )

    query = "/summarize"
    response = chat_client.post(f"/api/chat", json={"q": query, "conversation_id": str(conversation.id)})
    response_message = response.json()["response"]

    # Assert
    assert response_message is not None


@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
def test_summarize_no_files(chat_client, default_user2: KhojUser):
    # Arrange
    message_list = []
    conversation = create_conversation(message_list, default_user2)

    # Act
    query = "/summarize"
    response = chat_client.post(f"/api/chat", json={"q": query, "conversation_id": str(conversation.id)})
    response_message = response.json()["response"]

    # Assert
    assert response_message == "No files selected for summarization. Please add files using the section on the left."


@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
def test_summarize_different_conversation(chat_client, default_user2: KhojUser):
    # Arrange
    message_list = []
    conversation1 = create_conversation(message_list, default_user2)
    conversation2 = create_conversation(message_list, default_user2)

    file_list = (
        Entry.objects.filter(user=default_user2, file_source="computer")
        .distinct("file_path")
        .values_list("file_path", flat=True)
    )
    summarization_file = ""
    for file in file_list:
        if "Birthday Gift for Xiu turning 4.markdown" in file:
            summarization_file = file
            break
    assert summarization_file != ""

    # add file filter to conversation 1.
    response = chat_client.post(
        "api/chat/conversation/file-filters",
        json={"filename": summarization_file, "conversation_id": str(conversation1.id)},
    )

    # Act
    query = "/summarize"
    response = chat_client.post(f"/api/chat", json={"q": query, "conversation_id": str(conversation2.id)})
    response_message_conv2 = response.json()["response"]

    # now make sure that the file filter is still in conversation 1
    response = chat_client.post(f"/api/chat", json={"q": query, "conversation_id": str(conversation1.id)})
    response_message_conv1 = response.json()["response"]

    # Assert
    assert (
        response_message_conv2 == "No files selected for summarization. Please add files using the section on the left."
    )

    # now check the file filter is still in conversation 1
    assert response_message_conv1 != ""
    assert (
        response_message_conv1 != "No files selected for summarization. Please add files using the section on the left."
    )


@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
def test_summarize_nonexistant_file(chat_client, default_user2: KhojUser):
    message_list = []
    conversation = create_conversation(message_list, default_user2)
    # post "imaginary.markdown" file to the file filters
    response = chat_client.post(
        "api/chat/conversation/file-filters",
        json={"filename": "imaginary.markdown", "conversation_id": str(conversation.id)},
    )
    query = urllib.parse.quote("/summarize")
    response = chat_client.post(f"/api/chat", json={"q": query, "conversation_id": str(conversation.id)})
    response_message = response.json()["response"]
    # Assert
    assert response_message == "No files selected for summarization. Please add files using the section on the left."


@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
def test_summarize_diff_user_file(chat_client, default_user: KhojUser, pdf_configured_user1, default_user2: KhojUser):
    # Arrange
    message_list = []
    conversation = create_conversation(message_list, default_user2)
    # Get the pdf file called singlepage.pdf
    file_list = (
        Entry.objects.filter(user=default_user, file_source="computer")
        .distinct("file_path")
        .values_list("file_path", flat=True)
    )
    summarization_file = ""
    for file in file_list:
        if "singlepage.pdf" in file:
            summarization_file = file
            break
    assert summarization_file != ""
    # add singlepage.pdf to the file filters
    response = chat_client.post(
        "api/chat/conversation/file-filters",
        json={"filename": summarization_file, "conversation_id": str(conversation.id)},
    )

    # Act
    query = "/summarize"
    response = chat_client.post(f"/api/chat", json={"q": query, "conversation_id": str(conversation.id)})
    response_message = response.json()["response"]

    # Assert
    assert response_message == "No files selected for summarization. Please add files using the section on the left."


# ----------------------------------------------------------------------------------------------------
@pytest.mark.xfail(AssertionError, reason="Chat director not capable of answering time aware questions yet")
@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
@freeze_time("2023-04-01", ignore=["transformers"])
def test_answer_requires_current_date_awareness(chat_client):
    "Chat actor should be able to answer questions relative to current date using provided notes"
    # Act
    response = chat_client.post(f"/api/chat", json={"q": "Where did I have lunch today?", "stream": True})
    response_message = response.content.decode("utf-8")

    # Assert
    expected_responses = ["Arak", "Medellin"]
    assert response.status_code == 200
    assert any([expected_response in response_message for expected_response in expected_responses]), (
        "Expected chat director to say Arak, Medellin, but got: " + response_message
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
@freeze_time("2023-04-01", ignore=["transformers"])
def test_answer_requires_date_aware_aggregation_across_provided_notes(chat_client):
    "Chat director should be able to answer questions that require date aware aggregation across multiple notes"
    # Act
    query = "How much did I spend on dining this year?"
    response = chat_client.post(f"/api/chat", json={"q": query})
    response_message = response.json()["response"]

    # Assert
    assert response.status_code == 200
    assert "23" in response_message


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
def test_answer_general_question_not_in_chat_history_or_retrieved_content(chat_client, default_user2: KhojUser):
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 1st April 1984.", []),
        ("Where was I born?", "You were born Testville.", []),
    ]
    create_conversation(message_list, default_user2)

    # Act
    query = "Write a haiku about unit testing. Do not say anything else."
    response = chat_client.post(f"/api/chat", json={"q": query})
    response_message = response.json()["response"]

    # Assert
    expected_responses = ["test", "Test"]
    assert response.status_code == 200
    assert len(response_message.splitlines()) == 3  # haikus are 3 lines long
    assert any([expected_response in response_message for expected_response in expected_responses]), (
        "Expected [T|t]est in response, but got: " + response_message
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
def test_ask_for_clarification_if_not_enough_context_in_question(chat_client_no_background):
    # Act
    query = "What is the name of Namitas older son?"
    response = chat_client_no_background.post(f"/api/chat", json={"q": query})
    response_message = response.json()["response"].lower()

    # Assert
    expected_responses = [
        "which of them",
        "which one is",
        "which of namita's sons",
        "the birth order",
        "provide more context",
        "provide me with more context",
        "don't have that",
        "haven't provided me",
    ]
    assert response.status_code == 200
    assert any([expected_response in response_message for expected_response in expected_responses]), (
        "Expected chat director to ask for clarification in response, but got: " + response_message
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.xfail(reason="Chat director not capable of answering this question yet")
@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
def test_answer_in_chat_history_beyond_lookback_window(chat_client, default_user2: KhojUser):
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 1st April 1984.", []),
        ("Where was I born?", "You were born Testville.", []),
    ]
    create_conversation(message_list, default_user2)

    # Act
    query = "What is my name?"
    response = chat_client.post(f"/api/chat", json={"q": query})
    response_message = response.json()["response"]

    # Assert
    expected_responses = ["Testatron", "testatron"]
    assert response.status_code == 200
    assert any([expected_response in response_message.lower() for expected_response in expected_responses]), (
        "Expected [T|t]estatron in response, but got: " + response_message
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
@pytest.mark.django_db(transaction=True)
def test_answer_in_chat_history_by_conversation_id(chat_client, default_user2: KhojUser):
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 1st April 1984.", []),
        ("What's my favorite color", "Your favorite color is green.", []),
        ("Where was I born?", "You were born Testville.", []),
    ]
    message_list2 = [
        ("Hello, my name is Julia. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 14th August 1947.", []),
        ("What's my favorite color", "Your favorite color is maroon.", []),
        ("Where was I born?", "You were born in a potato farm.", []),
    ]
    conversation = create_conversation(message_list, default_user2)
    create_conversation(message_list2, default_user2)

    # Act
    query = "/general What is my favorite color?"
    response = chat_client.post(
        f"/api/chat", json={"q": query, "conversation_id": str(conversation.id), "stream": True}
    )
    response_message = response.content.decode("utf-8")

    # Assert
    expected_responses = ["green"]
    assert response.status_code == 200
    assert any([expected_response in response_message.lower() for expected_response in expected_responses]), (
        "Expected green in response, but got: " + response_message
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
@pytest.mark.django_db(transaction=True)
def test_answer_in_chat_history_by_conversation_id_with_agent(
    chat_client, default_user2: KhojUser, openai_agent: Agent
):
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 1st April 1984.", []),
        ("What's my favorite color", "Your favorite color is green.", []),
        ("Where was I born?", "You were born Testville.", []),
        (
            "What did I buy?",
            "You bought an apple for 2.00, an orange for 3.00, and a potato for 8.00 for breakfast",
            [],
        ),
    ]
    conversation = create_conversation(message_list, default_user2, openai_agent)

    # Act
    query = "/general What did I buy for breakfast?"
    response = chat_client.post(f"/api/chat", json={"q": query, "conversation_id": str(conversation.id)})
    response_message = response.json()["response"]

    # Assert that agent only responds with the summary of spending
    expected_responses = ["13.00", "13", "13.0", "thirteen"]
    assert response.status_code == 200
    assert any([expected_response in response_message.lower() for expected_response in expected_responses]), (
        "Expected amount in response, but got: " + response_message
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
def test_answer_requires_multiple_independent_searches(chat_client):
    "Chat director should be able to answer by doing multiple independent searches for required information"
    # Act
    query = "Is Xi Li older than Namita? Just say the older persons full name"
    response = chat_client.post(f"/api/chat", json={"q": query})
    response_message = response.json()["response"].lower()

    # Assert
    expected_responses = ["he is older than namita", "xi is older than namita", "xi li is older than namita"]
    only_full_name_check = "xi li" in response_message and "namita" not in response_message
    comparative_statement_check = any(
        [expected_response in response_message for expected_response in expected_responses]
    )

    assert response.status_code == 200
    assert only_full_name_check or comparative_statement_check, (
        "Expected Xi is older than Namita, but got: " + response_message
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_answer_using_file_filter(chat_client):
    "Chat should be able to use search filters in the query"
    # Act
    query = (
        'Is Xi Li older than Namita? Just say the older persons full name. file:"Namita.markdown" file:"Xi Li.markdown"'
    )
    response = chat_client.post(f"/api/chat", json={"q": query})
    response_message = response.json()["response"].lower()

    # Assert
    expected_responses = ["he is older than namita", "xi is older than namita", "xi li is older than namita"]
    only_full_name_check = "xi li" in response_message and "namita" not in response_message
    comparative_statement_check = any(
        [expected_response in response_message for expected_response in expected_responses]
    )

    assert response.status_code == 200
    assert only_full_name_check or comparative_statement_check, (
        "Expected Xi is older than Namita, but got: " + response_message
    )
