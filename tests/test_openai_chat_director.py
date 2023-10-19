# Standard Packages
import os
import urllib.parse

# External Packages
import pytest
from freezegun import freeze_time
from khoj.processor.conversation import prompts

# Internal Packages
from khoj.processor.conversation.utils import message_to_log
from tests.helpers import ConversationFactory
from database.models import KhojUser

# Initialize variables for tests
api_key = os.getenv("OPENAI_API_KEY")
if api_key is None:
    pytest.skip(
        reason="Set OPENAI_API_KEY environment variable to run tests below. Get OpenAI API key from https://platform.openai.com/account/api-keys",
        allow_module_level=True,
    )


# Helpers
# ----------------------------------------------------------------------------------------------------
def populate_chat_history(message_list, user=None):
    # Generate conversation logs
    conversation_log = {"chat": []}
    for user_message, gpt_message, context in message_list:
        conversation_log["chat"] += message_to_log(
            user_message,
            gpt_message,
            {"context": context, "intent": {"query": user_message, "inferred-queries": f'["{user_message}"]'}},
        )

    # Update Conversation Metadata Logs in Database
    ConversationFactory(user=user, conversation_log=conversation_log)


# Tests
# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
@pytest.mark.django_db(transaction=True)
def test_chat_with_no_chat_history_or_retrieved_content(chat_client):
    # Act
    response = chat_client.get(f'/api/chat?q="Hello, my name is Testatron. Who are you?"&stream=true')
    response_message = response.content.decode("utf-8")

    # Assert
    expected_responses = ["Khoj", "khoj"]
    assert response.status_code == 200
    assert any([expected_response in response_message for expected_response in expected_responses]), (
        "Expected assistants name, [K|k]hoj, in response but got: " + response_message
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
def test_answer_from_chat_history(chat_client, default_user2: KhojUser):
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 1st April 1984.", []),
    ]
    populate_chat_history(message_list, default_user2)

    # Act
    response = chat_client.get(f'/api/chat?q="What is my name?"&stream=true')
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
    populate_chat_history(message_list, default_user2)

    # Act
    response = chat_client.get(f'/api/chat?q="Where was Xi Li born?"')
    response_message = response.content.decode("utf-8")

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
    populate_chat_history(message_list, default_user2)

    # Act
    response = chat_client_no_background.get(f'/api/chat?q="Where was I born?"')
    response_message = response.content.decode("utf-8")

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
    populate_chat_history(message_list, default_user2)

    # Act
    response = chat_client.get(f'/api/chat?q="Where was I born?"')
    response_message = response.content.decode("utf-8")

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
    populate_chat_history(message_list, default_user2)

    # Act
    response = chat_client.get(f'/api/chat?q="Where was I born?"&stream=true')
    response_message = response.content.decode("utf-8")

    # Assert
    expected_responses = ["don't know", "do not know", "no information", "do not have", "don't have"]
    assert response.status_code == 200
    assert any([expected_response in response_message for expected_response in expected_responses]), (
        "Expected chat director to say they don't know in response, but got: " + response_message
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
def test_answer_using_general_command(chat_client, default_user2: KhojUser):
    # Arrange
    query = urllib.parse.quote("/general Where was Xi Li born?")
    message_list = []
    populate_chat_history(message_list, default_user2)

    # Act
    response = chat_client.get(f"/api/chat?q={query}&stream=true")
    response_message = response.content.decode("utf-8")

    # Assert
    assert response.status_code == 200
    assert "Fujiang" not in response_message


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
def test_answer_from_retrieved_content_using_notes_command(chat_client, default_user2: KhojUser):
    # Arrange
    query = urllib.parse.quote("/notes Where was Xi Li born?")
    message_list = []
    populate_chat_history(message_list, default_user2)

    # Act
    response = chat_client.get(f"/api/chat?q={query}&stream=true")
    response_message = response.content.decode("utf-8")

    # Assert
    assert response.status_code == 200
    assert "Fujiang" in response_message


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
def test_answer_not_known_using_notes_command(chat_client_no_background, default_user2: KhojUser):
    # Arrange
    query = urllib.parse.quote("/notes Where was Testatron born?")
    message_list = []
    populate_chat_history(message_list, default_user2)

    # Act
    response = chat_client_no_background.get(f"/api/chat?q={query}&stream=true")
    response_message = response.content.decode("utf-8")

    # Assert
    assert response.status_code == 200
    assert response_message == prompts.no_notes_found.format()


# ----------------------------------------------------------------------------------------------------
@pytest.mark.xfail(AssertionError, reason="Chat director not capable of answering time aware questions yet")
@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
@freeze_time("2023-04-01")
def test_answer_requires_current_date_awareness(chat_client):
    "Chat actor should be able to answer questions relative to current date using provided notes"
    # Act
    response = chat_client.get(f'/api/chat?q="Where did I have lunch today?"&stream=true')
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
@freeze_time("2023-04-01")
def test_answer_requires_date_aware_aggregation_across_provided_notes(chat_client):
    "Chat director should be able to answer questions that require date aware aggregation across multiple notes"
    # Act

    response = chat_client.get(f'/api/chat?q="How much did I spend on dining this year?"&stream=true')
    response_message = response.content.decode("utf-8")

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
    populate_chat_history(message_list, default_user2)

    # Act
    response = chat_client.get(
        f'/api/chat?q=""Write a haiku about unit testing. Do not say anything else."&stream=true'
    )
    response_message = response.content.decode("utf-8")

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

    response = chat_client_no_background.get(f'/api/chat?q="What is the name of Namitas older son"&stream=true')
    response_message = response.content.decode("utf-8")

    # Assert
    expected_responses = [
        "which of them",
        "which one is",
        "which of namita's sons",
        "the birth order",
    ]
    assert response.status_code == 200
    assert any([expected_response in response_message.lower() for expected_response in expected_responses]), (
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
    populate_chat_history(message_list, default_user2)

    # Act
    response = chat_client.get(f'/api/chat?q="What is my name?"&stream=true')
    response_message = response.content.decode("utf-8")

    # Assert
    expected_responses = ["Testatron", "testatron"]
    assert response.status_code == 200
    assert any([expected_response in response_message.lower() for expected_response in expected_responses]), (
        "Expected [T|t]estatron in response, but got: " + response_message
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
def test_answer_requires_multiple_independent_searches(chat_client):
    "Chat director should be able to answer by doing multiple independent searches for required information"
    # Act
    response = chat_client.get(f'/api/chat?q="Is Xi older than Namita?"&stream=true')
    response_message = response.content.decode("utf-8")

    # Assert
    expected_responses = ["he is older than namita", "xi is older than namita", "xi li is older than namita"]
    assert response.status_code == 200
    assert any([expected_response in response_message.lower() for expected_response in expected_responses]), (
        "Expected Xi is older than Namita, but got: " + response_message
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_answer_using_file_filter(chat_client):
    "Chat should be able to use search filters in the query"
    # Act
    query = urllib.parse.quote('Is Xi older than Namita? file:"Namita.markdown" file:"Xi Li.markdown"')

    response = chat_client.get(f"/api/chat?q={query}&stream=true")
    response_message = response.content.decode("utf-8")

    # Assert
    expected_responses = ["he is older than namita", "xi is older than namita", "xi li is older than namita"]
    assert response.status_code == 200
    assert any([expected_response in response_message.lower() for expected_response in expected_responses]), (
        "Expected Xi is older than Namita, but got: " + response_message
    )
