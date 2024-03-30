import os
import urllib.parse
from urllib.parse import quote

import pytest
from freezegun import freeze_time

from khoj.database.models import Agent, KhojUser
from khoj.processor.conversation import prompts
from khoj.processor.conversation.utils import message_to_log
from khoj.routers.helpers import aget_relevant_information_sources
from tests.helpers import ConversationFactory

# Initialize variables for tests
api_key = os.getenv("OPENAI_API_KEY")
if api_key is None:
    pytest.skip(
        reason="Set OPENAI_API_KEY environment variable to run tests below. Get OpenAI API key from https://platform.openai.com/account/api-keys",
        allow_module_level=True,
    )


# Helpers
# ----------------------------------------------------------------------------------------------------
def generate_history(message_list):
    # Generate conversation logs
    conversation_log = {"chat": []}
    for user_message, gpt_message, context in message_list:
        message_to_log(
            user_message,
            gpt_message,
            {"context": context, "intent": {"query": user_message, "inferred-queries": f'["{user_message}"]'}},
            conversation_log=conversation_log.get("chat", []),
        )
    return conversation_log


def create_conversation(message_list, user, agent=None):
    # Generate conversation logs
    conversation_log = generate_history(message_list)

    # Update Conversation Metadata Logs in Database
    return ConversationFactory(user=user, conversation_log=conversation_log, agent=agent)


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
@pytest.mark.skipif(os.getenv("SERPER_DEV_API_KEY") is None, reason="requires SERPER_DEV_API_KEY")
@pytest.mark.chatquality
@pytest.mark.django_db(transaction=True)
def test_chat_with_online_content(chat_client):
    # Act
    q = "/online give me the link to paul graham's essay how to do great work"
    encoded_q = quote(q, safe="")
    response = chat_client.get(f"/api/chat?q={encoded_q}&stream=true")
    response_message = response.content.decode("utf-8")

    response_message = response_message.split("### compiled references")[0]

    # Assert
    expected_responses = ["http://www.paulgraham.com/greatwork.html"]
    assert response.status_code == 200
    assert any([expected_response in response_message for expected_response in expected_responses]), (
        "Expected links or serper not setup in response but got: " + response_message
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.skipif(
    os.getenv("SERPER_DEV_API_KEY") is None or os.getenv("OLOSTEP_API_KEY") is None,
    reason="requires SERPER_DEV_API_KEY and OLOSTEP_API_KEY",
)
@pytest.mark.chatquality
@pytest.mark.django_db(transaction=True)
def test_chat_with_online_webpage_content(chat_client):
    # Act
    q = "/online how many firefighters were involved in the great chicago fire and which year did it take place?"
    encoded_q = quote(q, safe="")
    response = chat_client.get(f"/api/chat?q={encoded_q}&stream=true")
    response_message = response.content.decode("utf-8")

    response_message = response_message.split("### compiled references")[0]

    # Assert
    expected_responses = ["185", "1871", "horse"]
    assert response.status_code == 200
    assert any([expected_response in response_message for expected_response in expected_responses]), (
        "Expected links or serper not setup in response but got: " + response_message
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
    create_conversation(message_list, default_user2)

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
    create_conversation(message_list, default_user2)

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
    create_conversation(message_list, default_user2)

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
    create_conversation(message_list, default_user2)

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
    create_conversation(message_list, default_user2)

    # Act
    response = chat_client.get(f'/api/chat?q="Where was I born?"&stream=true')
    response_message = response.content.decode("utf-8")

    # Assert
    expected_responses = [
        "don't know",
        "do not know",
        "no information",
        "do not have",
        "don't have",
        "where were you born?",
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
    query = urllib.parse.quote("/general Where was Xi Li born?")
    message_list = []
    create_conversation(message_list, default_user2)

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
    create_conversation(message_list, default_user2)

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
    create_conversation(message_list, default_user2)

    # Act
    response = chat_client_no_background.get(f"/api/chat?q={query}&stream=true")
    response_message = response.content.decode("utf-8")

    # Assert
    assert response.status_code == 200
    assert response_message == prompts.no_entries_found.format()


# ----------------------------------------------------------------------------------------------------
@pytest.mark.xfail(AssertionError, reason="Chat director not capable of answering time aware questions yet")
@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
@freeze_time("2023-04-01", ignore=["transformers"])
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
@freeze_time("2023-04-01", ignore=["transformers"])
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
    create_conversation(message_list, default_user2)

    # Act
    response = chat_client.get(f'/api/chat?q="Write a haiku about unit testing. Do not say anything else."&stream=true')
    response_message = response.content.decode("utf-8").split("### compiled references")[0]

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

    response = chat_client_no_background.get(f'/api/chat?q="What is the name of Namitas older son?"&stream=true')
    response_message = response.content.decode("utf-8").split("### compiled references")[0].lower()

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
    response = chat_client.get(f'/api/chat?q="What is my name?"&stream=true')
    response_message = response.content.decode("utf-8")

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
    query = urllib.parse.quote("/general What is my favorite color?")
    response = chat_client.get(f"/api/chat?q={query}&conversation_id={conversation.id}&stream=true")
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
        ("What did I buy?", "You bought an apple for 2.00, an orange for 3.00, and a potato for 8.00", []),
    ]
    conversation = create_conversation(message_list, default_user2, openai_agent)

    # Act
    query = urllib.parse.quote("/general What did I eat for breakfast?")
    response = chat_client.get(f"/api/chat?q={query}&conversation_id={conversation.id}&stream=true")
    response_message = response.content.decode("utf-8")

    # Assert that agent only responds with the summary of spending
    expected_responses = ["13.00", "13", "13.0", "thirteen"]
    assert response.status_code == 200
    assert any([expected_response in response_message.lower() for expected_response in expected_responses]), (
        "Expected green in response, but got: " + response_message
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
@pytest.mark.chatquality
def test_answer_requires_multiple_independent_searches(chat_client):
    "Chat director should be able to answer by doing multiple independent searches for required information"
    # Act
    response = chat_client.get(f'/api/chat?q="Is Xi older than Namita? Just the older persons full name"&stream=true')
    response_message = response.content.decode("utf-8").split("### compiled references")[0].lower()

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
    query = urllib.parse.quote(
        'Is Xi older than Namita? Just say the older persons full name. file:"Namita.markdown" file:"Xi Li.markdown"'
    )

    response = chat_client.get(f"/api/chat?q={query}&stream=true")
    response_message = response.content.decode("utf-8").split("### compiled references")[0].lower()

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
@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_get_correct_tools_online(chat_client):
    # Arrange
    user_query = "What's the weather in Patagonia this week?"

    # Act
    tools = await aget_relevant_information_sources(user_query, {})

    # Assert
    tools = [tool.value for tool in tools]
    assert tools == ["online"]


# ----------------------------------------------------------------------------------------------------
@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_get_correct_tools_notes(chat_client):
    # Arrange
    user_query = "Where did I go for my first battleship training?"

    # Act
    tools = await aget_relevant_information_sources(user_query, {})

    # Assert
    tools = [tool.value for tool in tools]
    assert tools == ["notes"]


# ----------------------------------------------------------------------------------------------------
@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_get_correct_tools_online_or_general_and_notes(chat_client):
    # Arrange
    user_query = "What's the highest point in Patagonia and have I been there?"

    # Act
    tools = await aget_relevant_information_sources(user_query, {})

    # Assert
    tools = [tool.value for tool in tools]
    assert len(tools) == 2
    assert "online" in tools or "general" in tools
    assert "notes" in tools


# ----------------------------------------------------------------------------------------------------
@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_get_correct_tools_general(chat_client):
    # Arrange
    user_query = "How many noble gases are there?"

    # Act
    tools = await aget_relevant_information_sources(user_query, {})

    # Assert
    tools = [tool.value for tool in tools]
    assert tools == ["general"]


# ----------------------------------------------------------------------------------------------------
@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_get_correct_tools_with_chat_history(chat_client):
    # Arrange
    user_query = "What's the latest in the Israel/Palestine conflict?"
    chat_log = [
        (
            "Let's talk about the current events around the world.",
            "Sure, let's discuss the current events. What would you like to know?",
            [],
        ),
        ("What's up in New York City?", "A Pride parade has recently been held in New York City, on July 31st.", []),
    ]
    chat_history = generate_history(chat_log)

    # Act
    tools = await aget_relevant_information_sources(user_query, chat_history)

    # Assert
    tools = [tool.value for tool in tools]
    assert tools == ["online"]
