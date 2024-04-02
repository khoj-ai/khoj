import os
import urllib.parse
from urllib.parse import quote

import pytest
from faker import Faker
from freezegun import freeze_time

from khoj.database.models import Agent, KhojUser
from khoj.processor.conversation import prompts
from khoj.processor.conversation.utils import message_to_log
from khoj.routers.helpers import aget_relevant_information_sources
from tests.helpers import ConversationFactory

SKIP_TESTS = True
pytestmark = pytest.mark.skipif(
    SKIP_TESTS,
    reason="Disable in CI to avoid long test runs.",
)

fake = Faker()


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
@pytest.mark.xfail(AssertionError, reason="Chat director not capable of answering this question yet")
@pytest.mark.chatquality
@pytest.mark.django_db(transaction=True)
def test_offline_chat_with_no_chat_history_or_retrieved_content(client_offline_chat):
    # Act
    response = client_offline_chat.get(f'/api/chat?q="Hello, my name is Testatron. Who are you?"&stream=true')
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
def test_chat_with_online_content(client_offline_chat):
    # Act
    q = "/online give me the link to paul graham's essay how to do great work"
    encoded_q = quote(q, safe="")
    response = client_offline_chat.get(f"/api/chat?q={encoded_q}&stream=true")
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
def test_chat_with_online_webpage_content(client_offline_chat):
    # Act
    q = "/online how many firefighters were involved in the great chicago fire and which year did it take place?"
    encoded_q = quote(q, safe="")
    response = client_offline_chat.get(f"/api/chat?q={encoded_q}&stream=true")
    response_message = response.content.decode("utf-8")

    response_message = response_message.split("### compiled references")[0]

    # Assert
    expected_responses = ["185", "1871", "horse"]
    assert response.status_code == 200
    assert any([expected_response in response_message for expected_response in expected_responses]), (
        "Expected links or serper not setup in response but got: " + response_message
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
@pytest.mark.django_db(transaction=True)
def test_answer_from_chat_history(client_offline_chat, default_user2):
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 1st April 1984.", []),
    ]
    create_conversation(message_list, default_user2)

    # Act
    response = client_offline_chat.get(f'/api/chat?q="What is my name?"&stream=true')
    response_message = response.content.decode("utf-8")

    # Assert
    expected_responses = ["Testatron", "testatron"]
    assert response.status_code == 200
    assert any([expected_response in response_message for expected_response in expected_responses]), (
        "Expected [T|t]estatron in response but got: " + response_message
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
@pytest.mark.django_db(transaction=True)
def test_answer_from_currently_retrieved_content(client_offline_chat, default_user2):
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
    response = client_offline_chat.get(f'/api/chat?q="Where was Xi Li born?"')
    response_message = response.content.decode("utf-8")

    # Assert
    assert response.status_code == 200
    assert "Fujiang" in response_message


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
@pytest.mark.django_db(transaction=True)
def test_answer_from_chat_history_and_previously_retrieved_content(client_offline_chat, default_user2):
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
    response = client_offline_chat.get(f'/api/chat?q="Where was I born?"')
    response_message = response.content.decode("utf-8")

    # Assert
    assert response.status_code == 200
    # 1. Infer who I am from chat history
    # 2. Infer I was born in Testville from previously retrieved notes
    assert "Testville" in response_message


# ----------------------------------------------------------------------------------------------------
@pytest.mark.xfail(
    AssertionError,
    reason="Chat director not capable of answering this question yet because it requires extract_questions",
)
@pytest.mark.chatquality
@pytest.mark.django_db(transaction=True)
def test_answer_from_chat_history_and_currently_retrieved_content(client_offline_chat, default_user2):
    # Arrange
    message_list = [
        ("Hello, my name is Xi Li. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 1st April 1984.", []),
    ]
    create_conversation(message_list, default_user2)

    # Act
    response = client_offline_chat.get(f'/api/chat?q="Where was I born?"')
    response_message = response.content.decode("utf-8")

    # Assert
    assert response.status_code == 200
    # Inference in a multi-turn conversation
    # 1. Infer who I am from chat history
    # 2. Search for notes about when <my_name_from_chat_history> was born
    # 3. Extract where I was born from currently retrieved notes
    assert "Fujiang" in response_message


# ----------------------------------------------------------------------------------------------------
@pytest.mark.xfail(AssertionError, reason="Chat director not capable of answering this question yet")
@pytest.mark.chatquality
@pytest.mark.django_db(transaction=True)
def test_no_answer_in_chat_history_or_retrieved_content(client_offline_chat, default_user2):
    "Chat director should say don't know as not enough contexts in chat history or retrieved to answer question"
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 1st April 1984.", []),
    ]
    create_conversation(message_list, default_user2)

    # Act
    response = client_offline_chat.get(f'/api/chat?q="Where was I born?"&stream=true')
    response_message = response.content.decode("utf-8")

    # Assert
    expected_responses = ["don't know", "do not know", "no information", "do not have", "don't have"]
    assert response.status_code == 200
    assert any([expected_response in response_message for expected_response in expected_responses]), (
        "Expected chat director to say they don't know in response, but got: " + response_message
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
@pytest.mark.django_db(transaction=True)
def test_answer_using_general_command(client_offline_chat, default_user2):
    # Arrange
    query = urllib.parse.quote("/general Where was Xi Li born?")
    message_list = []
    create_conversation(message_list, default_user2)

    # Act
    response = client_offline_chat.get(f"/api/chat?q={query}&stream=true")
    response_message = response.content.decode("utf-8")

    # Assert
    assert response.status_code == 200
    assert "Fujiang" not in response_message


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
@pytest.mark.django_db(transaction=True)
def test_answer_from_retrieved_content_using_notes_command(client_offline_chat, default_user2):
    # Arrange
    query = urllib.parse.quote("/notes Where was Xi Li born?")
    message_list = []
    create_conversation(message_list, default_user2)

    # Act
    response = client_offline_chat.get(f"/api/chat?q={query}&stream=true")
    response_message = response.content.decode("utf-8")

    # Assert
    assert response.status_code == 200
    assert "Fujiang" in response_message


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
@pytest.mark.django_db(transaction=True)
def test_answer_using_file_filter(client_offline_chat, default_user2):
    # Arrange
    no_answer_query = urllib.parse.quote('Where was Xi Li born? file:"Namita.markdown"')
    answer_query = urllib.parse.quote('Where was Xi Li born? file:"Xi Li.markdown"')
    message_list = []
    create_conversation(message_list, default_user2)

    # Act
    no_answer_response = client_offline_chat.get(f"/api/chat?q={no_answer_query}&stream=true").content.decode("utf-8")
    answer_response = client_offline_chat.get(f"/api/chat?q={answer_query}&stream=true").content.decode("utf-8")

    # Assert
    assert "Fujiang" not in no_answer_response
    assert "Fujiang" in answer_response


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
@pytest.mark.django_db(transaction=True)
def test_answer_not_known_using_notes_command(client_offline_chat, default_user2):
    # Arrange
    query = urllib.parse.quote("/notes Where was Testatron born?")
    message_list = []
    create_conversation(message_list, default_user2)

    # Act
    response = client_offline_chat.get(f"/api/chat?q={query}&stream=true")
    response_message = response.content.decode("utf-8")

    # Assert
    assert response.status_code == 200
    assert response_message == prompts.no_notes_found.format()


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
@pytest.mark.django_db(transaction=True)
@freeze_time("2023-04-01", ignore=["transformers"])
def test_answer_requires_current_date_awareness(client_offline_chat):
    "Chat actor should be able to answer questions relative to current date using provided notes"
    # Arrange
    query = urllib.parse.quote("Where did I have lunch today?")

    # Act
    response = client_offline_chat.get(f"/api/chat?q={query}&stream=true")
    response_message = response.content.decode("utf-8")

    # Assert
    expected_responses = ["Arak", "Medellin"]
    assert response.status_code == 200
    assert any([expected_response in response_message for expected_response in expected_responses]), (
        "Expected chat director to say Arak, Medellin, but got: " + response_message
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.xfail(AssertionError, reason="Chat director not capable of answering this question yet")
@pytest.mark.chatquality
@pytest.mark.django_db(transaction=True)
@freeze_time("2023-04-01", ignore=["transformers"])
def test_answer_requires_date_aware_aggregation_across_provided_notes(client_offline_chat):
    "Chat director should be able to answer questions that require date aware aggregation across multiple notes"
    # Act
    response = client_offline_chat.get(f'/api/chat?q="How much did I spend on dining this year?"&stream=true')
    response_message = response.content.decode("utf-8")

    # Assert
    assert response.status_code == 200
    assert "26" in response_message


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
@pytest.mark.django_db(transaction=True)
def test_answer_general_question_not_in_chat_history_or_retrieved_content(client_offline_chat, default_user2):
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 1st April 1984.", []),
        ("Where was I born?", "You were born Testville.", []),
    ]
    create_conversation(message_list, default_user2)

    # Act
    response = client_offline_chat.get(
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
@pytest.mark.xfail(reason="Chat director not consistently capable of asking for clarification yet.")
@pytest.mark.chatquality
@pytest.mark.django_db(transaction=True)
def test_ask_for_clarification_if_not_enough_context_in_question(client_offline_chat, default_user2):
    # Act
    response = client_offline_chat.get(f'/api/chat?q="What is the name of Namitas older son"&stream=true')
    response_message = response.content.decode("utf-8")

    # Assert
    expected_responses = [
        "which of them is the older",
        "which one is older",
        "which of them is older",
        "which one is the older",
    ]
    assert response.status_code == 200
    assert any([expected_response in response_message.lower() for expected_response in expected_responses]), (
        "Expected chat director to ask for clarification in response, but got: " + response_message
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.xfail(reason="Chat director not capable of answering this question yet")
@pytest.mark.chatquality
@pytest.mark.django_db(transaction=True)
def test_answer_in_chat_history_beyond_lookback_window(client_offline_chat, default_user2: KhojUser):
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 1st April 1984.", []),
        ("Where was I born?", "You were born Testville.", []),
    ]
    create_conversation(message_list, default_user2)

    # Act
    response = client_offline_chat.get(f'/api/chat?q="What is my name?"&stream=true')
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
def test_answer_in_chat_history_by_conversation_id(client_offline_chat, default_user2: KhojUser):
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
    response = client_offline_chat.get(
        f'/api/chat?q="What is my favorite color?"&conversation_id={conversation.id}&stream=true'
    )
    response_message = response.content.decode("utf-8")

    # Assert
    expected_responses = ["green"]
    assert response.status_code == 200
    assert any([expected_response in response_message.lower() for expected_response in expected_responses]), (
        "Expected green in response, but got: " + response_message
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.xfail(reason="Chat director not great at adhering to agent instructions yet")
@pytest.mark.chatquality
@pytest.mark.django_db(transaction=True)
def test_answer_in_chat_history_by_conversation_id_with_agent(
    client_offline_chat, default_user2: KhojUser, offline_agent: Agent
):
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 1st April 1984.", []),
        ("What's my favorite color", "Your favorite color is green.", []),
        ("Where was I born?", "You were born Testville.", []),
        ("What did I buy?", "You bought an apple for 2.00, an orange for 3.00, and a potato for 8.00", []),
    ]
    conversation = create_conversation(message_list, default_user2, offline_agent)

    # Act
    query = urllib.parse.quote("/general What did I eat for breakfast?")
    response = client_offline_chat.get(f"/api/chat?q={query}&conversation_id={conversation.id}&stream=true")
    response_message = response.content.decode("utf-8")

    # Assert that agent only responds with the summary of spending
    expected_responses = ["13.00", "13", "13.0", "thirteen"]
    assert response.status_code == 200
    assert any([expected_response in response_message.lower() for expected_response in expected_responses]), (
        "Expected green in response, but got: " + response_message
    )


@pytest.mark.chatquality
@pytest.mark.django_db(transaction=True)
def test_answer_chat_history_very_long(client_offline_chat, default_user2):
    # Arrange
    message_list = [(" ".join([fake.paragraph() for _ in range(50)]), fake.sentence(), []) for _ in range(10)]

    create_conversation(message_list, default_user2)

    # Act
    response = client_offline_chat.get(f'/api/chat?q="What is my name?"&stream=true')
    response_message = response.content.decode("utf-8")

    # Assert
    assert response.status_code == 200
    assert len(response_message) > 0


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
@pytest.mark.django_db(transaction=True)
def test_answer_requires_multiple_independent_searches(client_offline_chat):
    "Chat director should be able to answer by doing multiple independent searches for required information"
    # Act
    response = client_offline_chat.get(f'/api/chat?q="Is Xi older than Namita?"&stream=true')
    response_message = response.content.decode("utf-8")

    # Assert
    expected_responses = ["he is older than namita", "xi is older than namita", "xi li is older than namita"]
    assert response.status_code == 200
    assert any([expected_response in response_message.lower() for expected_response in expected_responses]), (
        "Expected Xi is older than Namita, but got: " + response_message
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_get_correct_tools_online(client_offline_chat):
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
async def test_get_correct_tools_notes(client_offline_chat):
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
async def test_get_correct_tools_online_or_general_and_notes(client_offline_chat):
    # Arrange
    user_query = "What's the highest point in Patagonia and have I been there?"

    # Act
    tools = await aget_relevant_information_sources(user_query, {})

    # Assert
    tools = [tool.value for tool in tools]
    assert len(tools) == 2
    assert "online" or "general" in tools
    assert "notes" in tools


# ----------------------------------------------------------------------------------------------------
@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_get_correct_tools_general(client_offline_chat):
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
async def test_get_correct_tools_with_chat_history(client_offline_chat, default_user2):
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
    chat_history = create_conversation(chat_log, default_user2)

    # Act
    tools = await aget_relevant_information_sources(user_query, chat_history)

    # Assert
    tools = [tool.value for tool in tools]
    assert tools == ["online"]
