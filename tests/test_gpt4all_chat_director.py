# External Packages
import pytest
from freezegun import freeze_time
from faker import Faker


# Internal Packages
from khoj.processor.conversation.utils import message_to_log
from khoj.utils import state


SKIP_TESTS = True
pytestmark = pytest.mark.skipif(
    SKIP_TESTS,
    reason="The GPT4All library has some quirks that make it hard to test in CI. This causes some tests to fail. Hence, disable it in CI.",
)

fake = Faker()


# Helpers
# ----------------------------------------------------------------------------------------------------
def populate_chat_history(message_list):
    # Generate conversation logs
    conversation_log = {"chat": []}
    for user_message, llm_message, context in message_list:
        conversation_log["chat"] += message_to_log(
            user_message,
            llm_message,
            {"context": context, "intent": {"query": user_message, "inferred-queries": f'["{user_message}"]'}},
        )

    # Update Conversation Metadata Logs in Application State
    state.processor_config.conversation.meta_log = conversation_log


# Tests
# ----------------------------------------------------------------------------------------------------
@pytest.mark.xfail(AssertionError, reason="Chat director not capable of answering this question yet")
@pytest.mark.chatquality
def test_chat_with_no_chat_history_or_retrieved_content_gpt4all(client_offline_chat):
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
@pytest.mark.chatquality
def test_answer_from_chat_history(client_offline_chat):
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 1st April 1984.", []),
    ]
    populate_chat_history(message_list)

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
@pytest.mark.xfail(AssertionError, reason="Chat director not capable of answering this question yet")
@pytest.mark.chatquality
def test_answer_from_currently_retrieved_content(client_offline_chat):
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        (
            "When was I born?",
            "You were born on 1st April 1984.",
            ["Testatron was born on 1st April 1984 in Testville."],
        ),
    ]
    populate_chat_history(message_list)

    # Act
    response = client_offline_chat.get(f'/api/chat?q="Where was Xi Li born?"')
    response_message = response.content.decode("utf-8")

    # Assert
    assert response.status_code == 200
    assert "Fujiang" in response_message


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_answer_from_chat_history_and_previously_retrieved_content(client_offline_chat):
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        (
            "When was I born?",
            "You were born on 1st April 1984.",
            ["Testatron was born on 1st April 1984 in Testville."],
        ),
    ]
    populate_chat_history(message_list)

    # Act
    response = client_offline_chat.get(f'/api/chat?q="Where was I born?"')
    response_message = response.content.decode("utf-8")

    # Assert
    assert response.status_code == 200
    # 1. Infer who I am from chat history
    # 2. Infer I was born in Testville from previously retrieved notes
    assert "Testville" in response_message


# ----------------------------------------------------------------------------------------------------
@pytest.mark.xfail(AssertionError, reason="Chat director not capable of answering this question yet")
@pytest.mark.chatquality
def test_answer_from_chat_history_and_currently_retrieved_content(client_offline_chat):
    # Arrange
    message_list = [
        ("Hello, my name is Xi Li. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 1st April 1984.", []),
    ]
    populate_chat_history(message_list)

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
def test_no_answer_in_chat_history_or_retrieved_content(client_offline_chat):
    "Chat director should say don't know as not enough contexts in chat history or retrieved to answer question"
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 1st April 1984.", []),
    ]
    populate_chat_history(message_list)

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
@pytest.mark.xfail(AssertionError, reason="Chat director not capable of answering time aware questions yet")
@pytest.mark.chatquality
@freeze_time("2023-04-01")
def test_answer_requires_current_date_awareness(client_offline_chat):
    "Chat actor should be able to answer questions relative to current date using provided notes"
    # Act
    response = client_offline_chat.get(f'/api/chat?q="Where did I have lunch today?"&stream=true')
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
@freeze_time("2023-04-01")
def test_answer_requires_date_aware_aggregation_across_provided_notes(client_offline_chat):
    "Chat director should be able to answer questions that require date aware aggregation across multiple notes"
    # Act
    response = client_offline_chat.get(f'/api/chat?q="How much did I spend on dining this year?"&stream=true')
    response_message = response.content.decode("utf-8")

    # Assert
    assert response.status_code == 200
    assert "23" in response_message


# ----------------------------------------------------------------------------------------------------
@pytest.mark.xfail(AssertionError, reason="Chat director not capable of answering this question yet")
@pytest.mark.chatquality
def test_answer_general_question_not_in_chat_history_or_retrieved_content(client_offline_chat):
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 1st April 1984.", []),
        ("Where was I born?", "You were born Testville.", []),
    ]
    populate_chat_history(message_list)

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
def test_ask_for_clarification_if_not_enough_context_in_question(client_offline_chat):
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
def test_answer_in_chat_history_beyond_lookback_window(client_offline_chat):
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 1st April 1984.", []),
        ("Where was I born?", "You were born Testville.", []),
    ]
    populate_chat_history(message_list)

    # Act
    response = client_offline_chat.get(f'/api/chat?q="What is my name?"&stream=true')
    response_message = response.content.decode("utf-8")

    # Assert
    expected_responses = ["Testatron", "testatron"]
    assert response.status_code == 200
    assert any([expected_response in response_message.lower() for expected_response in expected_responses]), (
        "Expected [T|t]estatron in response, but got: " + response_message
    )


@pytest.mark.chatquality
def test_answer_chat_history_very_long(client_offline_chat):
    # Arrange
    message_list = [(" ".join([fake.paragraph() for _ in range(50)]), fake.sentence(), []) for _ in range(10)]

    populate_chat_history(message_list)

    # Act
    response = client_offline_chat.get(f'/api/chat?q="What is my name?"&stream=true')
    response_message = response.content.decode("utf-8")

    # Assert
    assert response.status_code == 200
    assert len(response_message) > 0


# ----------------------------------------------------------------------------------------------------
@pytest.mark.xfail(AssertionError, reason="Chat director not capable of answering this question yet")
@pytest.mark.chatquality
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
