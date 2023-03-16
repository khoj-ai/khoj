# Standard Packages
import os
from datetime import datetime

# External Packages
import pytest

# Internal Packages
from khoj.processor.conversation.gpt import converse
from khoj.processor.conversation.utils import message_to_log


# Initialize variables for tests
api_key = os.getenv("OPENAI_API_KEY")
if api_key is None:
    pytest.skip(
        reason="Set OPENAI_API_KEY environment variable to run tests below. Get OpenAI API key from https://platform.openai.com/account/api-keys",
        allow_module_level=True,
    )


# Test
# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_chat_with_no_chat_history_or_retrieved_content():
    # Act
    response = converse(
        text="",  # Assume no context retrieved from notes for the user_query
        user_query="Hello, my name is Testatron. Who are you?",
        api_key=api_key,
    )

    # Assert
    expected_responses = ["Khoj", "khoj"]
    assert len(response) > 0
    assert any([expected_response in response for expected_response in expected_responses]), (
        "Expected assistants name, [K|k]hoj, in response but got" + response
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_answer_from_chat_history_and_no_content():
    # Arrange
    conversation_log = {"chat": []}
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", ""),
        ("When was I born?", "You were born on 1st April 1984.", ""),
    ]
    # Generate conversation logs
    for user_message, gpt_message, _ in message_list:
        conversation_log["chat"] += message_to_log(user_message, gpt_message)

    # Act
    response = converse(
        text="",  # Assume no context retrieved from notes for the user_query
        user_query="What is my name?",
        conversation_log=conversation_log,
        api_key=api_key,
    )

    # Assert
    expected_responses = ["Testatron", "testatron"]
    assert len(response) > 0
    assert any([expected_response in response for expected_response in expected_responses]), (
        "Expected [T|t]estatron in response but got" + response
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_answer_from_chat_history_and_previously_retrieved_content():
    "Chat actor needs to use context in previous notes and chat history to answer question"
    # Arrange
    conversation_log = {"chat": []}
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", ""),
        ("When was I born?", "You were born on 1st April 1984.", "Testatron was born on 1st April 1984 in Testville."),
    ]
    # Generate conversation logs
    for user_message, gpt_message, context in message_list:
        conversation_log["chat"] += message_to_log(user_message, gpt_message, {"context": context})

    # Act
    response = converse(
        text="",  # Assume no context retrieved from notes for the user_query
        user_query="Where was I born?",
        conversation_log=conversation_log,
        api_key=api_key,
    )

    # Assert
    assert len(response) > 0
    # Infer who I am and use that to infer I was born in Testville using chat history and previously retrieved notes
    assert "Testville" in response


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_answer_from_chat_history_and_currently_retrieved_content():
    "Chat actor needs to use context across currently retrieved notes and chat history to answer question"
    # Arrange
    conversation_log = {"chat": []}
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", ""),
        ("When was I born?", "You were born on 1st April 1984.", ""),
    ]
    # Generate conversation logs
    for user_message, gpt_message, context in message_list:
        conversation_log["chat"] += message_to_log(user_message, gpt_message, {"context": context})

    # Act
    response = converse(
        text="Testatron was born on 1st April 1984 in Testville.",  # Assume context retrieved from notes for the user_query
        user_query="Where was I born?",
        conversation_log=conversation_log,
        api_key=api_key,
    )

    # Assert
    assert len(response) > 0
    assert "Testville" in response


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_no_answer_in_chat_history_or_retrieved_content():
    "Chat actor should say don't know as not enough contexts in chat history or retrieved to answer question"
    # Arrange
    conversation_log = {"chat": []}
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", ""),
        ("When was I born?", "You were born on 1st April 1984.", ""),
    ]
    # Generate conversation logs
    for user_message, gpt_message, context in message_list:
        conversation_log["chat"] += message_to_log(user_message, gpt_message, {"context": context})

    # Act
    response = converse(
        text="",  # Assume no context retrieved from notes for the user_query
        user_query="Where was I born?",
        conversation_log=conversation_log,
        api_key=api_key,
    )

    # Assert
    expected_responses = ["don't know", "do not know", "no information", "do not have", "don't have"]
    assert len(response) > 0
    assert any([expected_response in response for expected_response in expected_responses]), (
        "Expected chat actor to say they don't know in response, but got: " + response
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_answer_requires_current_date_awareness():
    "Chat actor should be able to answer questions relative to current date using provided notes"
    # Arrange
    context = f"""
    # {datetime.now().strftime("%Y-%m-%d")} "Naco Taco" "Tacos for Dinner"
      Expenses:Food:Dining  10.00 USD

    # {datetime.now().strftime("%Y-%m-%d")} "Sagar Ratna" "Dosa for Lunch"
      Expenses:Food:Dining  10.00 USD

    # 2020-04-01 "SuperMercado" "Bananas"
      Expenses:Food:Groceries  10.00 USD

    # 2020-01-01 "Naco Taco" "Burittos for Dinner"
      Expenses:Food:Dining  10.00 USD
    """

    # Act
    response = converse(
        text=context,  # Assume context retrieved from notes for the user_query
        user_query="What did I have for Dinner today?",
        api_key=api_key,
    )

    # Assert
    expected_responses = ["tacos", "Tacos"]
    assert len(response) > 0
    assert any([expected_response in response for expected_response in expected_responses]), (
        "Expected [T|t]acos in response, but got: " + response
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_answer_requires_date_aware_aggregation_across_provided_notes():
    "Chat actor should be able to answer questions that require date aware aggregation across multiple notes"
    # Arrange
    context = f"""
    # {datetime.now().strftime("%Y-%m-%d")} "Naco Taco" "Tacos for Dinner"
      Expenses:Food:Dining  10.00 USD

    # {datetime.now().strftime("%Y-%m-%d")} "Sagar Ratna" "Dosa for Lunch"
      Expenses:Food:Dining  10.00 USD

    # 2020-04-01 "SuperMercado" "Bananas"
      Expenses:Food:Groceries  10.00 USD

    # 2020-01-01 "Naco Taco" "Burittos for Dinner"
      Expenses:Food:Dining  10.00 USD
    """

    # Act
    response = converse(
        text=context,  # Assume context retrieved from notes for the user_query
        user_query="How much did I spend on dining this year?",
        api_key=api_key,
    )

    # Assert
    assert len(response) > 0
    assert "20" in response


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_answer_general_question_not_in_chat_history_or_retrieved_content():
    "Chat actor should be able to answer general questions not requiring looking at chat history or notes"
    # Arrange
    conversation_log = {"chat": []}
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", ""),
        ("When was I born?", "You were born on 1st April 1984.", ""),
        ("Where was I born?", "You were born Testville.", ""),
    ]
    # Generate conversation logs
    for user_message, gpt_message, context in message_list:
        conversation_log["chat"] += message_to_log(user_message, gpt_message, {"context": context})

    # Act
    response = converse(
        text="",  # Assume no context retrieved from notes for the user_query
        user_query="Write a haiku about unit testing",
        conversation_log=conversation_log,
        api_key=api_key,
    )

    # Assert
    expected_responses = ["test", "Test"]
    assert len(response.splitlines()) == 3  # haikus are 3 lines long
    assert any([expected_response in response for expected_response in expected_responses]), (
        "Expected [T|t]est in response, but got: " + response
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.xfail(reason="Chat actor not consistently capable of asking for clarification yet.")
@pytest.mark.chatquality
def test_ask_for_clarification_if_not_enough_context_in_question():
    "Chat actor should ask for clarification if question cannot be answered unambiguously with the provided context"
    # Arrange
    context = f"""
    # Ramya
    My sister, Ramya, is married to Kali Devi. They have 2 kids, Ravi and Rani.

    # Fang
    My sister, Fang Liu is married to Xi Li. They have 1 kid, Xiao Li.

    # Aiyla
    My sister, Aiyla is married to Tolga. They have 3 kids, Yildiz, Ali and Ahmet.
     """

    # Act
    response = converse(
        text=context,  # Assume context retrieved from notes for the user_query
        user_query="How many kids does my older sister have?",
        api_key=api_key,
    )

    # Assert
    expected_responses = ["which sister", "Which sister", "which of your sister", "Which of your sister"]
    assert any([expected_response in response for expected_response in expected_responses]), (
        "Expected chat actor to ask for clarification in response, but got: " + response
    )
