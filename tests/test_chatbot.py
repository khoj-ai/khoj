# Standard Packages
import os
from datetime import datetime

# External Packages
import pytest

# Internal Packages
from khoj.processor.conversation.gpt import converse, message_to_log, message_to_prompt


# Initialize variables for tests
api_key = os.getenv("OPENAI_API_KEY")  # Set your OPENAI_API_KEY as environment variable to run the tests below


# Test
# ----------------------------------------------------------------------------------------------------
def test_message_to_understand_prompt():
    # Arrange
    understand_primer = 'Extract information from each chat message\n\nremember(memory-type, data);\nmemory-type=["companion", "notes", "ledger", "image", "music"]\nsearch(search-type, data);\nsearch-type=["google", "youtube"]\ngenerate(activity);\nactivity=["paint","write", "chat"]\ntrigger-emotion(emotion);\nemotion=["happy","confidence","fear","surprise","sadness","disgust","anger", "curiosity", "calm"]\n\nQ: How are you doing?\nA: activity("chat"); trigger-emotion("surprise")\nQ: Do you remember what I told you about my brother Antoine when we were at the beach?\nA: remember("notes", "Brother Antoine when we were at the beach"); trigger-emotion("curiosity");\nQ: what did we talk about last time?\nA: remember("notes", "talk last time"); trigger-emotion("curiosity");\nQ: Let\'s make some drawings!\nA: generate("paint"); trigger-emotion("happy");\nQ: Do you know anything about Lebanon?\nA: search("google", "lebanon");  trigger-emotion("confidence");\nQ: Find a video about a panda rolling in the grass\nA: search("youtube","panda rolling in the grass");  trigger-emotion("happy"); \nQ: Tell me a scary story\nA: generate("write" "A story about some adventure"); trigger-emotion("fear");\nQ: What fiction book was I reading last week about AI starship?\nA: remember("notes", "read fiction book about AI starship last week"); trigger-emotion("curiosity");\nQ: How much did I spend at Subway for dinner last time?\nA: remember("ledger", "last Subway dinner"); trigger-emotion("curiosity");\nQ: I\'m feeling sleepy\nA: activity("chat"); trigger-emotion("calm")\nQ: What was that popular Sri lankan song that Alex showed me recently?\nA: remember("music", "popular Sri lankan song that Alex showed recently");  trigger-emotion("curiosity"); \nQ: You\'re pretty funny!\nA: activity("chat"); trigger-emotion("pride")'
    expected_response = 'Extract information from each chat message\n\nremember(memory-type, data);\nmemory-type=["companion", "notes", "ledger", "image", "music"]\nsearch(search-type, data);\nsearch-type=["google", "youtube"]\ngenerate(activity);\nactivity=["paint","write", "chat"]\ntrigger-emotion(emotion);\nemotion=["happy","confidence","fear","surprise","sadness","disgust","anger", "curiosity", "calm"]\n\nQ: How are you doing?\nA: activity("chat"); trigger-emotion("surprise")\nQ: Do you remember what I told you about my brother Antoine when we were at the beach?\nA: remember("notes", "Brother Antoine when we were at the beach"); trigger-emotion("curiosity");\nQ: what did we talk about last time?\nA: remember("notes", "talk last time"); trigger-emotion("curiosity");\nQ: Let\'s make some drawings!\nA: generate("paint"); trigger-emotion("happy");\nQ: Do you know anything about Lebanon?\nA: search("google", "lebanon");  trigger-emotion("confidence");\nQ: Find a video about a panda rolling in the grass\nA: search("youtube","panda rolling in the grass");  trigger-emotion("happy"); \nQ: Tell me a scary story\nA: generate("write" "A story about some adventure"); trigger-emotion("fear");\nQ: What fiction book was I reading last week about AI starship?\nA: remember("notes", "read fiction book about AI starship last week"); trigger-emotion("curiosity");\nQ: How much did I spend at Subway for dinner last time?\nA: remember("ledger", "last Subway dinner"); trigger-emotion("curiosity");\nQ: I\'m feeling sleepy\nA: activity("chat"); trigger-emotion("calm")\nQ: What was that popular Sri lankan song that Alex showed me recently?\nA: remember("music", "popular Sri lankan song that Alex showed recently");  trigger-emotion("curiosity"); \nQ: You\'re pretty funny!\nA: activity("chat"); trigger-emotion("pride")\nQ: When did I last dine at Burger King?\nA:'

    # Act
    actual_response = message_to_prompt(
        "When did I last dine at Burger King?", understand_primer, start_sequence="\nA:", restart_sequence="\nQ:"
    )

    # Assert
    assert actual_response == expected_response


# ----------------------------------------------------------------------------------------------------
@pytest.mark.skipif(
    api_key is None, reason="Set api_key variable to your OpenAI API key from https://beta.openai.com/account/api-keys"
)
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
@pytest.mark.skipif(
    api_key is None, reason="Set api_key variable to your OpenAI API key from https://beta.openai.com/account/api-keys"
)
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
@pytest.mark.skipif(
    api_key is None, reason="Set api_key variable to your OpenAI API key from https://beta.openai.com/account/api-keys"
)
def test_answer_from_chat_history_and_previously_retrieved_content():
    "Chatbot needs to use context in previous notes and chat history to answer question"
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
@pytest.mark.skipif(
    api_key is None, reason="Set api_key variable to your OpenAI API key from https://beta.openai.com/account/api-keys"
)
def test_answer_from_chat_history_and_currently_retrieved_content():
    "Chatbot needs to use context across currently retrieved notes and chat history to answer question"
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
@pytest.mark.skipif(
    api_key is None, reason="Set api_key variable to your OpenAI API key from https://beta.openai.com/account/api-keys"
)
def test_no_answer_in_chat_history_or_retrieved_content():
    "Chatbot should say don't know as not enough contexts in chat history or retrieved to answer question"
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
    assert any([expected_response in response for expected_response in expected_responses])


# ----------------------------------------------------------------------------------------------------
@pytest.mark.skipif(
    api_key is None, reason="Set api_key variable to your OpenAI API key from https://beta.openai.com/account/api-keys"
)
def test_answer_requires_current_date_awareness():
    "Chatbot should be able to answer questions relative to current date using provided notes"
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
@pytest.mark.skipif(
    api_key is None, reason="Set api_key variable to your OpenAI API key from https://beta.openai.com/account/api-keys"
)
def test_answer_requires_date_aware_aggregation_across_provided_notes():
    "Chatbot should be able to answer questions that require date aware aggregation across multiple notes"
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
@pytest.mark.skipif(
    api_key is None, reason="Set api_key variable to your OpenAI API key from https://beta.openai.com/account/api-keys"
)
def test_answer_general_question_not_in_chat_history_or_retrieved_content():
    "Chatbot should be able to answer general questions not requiring looking at chat history or notes"
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
@pytest.mark.xfail(reason="Chatbot not consistently capable of asking for clarification yet.")
@pytest.mark.skipif(
    api_key is None, reason="Set api_key variable to your OpenAI API key from https://beta.openai.com/account/api-keys"
)
def test_ask_for_clarification_if_not_enough_context_in_question():
    "Chatbot should ask for clarification if question cannot be answered unambiguously with the provided context"
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
        "Expected chatbot to ask for clarification in response, but got: " + response
    )
