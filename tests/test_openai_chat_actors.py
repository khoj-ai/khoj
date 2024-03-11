import os
from datetime import datetime

import freezegun
import pytest
from freezegun import freeze_time

from khoj.processor.conversation.openai.gpt import converse, extract_questions
from khoj.processor.conversation.utils import message_to_log
from khoj.routers.helpers import aget_relevant_output_modes

# Initialize variables for tests
api_key = os.getenv("OPENAI_API_KEY")
if api_key is None:
    pytest.skip(
        reason="Set OPENAI_API_KEY environment variable to run tests below. Get OpenAI API key from https://platform.openai.com/account/api-keys",
        allow_module_level=True,
    )

freezegun.configure(extend_ignore_list=["transformers"])


# Test
# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
@freeze_time("1984-04-02", ignore=["transformers"])
def test_extract_question_with_date_filter_from_relative_day():
    # Act
    response = extract_questions("Where did I go for dinner yesterday?")

    # Assert
    expected_responses = [
        ("dt='1984-04-01'", ""),
        ("dt>='1984-04-01'", "dt<'1984-04-02'"),
        ("dt>'1984-03-31'", "dt<'1984-04-02'"),
    ]
    assert len(response) == 1
    assert any([start in response[0] and end in response[0] for start, end in expected_responses]), (
        "Expected date filter to limit to 1st April 1984 in response but got: " + response[0]
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
@freeze_time("1984-04-02", ignore=["transformers"])
def test_extract_question_with_date_filter_from_relative_month():
    # Act
    response = extract_questions("Which countries did I visit last month?")

    # Assert
    expected_responses = [("dt>='1984-03-01'", "dt<'1984-04-01'"), ("dt>='1984-03-01'", "dt<='1984-03-31'")]
    assert len(response) == 1
    assert any([start in response[0] and end in response[0] for start, end in expected_responses]), (
        "Expected date filter to limit to March 1984 in response but got: " + response[0]
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
@freeze_time("1984-04-02", ignore=["transformers"])
def test_extract_question_with_date_filter_from_relative_year():
    # Act
    response = extract_questions("Which countries have I visited this year?")

    # Assert
    expected_responses = [
        ("dt>='1984-01-01'", ""),
        ("dt>='1984-01-01'", "dt<'1985-01-01'"),
        ("dt>='1984-01-01'", "dt<='1984-12-31'"),
    ]
    assert len(response) == 1
    assert any([start in response[0] and end in response[0] for start, end in expected_responses]), (
        "Expected date filter to limit to 1984 in response but got: " + response[0]
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_extract_multiple_explicit_questions_from_message():
    # Act
    response = extract_questions("What is the Sun? What is the Moon?")

    # Assert
    expected_responses = [
        ("sun", "moon"),
    ]
    assert len(response) == 2
    assert any([start in response[0].lower() and end in response[1].lower() for start, end in expected_responses]), (
        "Expected two search queries in response but got: " + response[0]
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_extract_multiple_implicit_questions_from_message():
    # Act
    response = extract_questions("Is Morpheus taller than Neo?")

    # Assert
    expected_responses = [
        ("morpheus", "neo"),
    ]
    assert len(response) == 2
    assert any([start in response[0].lower() and end in response[1].lower() for start, end in expected_responses]), (
        "Expected two search queries in response but got: " + response[0]
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_generate_search_query_using_question_from_chat_history():
    # Arrange
    message_list = [
        ("What is the name of Mr. Vader's daughter?", "Princess Leia", []),
    ]

    # Act
    response = extract_questions("Does he have any sons?", conversation_log=populate_chat_history(message_list))

    # Assert
    assert len(response) == 1
    assert "Vader" in response[0]


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_generate_search_query_using_answer_from_chat_history():
    # Arrange
    message_list = [
        ("What is the name of Mr. Vader's daughter?", "Princess Leia", []),
    ]

    # Act
    response = extract_questions("Is she a Jedi?", conversation_log=populate_chat_history(message_list))

    # Assert
    assert len(response) == 1
    assert "Leia" in response[0]


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_generate_search_query_using_question_and_answer_from_chat_history():
    # Arrange
    message_list = [
        ("Does Luke Skywalker have any Siblings?", "Yes, Princess Leia", []),
    ]

    # Act
    response = extract_questions("Who is their father?", conversation_log=populate_chat_history(message_list))

    # Assert
    assert len(response) == 1
    assert "Leia" in response[0] and "Luke" in response[0]


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_generate_search_query_with_date_and_context_from_chat_history():
    # Arrange
    message_list = [
        ("When did I visit Masai Mara?", "You visited Masai Mara in April 2000", []),
    ]

    # Act
    response = extract_questions(
        "What was the Pizza place we ate at over there?", conversation_log=populate_chat_history(message_list)
    )

    # Assert
    expected_responses = [
        ("dt>='2000-04-01'", "dt<'2000-05-01'"),
        ("dt>='2000-04-01'", "dt<='2000-04-30'"),
        ('dt>="2000-04-01"', 'dt<"2000-05-01"'),
        ('dt>="2000-04-01"', 'dt<="2000-04-30"'),
    ]
    assert len(response) == 1
    assert "Masai Mara" in response[0]
    assert any([start in response[0] and end in response[0] for start, end in expected_responses]), (
        "Expected date filter to limit to April 2000 in response but got: " + response[0]
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_chat_with_no_chat_history_or_retrieved_content():
    # Act
    response_gen = converse(
        references=[],  # Assume no context retrieved from notes for the user_query
        user_query="Hello, my name is Testatron. Who are you?",
        api_key=api_key,
    )
    response = "".join([response_chunk for response_chunk in response_gen])

    # Assert
    expected_responses = ["Khoj", "khoj"]
    assert len(response) > 0
    assert any([expected_response in response for expected_response in expected_responses]), (
        "Expected assistants name, [K|k]hoj, in response but got: " + response
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_answer_from_chat_history_and_no_content():
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 1st April 1984.", []),
    ]

    # Act
    response_gen = converse(
        references=[],  # Assume no context retrieved from notes for the user_query
        user_query="What is my name?",
        conversation_log=populate_chat_history(message_list),
        api_key=api_key,
    )
    response = "".join([response_chunk for response_chunk in response_gen])

    # Assert
    expected_responses = ["Testatron", "testatron"]
    assert len(response) > 0
    assert any([expected_response in response for expected_response in expected_responses]), (
        "Expected [T|t]estatron in response but got: " + response
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_answer_from_chat_history_and_previously_retrieved_content():
    "Chat actor needs to use context in previous notes and chat history to answer question"
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        (
            "When was I born?",
            "You were born on 1st April 1984.",
            ["Testatron was born on 1st April 1984 in Testville."],
        ),
    ]

    # Act
    response_gen = converse(
        references=[],  # Assume no context retrieved from notes for the user_query
        user_query="Where was I born?",
        conversation_log=populate_chat_history(message_list),
        api_key=api_key,
    )
    response = "".join([response_chunk for response_chunk in response_gen])

    # Assert
    assert len(response) > 0
    # Infer who I am and use that to infer I was born in Testville using chat history and previously retrieved notes
    assert "Testville" in response


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_answer_from_chat_history_and_currently_retrieved_content():
    "Chat actor needs to use context across currently retrieved notes and chat history to answer question"
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 1st April 1984.", []),
    ]

    # Act
    response_gen = converse(
        references=[
            "Testatron was born on 1st April 1984 in Testville."
        ],  # Assume context retrieved from notes for the user_query
        user_query="Where was I born?",
        conversation_log=populate_chat_history(message_list),
        api_key=api_key,
    )
    response = "".join([response_chunk for response_chunk in response_gen])

    # Assert
    assert len(response) > 0
    assert "Testville" in response


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_refuse_answering_unanswerable_question():
    "Chat actor should not try make up answers to unanswerable questions."
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 1st April 1984.", []),
    ]

    # Act
    response_gen = converse(
        references=[],  # Assume no context retrieved from notes for the user_query
        user_query="Where was I born?",
        conversation_log=populate_chat_history(message_list),
        api_key=api_key,
    )
    response = "".join([response_chunk for response_chunk in response_gen])

    # Assert
    expected_responses = [
        "don't know",
        "do not know",
        "no information",
        "do not have",
        "don't have",
        "cannot answer",
        "I'm sorry",
    ]
    assert len(response) > 0
    assert any([expected_response in response for expected_response in expected_responses]), (
        "Expected chat actor to say they don't know in response, but got: " + response
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_answer_requires_current_date_awareness():
    "Chat actor should be able to answer questions relative to current date using provided notes"
    # Arrange
    context = [
        f"""{datetime.now().strftime("%Y-%m-%d")} "Naco Taco" "Tacos for Dinner"
Expenses:Food:Dining  10.00 USD""",
        f"""{datetime.now().strftime("%Y-%m-%d")} "Sagar Ratna" "Dosa for Lunch"
Expenses:Food:Dining  10.00 USD""",
        f"""2020-04-01 "SuperMercado" "Bananas"
Expenses:Food:Groceries  10.00 USD""",
        f"""2020-01-01 "Naco Taco" "Burittos for Dinner"
Expenses:Food:Dining  10.00 USD""",
    ]

    # Act
    response_gen = converse(
        references=context,  # Assume context retrieved from notes for the user_query
        user_query="What did I have for Dinner today?",
        api_key=api_key,
    )
    response = "".join([response_chunk for response_chunk in response_gen])

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
    context = [
        f"""# {datetime.now().strftime("%Y-%m-%d")} "Naco Taco" "Tacos for Dinner"
Expenses:Food:Dining  10.00 USD""",
        f"""{datetime.now().strftime("%Y-%m-%d")} "Sagar Ratna" "Dosa for Lunch"
Expenses:Food:Dining  10.00 USD""",
        f"""2020-04-01 "SuperMercado" "Bananas"
Expenses:Food:Groceries  10.00 USD""",
        f"""2020-01-01 "Naco Taco" "Burittos for Dinner"
Expenses:Food:Dining  10.00 USD""",
    ]

    # Act
    response_gen = converse(
        references=context,  # Assume context retrieved from notes for the user_query
        user_query="How much did I spend on dining this year?",
        api_key=api_key,
    )
    response = "".join([response_chunk for response_chunk in response_gen])

    # Assert
    assert len(response) > 0
    assert "20" in response


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_answer_general_question_not_in_chat_history_or_retrieved_content():
    "Chat actor should be able to answer general questions not requiring looking at chat history or notes"
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 1st April 1984.", []),
        ("Where was I born?", "You were born Testville.", []),
    ]

    # Act
    response_gen = converse(
        references=[],  # Assume no context retrieved from notes for the user_query
        user_query="Write a haiku about unit testing in 3 lines",
        conversation_log=populate_chat_history(message_list),
        api_key=api_key,
    )
    response = "".join([response_chunk for response_chunk in response_gen])

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
    context = [
        f"""# Ramya
My sister, Ramya, is married to Kali Devi. They have 2 kids, Ravi and Rani.""",
        f"""# Fang
My sister, Fang Liu is married to Xi Li. They have 1 kid, Xiao Li.""",
        f"""# Aiyla
My sister, Aiyla is married to Tolga. They have 3 kids, Yildiz, Ali and Ahmet.""",
    ]

    # Act
    response_gen = converse(
        references=context,  # Assume context retrieved from notes for the user_query
        user_query="How many kids does my older sister have?",
        api_key=api_key,
    )
    response = "".join([response_chunk for response_chunk in response_gen])

    # Assert
    expected_responses = ["which sister", "Which sister", "which of your sister", "Which of your sister"]
    assert any([expected_response in response for expected_response in expected_responses]), (
        "Expected chat actor to ask for clarification in response, but got: " + response
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_agent_prompt_should_be_used(openai_agent):
    "Chat actor should ask be tuned to think like an accountant based on the agent definition"
    # Arrange
    context = [
        f"""I went to the store and bought some bananas for 2.20""",
        f"""I went to the store and bought some apples for 1.30""",
        f"""I went to the store and bought some oranges for 6.00""",
    ]
    expected_responses = ["9.50", "9.5"]

    # Act
    response_gen = converse(
        references=context,  # Assume context retrieved from notes for the user_query
        user_query="What did I buy?",
        api_key=api_key,
    )
    no_agent_response = "".join([response_chunk for response_chunk in response_gen])
    response_gen = converse(
        references=context,  # Assume context retrieved from notes for the user_query
        user_query="What did I buy?",
        api_key=api_key,
        agent=openai_agent,
    )
    agent_response = "".join([response_chunk for response_chunk in response_gen])

    # Assert that the model without the agent prompt does not include the summary of purchases
    assert all([expected_response not in no_agent_response for expected_response in expected_responses]), (
        "Expected chat actor to summarize values of purchases" + no_agent_response
    )
    assert any([expected_response in agent_response for expected_response in expected_responses]), (
        "Expected chat actor to summarize values of purchases" + agent_response
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_use_default_response_mode(chat_client):
    # Arrange
    user_query = "What's the latest in the Israel/Palestine conflict?"

    # Act
    mode = await aget_relevant_output_modes(user_query, {})

    # Assert
    assert mode.value == "default"


# ----------------------------------------------------------------------------------------------------
@pytest.mark.anyio
@pytest.mark.django_db(transaction=True)
async def test_use_image_response_mode(chat_client):
    # Arrange
    user_query = "Paint a picture of the scenery in Timbuktu in the winter"

    # Act
    mode = await aget_relevant_output_modes(user_query, {})

    # Assert
    assert mode.value == "image"


# Helpers
# ----------------------------------------------------------------------------------------------------
def populate_chat_history(message_list):
    # Generate conversation logs
    conversation_log = {"chat": []}
    for user_message, gpt_message, context in message_list:
        conversation_log["chat"] += message_to_log(
            user_message,
            gpt_message,
            {"context": context, "intent": {"query": user_message, "inferred-queries": f'["{user_message}"]'}},
        )
    return conversation_log
