# Standard Packages
from datetime import datetime

# External Packages
import pytest

SKIP_TESTS = False
pytestmark = pytest.mark.skipif(
    SKIP_TESTS,
    reason="The GPT4All library has some quirks that make it hard to test in CI. This causes some tests to fail. Hence, disable it in CI.",
)

import freezegun
from freezegun import freeze_time

from gpt4all import GPT4All

# Internal Packages
from khoj.processor.conversation.gpt4all.chat_model import converse_llama, extract_questions_llama, filter_questions
from khoj.processor.conversation.gpt4all.utils import download_model

from khoj.processor.conversation.utils import message_to_log

MODEL_NAME = "llama-2-7b-chat.ggmlv3.q4_K_S.bin"


@pytest.fixture(scope="session")
def loaded_model():
    download_model(MODEL_NAME)
    return GPT4All(MODEL_NAME)


freezegun.configure(extend_ignore_list=["transformers"])


# Test
# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
@freeze_time("1984-04-02")
def test_extract_question_with_date_filter_from_relative_day(loaded_model):
    # Act
    response = extract_questions_llama("Where did I go for dinner yesterday?", loaded_model=loaded_model)

    assert len(response) >= 1

    assert any(
        [
            "dt>='1984-04-01'" in response[0] and "dt<'1984-04-02'" in response[0],
            "dt>='1984-04-01'" in response[0] and "dt<='1984-04-01'" in response[0],
            'dt>="1984-04-01"' in response[0] and 'dt<"1984-04-02"' in response[0],
            'dt>="1984-04-01"' in response[0] and 'dt<="1984-04-01"' in response[0],
        ]
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.xfail(reason="Chat actor still isn't very date aware nor capable of formatting")
@pytest.mark.chatquality
@freeze_time("1984-04-02")
def test_extract_question_with_date_filter_from_relative_month(loaded_model):
    # Act
    response = extract_questions_llama("Which countries did I visit last month?", loaded_model=loaded_model)

    # Assert
    assert len(response) >= 1
    # The user query should be the last question in the response
    assert response[-1] == ["Which countries did I visit last month?"]
    assert any(
        [
            "dt>='1984-03-01'" in response[0] and "dt<'1984-04-01'" in response[0],
            "dt>='1984-03-01'" in response[0] and "dt<='1984-03-31'" in response[0],
            'dt>="1984-03-01"' in response[0] and 'dt<"1984-04-01"' in response[0],
            'dt>="1984-03-01"' in response[0] and 'dt<="1984-03-31"' in response[0],
        ]
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
@freeze_time("1984-04-02")
def test_extract_question_with_date_filter_from_relative_year(loaded_model):
    # Act
    response = extract_questions_llama("Which countries have I visited this year?", loaded_model=loaded_model)

    # Assert
    assert len(response) >= 1
    assert response[-1] == "Which countries have I visited this year?"


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_extract_multiple_explicit_questions_from_message(loaded_model):
    # Act
    response = extract_questions_llama("What is the Sun? What is the Moon?", loaded_model=loaded_model)

    # Assert
    expected_responses = ["What is the Sun?", "What is the Moon?"]
    assert len(response) >= 2
    assert expected_responses[0] == response[-2]
    assert expected_responses[1] == response[-1]


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_extract_multiple_implicit_questions_from_message(loaded_model):
    # Act
    response = extract_questions_llama("Is Morpheus taller than Neo?", loaded_model=loaded_model)

    # Assert
    expected_responses = [
        ("morpheus", "neo", "height", "taller", "shorter"),
    ]
    assert len(response) == 3
    assert any([start in response[0].lower() and end in response[1].lower() for start, end in expected_responses]), (
        "Expected two search queries in response but got: " + response[0]
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_generate_search_query_using_question_from_chat_history(loaded_model):
    # Arrange
    message_list = [
        ("What is the name of Mr. Vader's daughter?", "Princess Leia", []),
    ]

    # Act
    response = extract_questions_llama(
        "Does he have any sons?",
        conversation_log=populate_chat_history(message_list),
        loaded_model=loaded_model,
        use_history=True,
    )

    expected_responses = [
        "Vader",
        "sons",
        "son",
        "Darth",
        "children",
    ]

    # Assert
    assert len(response) >= 1
    assert any([expected_response in response[0] for expected_response in expected_responses]), (
        "Expected chat actor to ask for clarification in response, but got: " + response[0]
    )


# ----------------------------------------------------------------------------------------------------
# @pytest.mark.xfail(reason="Chat actor does not consistently follow template instructions.")
@pytest.mark.chatquality
def test_generate_search_query_using_answer_from_chat_history(loaded_model):
    # Arrange
    message_list = [
        ("What is the name of Mr. Vader's daughter?", "Princess Leia", []),
    ]

    # Act
    response = extract_questions_llama(
        "Is she a Jedi?",
        conversation_log=populate_chat_history(message_list),
        loaded_model=loaded_model,
        use_history=True,
    )

    expected_responses = [
        "Leia",
        "Vader",
        "daughter",
    ]

    # Assert
    assert len(response) >= 1
    assert any([expected_response in response[0] for expected_response in expected_responses]), (
        "Expected chat actor to ask for clarification in response, but got: " + response[0]
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.xfail(reason="Chat actor is not sufficiently date-aware")
@pytest.mark.chatquality
def test_generate_search_query_with_date_and_context_from_chat_history(loaded_model):
    # Arrange
    message_list = [
        ("When did I visit Masai Mara?", "You visited Masai Mara in April 2000", []),
    ]

    # Act
    response = extract_questions_llama(
        "What was the Pizza place we ate at over there?",
        conversation_log=populate_chat_history(message_list),
        loaded_model=loaded_model,
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
def test_chat_with_no_chat_history_or_retrieved_content(loaded_model):
    # Act
    response_gen = converse_llama(
        references=[],  # Assume no context retrieved from notes for the user_query
        user_query="Hello, my name is Testatron. Who are you?",
        loaded_model=loaded_model,
    )
    response = "".join([response_chunk for response_chunk in response_gen])

    # Assert
    expected_responses = ["Khoj", "khoj", "khooj", "Khooj", "KHOJ"]
    assert len(response) > 0
    assert any([expected_response in response for expected_response in expected_responses]), (
        "Expected assistants name, [K|k]hoj, in response but got: " + response
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_answer_from_chat_history_and_previously_retrieved_content(loaded_model):
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
    response_gen = converse_llama(
        references=[],  # Assume no context retrieved from notes for the user_query
        user_query="Where was I born?",
        conversation_log=populate_chat_history(message_list),
        loaded_model=loaded_model,
    )
    response = "".join([response_chunk for response_chunk in response_gen])

    # Assert
    assert len(response) > 0
    # Infer who I am and use that to infer I was born in Testville using chat history and previously retrieved notes
    assert "Testville" in response


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_answer_from_chat_history_and_currently_retrieved_content(loaded_model):
    "Chat actor needs to use context across currently retrieved notes and chat history to answer question"
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 1st April 1984.", []),
    ]

    # Act
    response_gen = converse_llama(
        references=[
            "Testatron was born on 1st April 1984 in Testville."
        ],  # Assume context retrieved from notes for the user_query
        user_query="Where was I born?",
        conversation_log=populate_chat_history(message_list),
        loaded_model=loaded_model,
    )
    response = "".join([response_chunk for response_chunk in response_gen])

    # Assert
    assert len(response) > 0
    assert "Testville" in response


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_refuse_answering_unanswerable_question(loaded_model):
    "Chat actor should not try make up answers to unanswerable questions."
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 1st April 1984.", []),
    ]

    # Act
    response_gen = converse_llama(
        references=[],  # Assume no context retrieved from notes for the user_query
        user_query="Where was I born?",
        conversation_log=populate_chat_history(message_list),
        loaded_model=loaded_model,
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
def test_answer_requires_current_date_awareness(loaded_model):
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
    response_gen = converse_llama(
        references=context,  # Assume context retrieved from notes for the user_query
        user_query="What did I have for Dinner today?",
        loaded_model=loaded_model,
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
def test_answer_requires_date_aware_aggregation_across_provided_notes(loaded_model):
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
    response_gen = converse_llama(
        references=context,  # Assume context retrieved from notes for the user_query
        user_query="How much did I spend on dining this year?",
        loaded_model=loaded_model,
    )
    response = "".join([response_chunk for response_chunk in response_gen])

    # Assert
    assert len(response) > 0
    assert "20" in response


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_answer_general_question_not_in_chat_history_or_retrieved_content(loaded_model):
    "Chat actor should be able to answer general questions not requiring looking at chat history or notes"
    # Arrange
    message_list = [
        ("Hello, my name is Testatron. Who are you?", "Hi, I am Khoj, a personal assistant. How can I help?", []),
        ("When was I born?", "You were born on 1st April 1984.", []),
        ("Where was I born?", "You were born Testville.", []),
    ]

    # Act
    response_gen = converse_llama(
        references=[],  # Assume no context retrieved from notes for the user_query
        user_query="Write a haiku about unit testing in 3 lines",
        conversation_log=populate_chat_history(message_list),
        loaded_model=loaded_model,
    )
    response = "".join([response_chunk for response_chunk in response_gen])

    # Assert
    expected_responses = ["test", "testing"]
    assert len(response.splitlines()) >= 3  # haikus are 3 lines long, but Falcon tends to add a lot of new lines.
    assert any([expected_response in response.lower() for expected_response in expected_responses]), (
        "Expected [T|t]est in response, but got: " + response
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.chatquality
def test_ask_for_clarification_if_not_enough_context_in_question(loaded_model):
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
    response_gen = converse_llama(
        references=context,  # Assume context retrieved from notes for the user_query
        user_query="How many kids does my older sister have?",
        loaded_model=loaded_model,
    )
    response = "".join([response_chunk for response_chunk in response_gen])

    # Assert
    expected_responses = ["which sister", "Which sister", "which of your sister", "Which of your sister"]
    assert any([expected_response in response for expected_response in expected_responses]), (
        "Expected chat actor to ask for clarification in response, but got: " + response
    )


def test_filter_questions():
    test_questions = [
        "I don't know how to answer that",
        "I cannot answer anything about the nuclear secrets",
        "Who is on the basketball team?",
    ]
    filtered_questions = filter_questions(test_questions)
    assert len(filtered_questions) == 1
    assert filtered_questions[0] == "Who is on the basketball team?"


# Helpers
# ----------------------------------------------------------------------------------------------------
def populate_chat_history(message_list):
    # Generate conversation logs
    conversation_log = {"chat": []}
    for user_message, chat_response, context in message_list:
        message_to_log(
            user_message,
            chat_response,
            {"context": context, "intent": {"query": user_message, "inferred-queries": f'["{user_message}"]'}},
            conversation_log=conversation_log["chat"],
        )
    return conversation_log
