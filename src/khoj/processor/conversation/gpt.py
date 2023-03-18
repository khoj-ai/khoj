# Standard Packages
import os
import json
import logging
from datetime import datetime

# External Packages
import openai

# Internal Packages
from khoj.utils.constants import empty_escape_sequences
from khoj.processor.conversation.utils import (
    message_to_prompt,
    message_to_chatml,
    generate_chatml_messages_with_context,
)


logger = logging.getLogger(__name__)


def answer(text, user_query, model, api_key=None, temperature=0.5, max_tokens=500):
    """
    Answer user query using provided text as reference with OpenAI's GPT
    """
    # Initialize Variables
    openai.api_key = api_key or os.getenv("OPENAI_API_KEY")

    # Setup Prompt based on Summary Type
    prompt = f"""
You are a friendly, helpful personal assistant.
Using the users notes below, answer their following question. If the answer is not contained within the notes, say "I don't know."

Notes:
{text}

Question: {user_query}

Answer (in second person):"""
    # Get Response from GPT
    logger.debug(f"Prompt for GPT: {prompt}")
    response = openai.Completion.create(
        prompt=prompt, model=model, temperature=temperature, max_tokens=max_tokens, stop='"""'
    )

    # Extract, Clean Message from GPT's Response
    story = response["choices"][0]["text"]
    return str(story).replace("\n\n", "")


def summarize(text, summary_type, model, user_query=None, api_key=None, temperature=0.5, max_tokens=200):
    """
    Summarize user input using OpenAI's GPT
    """
    # Initialize Variables
    openai.api_key = api_key or os.getenv("OPENAI_API_KEY")

    # Setup Prompt based on Summary Type
    if summary_type == "chat":
        prompt = f"""
You are an AI. Summarize the conversation below from your perspective:

{text}

Summarize the conversation from the AI's first-person perspective:"""
    elif summary_type == "notes":
        prompt = f"""
Summarize the below notes about {user_query}:

{text}

Summarize the notes in second person perspective:"""

    # Get Response from GPT
    logger.debug(f"Prompt for GPT: {prompt}")
    response = openai.Completion.create(
        prompt=prompt, model=model, temperature=temperature, max_tokens=max_tokens, frequency_penalty=0.2, stop='"""'
    )

    # Extract, Clean Message from GPT's Response
    story = response["choices"][0]["text"]
    return str(story).replace("\n\n", "")


def extract_questions(message, model="gpt-3.5-turbo", conversation_log={}, api_key=None, temperature=0):
    """
    Infer search queries to retrieve relevant notes to respond to user's message
    """
    # Initialize Variables
    openai.api_key = api_key or os.getenv("OPENAI_API_KEY")

    # Get dates relative to today for prompt creation
    today = datetime.today()
    current_new_year = today.replace(month=1, day=1)
    last_new_year = current_new_year.replace(year=today.year - 1)

    personality_primer = f"""
You are Khoj, an extremely smart and helpful search assistant with the ability to retrieve information from the users notes.
The user will provide their questions and answers to you for context.
You can:
- Add as much context from the previous questions and answers as required into your search queries.
- Break messages into multiple search queries when required to retrieve the relevant information.
- Add date filters to your search queries from questions and answers when required to retrieve the relevant information.

What searches, if any, will you need to perform to answer the users question?
Provide search queries as a JSON list of strings
Current Date: {today.strftime("%H:%M %A %Y-%m-%d")}"""

    # Extract Past User Message and Inferred Questions from Conversation Log
    # fmt: off
    messages = [
        # Add system message to prime GPT for the task
        message_to_chatml(personality_primer, role="system"),

        # Add example user message, inferred question and answer triplets to explain the task
        message_to_chatml("Q: How was my trip to Cambodia?", role="user"),
        message_to_chatml('["How was my trip to Cambodia?"]', role="assistant"),
        message_to_chatml("A: The trip was amazing. I went to the Angkor Wat temple in August 2017 and it was beautiful.", role="user"),

        message_to_chatml("Q: Who did I visit that temple with?", role="user"),
        message_to_chatml('["Who did I visit the Angkor Wat Temple in Cambodia with? dt>=\\"2017-08-01\\" dt<\\"2017-09-01\\""]', role="assistant"),
        message_to_chatml("A: You visited the Angkor Wat Temple in Cambodia with Pablo, Namita and Xi.", role="user"),

        message_to_chatml("Q: What national parks did I go to last year?", role="user"),
        message_to_chatml(
            f'["National park I visited in {last_new_year.strftime("%Y")} dt>=\\"{last_new_year.strftime("%Y-%m-%d")}\\" dt<\\"{current_new_year.strftime("%Y-%m-%d")}\\""]',
            role="assistant",
        ),
        message_to_chatml(
            f'A: You visited the Grand Canyon and Yellowstone National Park in {last_new_year.strftime("%Y")}.',
            role="user",
        ),

        message_to_chatml("Q: How are you feeling?", role="user"),
        message_to_chatml("[]", role="assistant"),
        message_to_chatml("A: I'm feeling a little bored. Helping you will hopefully make me feel better!", role="user"),

        message_to_chatml("Q: How many tennis balls fit in the back of a 2002 Honda Civic?", role="user"),
        message_to_chatml('["What is the size of a tennis ball?", "What is the trunk size of a 2002 Honda Civic?"]', role="assistant"),
        message_to_chatml("A: 1085 tennis balls will fit in the trunk of a Honda Civic", role="user"),

        message_to_chatml("Q: Is Bob older than Tom?", role="user"),
        message_to_chatml('["When was Bob born?", "What is Tom\'s age?"]', role="assistant"),
        message_to_chatml("A: Yes, Bob is older than Tom. As Bob was born on 1984-01-01 and Tom is 30 years old.", role="user"),

        message_to_chatml("Q: What is their age difference?", role="user"),
        message_to_chatml('["What is Bob\'s age?", "What is Tom\'s age?"]', role="assistant"),
        message_to_chatml(
            f"A: Bob is {current_new_year.year - 1984 - 30} years older than Tom. As Bob is {current_new_year.year - 1984} years old and Tom is 30 years old.",
            role="user",
        ),
    ]
    # fmt: on

    # Add last few user messages, inferred queries and answer triplets from actual conversation for context
    for chat in conversation_log.get("chat", [])[-4:]:
        if chat["by"] == "khoj":
            queries = (
                chat["intent"]["inferred-queries"]
                if chat["intent"].get("inferred-queries", "[]") != "[]"
                else [chat["intent"]["query"]]
            )
            messages.extend(
                [
                    message_to_chatml(f'Q: {chat["intent"]["query"]}', role="user"),
                    message_to_chatml(f"{queries}", role="assistant"),
                    message_to_chatml(f'{chat["message"]}', role="user"),
                ]
            )

    # Finally add current user message for which to infer search queries to ChatML message list
    messages.append(message_to_chatml(f"Q: {message}", role="user"))

    # Get Response from GPT
    response = openai.ChatCompletion.create(
        messages=messages,
        model=model,
        temperature=temperature,
    )

    # Extract, Clean Message from GPT's Response
    response_text = response["choices"][0]["message"]["content"]
    try:
        questions = json.loads(
            # Clean response to increase likelihood of valid JSON. E.g replace ' with " to enclose strings
            response_text.strip(empty_escape_sequences)
            .replace("['", '["')
            .replace("']", '"]')
            .replace("', '", '", "')
        )
    except json.decoder.JSONDecodeError:
        logger.warn(f"GPT returned invalid JSON. Set question to empty list.\n{response_text}")
        questions = [text]
    logger.debug(f"Extracted Questions by GPT: {questions}")
    return questions


def extract_search_type(text, model, api_key=None, temperature=0.5, max_tokens=100, verbose=0):
    """
    Extract search type from user query using OpenAI's GPT
    """
    # Initialize Variables
    openai.api_key = api_key or os.getenv("OPENAI_API_KEY")
    understand_primer = """
Objective: Extract search type from user query and return information as JSON

Allowed search types are listed below:
  - search-type=["notes","ledger","image","music"]

Some examples are given below for reference:
Q:What fiction book was I reading last week about AI starship?
A:{ "search-type": "notes" }
Q:Play some calm classical music?
A:{ "search-type": "music" }
Q:How much did I spend at Subway for dinner last time?
A:{ "search-type": "ledger" }
Q:What was that popular Sri lankan song that Alex had mentioned?
A:{ "search-type": "music" }
Q:Can you recommend a movie to watch from my notes?
A:{ "search-type": "notes" }
Q: When did I buy Groceries last?
A:{ "search-type": "ledger" }
Q:When did I go surfing last?
A:{ "search-type": "notes" }"""

    # Setup Prompt with Understand Primer
    prompt = message_to_prompt(text, understand_primer, start_sequence="\nA:", restart_sequence="\nQ:")
    if verbose > 1:
        print(f"Message -> Prompt: {text} -> {prompt}")

    # Get Response from GPT
    logger.debug(f"Prompt for GPT: {prompt}")
    response = openai.Completion.create(
        prompt=prompt, model=model, temperature=temperature, max_tokens=max_tokens, frequency_penalty=0.2, stop=["\n"]
    )

    # Extract, Clean Message from GPT's Response
    story = str(response["choices"][0]["text"])
    return json.loads(story.strip(empty_escape_sequences))


def converse(text, user_query, conversation_log={}, api_key=None, temperature=0.2):
    """
    Converse with user using OpenAI's ChatGPT
    """
    # Initialize Variables
    model = "gpt-3.5-turbo"
    openai.api_key = api_key or os.getenv("OPENAI_API_KEY")

    personality_primer = "You are Khoj, a friendly, smart and helpful personal assistant."
    conversation_primer = f"""
Using the notes and our past conversations as context, answer the following question.
Current Date: {datetime.now().strftime("%Y-%m-%d")}

Notes:
{text}

Question: {user_query}"""

    # Setup Prompt with Primer or Conversation History
    messages = generate_chatml_messages_with_context(
        conversation_primer,
        personality_primer,
        conversation_log,
    )

    # Get Response from GPT
    logger.debug(f"Conversation Context for GPT: {messages}")
    response = openai.ChatCompletion.create(
        messages=messages,
        model=model,
        temperature=temperature,
    )

    # Extract, Clean Message from GPT's Response
    story = str(response["choices"][0]["message"]["content"])
    return story.strip(empty_escape_sequences)
