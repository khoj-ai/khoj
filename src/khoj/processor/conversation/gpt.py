# Standard Packages
import os
import json
import logging
from datetime import datetime

# External Packages
import openai

# Internal Packages
from khoj.utils.constants import empty_escape_sequences
from khoj.processor.conversation.utils import message_to_prompt, generate_chatml_messages_with_context


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
