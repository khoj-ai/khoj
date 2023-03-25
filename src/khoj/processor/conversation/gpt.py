# Standard Packages
import json
import logging
from datetime import datetime

# Internal Packages
from khoj.utils.constants import empty_escape_sequences
from khoj.processor.conversation.utils import (
    chat_completion_with_backoff,
    completion_with_backoff,
    message_to_prompt,
    generate_chatml_messages_with_context,
)


logger = logging.getLogger(__name__)


def answer(text, user_query, model, api_key=None, temperature=0.5, max_tokens=500):
    """
    Answer user query using provided text as reference with OpenAI's GPT
    """
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
    response = completion_with_backoff(
        prompt=prompt,
        model=model,
        temperature=temperature,
        max_tokens=max_tokens,
        stop='"""',
        api_key=api_key,
    )

    # Extract, Clean Message from GPT's Response
    story = response["choices"][0]["text"]
    return str(story).replace("\n\n", "")


def summarize(text, summary_type, model, user_query=None, api_key=None, temperature=0.5, max_tokens=200):
    """
    Summarize user input using OpenAI's GPT
    """
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
    response = completion_with_backoff(
        prompt=prompt,
        model=model,
        temperature=temperature,
        max_tokens=max_tokens,
        frequency_penalty=0.2,
        stop='"""',
        api_key=api_key,
    )

    # Extract, Clean Message from GPT's Response
    story = response["choices"][0]["text"]
    return str(story).replace("\n\n", "")


def extract_questions(text, model="text-davinci-003", conversation_log={}, api_key=None, temperature=0, max_tokens=100):
    """
    Infer search queries to retrieve relevant notes to answer user query
    """
    # Extract Past User Message and Inferred Questions from Conversation Log
    chat_history = "".join(
        [
            f'Q: {chat["intent"]["query"]}\n\n{chat["intent"].get("inferred-queries") or list([chat["intent"]["query"]])}\n\n{chat["message"]}\n\n'
            for chat in conversation_log.get("chat", [])[-4:]
            if chat["by"] == "khoj"
        ]
    )

    # Get dates relative to today for prompt creation
    today = datetime.today()
    current_new_year = today.replace(month=1, day=1)
    last_new_year = current_new_year.replace(year=today.year - 1)

    prompt = f"""
You are Khoj, an extremely smart and helpful search assistant with the ability to retrieve information from the users notes.
- The user will provide their questions and answers to you for context.
- Add as much context from the previous questions and answers as required into your search queries.
- Break messages into multiple search queries when required to retrieve the relevant information.
- Add date filters to your search queries from questions and answers when required to retrieve the relevant information.

What searches, if any, will you need to perform to answer the users question?
Provide search queries as a JSON list of strings
Current Date: {today.strftime("%A, %Y-%m-%d")}

Q: How was my trip to Cambodia?

["How was my trip to Cambodia?"]

A: The trip was amazing. I went to the Angkor Wat temple and it was beautiful.

Q: Who did i visit that temple with?

["Who did I visit the Angkor Wat Temple in Cambodia with?"]

A: You visited the Angkor Wat Temple in Cambodia with Pablo, Namita and Xi.

Q: What national parks did I go to last year?

["National park I visited in {last_new_year.strftime("%Y")} dt>=\\"{last_new_year.strftime("%Y-%m-%d")}\\" dt<\\"{current_new_year.strftime("%Y-%m-%d")}\\""]

A: You visited the Grand Canyon and Yellowstone National Park in {last_new_year.strftime("%Y")}.

Q: How are you feeling today?

[]

A: I'm feeling a little bored. Helping you will hopefully make me feel better!

Q: How many tennis balls fit in the back of a 2002 Honda Civic?

["What is the size of a tennis ball?", "What is the trunk size of a 2002 Honda Civic?"]

A: 1085 tennis balls will fit in the trunk of a Honda Civic

Q: Is Bob older than Tom?

["When was Bob born?", "What is Tom's age?"]

A: Yes, Bob is older than Tom. As Bob was born on 1984-01-01 and Tom is 30 years old.

Q: What is their age difference?

["What is Bob's age?", "What is Tom's age?"]

A: Bob is {current_new_year.year - 1984 - 30} years older than Tom. As Bob is {current_new_year.year - 1984} years old and Tom is 30 years old.

{chat_history}
Q: {text}

"""

    # Get Response from GPT
    response = completion_with_backoff(
        prompt=prompt,
        model=model,
        temperature=temperature,
        max_tokens=max_tokens,
        stop=["A: ", "\n"],
        api_key=api_key,
    )

    # Extract, Clean Message from GPT's Response
    response_text = response["choices"][0]["text"]
    try:
        questions = json.loads(
            # Clean response to increase likelihood of valid JSON. E.g replace ' with " to enclose strings
            response_text.strip(empty_escape_sequences)
            .replace("['", '["')
            .replace("']", '"]')
            .replace("', '", '", "')
        )
    except json.decoder.JSONDecodeError:
        logger.warn(f"GPT returned invalid JSON. Falling back to using user message as search query.\n{response_text}")
        questions = [text]
    logger.debug(f"Extracted Questions by GPT: {questions}")
    return questions


def extract_search_type(text, model, api_key=None, temperature=0.5, max_tokens=100, verbose=0):
    """
    Extract search type from user query using OpenAI's GPT
    """
    # Initialize Variables
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
    response = completion_with_backoff(
        prompt=prompt,
        model=model,
        temperature=temperature,
        max_tokens=max_tokens,
        frequency_penalty=0.2,
        stop=["\n"],
        api_key=api_key,
    )

    # Extract, Clean Message from GPT's Response
    story = str(response["choices"][0]["text"])
    return json.loads(story.strip(empty_escape_sequences))


def converse(references, user_query, conversation_log={}, api_key=None, temperature=0.2):
    """
    Converse with user using OpenAI's ChatGPT
    """
    # Initialize Variables
    model = "gpt-3.5-turbo"
    compiled_references = "\n\n".join({f"# {item}" for item in references})

    personality_primer = "You are Khoj, a friendly, smart and helpful personal assistant."
    conversation_primer = f"""
Using the notes and our past conversations as context, answer the following question.
Current Date: {datetime.now().strftime("%Y-%m-%d")}

Notes:
{compiled_references}

Question: {user_query}"""

    # Setup Prompt with Primer or Conversation History
    messages = generate_chatml_messages_with_context(
        conversation_primer,
        personality_primer,
        conversation_log,
        model,
    )

    # Get Response from GPT
    logger.debug(f"Conversation Context for GPT: {messages}")
    response = chat_completion_with_backoff(
        messages=messages,
        model=model,
        temperature=temperature,
        api_key=api_key,
    )

    # Extract, Clean Message from GPT's Response
    story = str(response["choices"][0]["message"]["content"])
    return story.strip(empty_escape_sequences)
