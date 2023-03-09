# Standard Packages
import os
import json
from datetime import datetime

# External Packages
import openai

# Internal Packages
from khoj.utils.constants import empty_escape_sequences
from khoj.utils.helpers import merge_dicts


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
    response = openai.Completion.create(
        prompt=prompt, model=model, temperature=temperature, max_tokens=max_tokens, frequency_penalty=0.2, stop=["\n"]
    )

    # Extract, Clean Message from GPT's Response
    story = str(response["choices"][0]["text"])
    return json.loads(story.strip(empty_escape_sequences))


def converse(text, user_query, conversation_log=None, api_key=None, temperature=0):
    """
    Converse with user using OpenAI's ChatGPT
    """
    # Initialize Variables
    model = "gpt-3.5-turbo"
    openai.api_key = api_key or os.getenv("OPENAI_API_KEY")

    personality_primer = "You are a friendly, helpful personal assistant."
    conversation_primer = f"""
Using our chats and notes as context, answer the following question.
If the answer is not contained within the provided context, say "I don't know." and provide reason.
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
    response = openai.ChatCompletion.create(
        messages=messages,
        model=model,
        temperature=temperature,
    )

    # Extract, Clean Message from GPT's Response
    story = str(response["choices"][0]["message"]["content"])
    return story.strip(empty_escape_sequences)


def generate_chatml_messages_with_context(user_message, system_message, conversation_log=None):
    """Generate messages for ChatGPT with context from previous conversation"""
    # Extract Chat History for Context
    chat_logs = [f'{chat["message"]}\n\nNotes:\n{chat.get("context","")}' for chat in conversation_log.get("chat", [])]
    last_backnforth = reciprocal_conversation_to_chatml(chat_logs[-2:])
    rest_backnforth = reciprocal_conversation_to_chatml(chat_logs[-4:-2])

    # Format user and system messages to chatml format
    system_chatml_message = [message_to_chatml(system_message, "system")]
    user_chatml_message = [message_to_chatml(user_message, "user")]

    return rest_backnforth + system_chatml_message + last_backnforth + user_chatml_message


def reciprocal_conversation_to_chatml(message_pair):
    """Convert a single back and forth between user and assistant to chatml format"""
    return [message_to_chatml(message, role) for message, role in zip(message_pair, ["user", "assistant"])]


def message_to_chatml(message, role="assistant"):
    """Create chatml message from message and role"""
    return {"role": role, "content": message}


def message_to_prompt(
    user_message, conversation_history="", gpt_message=None, start_sequence="\nAI:", restart_sequence="\nHuman:"
):
    """Create prompt for GPT from messages and conversation history"""
    gpt_message = f" {gpt_message}" if gpt_message else ""

    return f"{conversation_history}{restart_sequence} {user_message}{start_sequence}{gpt_message}"


def message_to_log(user_message, gpt_message, khoj_message_metadata={}, conversation_log=[]):
    """Create json logs from messages, metadata for conversation log"""
    default_khoj_message_metadata = {
        "intent": {"type": "remember", "memory-type": "notes", "query": user_message},
        "trigger-emotion": "calm",
    }
    current_dt = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    # Create json log from Human's message
    human_log = {"message": user_message, "by": "you", "created": current_dt}

    # Create json log from GPT's response
    khoj_log = merge_dicts(khoj_message_metadata, default_khoj_message_metadata)
    khoj_log = merge_dicts({"message": gpt_message, "by": "khoj", "created": current_dt}, khoj_log)

    conversation_log.extend([human_log, khoj_log])
    return conversation_log


def extract_summaries(metadata):
    """Extract summaries from metadata"""
    return "".join([f'\n{session["summary"]}' for session in metadata])
