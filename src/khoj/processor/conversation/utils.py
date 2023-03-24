# Standard Packages
from datetime import datetime

# External Packages
import tiktoken

# Internal Packages
from khoj.utils.helpers import merge_dicts


max_prompt_size = {"gpt-3.5-turbo": 4096, "gpt-4": 8192}


def generate_chatml_messages_with_context(
    user_message, system_message, conversation_log={}, model_name="gpt-3.5-turbo", lookback_turns=2
):
    """Generate messages for ChatGPT with context from previous conversation"""
    # Extract Chat History for Context
    chat_logs = [f'{chat["message"]}\n\nNotes:\n{chat.get("context","")}' for chat in conversation_log.get("chat", [])]
    rest_backnforths = []
    # Extract in reverse chronological order
    for user_msg, assistant_msg in zip(chat_logs[-2::-2], chat_logs[::-2]):
        if len(rest_backnforths) >= 2 * lookback_turns:
            break
        rest_backnforths += reciprocal_conversation_to_chatml([user_msg, assistant_msg])[::-1]

    # Format user and system messages to chatml format
    system_chatml_message = [message_to_chatml(system_message, "system")]
    user_chatml_message = [message_to_chatml(user_message, "user")]

    messages = user_chatml_message + rest_backnforths[:2] + system_chatml_message + rest_backnforths[2:]

    # Truncate oldest messages from conversation history until under max supported prompt size by model
    encoder = tiktoken.encoding_for_model(model_name)
    tokens = sum([len(encoder.encode(value)) for message in messages for value in message.values()])
    while tokens > max_prompt_size[model_name]:
        messages.pop()
        tokens = sum([len(encoder.encode(value)) for message in messages for value in message.values()])

    # Return message in chronological order
    return messages[::-1]


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


def message_to_log(user_message, gpt_message, user_message_metadata={}, khoj_message_metadata={}, conversation_log=[]):
    """Create json logs from messages, metadata for conversation log"""
    default_khoj_message_metadata = {
        "intent": {"type": "remember", "memory-type": "notes", "query": user_message},
        "trigger-emotion": "calm",
    }
    khoj_response_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    # Create json log from Human's message
    human_log = merge_dicts({"message": user_message, "by": "you"}, user_message_metadata)

    # Create json log from GPT's response
    khoj_log = merge_dicts(khoj_message_metadata, default_khoj_message_metadata)
    khoj_log = merge_dicts({"message": gpt_message, "by": "khoj", "created": khoj_response_time}, khoj_log)

    conversation_log.extend([human_log, khoj_log])
    return conversation_log


def extract_summaries(metadata):
    """Extract summaries from metadata"""
    return "".join([f'\n{session["summary"]}' for session in metadata])
