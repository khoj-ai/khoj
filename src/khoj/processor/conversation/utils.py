# Standard Packages
from datetime import datetime

# Internal Packages
from khoj.utils.helpers import merge_dicts


def generate_chatml_messages_with_context(user_message, system_message, conversation_log={}):
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
