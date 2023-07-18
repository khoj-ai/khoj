# Standard Packages
import logging
from time import perf_counter
import json
from datetime import datetime

# Internal Packages
import queue
from khoj.utils.helpers import merge_dicts

logger = logging.getLogger(__name__)


class ThreadedGenerator:
    def __init__(self, compiled_references, completion_func=None):
        self.queue = queue.Queue()
        self.compiled_references = compiled_references
        self.completion_func = completion_func
        self.response = ""
        self.start_time = perf_counter()

    def __iter__(self):
        return self

    def __next__(self):
        item = self.queue.get()
        if item is StopIteration:
            time_to_response = perf_counter() - self.start_time
            logger.info(f"Chat streaming took: {time_to_response:.3f} seconds")
            if self.completion_func:
                # The completion func effectively acts as a callback.
                # It adds the aggregated response to the conversation history.
                self.completion_func(chat_response=self.response)
            raise StopIteration
        return item

    def send(self, data):
        self.response += data
        self.queue.put(data)

    def close(self):
        if self.compiled_references and len(self.compiled_references) > 0:
            self.queue.put(f"### compiled references:{json.dumps(self.compiled_references)}")
        self.queue.put(StopIteration)


def message_to_log(
    user_message, chat_response, user_message_metadata={}, khoj_message_metadata={}, conversation_log=[]
):
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
    khoj_log = merge_dicts({"message": chat_response, "by": "khoj", "created": khoj_response_time}, khoj_log)

    conversation_log.extend([human_log, khoj_log])
    return conversation_log
