from copy import deepcopy

import tiktoken
from langchain_core.messages.chat import ChatMessage

from khoj.processor.conversation import utils


class TestTruncateMessage:
    max_prompt_size = 40
    model_name = "gpt-4o-mini"
    encoder = tiktoken.encoding_for_model(model_name)

    def test_truncate_message_all_small(self):
        # Arrange
        chat_history = generate_chat_history(50)

        # Act
        truncated_chat_history = utils.truncate_messages(chat_history, self.max_prompt_size, self.model_name)
        tokens = sum([utils.count_tokens(message.content, self.encoder) for message in truncated_chat_history])

        # Assert
        # The original object has been modified. Verify certain properties
        assert len(chat_history) < 50
        assert len(chat_history) > 5
        assert tokens <= self.max_prompt_size

    def test_truncate_message_only_oldest_big(self):
        # Arrange
        chat_history = generate_chat_history(5)
        big_chat_message = ChatMessage(role="user", content=generate_content(100, suffix="Question?"))
        chat_history.append(big_chat_message)

        # Act
        truncated_chat_history = utils.truncate_messages(chat_history, self.max_prompt_size, self.model_name)
        tokens = sum([utils.count_tokens(message.content, self.encoder) for message in truncated_chat_history])

        # Assert
        # The original object has been modified. Verify certain properties
        assert len(chat_history) == 5
        assert tokens <= self.max_prompt_size

    def test_truncate_message_with_image(self):
        # Arrange
        image_content_item = {"type": "image_url", "image_url": {"url": "placeholder"}}
        content_list = [{"type": "text", "text": f"{index}"} for index in range(100)]
        content_list += [image_content_item, {"type": "text", "text": "Question?"}]
        big_chat_message = ChatMessage(role="user", content=content_list)
        copy_big_chat_message = deepcopy(big_chat_message)
        chat_history = [big_chat_message]
        tokens = sum([utils.count_tokens(message.content, self.encoder) for message in chat_history])

        # Act
        truncated_chat_history = utils.truncate_messages(chat_history, self.max_prompt_size, self.model_name)
        tokens = sum([utils.count_tokens(message.content, self.encoder) for message in truncated_chat_history])

        # Assert
        # The original object has been modified. Verify certain properties
        assert truncated_chat_history[0] != copy_big_chat_message, "Original message should be modified"
        assert truncated_chat_history[0].content[-1]["text"] == "Question?", "Query should be preserved"
        assert tokens <= self.max_prompt_size, "Truncated message should be within max prompt size"

    def test_truncate_message_with_content_list(self):
        # Arrange
        chat_history = generate_chat_history(5)
        content_list = [{"type": "text", "text": f"{index}"} for index in range(100)]
        content_list += [{"type": "text", "text": "Question?"}]
        big_chat_message = ChatMessage(role="user", content=content_list)
        copy_big_chat_message = deepcopy(big_chat_message)
        chat_history.insert(0, big_chat_message)
        tokens = sum([utils.count_tokens(message.content, self.encoder) for message in chat_history])

        # Act
        truncated_chat_history = utils.truncate_messages(chat_history, self.max_prompt_size, self.model_name)
        tokens = sum([utils.count_tokens(message.content, self.encoder) for message in truncated_chat_history])

        # Assert
        # The original object has been modified. Verify certain properties
        assert (
            len(chat_history) == 1
        ), "Only most recent message should be present as it itself is larger than context size"
        assert len(truncated_chat_history[0].content) < len(
            copy_big_chat_message.content
        ), "message content list should be modified"
        assert truncated_chat_history[0].content[-1]["text"] == "Question?", "Query should be preserved"
        assert tokens <= self.max_prompt_size, "Truncated message should be within max prompt size"

    def test_truncate_message_first_large(self):
        # Arrange
        chat_history = generate_chat_history(5)
        big_chat_message = ChatMessage(role="user", content=generate_content(100, suffix="Question?"))
        copy_big_chat_message = big_chat_message.copy()
        chat_history.insert(0, big_chat_message)
        tokens = sum([utils.count_tokens(message.content, self.encoder) for message in chat_history])

        # Act
        truncated_chat_history = utils.truncate_messages(chat_history, self.max_prompt_size, self.model_name)
        tokens = sum([utils.count_tokens(message.content, self.encoder) for message in truncated_chat_history])

        # Assert
        # The original object has been modified. Verify certain properties
        assert (
            len(chat_history) == 1
        ), "Only most recent message should be present as it itself is larger than context size"
        assert truncated_chat_history[0] != copy_big_chat_message, "Original message should be modified"
        assert truncated_chat_history[0].content[0]["text"].endswith("\nQuestion?"), "Query should be preserved"
        assert tokens <= self.max_prompt_size, "Truncated message should be within max prompt size"

    def test_truncate_message_large_system_message_first(self):
        # Arrange
        chat_history = generate_chat_history(5)
        chat_history[0].role = "system"  # Mark the first message as system message
        big_chat_message = ChatMessage(role="user", content=generate_content(100, suffix="Question?"))
        copy_big_chat_message = big_chat_message.copy()

        chat_history.insert(0, big_chat_message)
        initial_tokens = sum([utils.count_tokens(message.content, self.encoder) for message in chat_history])

        # Act
        truncated_chat_history = utils.truncate_messages(chat_history, self.max_prompt_size, self.model_name)
        final_tokens = sum([utils.count_tokens(message.content, self.encoder) for message in truncated_chat_history])

        # Assert
        # The original object has been modified. Verify certain properties.
        assert (
            len(truncated_chat_history) == len(chat_history) + 1
        )  # Because the system_prompt is popped off from the chat_messages list
        assert len(truncated_chat_history) < 10
        assert len(truncated_chat_history) > 1
        assert truncated_chat_history[0] != copy_big_chat_message, "Original message should be modified"
        assert truncated_chat_history[0].content[0]["text"].endswith("\nQuestion?"), "Query should be preserved"
        assert initial_tokens > self.max_prompt_size, "Initial tokens should be greater than max prompt size"
        assert final_tokens <= self.max_prompt_size, "Final tokens should be within max prompt size"

    def test_truncate_single_large_non_system_message(self):
        # Arrange
        big_chat_message = ChatMessage(role="user", content=generate_content(100, suffix="Question?"))
        copy_big_chat_message = big_chat_message.copy()
        chat_messages = [big_chat_message]
        initial_tokens = sum([utils.count_tokens(message.content, self.encoder) for message in chat_messages])

        # Act
        truncated_chat_history = utils.truncate_messages(chat_messages, self.max_prompt_size, self.model_name)
        final_tokens = sum([utils.count_tokens(message.content, self.encoder) for message in truncated_chat_history])

        # Assert
        # The original object has been modified. Verify certain properties
        assert initial_tokens > self.max_prompt_size, "Initial tokens should be greater than max prompt size"
        assert final_tokens <= self.max_prompt_size, "Final tokens should be within max prompt size"
        assert (
            len(chat_messages) == 1
        ), "Only most recent message should be present as it itself is larger than context size"
        assert truncated_chat_history[0] != copy_big_chat_message, "Original message should be modified"
        assert truncated_chat_history[0].content[0]["text"].endswith("\nQuestion?"), "Query should be preserved"

    def test_truncate_single_large_question(self):
        # Arrange
        big_chat_message_content = [{"type": "text", "text": " ".join(["hi"] * (self.max_prompt_size + 1))}]
        big_chat_message = ChatMessage(role="user", content=big_chat_message_content)
        copy_big_chat_message = big_chat_message.copy()
        chat_messages = [big_chat_message]
        initial_tokens = sum([utils.count_tokens(message.content, self.encoder) for message in chat_messages])

        # Act
        truncated_chat_history = utils.truncate_messages(chat_messages, self.max_prompt_size, self.model_name)
        final_tokens = sum([utils.count_tokens(message.content, self.encoder) for message in truncated_chat_history])

        # Assert
        # The original object has been modified. Verify certain properties
        assert initial_tokens > self.max_prompt_size, "Initial tokens should be greater than max prompt size"
        assert final_tokens <= self.max_prompt_size, "Final tokens should be within max prompt size"
        assert (
            len(chat_messages) == 1
        ), "Only most recent message should be present as it itself is larger than context size"
        assert truncated_chat_history[0] != copy_big_chat_message, "Original message should be modified"


class TestLoadComplexJson:
    def test_load_complex_raw_json_string(self):
        # Arrange
        raw_json = r"""{"key": "value with unescaped " and unescaped \' and escaped \" and escaped \\'"}"""
        expected_json = {"key": "value with unescaped \" and unescaped \\' and escaped \" and escaped \\'"}

        # Act
        parsed_json = utils.load_complex_json(raw_json)

        # Assert
        assert parsed_json == expected_json

    def test_load_complex_json_with_python_code(self):
        # Arrange
        raw_json = r"""{"python": "import os\nvalue = \"\"\"\nfirst line of "text"\nsecond line of 'text'\n\"\"\"\nprint(value)"}"""
        expected_json = {
            "python": 'import os\nvalue = """\nfirst line of "text"\nsecond line of \'text\'\n"""\nprint(value)'
        }

        # Act
        parsed_json = utils.load_complex_json(raw_json)

        # Assert
        assert parsed_json == expected_json

    def test_load_complex_json_inline(self):
        # Arrange
        raw_json = """
    {"key1": "value1", "key2": "value2"}plain text suffix
    """
        expected_json = {
            "key1": "value1",
            "key2": "value2",
        }

        # Act
        parsed_json = utils.load_complex_json(raw_json)

        # Assert
        assert parsed_json == expected_json


def generate_content(count, suffix=""):
    return [{"type": "text", "text": " ".join([f"{index}" for index, _ in enumerate(range(count))]) + "\n" + suffix}]


def generate_chat_history(count):
    return [
        ChatMessage(role="user" if index % 2 == 0 else "assistant", content=[{"type": "text", "text": f"{index}"}])
        for index, _ in enumerate(range(count))
    ]
