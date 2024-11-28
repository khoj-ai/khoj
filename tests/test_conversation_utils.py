import tiktoken
from langchain.schema import ChatMessage

from khoj.processor.conversation import utils


class TestTruncateMessage:
    max_prompt_size = 10
    model_name = "gpt-4o-mini"
    encoder = tiktoken.encoding_for_model(model_name)

    def test_truncate_message_all_small(self):
        # Arrange
        chat_history = generate_chat_history(50)

        # Act
        truncated_chat_history = utils.truncate_messages(chat_history, self.max_prompt_size, self.model_name)
        tokens = sum([len(self.encoder.encode(message.content)) for message in truncated_chat_history])

        # Assert
        # The original object has been modified. Verify certain properties
        assert len(chat_history) < 50
        assert len(chat_history) > 1
        assert tokens <= self.max_prompt_size

    def test_truncate_message_first_large(self):
        # Arrange
        chat_history = generate_chat_history(5)
        big_chat_message = ChatMessage(role="user", content=f"{generate_content(6)}\nQuestion?")
        copy_big_chat_message = big_chat_message.copy()
        chat_history.insert(0, big_chat_message)
        tokens = sum([len(self.encoder.encode(message.content)) for message in chat_history])

        # Act
        truncated_chat_history = utils.truncate_messages(chat_history, self.max_prompt_size, self.model_name)
        tokens = sum([len(self.encoder.encode(message.content)) for message in truncated_chat_history])

        # Assert
        # The original object has been modified. Verify certain properties
        assert len(chat_history) == 1
        assert truncated_chat_history[0] != copy_big_chat_message
        assert tokens <= self.max_prompt_size

    def test_truncate_message_last_large(self):
        # Arrange
        chat_history = generate_chat_history(5)
        chat_history[0].role = "system"  # Mark the first message as system message
        big_chat_message = ChatMessage(role="user", content=f"{generate_content(11)}\nQuestion?")
        copy_big_chat_message = big_chat_message.copy()

        chat_history.insert(0, big_chat_message)
        initial_tokens = sum([len(self.encoder.encode(message.content)) for message in chat_history])

        # Act
        truncated_chat_history = utils.truncate_messages(chat_history, self.max_prompt_size, self.model_name)
        final_tokens = sum([len(self.encoder.encode(message.content)) for message in truncated_chat_history])

        # Assert
        # The original object has been modified. Verify certain properties.
        assert (
            len(truncated_chat_history) == len(chat_history) + 1
        )  # Because the system_prompt is popped off from the chat_messages list
        assert len(truncated_chat_history) < 10
        assert len(truncated_chat_history) > 1
        assert truncated_chat_history[0] != copy_big_chat_message
        assert initial_tokens > self.max_prompt_size
        assert final_tokens <= self.max_prompt_size

    def test_truncate_single_large_non_system_message(self):
        # Arrange
        big_chat_message = ChatMessage(role="user", content=f"{generate_content(11)}\nQuestion?")
        copy_big_chat_message = big_chat_message.copy()
        chat_messages = [big_chat_message]
        initial_tokens = sum([len(self.encoder.encode(message.content)) for message in chat_messages])

        # Act
        truncated_chat_history = utils.truncate_messages(chat_messages, self.max_prompt_size, self.model_name)
        final_tokens = sum([len(self.encoder.encode(message.content)) for message in truncated_chat_history])

        # Assert
        # The original object has been modified. Verify certain properties
        assert initial_tokens > self.max_prompt_size
        assert final_tokens <= self.max_prompt_size
        assert len(chat_messages) == 1
        assert truncated_chat_history[0] != copy_big_chat_message

    def test_truncate_single_large_question(self):
        # Arrange
        big_chat_message_content = " ".join(["hi"] * (self.max_prompt_size + 1))
        big_chat_message = ChatMessage(role="user", content=big_chat_message_content)
        copy_big_chat_message = big_chat_message.copy()
        chat_messages = [big_chat_message]
        initial_tokens = sum([len(self.encoder.encode(message.content)) for message in chat_messages])

        # Act
        truncated_chat_history = utils.truncate_messages(chat_messages, self.max_prompt_size, self.model_name)
        final_tokens = sum([len(self.encoder.encode(message.content)) for message in truncated_chat_history])

        # Assert
        # The original object has been modified. Verify certain properties
        assert initial_tokens > self.max_prompt_size
        assert final_tokens <= self.max_prompt_size
        assert len(chat_messages) == 1
        assert truncated_chat_history[0] != copy_big_chat_message


def test_load_complex_raw_json_string():
    # Arrange
    raw_json = r"""{"key": "value with unescaped " and unescaped \' and escaped \" and escaped \\'"}"""
    expeced_json = {"key": "value with unescaped \" and unescaped \\' and escaped \" and escaped \\'"}

    # Act
    parsed_json = utils.load_complex_json(raw_json)

    # Assert
    assert parsed_json == expeced_json


def generate_content(count):
    return " ".join([f"{index}" for index, _ in enumerate(range(count))])


def generate_chat_history(count):
    return [
        ChatMessage(role="user" if index % 2 == 0 else "assistant", content=f"{index}")
        for index, _ in enumerate(range(count))
    ]
