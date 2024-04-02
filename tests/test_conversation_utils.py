import factory
import tiktoken
from langchain.schema import ChatMessage

from khoj.processor.conversation import utils


class ChatMessageFactory(factory.Factory):
    class Meta:
        model = ChatMessage

    content = factory.Faker("paragraph")
    role = factory.Faker("name")


class TestTruncateMessage:
    max_prompt_size = 4096
    model_name = "gpt-3.5-turbo"
    encoder = tiktoken.encoding_for_model(model_name)

    def test_truncate_message_all_small(self):
        # Arrange
        chat_history = ChatMessageFactory.build_batch(500)

        # Act
        truncated_chat_history = utils.truncate_messages(chat_history, self.max_prompt_size, self.model_name)
        tokens = sum([len(self.encoder.encode(message.content)) for message in truncated_chat_history])

        # Assert
        # The original object has been modified. Verify certain properties
        assert len(chat_history) < 500
        assert len(chat_history) > 1
        assert tokens <= self.max_prompt_size

    def test_truncate_message_first_large(self):
        # Arrange
        chat_history = ChatMessageFactory.build_batch(25)
        big_chat_message = ChatMessageFactory.build(content=factory.Faker("paragraph", nb_sentences=2000))
        big_chat_message.content = big_chat_message.content + "\n" + "Question?"
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
        chat_history = ChatMessageFactory.build_batch(25)
        chat_history[0].role = "system"  # Mark the first message as system message
        big_chat_message = ChatMessageFactory.build(content=factory.Faker("paragraph", nb_sentences=1000))
        big_chat_message.content = big_chat_message.content + "\n" + "Question?"
        copy_big_chat_message = big_chat_message.copy()

        chat_history.insert(0, big_chat_message)
        initial_tokens = sum([len(self.encoder.encode(message.content)) for message in chat_history])

        # Act
        truncated_chat_history = utils.truncate_messages(chat_history, self.max_prompt_size, self.model_name)
        final_tokens = sum([len(self.encoder.encode(message.content)) for message in truncated_chat_history])

        # Assert
        # The original object has been modified. Verify certain properties.
        assert len(truncated_chat_history) == (
            len(chat_history) + 1
        )  # Because the system_prompt is popped off from the chat_messages lsit
        assert len(truncated_chat_history) < 26
        assert len(truncated_chat_history) > 1
        assert truncated_chat_history[0] != copy_big_chat_message
        assert initial_tokens > self.max_prompt_size
        assert final_tokens <= self.max_prompt_size

    def test_truncate_single_large_non_system_message(self):
        # Arrange
        big_chat_message = ChatMessageFactory.build(content=factory.Faker("paragraph", nb_sentences=2000))
        big_chat_message.content = big_chat_message.content + "\n" + "Question?"
        big_chat_message.role = "user"
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
        big_chat_message = ChatMessageFactory.build(content=big_chat_message_content)
        big_chat_message.role = "user"
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
