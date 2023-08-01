from khoj.processor.conversation import utils
from langchain.schema import ChatMessage
import factory
import tiktoken


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
        chat_messages = ChatMessageFactory.build_batch(500)

        prompt = utils.truncate_messages(chat_messages, self.max_prompt_size, self.model_name)
        tokens = sum([len(self.encoder.encode(message.content)) for message in prompt])

        # The original object has been modified. Verify certain properties
        assert len(chat_messages) < 500
        assert len(chat_messages) > 1
        assert tokens <= self.max_prompt_size

    def test_truncate_message_first_large(self):
        chat_messages = ChatMessageFactory.build_batch(25)
        big_chat_message = ChatMessageFactory.build(content=factory.Faker("paragraph", nb_sentences=2000))
        big_chat_message.content = big_chat_message.content + "\n" + "Question?"
        copy_big_chat_message = big_chat_message.copy()
        chat_messages.insert(0, big_chat_message)
        tokens = sum([len(self.encoder.encode(message.content)) for message in chat_messages])

        prompt = utils.truncate_messages(chat_messages, self.max_prompt_size, self.model_name)
        tokens = sum([len(self.encoder.encode(message.content)) for message in prompt])

        # The original object has been modified. Verify certain properties
        assert len(chat_messages) == 1
        assert prompt[0] != copy_big_chat_message
        assert tokens <= self.max_prompt_size

    def test_truncate_message_last_large(self):
        chat_messages = ChatMessageFactory.build_batch(25)
        big_chat_message = ChatMessageFactory.build(content=factory.Faker("paragraph", nb_sentences=1000))
        big_chat_message.content = big_chat_message.content + "\n" + "Question?"
        copy_big_chat_message = big_chat_message.copy()

        chat_messages.insert(0, big_chat_message)
        tokens = sum([len(self.encoder.encode(message.content)) for message in chat_messages])

        prompt = utils.truncate_messages(chat_messages, self.max_prompt_size, self.model_name)
        tokens = sum([len(self.encoder.encode(message.content)) for message in prompt])

        # The original object has been modified. Verify certain properties
        assert len(prompt) < 26
        assert len(prompt) > 1
        assert prompt[0] != copy_big_chat_message
        assert tokens <= self.max_prompt_size
