from copy import deepcopy

import tiktoken
from langchain_core.messages.chat import ChatMessage

from khoj.database.models import Context, ChatMessageModel
from khoj.processor.conversation import prompts, utils


class TestChatHistoryContextReconstruction:
    """Tests for chat history context reconstruction in generate_chatml_messages_with_context"""

    def test_context_objects_are_serialized_in_chat_history(self):
        """
        Regression test: Context objects (Pydantic models) should be properly
        serialized when reconstructing chat history references.
        
        Bug: isinstance(item, dict) check at utils.py:750 incorrectly filters
        out all Context objects since they are Pydantic models, not dicts.
        """
        # Arrange: Create Context objects as Pydantic would after model_validate
        context_items = [
            Context(compiled="HueCode is a meta-marker system for fiducial markers", file="/docs/huecode.pdf"),
            Context(compiled="It uses optimal color schemes for illumination robustness", file="/docs/huecode2.pdf"),
        ]
        
        # Create a chat message with context (simulating loaded chat history)
        chat_message = ChatMessageModel(
            by="khoj",
            message="Here's what I found about HueCode in your documents.",
            context=context_items,
        )
        
        # Act: Generate chatml messages (this reconstructs chat history)
        messages = utils.generate_chatml_messages_with_context(
            user_message="tell me more about the color schemes",
            context_message="",
            chat_history=[chat_message],
            system_message="You are a helpful assistant.",
            model_name="gpt-4o-mini",
        )
        
        # Assert: Context from chat history should be present in messages
        all_content = " ".join(str(m.content) for m in messages)
        
        assert "HueCode is a meta-marker system" in all_content, (
            "First context item missing - isinstance(item, dict) bug filters out Context objects"
        )
        assert "optimal color schemes" in all_content, (
            "Second context item missing - isinstance(item, dict) bug filters out Context objects"
        )

    def test_empty_context_does_not_add_references(self):
        """Verify that empty context doesn't add spurious references to chat history"""
        # Arrange
        chat_message = ChatMessageModel(
            by="khoj",
            message="I don't have any relevant notes.",
            context=[],  # Empty context
        )
        
        # Act
        messages = utils.generate_chatml_messages_with_context(
            user_message="what about now?",
            context_message="",
            chat_history=[chat_message],
            system_message="You are a helpful assistant.",
            model_name="gpt-4o-mini",
        )
        
        # Assert: No "User's Notes" section should be added for empty context
        all_content = " ".join(str(m.content) for m in messages)
        # The notes_conversation prompt shouldn't appear for empty context
        assert all_content.count("User's Notes") == 0, "Empty context should not add notes section"


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
        # Verify certain properties of the truncated chat history
        assert len(truncated_chat_history) < 50
        assert len(truncated_chat_history) > 5
        assert tokens <= self.max_prompt_size

    def test_truncate_message_only_oldest_big(self):
        # Arrange
        chat_history = generate_chat_history(5)
        big_chat_message = ChatMessage(role="user", content=generate_content(100, suffix="Question?"))
        chat_history.insert(0, big_chat_message)

        # Act
        truncated_chat_history = utils.truncate_messages(chat_history, self.max_prompt_size, self.model_name)
        tokens = sum([utils.count_tokens(message.content, self.encoder) for message in truncated_chat_history])

        # Assert
        # Verify certain properties of the truncated chat history
        assert len(truncated_chat_history) == 5
        assert tokens <= self.max_prompt_size

    def test_truncate_message_with_image(self):
        # Arrange
        image_content_item = {"type": "image_url", "image_url": {"url": "placeholder"}}
        content_list = [{"type": "text", "text": f"{index}"} for index in range(100)]
        content_list += [image_content_item, {"type": "text", "text": "Question?"}]
        big_chat_message = ChatMessage(role="user", content=content_list)
        copy_big_chat_message = deepcopy(big_chat_message)
        chat_history = [big_chat_message]
        initial_tokens = sum([utils.count_tokens(message.content, self.encoder) for message in chat_history])

        # Act
        truncated_chat_history = utils.truncate_messages(chat_history, self.max_prompt_size, self.model_name)
        final_tokens = sum([utils.count_tokens(message.content, self.encoder) for message in truncated_chat_history])

        # Assert
        # Verify certain properties of the truncated chat history
        assert truncated_chat_history[0] != copy_big_chat_message, "Original message should be modified"
        assert truncated_chat_history[0].content[-1]["text"] == "Question?", "Query should be preserved"
        assert initial_tokens > self.max_prompt_size, "Initial tokens should be greater than max prompt size"
        assert final_tokens <= self.max_prompt_size, "Truncated message should be within max prompt size"

    def test_truncate_message_with_content_list(self):
        # Arrange
        chat_history = generate_chat_history(5)
        content_list = [{"type": "text", "text": f"{index}"} for index in range(100)]
        content_list += [{"type": "text", "text": "Question?"}]
        big_chat_message = ChatMessage(role="user", content=content_list)
        copy_big_chat_message = deepcopy(big_chat_message)
        chat_history.append(big_chat_message)
        initial_tokens = sum([utils.count_tokens(message.content, self.encoder) for message in chat_history])

        # Act
        truncated_chat_history = utils.truncate_messages(chat_history, self.max_prompt_size, self.model_name)
        final_tokens = sum([utils.count_tokens(message.content, self.encoder) for message in truncated_chat_history])

        # Assert
        # Verify certain properties of the truncated chat history
        assert len(truncated_chat_history) == 1, (
            "Only most recent message should be present as it itself is larger than context size"
        )
        assert len(truncated_chat_history[0].content) < len(copy_big_chat_message.content), (
            "message content list should be modified"
        )
        assert truncated_chat_history[0].content[-1]["text"] == "Question?", "Query should be preserved"
        assert initial_tokens > self.max_prompt_size, "Initial tokens should be greater than max prompt size"
        assert final_tokens <= self.max_prompt_size, "Truncated message should be within max prompt size"

    def test_truncate_message_first_large(self):
        # Arrange
        chat_history = generate_chat_history(5)
        big_chat_message = ChatMessage(role="user", content=generate_content(100, suffix="Question?"))
        copy_big_chat_message = big_chat_message.model_copy()
        chat_history.append(big_chat_message)
        initial_tokens = sum([utils.count_tokens(message.content, self.encoder) for message in chat_history])

        # Act
        truncated_chat_history = utils.truncate_messages(chat_history, self.max_prompt_size, self.model_name)
        final_tokens = sum([utils.count_tokens(message.content, self.encoder) for message in truncated_chat_history])

        # Assert
        # Verify certain properties of the truncated chat history
        assert len(truncated_chat_history) == 1, (
            "Only most recent message should be present as it itself is larger than context size"
        )
        assert truncated_chat_history[-1] != copy_big_chat_message, "Original message should be modified"
        assert truncated_chat_history[-1].content[0]["text"].endswith("\nQuestion?"), "Query should be preserved"
        assert initial_tokens > self.max_prompt_size, "Initial tokens should be greater than max prompt size"
        assert final_tokens <= self.max_prompt_size, "Truncated message should be within max prompt size"

    def test_truncate_message_large_system_message_first(self):
        # Arrange
        chat_history = generate_chat_history(5)
        chat_history[0].role = "system"  # Mark the first message as system message
        big_chat_message = ChatMessage(role="user", content=generate_content(100, suffix="Question?"))
        copy_big_chat_message = big_chat_message.model_copy()

        chat_history.append(big_chat_message)
        initial_tokens = sum([utils.count_tokens(message.content, self.encoder) for message in chat_history])

        # Act
        truncated_chat_history = utils.truncate_messages(chat_history, self.max_prompt_size, self.model_name)
        final_tokens = sum([utils.count_tokens(message.content, self.encoder) for message in truncated_chat_history])

        # Assert
        # Verify certain properties of the truncated chat history
        assert len(truncated_chat_history) == 2, "Expected system message + last big message after truncation"
        assert truncated_chat_history[-1] != copy_big_chat_message, "Original message should be modified"
        assert truncated_chat_history[-1].content[0]["text"].endswith("\nQuestion?"), "Query should be preserved"
        assert initial_tokens > self.max_prompt_size, "Initial tokens should be greater than max prompt size"
        assert final_tokens <= self.max_prompt_size, "Final tokens should be within max prompt size"

    def test_truncate_single_large_non_system_message(self):
        # Arrange
        big_chat_message = ChatMessage(role="user", content=generate_content(100, suffix="Question?"))
        copy_big_chat_message = big_chat_message.model_copy()
        chat_messages = [big_chat_message]
        initial_tokens = sum([utils.count_tokens(message.content, self.encoder) for message in chat_messages])

        # Act
        truncated_chat_history = utils.truncate_messages(chat_messages, self.max_prompt_size, self.model_name)
        final_tokens = sum([utils.count_tokens(message.content, self.encoder) for message in truncated_chat_history])

        # Assert
        # Verify certain properties of the truncated chat history
        assert initial_tokens > self.max_prompt_size, "Initial tokens should be greater than max prompt size"
        assert final_tokens <= self.max_prompt_size, "Final tokens should be within max prompt size"
        assert len(truncated_chat_history) == 1, (
            "Only most recent message should be present as it itself is larger than context size"
        )
        assert truncated_chat_history[0] != copy_big_chat_message, "Original message should be modified"
        assert truncated_chat_history[0].content[0]["text"].endswith("\nQuestion?"), "Query should be preserved"

    def test_truncate_single_large_question(self):
        # Arrange
        big_chat_message_content = [{"type": "text", "text": " ".join(["hi"] * (self.max_prompt_size + 1))}]
        big_chat_message = ChatMessage(role="user", content=big_chat_message_content)
        copy_big_chat_message = big_chat_message.model_copy()
        chat_messages = [big_chat_message]
        initial_tokens = sum([utils.count_tokens(message.content, self.encoder) for message in chat_messages])

        # Act
        truncated_chat_history = utils.truncate_messages(chat_messages, self.max_prompt_size, self.model_name)
        final_tokens = sum([utils.count_tokens(message.content, self.encoder) for message in truncated_chat_history])

        # Assert
        # Verify certain properties of the truncated chat history
        assert initial_tokens > self.max_prompt_size, "Initial tokens should be greater than max prompt size"
        assert final_tokens <= self.max_prompt_size, "Final tokens should be within max prompt size"
        assert len(truncated_chat_history) == 1, (
            "Only most recent message should be present as it itself is larger than context size"
        )
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


class TestStrictNotesMode:
    """Tests for strict notes mode when /notes command is explicitly used"""

    def test_strict_notes_mode_parameter_exists(self):
        """
        Verify build_conversation_context accepts strict_notes_mode parameter.
        This test will fail until the parameter is added.
        """
        from khoj.routers.helpers import build_conversation_context
        import inspect
        
        sig = inspect.signature(build_conversation_context)
        param_names = list(sig.parameters.keys())
        
        assert "strict_notes_mode" in param_names, (
            "build_conversation_context should accept strict_notes_mode parameter"
        )

    def test_strict_prompt_used_when_strict_mode_enabled(self):
        """
        When strict_notes_mode=True, the strict prompt template should be used
        that forces the model to ONLY use provided notes.
        """
        from khoj.routers.helpers import build_conversation_context
        
        references = [
            {"compiled": "HueCode is a meta-marker", "file": "huecode.pdf", "query": "huecode", "uri": "file://huecode.pdf"}
        ]
        
        messages = build_conversation_context(
            user_query="what are huecodes?",
            references=references,
            online_results={},
            code_results={},
            operator_results=[],
            strict_notes_mode=True,
        )
        
        # Combine all message content
        all_content = " ".join(str(m.content) for m in messages)
        
        # Strict prompt should contain emphatic language
        assert "ONLY" in all_content, "Strict prompt should emphasize ONLY using notes"
        assert "Retrieved Notes" in all_content, "Strict prompt should label notes as 'Retrieved'"

    def test_regular_prompt_used_when_strict_mode_disabled(self):
        """
        When strict_notes_mode=False (default), the regular softer prompt should be used.
        """
        from khoj.routers.helpers import build_conversation_context
        
        references = [
            {"compiled": "Some note content", "file": "notes.md", "query": "query", "uri": "file://notes.md"}
        ]
        
        messages = build_conversation_context(
            user_query="some question",
            references=references,
            online_results={},
            code_results={},
            operator_results=[],
            strict_notes_mode=False,
        )
        
        all_content = " ".join(str(m.content) for m in messages)
        
        # Regular prompt should NOT have strict labeling
        assert "Retrieved Notes:" not in all_content, (
            "Regular prompt should not use strict 'Retrieved Notes:' labeling"
        )
        assert "User's Notes" in all_content, "Regular prompt should use 'User's Notes'"

    def test_strict_prompt_template_exists(self):
        """Verify the strict notes prompt template exists in prompts module"""
        from khoj.processor.conversation import prompts
        
        assert hasattr(prompts, 'notes_conversation_strict'), (
            "prompts module should have notes_conversation_strict template"
        )
        
        # Verify it has the expected structure
        template_str = prompts.notes_conversation_strict.template
        assert "{references}" in template_str, "Template should have {references} placeholder"
        assert "ONLY" in template_str, "Strict template should contain emphatic language about ONLY using notes"
