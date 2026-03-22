"""
Novita AI chat integration.

Novita AI exposes an OpenAI-compatible chat completions endpoint, so this
module delegates directly to the shared OpenAI conversation utilities.
Base URL: https://api.novita.ai/openai
"""

from khoj.processor.conversation.openai.gpt import converse_openai, openai_send_message_to_model

novita_send_message_to_model = openai_send_message_to_model
converse_novita = converse_openai
