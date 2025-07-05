import logging
import os
from textwrap import dedent

import requests

from khoj.database.models import KhojUser
from khoj.processor.conversation.helpers import send_message_to_model_wrapper

logger = logging.getLogger(__name__)


# Define constants for the script
CHUNK_SIZE = 1024  # Size of chunks to read/write at a time
ELEVEN_LABS_API_KEY = os.getenv("ELEVEN_LABS_API_KEY", None)  # Your API key for authentication
VOICE_ID = "RPEIZnKMqlQiZyZd1Dae"  # ID of the voice model to use. MALE - Christopher - friendly guy next door.
ELEVEN_API_URL = "https://api.elevenlabs.io/v1/text-to-speech"  # Base URL for the Text-to-Speech API


def is_eleven_labs_enabled():
    return ELEVEN_LABS_API_KEY is not None


async def generate_text_to_speech(
    text_to_speak: str,
    user: KhojUser,
    voice_id: str = VOICE_ID,
    stream: bool = True,
    output_format: str = "mp3_44100_128",
    is_thought: bool = False,
):
    if not is_eleven_labs_enabled():
        return "Eleven Labs API key is not set"

    # Convert the incoming text from markdown format to plain text
    text_to_voice_chat_prompt = dedent(
        f"""
        Clean the text to make it easy to understand when heard out loud. Remove all HTML tags, formatting, links, or markdown. Make it sound natural and conversational.

        {text_to_speak}
        """
    ).strip()
    thought_to_voice_chat_prompt = dedent(
        f"""
        You are working on a task I've assigned to you. Summarize your inner monologue shared below into 1 short sentence. Do not say anything else.

        {text_to_speak}
        """
    )
    response = await send_message_to_model_wrapper(
        query=thought_to_voice_chat_prompt if is_thought else text_to_voice_chat_prompt,
        system_message="You are Khoj, a helpful AI assistant. Generate text to be spoken out loud to the user.",
        user=user,
    )
    text = response.text

    # Log input text, output text and prompt used for TTS
    logger.debug(f"Thought Prompt: {is_thought}")
    logger.debug(f"Input text for TTS: {text_to_speak}")
    logger.debug(f"Output text for TTS: {text}")

    # Construct the URL for the Text-to-Speech API request
    tts_url = f"{ELEVEN_API_URL}/{voice_id}/stream" if stream else f"{ELEVEN_API_URL}/{voice_id}"

    # Set up headers for the API request, including the API key for authentication
    headers = {"Accept": "application/json", "xi-api-key": ELEVEN_LABS_API_KEY}

    # Set up the data payload for the API request, including the text and voice settings
    data = {
        "text": text,
        "output_format": output_format,
        "voice_settings": {"stability": 0.5, "similarity_boost": 0.8, "style": 0.0, "use_speaker_boost": True},
    }

    # Make the POST request to the TTS API with headers and data, enabling streaming response
    response = requests.post(tts_url, headers=headers, json=data, stream=stream)

    if response.ok:
        return response
    else:
        raise Exception(f"Failed to generate text-to-speech: {response.text}")
