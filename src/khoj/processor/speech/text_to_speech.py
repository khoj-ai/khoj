import os

import requests  # Used for making HTTP requests
from bs4 import BeautifulSoup
from markdown_it import MarkdownIt

# Define constants for the script
CHUNK_SIZE = 1024  # Size of chunks to read/write at a time
ELEVEN_LABS_API_KEY = os.getenv("ELEVEN_LABS_API_KEY", None)  # Your API key for authentication
VOICE_ID = "RPEIZnKMqlQiZyZd1Dae"  # ID of the voice model to use. MALE - Christopher - friendly guy next door.
ELEVEN_API_URL = "https://api.elevenlabs.io/v1/text-to-speech"  # Base URL for the Text-to-Speech API

GANDR_API_KEY = os.getenv("GANDR_API_KEY", None)  # Your Gandr API key for authentication
GANDR_VOICE_ID = "gandr-mia"  # Default Gandr voice. Also available: gandr-ava, gandr-jenny, gandr-dane, gandr-leo, gandr-lewis
GANDR_API_URL = "https://tts.gandr.ai/v1/audio/speech"  # OpenAI compatible speech endpoint
GANDR_MAX_CHARS = 2000  # The Gandr API accepts up to 2000 characters per request

markdown_renderer = MarkdownIt()


def is_eleven_labs_enabled():
    return ELEVEN_LABS_API_KEY is not None


def is_gandr_enabled():
    return GANDR_API_KEY is not None


def is_text_to_speech_enabled():
    return is_eleven_labs_enabled() or is_gandr_enabled()


class TextToSpeechError(Exception):
    """Exception raised when text-to-speech generation fails."""


def generate_text_to_speech(
    text_to_speak: str,
    voice_id: str = None,
):
    # Convert the incoming text from markdown format to plain text
    html = markdown_renderer.render(text_to_speak)
    text = "".join(BeautifulSoup(html, features="lxml").findAll(text=True))

    if is_eleven_labs_enabled():
        return generate_eleven_labs_text_to_speech(text, voice_id or VOICE_ID)
    if is_gandr_enabled():
        return generate_gandr_text_to_speech(text, voice_id or GANDR_VOICE_ID)
    raise TextToSpeechError("No text-to-speech API key is set. Set ELEVEN_LABS_API_KEY or GANDR_API_KEY.")


def generate_eleven_labs_text_to_speech(text: str, voice_id: str):
    # Construct the URL for the Text-to-Speech API request
    tts_url = f"{ELEVEN_API_URL}/{voice_id}/stream"

    # Set up headers for the API request, including the API key for authentication
    headers = {"Accept": "application/json", "xi-api-key": ELEVEN_LABS_API_KEY}

    # Set up the data payload for the API request, including the text and voice settings
    data = {
        "text": text,
        # "model_id": "eleven_multilingual_v2",
        "voice_settings": {"stability": 0.5, "similarity_boost": 0.8, "style": 0.0, "use_speaker_boost": True},
    }

    # Make the POST request to the TTS API with headers and data, enabling streaming response
    response = requests.post(tts_url, headers=headers, json=data, stream=True)

    if response.ok:
        return response
    else:
        raise TextToSpeechError(f"Failed to generate text-to-speech: {response.text}")


def generate_gandr_text_to_speech(text: str, voice_id: str):
    # Set up headers for the API request, including the API key for authentication
    headers = {"Authorization": f"Bearer {GANDR_API_KEY}"}

    # The Gandr API accepts up to 2000 characters per request
    text = text[:GANDR_MAX_CHARS]

    # Set up the data payload for the OpenAI compatible speech request
    data = {
        "model": "tts-1",
        "input": text,
        "voice": voice_id,
        "response_format": "mp3",
    }

    # Make the POST request to the TTS API with headers and data, enabling streaming response
    response = requests.post(GANDR_API_URL, headers=headers, json=data, stream=True)

    if response.ok:
        return response
    else:
        raise TextToSpeechError(f"Failed to generate text-to-speech: {response.text}")
