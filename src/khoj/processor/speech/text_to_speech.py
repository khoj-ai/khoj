import json  # Used for working with JSON data
import os

import requests  # Used for making HTTP requests
from bs4 import BeautifulSoup
from markdown_it import MarkdownIt

# Define constants for the script
CHUNK_SIZE = 1024  # Size of chunks to read/write at a time
ELEVEN_LABS_API_KEY = os.getenv("ELEVEN_LABS_API_KEY", None)  # Your API key for authentication
VOICE_ID = "RPEIZnKMqlQiZyZd1Dae"  # ID of the voice model to use. MALE - Christopher - friendly guy next door.
ELEVEN_API_URL = "https://api.elevenlabs.io/v1/text-to-speech"  # Base URL for the Text-to-Speech API

markdown_renderer = MarkdownIt()


def is_eleven_labs_enabled():
    return ELEVEN_LABS_API_KEY is not None


def generate_text_to_speech(
    text_to_speak: str,
    voice_id: str = VOICE_ID,
):
    if not is_eleven_labs_enabled():
        return "Eleven Labs API key is not set"

    # Convert the incoming text from markdown format to plain text
    html = markdown_renderer.render(text_to_speak)
    text = "".join(BeautifulSoup(html, features="lxml").findAll(text=True))

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
        raise Exception(f"Failed to generate text-to-speech: {response.text}")
