# Standard Packages
from io import BufferedReader

# External Packages
from asgiref.sync import sync_to_async
from openai import OpenAI


async def transcribe_audio(audio_file: BufferedReader, model, api_key) -> str:
    """
    Transcribe audio file using Whisper model via OpenAI's API
    """
    # Send the audio data to the Whisper API
    client = OpenAI(api_key=api_key)
    response = await sync_to_async(client.audio.translations.create)(model=model, file=audio_file)
    return response["text"]
