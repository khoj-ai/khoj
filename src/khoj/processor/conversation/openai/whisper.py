# Standard Packages
from io import BufferedReader

# External Packages
from asgiref.sync import sync_to_async
import openai


async def transcribe_audio(audio_file: BufferedReader, model, api_key) -> str | None:
    """
    Transcribe audio file using Whisper model via OpenAI's API
    """
    # Send the audio data to the Whisper API
    response = await sync_to_async(openai.Audio.translate)(model=model, file=audio_file, api_key=api_key)
    return response["text"]
