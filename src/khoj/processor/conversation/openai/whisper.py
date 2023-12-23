from io import BufferedReader

from asgiref.sync import sync_to_async
from openai import OpenAI


async def transcribe_audio(audio_file: BufferedReader, model, client: OpenAI) -> str:
    """
    Transcribe audio file using Whisper model via OpenAI's API
    """
    # Send the audio data to the Whisper API
    response = await sync_to_async(client.audio.translations.create)(model=model, file=audio_file)
    return response.text
