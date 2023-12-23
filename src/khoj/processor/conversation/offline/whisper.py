import whisper
from asgiref.sync import sync_to_async

from khoj.utils import state


async def transcribe_audio_offline(audio_filename: str, model: str) -> str:
    """
    Transcribe audio file offline using Whisper
    """
    # Send the audio data to the Whisper API
    if not state.whisper_model:
        state.whisper_model = whisper.load_model(model)
    response = await sync_to_async(state.whisper_model.transcribe)(audio_filename)
    return response["text"]
