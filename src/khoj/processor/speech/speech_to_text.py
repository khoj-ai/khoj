import logging
import os
import tempfile
from io import BufferedReader
from typing import Optional, Union

from khoj.database.adapters import ConversationAdapters
from khoj.database.models import SpeechToTextModelOptions
from khoj.processor.conversation.offline.whisper import transcribe_audio_offline
from khoj.processor.conversation.openai.whisper import transcribe_audio
from khoj.utils import state
from khoj.utils.helpers import get_openai_client

logger = logging.getLogger(__name__)


async def transcribe_audio_from_file(audio_file: BufferedReader, audio_filename: str) -> Optional[str]:
    """
    Transcribe audio from a file using the configured speech-to-text model.

    Args:
        audio_file: The audio file buffer to transcribe
        audio_filename: The filename of the audio file for offline transcription

    Returns:
        The transcribed text or None if transcription fails
    """
    user_message: Optional[str] = None

    try:
        # Get the speech-to-text configuration
        speech_to_text_config = await ConversationAdapters.get_speech_to_text_config()

        if not speech_to_text_config:
            logger.warning("No speech-to-text configuration found")
            return None

        if speech_to_text_config.model_type == SpeechToTextModelOptions.ModelType.OFFLINE:
            # Use offline transcription
            speech2text_model = speech_to_text_config.model_name
            user_message = await transcribe_audio_offline(audio_filename, speech2text_model)

        elif speech_to_text_config.model_type == SpeechToTextModelOptions.ModelType.OPENAI:
            # Use OpenAI transcription
            speech2text_model = speech_to_text_config.model_name
            openai_client = None

            if speech_to_text_config.ai_model_api:
                api_key = speech_to_text_config.ai_model_api.api_key
                api_base_url = speech_to_text_config.ai_model_api.api_base_url
                openai_client = get_openai_client(api_key=api_key, api_base_url=api_base_url)
            elif state.openai_client:
                openai_client = state.openai_client

            if openai_client:
                user_message = await transcribe_audio(audio_file, speech2text_model, client=openai_client)
            else:
                logger.warning("No OpenAI client available for transcription")
                return None

    except Exception as e:
        logger.error(f"Error during transcription: {e}", exc_info=True)
        return None

    return user_message


async def transcribe_audio_from_data(audio_data: bytes, file_extension: str = "webm") -> Optional[str]:
    """
    Transcribe audio from raw audio data.

    Args:
        audio_data: Raw audio data bytes
        file_extension: File extension for the temporary audio file (default: webm)

    Returns:
        The transcribed text or None if transcription fails
    """
    audio_filename = None
    audio_file = None

    try:
        # Create temporary file with unique name
        with tempfile.NamedTemporaryFile(delete=False, suffix=f".{file_extension}") as temp_file:
            temp_file.write(audio_data)
            audio_filename = temp_file.name

        # Open the file for reading
        audio_file = open(audio_filename, "rb")

        # Transcribe the audio
        user_message = await transcribe_audio_from_file(audio_file, audio_filename)

        return user_message

    except Exception as e:
        logger.error(f"Error transcribing audio from data: {e}", exc_info=True)
        return None

    finally:
        # Clean up resources
        if audio_file:
            audio_file.close()
        if audio_filename and os.path.exists(audio_filename):
            os.remove(audio_filename)


async def transcribe_audio_from_buffer(
    audio_buffer: Union[bytes, BufferedReader], file_extension: str = "wav"
) -> Optional[str]:
    """
    Transcribe audio from a buffer or file-like object.

    Args:
        audio_buffer: Audio data as bytes or BufferedReader
        file_extension: File extension for the temporary audio file (default: wav)

    Returns:
        The transcribed text or None if transcription fails
    """
    if isinstance(audio_buffer, bytes):
        return await transcribe_audio_from_data(audio_buffer, file_extension)
    elif hasattr(audio_buffer, "read"):
        # It's a file-like object
        audio_data = audio_buffer.read()
        audio_buffer.seek(0)  # Reset position
        return await transcribe_audio_from_data(audio_data, file_extension)
    else:
        logger.error(f"Unsupported audio buffer type: {type(audio_buffer)}")
        return None
