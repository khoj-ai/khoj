import json
import os
from collections.abc import Iterator
from typing import Any

import requests  # Used for making HTTP requests
from bs4 import BeautifulSoup
from markdown_it import MarkdownIt

# Define constants for the script
CHUNK_SIZE = 1024  # Size of chunks to read/write at a time
ELEVEN_LABS_API_KEY = os.getenv("ELEVEN_LABS_API_KEY", None)  # Your API key for authentication
VOICE_ID = "RPEIZnKMqlQiZyZd1Dae"  # ID of the voice model to use. MALE - Christopher - friendly guy next door.
ELEVEN_API_URL = "https://api.elevenlabs.io/v1/text-to-speech"  # Base URL for the Text-to-Speech API
MINIMAX_API_KEY = os.getenv("MINIMAX_API_KEY", None)
MINIMAX_API_REGION = os.getenv("MINIMAX_API_REGION", "global_en")
MINIMAX_SPEECH_API_URLS = {
    "global_en": "https://api.minimax.io",
    "cn_zh": "https://api.minimaxi.com",
}
MINIMAX_SPEECH_MODELS = (
    "speech-2.8-hd",
    "speech-2.8-turbo",
    "speech-2.6-hd",
    "speech-2.6-turbo",
    "speech-02-hd",
    "speech-02-turbo",
    "speech-01-hd",
    "speech-01-turbo",
)
MINIMAX_AUDIO_FORMATS = ("mp3", "wav", "flac", "pcm")
MINIMAX_DEFAULT_SPEECH_MODEL = os.getenv("MINIMAX_TTS_MODEL", MINIMAX_SPEECH_MODELS[0])
MINIMAX_DEFAULT_VOICE_ID = os.getenv("MINIMAX_TTS_VOICE_ID", "English_expressive_narrator")

markdown_renderer = MarkdownIt()


def is_eleven_labs_enabled():
    return ELEVEN_LABS_API_KEY is not None


def is_text_to_speech_enabled():
    return MINIMAX_API_KEY is not None or is_eleven_labs_enabled()


class TextToSpeechError(Exception):
    """Exception raised when text-to-speech generation fails."""


def _minimax_url(region: str, path: str) -> str:
    try:
        base_url = MINIMAX_SPEECH_API_URLS[region]
    except KeyError as error:
        supported_regions = ", ".join(MINIMAX_SPEECH_API_URLS)
        raise TextToSpeechError(
            f"Unsupported MiniMax API region: {region}. Choose one of: {supported_regions}"
        ) from error
    return f"{base_url}{path}"


def _validate_minimax_model(model: str) -> None:
    if model not in MINIMAX_SPEECH_MODELS:
        supported_models = ", ".join(MINIMAX_SPEECH_MODELS)
        raise TextToSpeechError(f"Unsupported MiniMax speech model: {model}. Choose one of: {supported_models}")


def _minimax_headers() -> dict[str, str]:
    if MINIMAX_API_KEY is None:
        raise TextToSpeechError("MiniMax API key is not set")
    return {
        "Authorization": f"Bearer {MINIMAX_API_KEY}",
        "Content-Type": "application/json",
    }


def _parse_minimax_response(response: requests.Response) -> dict[str, Any]:
    if not response.ok:
        raise TextToSpeechError(f"MiniMax speech request failed with HTTP status {response.status_code}")
    try:
        payload = response.json()
    except requests.exceptions.JSONDecodeError as error:
        raise TextToSpeechError("MiniMax speech request returned invalid JSON") from error
    status = payload.get("base_resp") or {}
    if status.get("status_code") != 0:
        message = status.get("status_msg") or "unknown API error"
        raise TextToSpeechError(f"MiniMax speech request failed: {message}")
    return payload


def _minimax_request_data(
    text: str,
    model: str,
    voice_id: str,
    *,
    voice_setting: dict[str, Any] | None = None,
    language_boost: str | None = None,
    audio_setting: dict[str, Any] | None = None,
    pronunciation_dict: dict[str, Any] | None = None,
    voice_modify: dict[str, Any] | None = None,
) -> dict[str, Any]:
    _validate_minimax_model(model)
    if audio_setting and audio_setting.get("format") not in {None, *MINIMAX_AUDIO_FORMATS}:
        supported_formats = ", ".join(MINIMAX_AUDIO_FORMATS)
        raise TextToSpeechError(f"Unsupported MiniMax audio format. Choose one of: {supported_formats}")
    selected_voice_setting = {"voice_id": voice_id}
    if voice_setting:
        selected_voice_setting.update(voice_setting)
    data: dict[str, Any] = {
        "model": model,
        "text": text,
        "voice_setting": selected_voice_setting,
    }
    optional_fields = {
        "language_boost": language_boost,
        "audio_setting": audio_setting,
        "pronunciation_dict": pronunciation_dict,
        "voice_modify": voice_modify,
    }
    data.update({key: value for key, value in optional_fields.items() if value is not None})
    return data


class MiniMaxSpeechResponse:
    """Adapt MiniMax JSON and event-stream responses to the existing streaming interface."""

    def __init__(self, response: requests.Response, stream: bool, output_format: str):
        self.response = response
        self.stream = stream
        self.output_format = output_format

    def _audio_from_payload(self, payload: dict[str, Any]) -> bytes:
        status = payload.get("base_resp") or {}
        if status.get("status_code") != 0:
            message = status.get("status_msg") or "unknown API error"
            raise TextToSpeechError(f"MiniMax speech request failed: {message}")
        audio = (payload.get("data") or {}).get("audio")
        if not audio:
            return b""
        try:
            return bytes.fromhex(audio)
        except ValueError as error:
            raise TextToSpeechError("MiniMax speech response contained invalid hex audio") from error

    def iter_content(self, chunk_size: int = CHUNK_SIZE) -> Iterator[bytes]:
        if self.stream:
            if not self.response.ok:
                raise TextToSpeechError(f"MiniMax speech request failed with HTTP status {self.response.status_code}")
            for raw_line in self.response.iter_lines():
                line = raw_line.decode("utf-8") if isinstance(raw_line, bytes) else raw_line
                if line.startswith("data:"):
                    line = line.removeprefix("data:").strip()
                elif ":" in line and not line.startswith("{"):
                    continue
                if not line or line == "[DONE]":
                    continue
                try:
                    payload = json.loads(line)
                except json.JSONDecodeError as error:
                    raise TextToSpeechError("MiniMax speech stream returned invalid JSON") from error
                audio = self._audio_from_payload(payload)
                if audio:
                    yield audio
            return

        payload = _parse_minimax_response(self.response)
        audio = (payload.get("data") or {}).get("audio")
        if not audio:
            raise TextToSpeechError("MiniMax speech response did not contain audio")
        if self.output_format == "url":
            audio_response = requests.get(audio, stream=True)
            if not audio_response.ok:
                raise TextToSpeechError(f"MiniMax speech download failed with HTTP status {audio_response.status_code}")
            yield from audio_response.iter_content(chunk_size=chunk_size)
            return
        decoded_audio = self._audio_from_payload(payload)
        for offset in range(0, len(decoded_audio), chunk_size):
            yield decoded_audio[offset : offset + chunk_size]


def generate_minimax_text_to_speech(
    text_to_speak: str,
    voice_id: str = MINIMAX_DEFAULT_VOICE_ID,
    *,
    model: str = MINIMAX_DEFAULT_SPEECH_MODEL,
    region: str = MINIMAX_API_REGION,
    stream: bool = True,
    output_format: str = "hex",
    voice_setting: dict[str, Any] | None = None,
    language_boost: str | None = None,
    audio_setting: dict[str, Any] | None = None,
    pronunciation_dict: dict[str, Any] | None = None,
    voice_modify: dict[str, Any] | None = None,
    subtitle_enable: bool = False,
) -> MiniMaxSpeechResponse:
    if output_format not in {"hex", "url"}:
        raise TextToSpeechError("MiniMax speech output_format must be 'hex' or 'url'")
    if stream and output_format != "hex":
        raise TextToSpeechError("MiniMax streaming speech only supports hex output")

    data = _minimax_request_data(
        _markdown_to_text(text_to_speak),
        model,
        voice_id,
        voice_setting=voice_setting,
        language_boost=language_boost,
        audio_setting=audio_setting,
        pronunciation_dict=pronunciation_dict,
        voice_modify=voice_modify,
    )
    data.update({"stream": stream, "output_format": output_format, "subtitle_enable": subtitle_enable})
    if stream:
        data["stream_options"] = {"exclude_aggregated_audio": True}
    response = requests.post(
        _minimax_url(region, "/v1/t2a_v2"),
        headers=_minimax_headers(),
        json=data,
        stream=stream,
    )
    return MiniMaxSpeechResponse(response, stream=stream, output_format=output_format)


def create_minimax_text_to_speech_task(
    text_to_speak: str,
    voice_id: str = MINIMAX_DEFAULT_VOICE_ID,
    *,
    model: str = MINIMAX_DEFAULT_SPEECH_MODEL,
    region: str = MINIMAX_API_REGION,
    voice_setting: dict[str, Any] | None = None,
    language_boost: str | None = None,
    audio_setting: dict[str, Any] | None = None,
    pronunciation_dict: dict[str, Any] | None = None,
    voice_modify: dict[str, Any] | None = None,
) -> str:
    data = _minimax_request_data(
        _markdown_to_text(text_to_speak),
        model,
        voice_id,
        voice_setting=voice_setting,
        language_boost=language_boost,
        audio_setting=audio_setting,
        pronunciation_dict=pronunciation_dict,
        voice_modify=voice_modify,
    )
    response = requests.post(
        _minimax_url(region, "/v1/t2a_async_v2"),
        headers=_minimax_headers(),
        json=data,
    )
    payload = _parse_minimax_response(response)
    task_id = payload.get("task_id")
    if task_id is None:
        raise TextToSpeechError("MiniMax asynchronous speech response did not contain a task ID")
    return str(task_id)


def query_minimax_text_to_speech_task(task_id: str, *, region: str = MINIMAX_API_REGION) -> dict[str, Any]:
    response = requests.get(
        _minimax_url(region, "/v1/query/t2a_async_query_v2"),
        headers=_minimax_headers(),
        params={"task_id": task_id},
    )
    return _parse_minimax_response(response)


def _generate_eleven_labs_text_to_speech(
    text_to_speak: str,
    voice_id: str = VOICE_ID,
):
    if not is_eleven_labs_enabled():
        raise TextToSpeechError("Eleven Labs API key is not set")

    # Convert the incoming text from markdown format to plain text
    text = _markdown_to_text(text_to_speak)

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


def _markdown_to_text(markdown: str) -> str:
    html = markdown_renderer.render(markdown)
    return BeautifulSoup(html, features="lxml").get_text().strip()


def generate_text_to_speech(
    text_to_speak: str,
    voice_id: str | None = None,
    **kwargs,
):
    if MINIMAX_API_KEY is not None:
        return generate_minimax_text_to_speech(
            text_to_speak,
            voice_id=voice_id or MINIMAX_DEFAULT_VOICE_ID,
            **kwargs,
        )
    return _generate_eleven_labs_text_to_speech(text_to_speak, voice_id=voice_id or VOICE_ID)
