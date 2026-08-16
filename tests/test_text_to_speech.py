import json

import pytest

from khoj.processor.speech import text_to_speech


class FakeResponse:
    def __init__(self, payload=None, *, lines=None, ok=True, status_code=200):
        self.payload = payload
        self.lines = lines or []
        self.ok = ok
        self.status_code = status_code

    def json(self):
        return self.payload

    def iter_lines(self):
        return iter(self.lines)


def test_minimax_streaming_speech_uses_regional_endpoint_and_decodes_audio(monkeypatch):
    request = {}
    chunks = [
        {"data": {"audio": "0001", "status": 1}, "base_resp": {"status_code": 0}},
        {"data": {"audio": "0203", "status": 2}, "base_resp": {"status_code": 0}},
    ]

    def fake_post(url, **kwargs):
        request.update({"url": url, **kwargs})
        lines = [f"data: {json.dumps(chunk)}".encode() for chunk in chunks]
        return FakeResponse(lines=lines)

    monkeypatch.setattr(text_to_speech, "MINIMAX_API_KEY", "test-key")
    monkeypatch.setattr(text_to_speech.requests, "post", fake_post)

    response = text_to_speech.generate_minimax_text_to_speech(
        "Hello",
        voice_id="test-voice",
        model="speech-2.8-turbo",
        region="cn_zh",
        voice_setting={"speed": 1.2},
        language_boost="English",
        audio_setting={"format": "mp3"},
    )

    assert list(response.iter_content()) == [b"\x00\x01", b"\x02\x03"]
    assert request["url"] == "https://api.minimaxi.com/v1/t2a_v2"
    assert request["headers"]["Authorization"] == "Bearer test-key"
    assert request["json"] == {
        "model": "speech-2.8-turbo",
        "text": "Hello",
        "voice_setting": {"voice_id": "test-voice", "speed": 1.2},
        "language_boost": "English",
        "audio_setting": {"format": "mp3"},
        "stream": True,
        "output_format": "hex",
        "subtitle_enable": False,
        "stream_options": {"exclude_aggregated_audio": True},
    }


def test_minimax_non_streaming_speech_decodes_hex_audio(monkeypatch):
    payload = {"data": {"audio": "00010203", "status": 2}, "base_resp": {"status_code": 0}}
    monkeypatch.setattr(text_to_speech, "MINIMAX_API_KEY", "test-key")
    monkeypatch.setattr(text_to_speech.requests, "post", lambda *args, **kwargs: FakeResponse(payload))

    response = text_to_speech.generate_minimax_text_to_speech("Hello", stream=False)

    assert list(response.iter_content(chunk_size=2)) == [b"\x00\x01", b"\x02\x03"]


def test_minimax_asynchronous_speech_create_and_query(monkeypatch):
    requests = []

    def fake_post(url, **kwargs):
        requests.append(("POST", url, kwargs))
        return FakeResponse({"task_id": 1234, "base_resp": {"status_code": 0}})

    def fake_get(url, **kwargs):
        requests.append(("GET", url, kwargs))
        return FakeResponse({"task_id": 1234, "status": "Processing", "file_id": 5678, "base_resp": {"status_code": 0}})

    monkeypatch.setattr(text_to_speech, "MINIMAX_API_KEY", "test-key")
    monkeypatch.setattr(text_to_speech.requests, "post", fake_post)
    monkeypatch.setattr(text_to_speech.requests, "get", fake_get)

    task_id = text_to_speech.create_minimax_text_to_speech_task("Hello", region="global_en")
    task = text_to_speech.query_minimax_text_to_speech_task(task_id, region="global_en")

    assert task_id == "1234"
    assert task["status"] == "Processing"
    assert requests[0][0:2] == ("POST", "https://api.minimax.io/v1/t2a_async_v2")
    assert requests[1][0:2] == ("GET", "https://api.minimax.io/v1/query/t2a_async_query_v2")
    assert requests[1][2]["params"] == {"task_id": "1234"}


def test_minimax_speech_rejects_api_errors(monkeypatch):
    monkeypatch.setattr(text_to_speech, "MINIMAX_API_KEY", "test-key")
    monkeypatch.setattr(
        text_to_speech.requests,
        "post",
        lambda *args, **kwargs: FakeResponse({"base_resp": {"status_code": 1004, "status_msg": "denied"}}),
    )

    response = text_to_speech.generate_minimax_text_to_speech("Hello", stream=False)

    with pytest.raises(text_to_speech.TextToSpeechError, match="denied"):
        list(response.iter_content())


def test_minimax_speech_models_match_supported_models():
    assert text_to_speech.MINIMAX_SPEECH_MODELS == (
        "speech-2.8-hd",
        "speech-2.8-turbo",
        "speech-2.6-hd",
        "speech-2.6-turbo",
        "speech-02-hd",
        "speech-02-turbo",
        "speech-01-hd",
        "speech-01-turbo",
    )


def test_text_to_speech_is_enabled_with_minimax_key(monkeypatch):
    monkeypatch.setattr(text_to_speech, "MINIMAX_API_KEY", "test-key")

    assert text_to_speech.is_text_to_speech_enabled()
