import asyncio
import base64
import json
import logging
import struct
from typing import Callable, Optional

import websockets
from websockets.exceptions import ConnectionClosed, WebSocketException

logger = logging.getLogger(__name__)


def resample_audio(audio_bytes: bytes, original_rate: int, target_rate: int = 24000) -> bytes:
    """
    Simple audio resampling using linear interpolation
    OpenAI Realtime API expects 24kHz PCM16
    """
    if original_rate == target_rate:
        return audio_bytes

    # Convert bytes to 16-bit integers
    samples = struct.unpack("<" + "h" * (len(audio_bytes) // 2), audio_bytes)

    # Calculate resampling ratio
    ratio = original_rate / target_rate

    # Resample using linear interpolation
    resampled = []
    for i in range(int(len(samples) / ratio)):
        # Linear interpolation between samples
        float_idx = i * ratio
        idx = int(float_idx)
        frac = float_idx - idx

        if idx + 1 < len(samples):
            sample = samples[idx] * (1 - frac) + samples[idx + 1] * frac
        else:
            sample = samples[idx]

        resampled.append(int(sample))

    # Convert back to bytes
    return struct.pack("<" + "h" * len(resampled), *resampled)


class OpenAIRealtimeClient:
    """Client for OpenAI's Realtime API for voice conversation and transcription"""

    def __init__(
        self,
        api_key: str,
        audio_handler: Optional[Callable[[bytes], None]] = None,
        transcript_handler: Optional[Callable[[str], None]] = None,
        error_handler: Optional[Callable[[str], None]] = None,
        interrupt_handler: Optional[Callable[[], None]] = None,
        input_sample_rate: int = 24000,
    ):
        self.api_key = api_key
        self.audio_handler = audio_handler
        self.transcript_handler = transcript_handler
        self.error_handler = error_handler
        self.interrupt_handler = interrupt_handler
        self.websocket = None
        self.is_connected = False
        self._listen_task = None
        self._reconnect_task = None
        self.input_sample_rate = input_sample_rate
        self.audio_buffer = b""  # Buffer to accumulate audio chunks
        self._reconnect_attempts = 0
        self._max_reconnect_attempts = 5
        self._reconnect_delay_base = 1.0  # Base delay in seconds
        self._should_reconnect = True
        self._ping_interval = 30  # Send ping every 30 seconds
        self._ping_task = None

    async def connect(self):
        """Connect to OpenAI Realtime API"""
        try:
            # Correct endpoint for OpenAI Realtime API
            uri = f"wss://api.openai.com/v1/realtime?intent=transcription"
            headers = {"Authorization": f"Bearer {self.api_key}", "OpenAI-Beta": "realtime=v1"}

            # Close existing connection if any
            if self.websocket:
                try:
                    await self.websocket.close()
                except:
                    pass

            self.websocket = await websockets.connect(
                uri,
                extra_headers=headers,
                ping_interval=20,  # Send ping every 20 seconds
                ping_timeout=10,  # Wait 10 seconds for pong
                close_timeout=10,  # Wait 10 seconds when closing
            )
            self.is_connected = True
            self._reconnect_attempts = 0
            logger.info("Connected to OpenAI Realtime API")

            # Configure session for audio input transcription
            await self._send_session_update()

            # Start listening for messages
            self._listen_task = asyncio.create_task(self._listen_for_messages())

            # Start keepalive ping task
            self._ping_task = asyncio.create_task(self._keepalive_ping())

        except Exception as e:
            logger.error(f"Failed to connect to OpenAI Realtime API: {e}")
            self.is_connected = False
            if self.error_handler:
                self.error_handler(f"Connection failed: {e}")
            raise

    async def disconnect(self):
        """Disconnect from OpenAI Realtime API"""
        self.is_connected = False
        self._should_reconnect = False

        # Cancel all tasks
        for task in [self._listen_task, self._reconnect_task, self._ping_task]:
            if task and not task.done():
                task.cancel()
                try:
                    await task
                except asyncio.CancelledError:
                    pass

        if self.websocket:
            try:
                await self.websocket.close()
            except Exception as e:
                logger.error(f"Error closing OpenAI Realtime API connection: {e}")

        logger.info("Disconnected from OpenAI Realtime API")

    async def _keepalive_ping(self):
        """Send periodic ping to keep connection alive"""
        while self.is_connected and self._should_reconnect:
            try:
                await asyncio.sleep(self._ping_interval)
                if self.is_connected and self.websocket:
                    # WebSockets library handles ping/pong automatically
                    # Just log that we're maintaining the connection
                    logger.debug("Connection keepalive check")
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Error in keepalive ping: {e}")
                break

    async def _handle_connection_loss(self):
        """Handle connection loss and attempt reconnection"""
        if not self._should_reconnect:
            return

        self.is_connected = False

        logger.warning("OpenAI Realtime API connection lost, attempting to reconnect...")

        # Start reconnection attempts with exponential backoff
        if not self._reconnect_task or self._reconnect_task.done():
            self._reconnect_task = asyncio.create_task(self._reconnect_loop())

    async def _reconnect_loop(self):
        """Attempt to reconnect with exponential backoff"""
        while self._should_reconnect and self._reconnect_attempts < self._max_reconnect_attempts:
            try:
                # Calculate delay with exponential backoff
                delay = self._reconnect_delay_base * (2**self._reconnect_attempts)
                delay = min(delay, 60)  # Cap at 60 seconds

                logger.info(
                    f"Reconnection attempt {self._reconnect_attempts + 1}/{self._max_reconnect_attempts} in {delay} seconds"
                )
                await asyncio.sleep(delay)

                if not self._should_reconnect:
                    break

                self._reconnect_attempts += 1
                await self.connect()

                if self.is_connected:
                    logger.info("Successfully reconnected to OpenAI Realtime API")
                    return

            except Exception as e:
                logger.error(f"Reconnection attempt {self._reconnect_attempts} failed: {e}")

        # Max attempts reached
        if self._reconnect_attempts >= self._max_reconnect_attempts:
            logger.error("Max reconnection attempts reached. Giving up.")
            if self.error_handler:
                self.error_handler("Connection lost and max reconnection attempts reached")

    def _is_connection_healthy(self) -> bool:
        """Check if the connection is healthy"""
        if not self.is_connected or not self.websocket:
            return False

        # Check if websocket is in a good state
        try:
            if self.websocket.closed:
                return False
        except:
            return False

        return True

    async def _send_session_update(self):
        """Configure the session for audio input transcription"""
        session_config = {
            "type": "transcription_session.update",
            "session": {
                "input_audio_format": "pcm16",
                "input_audio_transcription": {
                    "model": "gpt-4o-mini-transcribe",
                    "prompt": "You are Khoj, a helpful personal assistant. When you receive audio, transcribe it accurately.",
                    "language": "en",
                },
                "turn_detection": {
                    "type": "server_vad",
                    "threshold": 0.5,
                    "prefix_padding_ms": 300,
                    "silence_duration_ms": 500,
                },
                "input_audio_noise_reduction": {"type": "near_field"},
            },
        }
        await self._send_message(session_config)

    async def _listen_for_messages(self):
        """Listen for messages from OpenAI Realtime API"""
        try:
            async for message in self.websocket:
                try:
                    data = json.loads(message)
                    await self._handle_message(data)
                except json.JSONDecodeError as e:
                    logger.error(f"Failed to decode message from OpenAI: {e}")
                except Exception as e:
                    logger.error(f"Error handling message from OpenAI: {e}")

        except ConnectionClosed as e:
            logger.info(f"OpenAI Realtime API connection closed: {e}")
            self.is_connected = False
            if self._should_reconnect:
                await self._handle_connection_loss()
        except WebSocketException as e:
            logger.error(f"WebSocket error with OpenAI Realtime API: {e}")
            self.is_connected = False
            if self._should_reconnect:
                await self._handle_connection_loss()
            if self.error_handler:
                self.error_handler(f"WebSocket error: {e}")
        except Exception as e:
            logger.error(f"Unexpected error in OpenAI Realtime API listener: {e}")
            self.is_connected = False
            if self._should_reconnect:
                await self._handle_connection_loss()
            if self.error_handler:
                self.error_handler(f"Unexpected error: {e}")

    async def _handle_message(self, data: dict):
        """Handle messages received from OpenAI Realtime API"""
        message_type = data.get("type")

        if message_type == "session.created":
            logger.info("OpenAI Realtime session created")

        elif message_type == "session.updated":
            logger.debug("OpenAI Realtime session updated")

        elif message_type == "input_audio_buffer.speech_started":
            if self.interrupt_handler:
                self.interrupt_handler()
            logger.debug("Speech started detected by OpenAI")

        elif message_type == "input_audio_buffer.speech_stopped":
            logger.debug("Speech stopped detected by OpenAI")

        elif message_type in [
            "conversation.item.created",
            "conversation.item.input_audio_transcription.delta",
            "conversation.audio_transcript.delta",
            "input_audio_buffer.committed",
            "input_audio_buffer.cleared",
        ]:
            # Ignore input events that are not relevant
            pass
        elif message_type in [
            "response.audio_transcript.done",
            "conversation.item.input_audio_transcription.completed",
        ]:
            # Handle transcription completion
            transcript: str = data["transcript"]
            logger.info(f"Received transcription from OpenAI: {transcript}")
            if self.transcript_handler and transcript.strip():
                await self.transcript_handler(transcript.strip())  # type: ignore

        elif message_type == "conversation.item.input_audio_transcription.failed":
            error_msg = data.get("error", {}).get("message", "Transcription failed")
            logger.error(f"OpenAI transcription failed: {error_msg}")
            if self.error_handler:
                self.error_handler(f"Transcription failed: {error_msg}")

        elif message_type in [
            "response.created",
            "response.text.delta",
            "response.text.done",
            "response.audio.done",
            "response.done",
            "rate_limits.updated",
            "transcription.session.created",
        ]:
            # Handle response messages
            logger.debug(f"Received response message of type: {message_type}")

        elif message_type == "response.audio.delta":
            # Handle audio response chunks
            audio_data = data.get("delta", "")
            if audio_data and self.audio_handler:
                try:
                    audio_bytes = base64.b64decode(audio_data)
                    self.audio_handler(audio_bytes)
                except Exception as e:
                    logger.error(f"Error processing audio delta: {e}")

        elif message_type == "error":
            error_msg = data.get("error", {}).get("message", "Unknown error")
            logger.error(f"OpenAI Realtime API error: {error_msg}")
            if self.error_handler:
                self.error_handler(error_msg)

        else:
            logger.debug(f"Received unhandled message type: {message_type}")
            if logger.isEnabledFor(logging.DEBUG):
                logger.debug(f"Message data: {data}")

    async def _send_message(self, message: dict):
        """Send a message to the OpenAI Realtime API"""
        if not self._is_connection_healthy():
            logger.warning("Cannot send message: connection not healthy")
            if self._should_reconnect and not self._reconnect_task:
                await self._handle_connection_loss()
            raise RuntimeError("Not connected to OpenAI Realtime API")

        try:
            if self.websocket:
                await self.websocket.send(json.dumps(message))
        except ConnectionClosed as e:
            logger.warning(f"Connection closed while sending message: {e}")
            self.is_connected = False
            if self._should_reconnect:
                await self._handle_connection_loss()
            raise
        except Exception as e:
            logger.error(f"Error sending message to OpenAI: {e}")
            raise

    async def send_audio_chunk(self, audio_data: bytes, sample_rate: int = None):
        """Send an audio chunk to OpenAI for processing"""
        if not self._is_connection_healthy():
            logger.warning("Cannot send audio: connection not healthy")
            return

        try:
            # Use provided sample rate or fallback to instance default
            if sample_rate is None:
                sample_rate = self.input_sample_rate

            # Resample audio to 24kHz if needed
            if sample_rate != 24000:
                audio_data = resample_audio(audio_data, sample_rate, 24000)

            # Add to buffer
            self.audio_buffer += audio_data

            # Check if we have enough audio (at least 100ms worth)
            # 24kHz * 2 bytes/sample * 0.1s = 4800 bytes minimum
            min_buffer_size = 4800

            if len(self.audio_buffer) >= min_buffer_size:
                # Send the accumulated buffer
                audio_base64 = base64.b64encode(self.audio_buffer).decode("utf-8")

                message = {"type": "input_audio_buffer.append", "audio": audio_base64}
                await self._send_message(message)

                # Clear the buffer
                self.audio_buffer = b""

        except ConnectionClosed:
            # Connection was closed, reconnection already handled by _send_message
            logger.debug("Audio chunk send interrupted by connection closure")
        except Exception as e:
            logger.error(f"Failed to send audio chunk: {e}")
            if self.error_handler:
                self.error_handler(f"Failed to send audio: {e}")

    async def send_audio_end(self):
        """Signal the end of audio input"""
        if not self._is_connection_healthy():
            logger.warning("Cannot send audio end: connection not healthy")
            return

        try:
            # Send any remaining buffered audio
            if self.audio_buffer:
                logger.debug(f"Discarded final {len(self.audio_buffer)} bytes of buffered audio as too short")
                self.audio_buffer = b""

            # Commit the audio buffer
            await self.commit_audio_buffer()
            logger.debug("Sent audio end signal and committed buffer")

        except ConnectionClosed:
            # Connection was closed, reconnection already handled by _send_message
            logger.debug("Audio end signal interrupted by connection closure")
        except Exception as e:
            logger.error(f"Failed to send audio end signal: {e}")
            if self.error_handler:
                self.error_handler(f"Failed to signal audio end: {e}")

    async def commit_audio_buffer(self):
        """Commit the current audio buffer and trigger processing"""
        if not self._is_connection_healthy():
            logger.warning("Cannot commit audio buffer: connection not healthy")
            return

        try:
            message = {"type": "input_audio_buffer.commit"}
            await self._send_message(message)
            logger.debug("Committed audio buffer to OpenAI")
        except ConnectionClosed:
            # Connection was closed, reconnection already handled by _send_message
            logger.debug("Audio buffer commit interrupted by connection closure")
        except Exception as e:
            logger.error(f"Failed to commit audio buffer: {e}")

    async def clear_audio_buffer(self):
        """Clear the current audio buffer"""
        if not self._is_connection_healthy():
            logger.warning("Cannot clear audio buffer: connection not healthy")
            return

        try:
            # Clear internal buffer
            self.audio_buffer = b""

            # Clear OpenAI's buffer
            message = {"type": "input_audio_buffer.clear"}
            await self._send_message(message)
            logger.debug("Cleared audio buffer")
        except ConnectionClosed:
            # Connection was closed, reconnection already handled by _send_message
            logger.debug("Audio buffer clear interrupted by connection closure")
        except Exception as e:
            logger.error(f"Failed to clear audio buffer: {e}")


async def create_realtime_session(
    api_key: str,
    audio_handler: Optional[Callable[[bytes], None]] = None,
    transcript_handler: Optional[Callable[[str], None]] = None,
    error_handler: Optional[Callable[[str], None]] = None,
    interrupt_handler: Optional[Callable[[], None]] = None,
    input_sample_rate: int = 24000,
) -> OpenAIRealtimeClient:
    """
    Create and manage an OpenAI Realtime API session

    Args:
        api_key: OpenAI API key
        audio_handler: Optional function to handle audio responses
        transcript_handler: Optional function to handle transcription results
        error_handler: Optional function to handle errors
        input_sample_rate: Sample rate of input audio (will be resampled to 24kHz)

    Returns:
        OpenAIRealtimeClient instance
    """
    client = OpenAIRealtimeClient(
        api_key=api_key,
        audio_handler=audio_handler,
        transcript_handler=transcript_handler,
        error_handler=error_handler,
        interrupt_handler=interrupt_handler,
        input_sample_rate=input_sample_rate,
    )

    try:
        await client.connect()
        return client

    except Exception as e:
        logger.error(f"Failed to create OpenAI Realtime session: {e}")
        await client.disconnect()
        raise
