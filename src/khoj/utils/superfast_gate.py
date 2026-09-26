"""
Superfast Decision Gate -- System One front-door classifier.

A small, non-autoregressive "System One" decision model (Von, or any
Jev-compatible server) evaluates the pending user turn in a single forward
pass and returns typed, calibrated answers (choice / noul) without
generating text. The Decision Gate turns those answers into a routing
recommendation so the harness can skip expensive System Two work when the
decision is obvious.

Design contract:
  - Off by default. Nothing here runs unless the SUPERFAST_ENABLED env var is
    explicitly set to "true". A user who does nothing sees the exact current
    behavior.
  - Shadow mode. This first increment only classifies and logs. It does not
    change routing, skip the model call, or alter any user-visible behavior.
  - Fail-open. Any error, timeout, non-2xx response, unreachable backend, or
    malformed body returns no opinion. Callers must treat that as "the gate is
    off" and continue exactly as before. It never raises into the agent loop and
    never adds latency to the real turn (the shadow call runs in the background).
  - No new runtime dependencies. The Gate talks to a local HTTP endpoint
    (/v1/systemone) with aiohttp, which Khoj already uses. The decision model
    is installed out-of-band, not bundled.

Concept and reference implementation by Andrea Bruno, licensed CC BY 4.0.
White paper: https://github.com/Andrea-Bruno/harness-superfast
The decision models themselves (Von, OpenJev, Laya) are third-party open
models; only this integration architecture and routing method are ours.
"""

from __future__ import annotations

import asyncio
import logging
import os
import time
from typing import Any, Dict, Optional

import aiohttp

logger = logging.getLogger(__name__)

# Master switch. The Gate never runs unless this env var is explicitly "true".
ENV_ENABLED = "SUPERFAST_ENABLED"
# Jev-compatible decision endpoint, model id and timeout, each overridable by env.
ENV_ENDPOINT = "SUPERFAST_ENDPOINT"
ENV_MODEL = "SUPERFAST_MODEL"
ENV_TIMEOUT_MS = "SUPERFAST_TIMEOUT_MS"

DEFAULT_ENDPOINT = "http://localhost:8000/v1/systemone"
DEFAULT_MODEL = "von-1.2.0"
DEFAULT_TIMEOUT_MS = 150

# Minimum calibrated intent confidence required for the plain_chat fast route.
PLAIN_CHAT_CONFIDENCE_FLOOR = 0.5


def is_enabled() -> bool:
    """True only when SUPERFAST_ENABLED is explicitly set to "true"."""
    return os.getenv(ENV_ENABLED, "false").lower() == "true"


def _endpoint() -> str:
    return os.getenv(ENV_ENDPOINT) or DEFAULT_ENDPOINT


def _model() -> str:
    return os.getenv(ENV_MODEL) or DEFAULT_MODEL


def _timeout_ms() -> int:
    raw = os.getenv(ENV_TIMEOUT_MS)
    try:
        value = int(raw) if raw is not None else DEFAULT_TIMEOUT_MS
    except (TypeError, ValueError):
        return DEFAULT_TIMEOUT_MS
    return value if value > 0 else DEFAULT_TIMEOUT_MS


# Standard question set for classifying an incoming user turn. Kept small so the
# single forward pass stays well under the timeout budget.
TURN_QUESTIONS: Dict[str, Dict[str, Any]] = {
    "needs_tool": {
        "type": "noul",
        "instructions": (
            "Does answering this request require taking an action with a tool (reading, writing, running, searching), "
            "rather than replying from what is already known?"
        ),
    },
    "answerable_from_context": {
        "type": "noul",
        "instructions": (
            "Can this request be answered from information already present in the conversation, without any new "
            "investigation?"
        ),
    },
    "intent": {
        "type": "choice",
        "instructions": "Classify the primary intent of the user request.",
        "criteria": {
            "code_change": "Create, edit, or delete code or files.",
            "code_question": "Explain or reason about code without changing it.",
            "command": "Run a command or operation.",
            "chat": "Casual conversation or a question needing no tools.",
            "other": "None of the above.",
        },
    },
}


def _is_finite_unit(value: Any) -> bool:
    """True only for a real number (not bool) that is finite and within [0, 1]."""
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        return False
    if value != value or value in (float("inf"), float("-inf")):  # NaN / Infinity
        return False
    return 0.0 <= value <= 1.0


def _read_noul(answer: Any) -> Optional[float]:
    """Return the noul probability only when it is a real finite value in [0, 1].
    Anything else (absent, NaN, Infinity, out of range, wrong type) is treated as
    "no evidence" so a bad answer can never produce a decisive fast route."""
    if not isinstance(answer, dict):
        return None
    value = answer.get("noul")
    return float(value) if _is_finite_unit(value) else None


def _confident(answer: Any, floor: float) -> bool:
    """True only for a real, finite confidence in [0, 1] at or above the floor."""
    if not isinstance(answer, dict):
        return False
    value = answer.get("confidence")
    return _is_finite_unit(value) and value >= floor


def _derive_route(answers: Dict[str, Any]) -> str:
    """Derive a conservative route from the answers. The Gate only recommends a
    fast route when the relevant probabilities are decisive; otherwise it says
    "unknown" so the caller falls back to the normal path."""
    needs_tool = _read_noul(answers.get("needs_tool"))
    from_context = _read_noul(answers.get("answerable_from_context"))

    # Decisive "needs a tool" wins first -- the harness must not skip work.
    if needs_tool is not None and needs_tool >= 0.85:
        return "needs_tool"

    # Strongly answerable from context, with a present and low tool-need signal.
    if from_context is not None and from_context >= 0.85 and needs_tool is not None and needs_tool <= 0.3:
        return "answer_from_context"

    # Clearly chat, with a calibrated intent and a present, low tool-need signal.
    intent = answers.get("intent")
    if (
        isinstance(intent, dict)
        and intent.get("choice") == "chat"
        and _confident(intent, PLAIN_CHAT_CONFIDENCE_FLOOR)
        and needs_tool is not None
        and needs_tool <= 0.2
    ):
        return "plain_chat"

    return "unknown"


async def classify_turn(user_message: str) -> Optional[Dict[str, Any]]:
    """Classify a user turn through the Gate. Returns None when the Gate is
    disabled or unavailable (fail-open). Otherwise returns a dict with a
    conservative "route", the raw "answers", and the measured "latency_ms"."""
    if not is_enabled():
        return None

    endpoint = _endpoint()
    timeout_ms = _timeout_ms()
    started_at = time.perf_counter()
    body = {"model": _model(), "state": user_message, "questions": TURN_QUESTIONS}
    try:
        timeout = aiohttp.ClientTimeout(total=timeout_ms / 1000.0)
        async with aiohttp.ClientSession(timeout=timeout) as session:
            async with session.post(endpoint, json=body) as resp:
                if resp.status < 200 or resp.status >= 300:
                    logger.debug(
                        "Superfast gate non-2xx status=%s latency_ms=%.1f",
                        resp.status,
                        (time.perf_counter() - started_at) * 1000,
                    )
                    return None
                data = await resp.json(content_type=None)
    except Exception as e:  # fail-open on any error, timeout or unreachable backend
        logger.debug(
            "Superfast gate unavailable (fail-open) latency_ms=%.1f cause=%s",
            (time.perf_counter() - started_at) * 1000,
            type(e).__name__,
        )
        return None

    latency_ms = (time.perf_counter() - started_at) * 1000.0
    if not isinstance(data, dict) or not isinstance(data.get("answers"), dict):
        logger.debug("Superfast gate malformed response latency_ms=%.1f", latency_ms)
        return None

    return {"route": _derive_route(data["answers"]), "answers": data["answers"], "latency_ms": latency_ms}


# Keep strong references to in-flight shadow tasks so the event loop does not
# garbage-collect them before they complete.
_pending_shadow_tasks: set = set()


def shadow_log_turn(user_message: str) -> None:
    """Fire-and-forget shadow classification. Schedules a background task that
    classifies the turn and logs the recommended route and latency. It never
    blocks the caller and never raises into the agent loop. No-op when disabled."""
    if not is_enabled():
        return
    task = asyncio.create_task(_shadow_worker(user_message))
    _pending_shadow_tasks.add(task)
    task.add_done_callback(_pending_shadow_tasks.discard)


async def _shadow_worker(user_message: str) -> None:
    try:
        decision = await classify_turn(user_message)
        if decision:
            logger.info(
                "Superfast gate shadow route=%s latency_ms=%.1f",
                decision["route"],
                decision["latency_ms"],
            )
    except Exception as e:  # shadow mode must never raise into the agent loop
        logger.debug("Superfast gate shadow worker error (ignored): %s", type(e).__name__)
