import asyncio
import importlib.util
import smtplib
import socketserver
import threading
from email import policy
from email.parser import BytesParser
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import Mock

import pytest
from fastapi import FastAPI
from fastapi.testclient import TestClient
from jinja2 import Environment, FileSystemLoader
from starlette.authentication import AuthenticationBackend
from starlette.middleware.authentication import AuthenticationMiddleware
from starlette.middleware.sessions import SessionMiddleware

from khoj.database.models import KhojUser
from khoj.routers import auth, email, helpers
from khoj.utils import state


@pytest.fixture
def email_config(settings, monkeypatch):
    settings.EMAIL_HOST = ""
    settings.EMAIL_BACKEND = "django.core.mail.backends.locmem.EmailBackend"
    settings.DEFAULT_FROM_EMAIL = "Khoj <noreply@example.com>"
    monkeypatch.setattr(email, "RESEND_API_KEY", None)
    monkeypatch.setattr(email, "RESEND_AUDIENCE_ID", None)
    monkeypatch.setattr(email.resend.Emails, "send", Mock(return_value={"id": "message-id"}))
    monkeypatch.setattr(email.resend.Contacts, "create", Mock())
    monkeypatch.setattr(email, "env", Environment(loader=FileSystemLoader(settings.BASE_DIR / "interface/email")))
    return settings


async def send_message(kind):
    if kind == "magic":
        return await email.send_magic_link_email("user@example.com", "login-code", "https://app.example/")
    if kind == "welcome":
        return await email.send_welcome_email("Reader", "user@example.com")
    if kind == "feedback":
        return await email.send_query_feedback("Question", "Answer", "good", "user@example.com")
    return email.send_task_email("Reader", "user@example.com", "Question", "**Answer**", "Daily report")


@pytest.mark.asyncio
@pytest.mark.parametrize("kind", ["magic", "welcome", "feedback", "task"])
async def test_smtp_sends_each_email_kind(kind, email_config, mailoutbox):
    email_config.EMAIL_HOST = "smtp.example.com"

    await send_message(kind)

    assert len(mailoutbox) == 1
    message = mailoutbox[0]
    assert message.from_email == "Khoj <noreply@example.com>"
    assert message.to == (["team@khoj.dev"] if kind == "feedback" else ["user@example.com"])
    assert message.body
    assert message.alternatives[0][1] == "text/html"
    if kind == "magic":
        assert "https://app.example/auth/magic?code=login-code" in message.alternatives[0][0]
    if kind == "task":
        assert "<strong>Answer</strong>" in message.alternatives[0][0]
    email.resend.Emails.send.assert_not_called()
    email.resend.Contacts.create.assert_not_called()


@pytest.mark.asyncio
@pytest.mark.parametrize("kind", ["magic", "welcome", "feedback", "task"])
async def test_resend_remains_available(kind, email_config, monkeypatch, mailoutbox):
    monkeypatch.setattr(email, "RESEND_API_KEY", "test-key")
    monkeypatch.setattr(email, "RESEND_AUDIENCE_ID", "test-audience")

    await send_message(kind)

    payload = email.resend.Emails.send.call_args.args[0]
    assert payload["to"] == ("team@khoj.dev" if kind == "feedback" else "user@example.com")
    assert payload["subject"] and payload["html"]
    assert not mailoutbox
    if kind == "welcome":
        email.resend.Contacts.create.assert_called_once_with(
            {"email": "user@example.com", "audience_id": "test-audience", "first_name": "Reader"}
        )
    else:
        email.resend.Contacts.create.assert_not_called()


@pytest.mark.asyncio
@pytest.mark.parametrize("kind", ["magic", "welcome", "feedback", "task"])
async def test_email_stays_disabled_without_a_provider(kind, email_config, mailoutbox):
    await send_message(kind)

    assert not mailoutbox
    email.resend.Emails.send.assert_not_called()
    email.resend.Contacts.create.assert_not_called()


@pytest.mark.asyncio
async def test_smtp_takes_precedence_over_resend(email_config, monkeypatch, mailoutbox):
    email_config.EMAIL_HOST = "smtp.example.com"
    monkeypatch.setattr(email, "RESEND_API_KEY", "test-key")
    monkeypatch.setattr(email, "RESEND_AUDIENCE_ID", "test-audience")

    await send_message("welcome")

    assert len(mailoutbox) == 1
    email.resend.Emails.send.assert_not_called()
    email.resend.Contacts.create.assert_not_called()


def test_automation_uses_smtp_without_resend(email_config, monkeypatch, mailoutbox):
    email_config.EMAIL_HOST = "smtp.example.com"
    monkeypatch.setattr(state, "anonymous_mode", True)
    monkeypatch.setattr(
        helpers.requests,
        "post",
        Mock(return_value=SimpleNamespace(status_code=200, headers={"Content-Type": "text/plain"}, text="Answer")),
    )
    monkeypatch.setattr(helpers, "should_notify", Mock(return_value=True))
    monkeypatch.setattr(helpers, "format_automation_response", Mock(return_value="A scheduled answer"))
    user = SimpleNamespace(email="user@example.com", get_short_name=lambda: "Reader")

    helpers.scheduled_chat("Question", "Daily question", "Daily report", user, "https://app.example/api/chat")

    assert len(mailoutbox) == 1
    assert mailoutbox[0].to == ["user@example.com"]
    assert "A scheduled answer" in mailoutbox[0].alternatives[0][0]


@pytest.mark.django_db(transaction=True)
def test_login_endpoint_sends_a_code_over_smtp(email_config, monkeypatch, mailoutbox):
    email_config.EMAIL_HOST = "smtp.example.com"
    monkeypatch.setattr(state, "billing_enabled", False)
    monkeypatch.setattr(auth, "update_telemetry_state", Mock())

    class AnonymousBackend(AuthenticationBackend):
        async def authenticate(self, conn):
            return None

    app = FastAPI()
    app.include_router(auth.auth_router, prefix="/auth")
    app.add_middleware(SessionMiddleware, secret_key="test-session-key")
    app.add_middleware(AuthenticationMiddleware, backend=AnonymousBackend())
    with TestClient(app) as client:
        response = client.post("/auth/magic", json={"email": "user@example.com"})

    assert response.status_code == 200
    user = KhojUser.objects.get(email="user@example.com")
    assert len(mailoutbox) == 1
    assert mailoutbox[0].to == ["user@example.com"]
    assert user.email_verification_code in mailoutbox[0].alternatives[0][0]


@pytest.mark.asyncio
async def test_smtp_errors_are_not_sent_to_resend(email_config, monkeypatch):
    email_config.EMAIL_HOST = "smtp.example.com"
    email_config.EMAIL_BACKEND = "django.core.mail.backends.smtp.EmailBackend"
    monkeypatch.setattr(email, "RESEND_API_KEY", "test-key")
    monkeypatch.setattr(smtplib, "SMTP", Mock(side_effect=OSError("relay unavailable")))

    with pytest.raises(OSError, match="relay unavailable"):
        await send_message("magic")
    email.resend.Emails.send.assert_not_called()


@pytest.mark.asyncio
async def test_smtp_login_delivery_does_not_block_the_event_loop(email_config, monkeypatch):
    email_config.EMAIL_HOST = "smtp.example.com"
    started = threading.Event()
    release = threading.Event()

    def deliver(*args, **kwargs):
        started.set()
        if not release.wait(timeout=2):
            raise TimeoutError("event loop did not release the relay")
        return 1

    monkeypatch.setattr(email, "send_mail", deliver)
    delivery = asyncio.create_task(send_message("magic"))
    try:
        assert await asyncio.to_thread(started.wait, 1)
        await asyncio.wait_for(asyncio.sleep(0), timeout=0.5)
        assert not delivery.done()
    finally:
        release.set()
        await delivery


def test_smtp_delivers_to_a_local_relay(email_config):
    messages = []

    class SMTPHandler(socketserver.StreamRequestHandler):
        def handle(self):
            self.wfile.write(b"220 localhost ready\r\n")
            while line := self.rfile.readline():
                command = line.split(None, 1)[0].upper()
                if command in (b"EHLO", b"HELO"):
                    self.wfile.write(b"250 localhost\r\n")
                elif command in (b"MAIL", b"RCPT"):
                    self.wfile.write(b"250 OK\r\n")
                elif command == b"DATA":
                    self.wfile.write(b"354 Send message\r\n")
                    body = []
                    while (line := self.rfile.readline()) not in (b".\r\n", b""):
                        body.append(line)
                    messages.append(BytesParser(policy=policy.default).parsebytes(b"".join(body)))
                    self.wfile.write(b"250 Accepted\r\n")
                elif command == b"QUIT":
                    self.wfile.write(b"221 Bye\r\n")
                    return
                else:
                    self.wfile.write(b"500 Unsupported command\r\n")

    with socketserver.TCPServer(("127.0.0.1", 0), SMTPHandler) as relay:
        thread = threading.Thread(target=relay.serve_forever, daemon=True)
        thread.start()
        email_config.EMAIL_HOST, email_config.EMAIL_PORT = relay.server_address
        email_config.EMAIL_BACKEND = "django.core.mail.backends.smtp.EmailBackend"
        email_config.EMAIL_USE_TLS = False
        email_config.EMAIL_USE_SSL = False
        email_config.EMAIL_HOST_USER = ""
        email_config.EMAIL_HOST_PASSWORD = ""
        email_config.EMAIL_TIMEOUT = 2
        try:
            email.send_task_email("Reader", "user@example.com", "Question", "**Answer**", "Daily report")
        finally:
            relay.shutdown()
            thread.join(timeout=3)

    assert len(messages) == 1
    assert str(messages[0]["From"]) == "Khoj <noreply@example.com>"
    assert str(messages[0]["To"]) == "user@example.com"
    assert "<strong>Answer</strong>" in messages[0].get_body(preferencelist=("html",)).get_content()


@pytest.mark.parametrize("transport", ["starttls", "ssl", "relay"])
def test_smtp_environment_settings(monkeypatch, transport):
    for name in ("EMAIL_USE_TLS", "EMAIL_USE_SSL", "EMAIL_PORT", "USE_EMBEDDED_DB"):
        monkeypatch.delenv(name, raising=False)
    monkeypatch.setenv("EMAIL_HOST", "smtp.example.com")
    monkeypatch.setenv("EMAIL_HOST_USER", "relay-user")
    monkeypatch.setenv("EMAIL_HOST_PASSWORD", "test-password")
    monkeypatch.setenv("DEFAULT_FROM_EMAIL", "noreply@example.com")
    monkeypatch.setenv("EMAIL_TIMEOUT", "7")
    if transport == "ssl":
        monkeypatch.setenv("EMAIL_USE_SSL", "true")
    elif transport == "relay":
        monkeypatch.setenv("EMAIL_USE_TLS", "false")
        monkeypatch.setenv("EMAIL_PORT", "25")
    spec = importlib.util.spec_from_file_location(
        "smtp_test_settings", Path(email.settings.BASE_DIR) / "app/settings.py"
    )
    config = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(config)

    assert config.EMAIL_HOST == "smtp.example.com"
    assert config.EMAIL_HOST_USER == "relay-user"
    assert config.EMAIL_HOST_PASSWORD == "test-password"
    assert config.DEFAULT_FROM_EMAIL == "noreply@example.com"
    assert config.EMAIL_TIMEOUT == 7
    assert config.EMAIL_USE_SSL is (transport == "ssl")
    assert config.EMAIL_USE_TLS is (transport == "starttls")
    assert config.EMAIL_PORT == {"ssl": 465, "starttls": 587, "relay": 25}[transport]
