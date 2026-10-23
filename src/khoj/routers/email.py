import asyncio
import logging
import os
from urllib.parse import quote

import markdown_it
import resend
from django.conf import settings
from django.core.mail import send_mail
from django.utils.html import strip_tags
from jinja2 import Environment, FileSystemLoader

from khoj.utils.helpers import is_none_or_empty

logger = logging.getLogger(__name__)


RESEND_API_KEY = os.getenv("RESEND_API_KEY")
RESEND_AUDIENCE_ID = os.getenv("RESEND_AUDIENCE_ID")

static_files = os.path.join(settings.BASE_DIR, "static")

env = Environment(loader=FileSystemLoader(static_files))

if RESEND_API_KEY:
    resend.api_key = RESEND_API_KEY
elif not settings.EMAIL_HOST:
    logger.warning("Email sending disabled. Set EMAIL_HOST or RESEND_API_KEY to enable it.")


def is_resend_enabled():
    return bool(RESEND_API_KEY)


def is_email_enabled():
    return bool(settings.EMAIL_HOST) or is_resend_enabled()


def _send_email(message):
    if settings.EMAIL_HOST:
        return send_mail(
            subject=message["subject"],
            message=strip_tags(message["html"]),
            from_email=settings.DEFAULT_FROM_EMAIL,
            recipient_list=[message["to"]],
            html_message=message["html"],
            fail_silently=False,
        )
    return resend.Emails.send(message)


async def send_magic_link_email(email, unique_id, host):
    sign_in_link = f"{host}auth/magic?code={quote(unique_id)}&email={quote(email)}"

    if not is_email_enabled():
        logger.debug(f"Email sending disabled. Share this sign-in link with the user: {sign_in_link}")
        return

    template = env.get_template("magic_link.html")

    html_content = template.render(link=sign_in_link, code=unique_id)

    await asyncio.to_thread(
        _send_email,
        {
            "sender": os.environ.get("RESEND_EMAIL", "noreply@khoj.dev"),
            "to": email,
            "subject": "Your login code to Khoj",
            "html": html_content,
        },
    )


async def send_welcome_email(name, email):
    if not is_email_enabled():
        logger.debug("Email sending disabled")
        return

    template = env.get_template("welcome.html")

    html_content = template.render(name=name if not is_none_or_empty(name) else "you")

    await asyncio.to_thread(
        _send_email,
        {
            "sender": os.environ.get("RESEND_EMAIL", "team@khoj.dev"),
            "to": email,
            "subject": f"{name}, four ways to use Khoj" if name else "Four ways to use Khoj",
            "html": html_content,
        },
    )

    if settings.EMAIL_HOST or not RESEND_AUDIENCE_ID:
        return

    contact_params = {
        "email": email,
        "audience_id": RESEND_AUDIENCE_ID,
    }

    if name:
        contact_params["first_name"] = name

    await asyncio.to_thread(resend.Contacts.create, contact_params)


async def send_query_feedback(uquery, kquery, sentiment, user_email):
    if not is_email_enabled():
        logger.debug(f"Sentiment: {sentiment}, Query: {uquery}, Khoj Response: {kquery}")
        return

    logger.info(f"Sending feedback email for query {uquery}")

    # render feedback email using feedback.html as template
    template = env.get_template("feedback.html")
    html_content = template.render(
        uquery=uquery if not is_none_or_empty(uquery) else "N/A",
        kquery=kquery if not is_none_or_empty(kquery) else "N/A",
        sentiment=sentiment if not is_none_or_empty(sentiment) else "N/A",
        user_email=user_email if not is_none_or_empty(user_email) else "N/A",
    )
    # send feedback to fixed account
    await asyncio.to_thread(
        _send_email,
        {
            "sender": os.environ.get("RESEND_EMAIL", "noreply@khoj.dev"),
            "to": "team@khoj.dev",
            "subject": "User Feedback",
            "html": html_content,
        },
    )
    return {"message": "Sent Email"}


def send_task_email(name, email, query, result, subject, is_image=False):
    if not is_email_enabled():
        logger.debug("Email sending disabled")
        return

    logger.info(f"Sending email to {email} for task {subject}")

    template = env.get_template("task.html")

    if is_image:
        image = result.get("image")
        result = f"![{subject}]({image})"

    html_result = markdown_it.MarkdownIt().render(result)
    html_content = template.render(name=name, subject=subject, query=query, result=html_result)

    r = _send_email(
        {
            "sender": f"Khoj <{os.environ.get('RESEND_EMAIL', 'khoj@khoj.dev')}>",
            "to": email,
            "subject": f"✨ {subject}",
            "html": html_content,
        }
    )
    return r
