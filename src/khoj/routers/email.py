import logging
import os

try:
    import resend
except ImportError:
    pass

import markdown_it
from django.conf import settings
from jinja2 import Environment, FileSystemLoader

from khoj.utils.helpers import is_none_or_empty

logger = logging.getLogger(__name__)


RESEND_API_KEY = os.getenv("RESEND_API_KEY")

static_files = os.path.join(settings.BASE_DIR, "static")

env = Environment(loader=FileSystemLoader(static_files))

if not RESEND_API_KEY:
    logger.warn("RESEND_API_KEY not set - email sending disabled")
else:
    resend.api_key = RESEND_API_KEY


def is_resend_enabled():
    return bool(RESEND_API_KEY)


async def send_welcome_email(name, email):
    if not is_resend_enabled():
        logger.debug("Email sending disabled")
        return

    template = env.get_template("welcome.html")

    html_content = template.render(name=name if not is_none_or_empty(name) else "you")

    r = resend.Emails.send(
        {
            "sender": "team@khoj.dev",
            "to": email,
            "subject": f"{name}, four ways to use Khoj" if name else "Four ways to use Khoj",
            "html": html_content,
        }
    )


async def send_query_feedback(uquery, kquery, sentiment, user_email):
    if not is_resend_enabled():
        logger.debug(f"Sentiment: {sentiment}, Query: {uquery}, Khoj Response: {kquery}")
        return

    logger.info(f"Sending feedback email for query {uquery}")

    # rendering feedback email using feedback.html as template
    template = env.get_template("feedback.html")
    html_content = template.render(
        uquery=uquery if not is_none_or_empty(uquery) else "N/A",
        kquery=kquery if not is_none_or_empty(kquery) else "N/A",
        sentiment=sentiment if not is_none_or_empty(sentiment) else "N/A",
        user_email=user_email if not is_none_or_empty(user_email) else "N/A",
    )
    # send feedback from two fixed accounts
    r = resend.Emails.send(
        {
            "sender": "saba@khoj.dev",
            "to": "team@khoj.dev",
            "subject": f"User Feedback",
            "html": html_content,
        }
    )
    return {"message": "Sent Email"}


def send_task_email(name, email, query, result, subject):
    if not is_resend_enabled():
        logger.debug("Email sending disabled")
        return

    logger.info(f"Sending email to {email} for task {subject}")

    template = env.get_template("task.html")

    html_result = markdown_it.MarkdownIt().render(result)
    html_content = template.render(name=name, subject=subject, query=query, result=html_result)

    r = resend.Emails.send(
        {
            "sender": "Khoj <khoj@khoj.dev>",
            "to": email,
            "subject": f"✨ {subject}",
            "html": html_content,
        }
    )
    return r
