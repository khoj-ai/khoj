import logging
import os

try:
    import resend
except ImportError:
    pass

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


def send_welcome_email(name, email):
    if not is_resend_enabled():
        logger.debug("Email sending disabled")
        return

    template = env.get_template("welcome.html")

    html_content = template.render(name=name if not is_none_or_empty(name) else "you")

    r = resend.Emails.send(
        {
            "from": "team@khoj.dev",
            "to": email,
            "subject": f"Welcome to Khoj, {name}!" if name else "Welcome to Khoj!",
            "html": html_content,
        }
    )


def send_task_email(name, email, query, result):
    if not is_resend_enabled():
        logger.debug("Email sending disabled")
        return

    template = env.get_template("task.html")

    html_content = template.render(name=name, query=query, result=result)

    resend.Emails.send(
        {
            "from": "Khoj <khoj@khoj.dev>",
            "to": email,
            "subject": f'✨ Your Task Results for "{query}"',
            "html": html_content,
        }
    )
