import logging
import os

import resend
from django.conf import settings
from jinja2 import Environment, FileSystemLoader

from khoj.utils.helpers import is_none_or_empty

logger = logging.getLogger(__name__)


RESEND_API_KEY = os.getenv("RESEND_API_KEY")

static_files = os.path.join(settings.BASE_DIR, "static")

env = Environment(loader=FileSystemLoader(static_files))

if not RESEND_API_KEY:
    logger.info("RESEND_API_KEY not set - email sending disabled")


resend.api_key = RESEND_API_KEY


async def send_welcome_email(name, email):
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
