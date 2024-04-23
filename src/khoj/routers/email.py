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

from fastapi import APIRouter

email = APIRouter()


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
            "from": "team@khoj.dev",
            "to": email,
            "subject": f"Welcome to Khoj, {name}!" if name else "Welcome to Khoj!",
            "html": html_content,
        }
    )


from pydantic import BaseModel


class TextData(BaseModel):
    uquery: str
    kquery: str
    sentiment: str


@email.post("/sendmail")
async def feedback(data: TextData):
    # console debug messages
    print("SENDING USER FEEDBACK...")
    print(f"User Query: {data.uquery}\n")
    print(f"Khoj Response: {data.kquery}\n")
    print(f"Sentiment: {data.sentiment}\n")
    # rendering feedback email using feedback.html as template
    template = env.get_template("feedback.html")
    html_content = template.render(
        uquery=data.uquery if not is_none_or_empty(data.uquery) else "N/A",
        kquery=data.kquery if not is_none_or_empty(data.kquery) else "N/A",
        sentiment=data.sentiment if not is_none_or_empty(data.sentiment) else "N/A",
    )
    # send feedback from two fixed accounts
    r = resend.Emails.send(
        {
            "from": "saba@khoj.dev",
            "to": "team@khoj.dev",
            "subject": f"User Feedback",
            "html": html_content,
        }
    )

    return {"message": "Sent Email"}
