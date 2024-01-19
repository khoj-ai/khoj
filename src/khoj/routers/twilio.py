import logging
import os

from twilio.rest import Client

from khoj.database.models import KhojUser

logger = logging.getLogger(__name__)

account_sid = os.getenv("TWILIO_ACCOUNT_SID")
auth_token = os.getenv("TWILIO_AUTH_TOKEN")
verification_sid = os.getenv("TWILIO_VERIFICATION_SID")

twilio_enabled = account_sid and auth_token and verification_sid
if twilio_enabled:
    client = Client(account_sid, auth_token)


def is_twilio_enabled():
    return twilio_enabled


def create_otp(user: KhojUser):
    """Create a new OTP for the user"""
    verification = client.verify.v2.services(verification_sid).verifications.create(
        to=str(user.phone_number), channel="whatsapp"
    )
    return verification.sid


def verify_otp(user: KhojUser, code: str):
    """Verify the OTP for the user"""
    verification_check = client.verify.v2.services(verification_sid).verification_checks.create(
        to=str(user.phone_number), code=code
    )
    return verification_check.status == "approved"
