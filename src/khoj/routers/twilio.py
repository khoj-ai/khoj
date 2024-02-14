import logging
import os

from khoj.database.models import KhojUser

logger = logging.getLogger(__name__)

account_sid = os.getenv("TWILIO_ACCOUNT_SID")
auth_token = os.getenv("TWILIO_AUTH_TOKEN")
verification_service_sid = os.getenv("TWILIO_VERIFICATION_SID")

twilio_enabled = account_sid is not None and auth_token is not None and verification_service_sid is not None
if twilio_enabled:
    from twilio.rest import Client

    client = Client(account_sid, auth_token)


def is_twilio_enabled():
    return twilio_enabled


def create_otp(user: KhojUser):
    """Create a new OTP for the user"""
    verification = client.verify.v2.services(verification_service_sid).verifications.create(
        to=str(user.phone_number), channel="whatsapp"
    )
    return verification.sid is not None


def verify_otp(user: KhojUser, code: str):
    """Verify the OTP for the user"""
    verification_check = client.verify.v2.services(verification_service_sid).verification_checks.create(
        to=str(user.phone_number), code=code
    )
    return verification_check.status == "approved"
