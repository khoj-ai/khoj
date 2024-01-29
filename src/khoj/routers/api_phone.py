import logging
from typing import Optional

from fastapi import APIRouter, Depends, HTTPException, Request
from starlette.authentication import requires

from khoj.database import adapters
from khoj.database.models import KhojUser
from khoj.routers.helpers import ApiUserRateLimiter, update_telemetry_state
from khoj.routers.twilio import create_otp, verify_otp

api_phone = APIRouter()
logger = logging.getLogger(__name__)


@api_phone.post("", status_code=200)
@requires(["authenticated"])
async def update_phone_number(
    request: Request,
    phone_number: str,
    client: Optional[str] = None,
    rate_limiter_per_day=Depends(
        ApiUserRateLimiter(requests=5, subscribed_requests=5, window=60 * 60 * 24, slug="update_phone")
    ),
):
    user = request.user.object

    await adapters.aset_user_phone_number(user, phone_number)
    create_otp(user)

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="set_phone_number",
        client=client,
        metadata={"phone_number": phone_number},
    )

    return {"status": "ok"}


@api_phone.delete("", status_code=200)
@requires(["authenticated"])
async def delete_phone_number(
    request: Request,
    client: Optional[str] = None,
):
    user = request.user.object

    await adapters.aremove_phone_number(user)

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="delete_phone_number",
        client=client,
    )

    return {"status": "ok"}


@api_phone.post("/verify", status_code=200)
@requires(["authenticated"])
async def verify_mobile_otp(
    request: Request,
    code: str,
    client: Optional[str] = None,
    rate_limiter_per_day=Depends(
        ApiUserRateLimiter(requests=5, subscribed_requests=5, window=60 * 60 * 24, slug="verify_phone")
    ),
):
    user: KhojUser = request.user.object

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="verify_phone_number",
        client=client,
    )

    if not verify_otp(user, code):
        raise HTTPException(status_code=400, detail="Invalid OTP")

    user.verified_phone_number = True
    await user.asave()
    return {"status": "ok"}
