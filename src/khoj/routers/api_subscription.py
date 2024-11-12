import json
import logging
import os
from datetime import datetime, timezone

from asgiref.sync import sync_to_async
from fastapi import APIRouter, Request, Response
from starlette.authentication import requires

from khoj.database import adapters
from khoj.database.models import KhojUser, Subscription
from khoj.routers.helpers import update_telemetry_state
from khoj.utils import state

# Stripe integration for Khoj Cloud Subscription
if state.billing_enabled:
    import stripe

    stripe.api_key = os.getenv("STRIPE_API_KEY")
endpoint_secret = os.getenv("STRIPE_SIGNING_SECRET")
logger = logging.getLogger(__name__)
subscription_router = APIRouter()


@subscription_router.post("")
async def subscribe(request: Request):
    """Webhook for Stripe to send subscription events to Khoj Cloud"""
    event = None
    try:
        payload = await request.body()
        sig_header = request.headers["stripe-signature"]
        event = stripe.Webhook.construct_event(payload, sig_header, endpoint_secret)
    except ValueError as e:
        # Invalid payload
        raise e
    except stripe.error.SignatureVerificationError as e:
        # Invalid signature
        raise e

    event_type = event["type"]
    if event_type not in {
        "invoice.paid",
        "customer.subscription.updated",
        "customer.subscription.deleted",
    }:
        logger.warning(f"Unhandled Stripe event type: {event['type']}")
        return {"success": False}

    # Retrieve the customer's details
    subscription = event["data"]["object"]
    customer_id = subscription["customer"]
    customer = stripe.Customer.retrieve(customer_id)
    customer_email = customer["email"]
    user = None
    is_new = False

    # Handle valid stripe webhook events
    success = True
    if event_type in {"invoice.paid"}:
        # Mark the user as subscribed and update the next renewal date on payment
        subscription = stripe.Subscription.list(customer=customer_id).data[0]
        renewal_date = datetime.fromtimestamp(subscription["current_period_end"], tz=timezone.utc)
        user, is_new = await adapters.set_user_subscription(
            customer_email, is_recurring=True, renewal_date=renewal_date
        )
        success = user is not None
    elif event_type in {"customer.subscription.updated"}:
        user_subscription = await sync_to_async(adapters.get_user_subscription)(customer_email)
        # Allow updating subscription status if paid user
        if user_subscription and user_subscription.renewal_date:
            # Mark user as unsubscribed or resubscribed
            is_recurring = not subscription["cancel_at_period_end"]
            user, is_new = await adapters.set_user_subscription(customer_email, is_recurring=is_recurring)
            success = user is not None
    elif event_type in {"customer.subscription.deleted"}:
        # Reset the user to trial state
        user, is_new = await adapters.set_user_subscription(
            customer_email, is_recurring=False, renewal_date=False, type=Subscription.Type.TRIAL
        )
        success = user is not None

    if user and is_new:
        update_telemetry_state(
            request=request,
            telemetry_type="api",
            api="create_user",
            metadata={"server_id": str(user.user.uuid)},
        )
        logger.log(logging.INFO, f"🥳 New User Created: {user.user.uuid}")

    logger.info(f'Stripe subscription {event["type"]} for {customer_email}')
    return {"success": success}


@subscription_router.patch("")
@requires(["authenticated"])
async def update_subscription(request: Request, operation: str):
    # Retrieve the customer's details
    email = request.user.object.email
    customers = stripe.Customer.list(email=email).auto_paging_iter()
    customer = next(customers, None)
    if customer is None:
        return {"success": False, "message": "Customer not found"}

    if operation == "cancel":
        customer_id = customer.id
        for subscription in stripe.Subscription.list(customer=customer_id):
            stripe.Subscription.modify(subscription.id, cancel_at_period_end=True)
        return {"success": True}

    elif operation == "resubscribe":
        subscriptions = stripe.Subscription.list(customer=customer.id).auto_paging_iter()
        # Find the subscription that is set to cancel at the end of the period
        for subscription in subscriptions:
            if subscription.cancel_at_period_end:
                # Update the subscription to not cancel at the end of the period
                stripe.Subscription.modify(subscription.id, cancel_at_period_end=False)
                return {"success": True}
        return {"success": False, "message": "No subscription found that is set to cancel"}

    return {"success": False, "message": "Invalid operation"}


@subscription_router.post("/trial", response_class=Response)
@requires(["authenticated"])
async def start_trial(request: Request) -> Response:
    user: KhojUser = request.user.object

    # Start a trial for the user
    updated_subscription = await adapters.astart_trial_subscription(user)

    # Return trial status as a JSON response
    return Response(
        content=json.dumps({"trial_enabled": updated_subscription is not None}),
        media_type="application/json",
        status_code=200,
    )
