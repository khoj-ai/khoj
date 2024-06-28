import json
import logging
import os
from datetime import datetime, timedelta
from enum import Enum
from typing import Optional

import openai
import requests
import schedule
from django.utils.timezone import make_aware
from fastapi import Response
from starlette.authentication import (
    AuthCredentials,
    AuthenticationBackend,
    SimpleUser,
    UnauthenticatedUser,
)
from starlette.middleware import Middleware
from starlette.middleware.authentication import AuthenticationMiddleware
from starlette.middleware.sessions import SessionMiddleware
from starlette.requests import HTTPConnection
from starlette.types import ASGIApp, Receive, Scope, Send

from khoj.database.adapters import (
    AgentAdapters,
    ClientApplicationAdapters,
    ConversationAdapters,
    ProcessLockAdapters,
    SubscriptionState,
    aget_or_create_user_by_phone_number,
    aget_user_by_phone_number,
    aget_user_subscription_state,
    delete_user_requests,
    get_all_users,
    get_or_create_search_models,
)
from khoj.database.models import ClientApplication, KhojUser, ProcessLock, Subscription
from khoj.processor.embeddings import CrossEncoderModel, EmbeddingsModel
from khoj.routers.indexer import configure_content, configure_search
from khoj.routers.twilio import is_twilio_enabled
from khoj.utils import constants, state
from khoj.utils.config import SearchType
from khoj.utils.fs_syncer import collect_files
from khoj.utils.helpers import is_none_or_empty, telemetry_disabled, timer
from khoj.utils.rawconfig import FullConfig

logger = logging.getLogger(__name__)


class AuthenticatedKhojUser(SimpleUser):
    def __init__(self, user, client_app: Optional[ClientApplication] = None):
        self.object = user
        self.client_app = client_app
        super().__init__(user.username)


class UserAuthenticationBackend(AuthenticationBackend):
    def __init__(
        self,
    ):
        from khoj.database.models import KhojApiUser, KhojUser

        self.khojuser_manager = KhojUser.objects
        self.khojapiuser_manager = KhojApiUser.objects
        self._initialize_default_user()
        super().__init__()

    def _initialize_default_user(self):
        if not self.khojuser_manager.filter(username="default").exists():
            default_user = self.khojuser_manager.create_user(
                username="default",
                email="default@example.com",
                password="default",
            )
            renewal_date = make_aware(datetime.strptime("2100-04-01", "%Y-%m-%d"))
            Subscription.objects.create(user=default_user, type="standard", renewal_date=renewal_date)

    async def authenticate(self, request: HTTPConnection):
        # Request from Web client
        current_user = request.session.get("user")
        if current_user and current_user.get("email"):
            user = (
                await self.khojuser_manager.filter(email=current_user.get("email"))
                .prefetch_related("subscription")
                .afirst()
            )
            if user:
                if not state.billing_enabled:
                    return AuthCredentials(["authenticated", "premium"]), AuthenticatedKhojUser(user)

                subscription_state = await aget_user_subscription_state(user)
                subscribed = (
                    subscription_state == SubscriptionState.SUBSCRIBED.value
                    or subscription_state == SubscriptionState.TRIAL.value
                    or subscription_state == SubscriptionState.UNSUBSCRIBED.value
                )
                if subscribed:
                    return AuthCredentials(["authenticated", "premium"]), AuthenticatedKhojUser(user)
                return AuthCredentials(["authenticated"]), AuthenticatedKhojUser(user)

        # Request from Desktop, Emacs, Obsidian clients
        if len(request.headers.get("Authorization", "").split("Bearer ")) == 2:
            # Get bearer token from header
            bearer_token = request.headers["Authorization"].split("Bearer ")[1]
            # Get user owning token
            user_with_token = (
                await self.khojapiuser_manager.filter(token=bearer_token)
                .select_related("user")
                .prefetch_related("user__subscription")
                .afirst()
            )
            if user_with_token:
                if not state.billing_enabled:
                    return AuthCredentials(["authenticated", "premium"]), AuthenticatedKhojUser(user_with_token.user)

                subscription_state = await aget_user_subscription_state(user_with_token.user)
                subscribed = (
                    subscription_state == SubscriptionState.SUBSCRIBED.value
                    or subscription_state == SubscriptionState.TRIAL.value
                    or subscription_state == SubscriptionState.UNSUBSCRIBED.value
                )
                if subscribed:
                    return AuthCredentials(["authenticated", "premium"]), AuthenticatedKhojUser(user_with_token.user)
                return AuthCredentials(["authenticated"]), AuthenticatedKhojUser(user_with_token.user)

        # Request from Whatsapp client
        client_id = request.query_params.get("client_id")
        if client_id:
            # Get the client secret, which is passed in the Authorization header
            client_secret = request.headers["Authorization"].split("Bearer ")[1]
            if not client_secret:
                return Response(
                    status_code=401,
                    content="Please provide a client secret in the Authorization header with a client_id query param.",
                )

            # Get the client application
            client_application = await ClientApplicationAdapters.aget_client_application_by_id(client_id, client_secret)
            if client_application is None:
                return AuthCredentials(), UnauthenticatedUser()
            # Get the identifier used for the user
            phone_number = request.query_params.get("phone_number")
            if is_none_or_empty(phone_number):
                return AuthCredentials(), UnauthenticatedUser()

            if not phone_number.startswith("+"):
                phone_number = f"+{phone_number}"

            create_if_not_exists = request.query_params.get("create_if_not_exists")
            if create_if_not_exists:
                user = await aget_or_create_user_by_phone_number(phone_number)
            else:
                user = await aget_user_by_phone_number(phone_number)

            if user is None:
                return AuthCredentials(), UnauthenticatedUser()

            if not state.billing_enabled:
                return AuthCredentials(["authenticated", "premium"]), AuthenticatedKhojUser(user, client_application)

            subscription_state = await aget_user_subscription_state(user)
            subscribed = (
                subscription_state == SubscriptionState.SUBSCRIBED.value
                or subscription_state == SubscriptionState.TRIAL.value
                or subscription_state == SubscriptionState.UNSUBSCRIBED.value
            )
            if subscribed:
                return (
                    AuthCredentials(["authenticated", "premium"]),
                    AuthenticatedKhojUser(user, client_application),
                )
            return AuthCredentials(["authenticated"]), AuthenticatedKhojUser(user, client_application)

        # No auth required if server in anonymous mode
        if state.anonymous_mode:
            user = await self.khojuser_manager.filter(username="default").prefetch_related("subscription").afirst()
            if user:
                return AuthCredentials(["authenticated", "premium"]), AuthenticatedKhojUser(user)

        return AuthCredentials(), UnauthenticatedUser()


def initialize_server(config: Optional[FullConfig]):
    try:
        configure_server(config, init=True)
    except Exception as e:
        logger.error(f"🚨 Failed to configure server on app load: {e}", exc_info=True)


def configure_server(
    config: FullConfig,
    regenerate: bool = False,
    search_type: Optional[SearchType] = None,
    init=False,
    user: KhojUser = None,
):
    # Update Config
    if config == None:
        logger.info(f"🚨 Khoj is not configured.\nInitializing it with a default config.")
        config = FullConfig()
    state.config = config

    if ConversationAdapters.has_valid_openai_conversation_config():
        openai_config = ConversationAdapters.get_openai_conversation_config()
        state.openai_client = openai.OpenAI(api_key=openai_config.api_key)

    # Initialize Search Models from Config and initialize content
    try:
        search_models = get_or_create_search_models()
        state.embeddings_model = dict()
        state.cross_encoder_model = dict()

        for model in search_models:
            state.embeddings_model.update(
                {
                    model.name: EmbeddingsModel(
                        model.bi_encoder,
                        model.embeddings_inference_endpoint,
                        model.embeddings_inference_endpoint_api_key,
                        query_encode_kwargs=model.bi_encoder_query_encode_config,
                        docs_encode_kwargs=model.bi_encoder_docs_encode_config,
                        model_kwargs=model.bi_encoder_model_config,
                    )
                }
            )
            state.cross_encoder_model.update(
                {
                    model.name: CrossEncoderModel(
                        model.cross_encoder,
                        model.cross_encoder_inference_endpoint,
                        model.cross_encoder_inference_endpoint_api_key,
                    )
                }
            )

        state.SearchType = configure_search_types()
        state.search_models = configure_search(state.search_models, state.config.search_type)
        setup_default_agent()

        message = "📡 Telemetry disabled" if telemetry_disabled(state.config.app) else "📡 Telemetry enabled"
        logger.info(message)

        if not init:
            initialize_content(regenerate, search_type, user)

    except Exception as e:
        raise e


def setup_default_agent():
    AgentAdapters.create_default_agent()


def initialize_content(regenerate: bool, search_type: Optional[SearchType] = None, user: KhojUser = None):
    # Initialize Content from Config
    if state.search_models:
        try:
            logger.info("📬 Updating content index...")
            all_files = collect_files(user=user)
            status = configure_content(
                all_files,
                regenerate,
                search_type,
                user=user,
            )
            if not status:
                raise RuntimeError("Failed to update content index")
        except Exception as e:
            raise e


def configure_routes(app):
    # Import APIs here to setup search types before while configuring server
    from khoj.routers.api import api
    from khoj.routers.api_agents import api_agents
    from khoj.routers.api_chat import api_chat
    from khoj.routers.api_config import api_config
    from khoj.routers.indexer import indexer
    from khoj.routers.notion import notion_router
    from khoj.routers.web_client import web_client

    app.include_router(api, prefix="/api")
    app.include_router(api_chat, prefix="/api/chat")
    app.include_router(api_agents, prefix="/api/agents")
    app.include_router(api_config, prefix="/api/config")
    app.include_router(indexer, prefix="/api/v1/index")
    app.include_router(notion_router, prefix="/api/notion")
    app.include_router(web_client)

    if not state.anonymous_mode:
        from khoj.routers.auth import auth_router

        app.include_router(auth_router, prefix="/auth")
        logger.info("🔑 Enabled Authentication")

    if state.billing_enabled:
        from khoj.routers.subscription import subscription_router

        app.include_router(subscription_router, prefix="/api/subscription")
        logger.info("💳 Enabled Billing")

    if is_twilio_enabled():
        from khoj.routers.api_phone import api_phone

        app.include_router(api_phone, prefix="/api/config/phone")
        logger.info("📞 Enabled Twilio")


def configure_middleware(app):
    class NextJsMiddleware(Middleware):
        async def __call__(self, scope: Scope, receive: Receive, send: Send) -> None:
            if scope["type"] == "http" and scope["path"].startswith("/_next"):
                scope["path"] = "/static" + scope["path"]
            await self.app(scope, receive, send)

        def __init__(self, app: ASGIApp) -> None:
            super().__init__(app)
            self.app = app

    app.add_middleware(AuthenticationMiddleware, backend=UserAuthenticationBackend())
    app.add_middleware(NextJsMiddleware)
    app.add_middleware(SessionMiddleware, secret_key=os.environ.get("KHOJ_DJANGO_SECRET_KEY", "!secret"))


def update_content_index():
    for user in get_all_users():
        all_files = collect_files(user=user)
        success = configure_content(all_files, user=user)
    all_files = collect_files(user=None)
    success = configure_content(all_files, user=None)
    if not success:
        raise RuntimeError("Failed to update content index")
    logger.info("📪 Content index updated via Scheduler")


@schedule.repeat(schedule.every(22).to(25).hours)
def update_content_index_regularly():
    ProcessLockAdapters.run_with_lock(
        update_content_index, ProcessLock.Operation.INDEX_CONTENT, max_duration_in_seconds=60 * 60 * 2
    )


@schedule.repeat(schedule.every(30).to(59).minutes)
def close_all_db_connections():
    from django import db

    db.close_old_connections()
    logger.info("🔌 Closed all database connections for explicit recycling.")


def configure_search_types():
    # Extract core search types
    core_search_types = {e.name: e.value for e in SearchType}

    # Dynamically generate search type enum by merging core search types with configured plugin search types
    return Enum("SearchType", core_search_types)


@schedule.repeat(schedule.every(2).minutes)
def upload_telemetry():
    if telemetry_disabled(state.config.app) or not state.telemetry:
        return

    try:
        logger.info(f"📡 Uploading telemetry to {constants.telemetry_server}...")
        logger.debug(f"Telemetry state:\n{state.telemetry}")
        for log in state.telemetry:
            for field in log:
                # Check if the value for the field is JSON serializable
                if log[field] is None:
                    log[field] = ""
                try:
                    json.dumps(log[field])
                except TypeError:
                    log[field] = str(log[field])
        response = requests.post(constants.telemetry_server, json=state.telemetry)
        response.raise_for_status()
    except Exception as e:
        logger.error(f"📡 Error uploading telemetry: {e}", exc_info=True)
    else:
        state.telemetry = []


@schedule.repeat(schedule.every(31).minutes)
def delete_old_user_requests():
    num_deleted = delete_user_requests()
    logger.debug(f"🗑️ Deleted {num_deleted[0]} day-old user requests")


@schedule.repeat(schedule.every(17).minutes)
def wakeup_scheduler():
    # Wake up the scheduler to ensure it runs the scheduled tasks. This is because the elected leader may not always be aware of tasks scheduled on other workers.
    if state.schedule_leader_process_lock:
        state.scheduler.wakeup()
    else:
        # Make sure the other workers don't run the scheduled tasks
        state.scheduler.pause()
