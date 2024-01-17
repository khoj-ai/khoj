import json
import logging
import os
from enum import Enum
from typing import Optional

import openai
import requests
import schedule
from starlette.authentication import (
    AuthCredentials,
    AuthenticationBackend,
    SimpleUser,
    UnauthenticatedUser,
)
from starlette.middleware.authentication import AuthenticationMiddleware
from starlette.middleware.sessions import SessionMiddleware
from starlette.requests import HTTPConnection

from khoj.database.adapters import (
    ConversationAdapters,
    SubscriptionState,
    aget_user_subscription_state,
    get_all_users,
    get_or_create_search_models,
)
from khoj.database.models import KhojUser, Subscription
from khoj.processor.embeddings import CrossEncoderModel, EmbeddingsModel
from khoj.routers.indexer import configure_content, configure_search, load_content
from khoj.utils import constants, state
from khoj.utils.config import SearchType
from khoj.utils.fs_syncer import collect_files
from khoj.utils.rawconfig import FullConfig

logger = logging.getLogger(__name__)


class AuthenticatedKhojUser(SimpleUser):
    def __init__(self, user):
        self.object = user
        super().__init__(user.email)


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
            Subscription.objects.create(user=default_user, type="standard", renewal_date="2100-04-01")

    async def authenticate(self, request: HTTPConnection):
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
        initialize_content(regenerate, search_type, init, user)
    except Exception as e:
        raise e


def initialize_content(regenerate: bool, search_type: Optional[SearchType] = None, init=False, user: KhojUser = None):
    # Initialize Content from Config
    if state.search_models:
        try:
            if init:
                logger.info("📬 Initializing content index...")
                state.content_index = load_content(state.config.content_type, state.content_index, state.search_models)
            else:
                logger.info("📬 Updating content index...")
                all_files = collect_files(user=user)
                state.content_index, status = configure_content(
                    state.content_index,
                    state.config.content_type,
                    all_files,
                    state.search_models,
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
    from khoj.routers.api_beta import api_beta
    from khoj.routers.auth import auth_router
    from khoj.routers.indexer import indexer
    from khoj.routers.subscription import subscription_router
    from khoj.routers.web_client import web_client

    app.include_router(api, prefix="/api")
    app.include_router(api_beta, prefix="/api/beta")
    app.include_router(indexer, prefix="/api/v1/index")
    if state.billing_enabled:
        logger.info("💳 Enabled Billing")
        app.include_router(subscription_router, prefix="/api/subscription")
    app.include_router(web_client)
    app.include_router(auth_router, prefix="/auth")


def configure_middleware(app):
    app.add_middleware(AuthenticationMiddleware, backend=UserAuthenticationBackend())
    app.add_middleware(SessionMiddleware, secret_key=os.environ.get("KHOJ_DJANGO_SECRET_KEY", "!secret"))


@schedule.repeat(schedule.every(61).minutes)
def update_search_index():
    try:
        logger.info("📬 Updating content index via Scheduler")
        for user in get_all_users():
            all_files = collect_files(user=user)
            state.content_index, success = configure_content(
                state.content_index, state.config.content_type, all_files, state.search_models, user=user
            )
        all_files = collect_files(user=None)
        state.content_index, success = configure_content(
            state.content_index, state.config.content_type, all_files, state.search_models, user=None
        )
        if not success:
            raise RuntimeError("Failed to update content index")
        logger.info("📪 Content index updated via Scheduler")
    except Exception as e:
        logger.error(f"🚨 Error updating content index via Scheduler: {e}", exc_info=True)


def configure_search_types():
    # Extract core search types
    core_search_types = {e.name: e.value for e in SearchType}

    # Dynamically generate search type enum by merging core search types with configured plugin search types
    return Enum("SearchType", core_search_types)


@schedule.repeat(schedule.every(59).minutes)
def upload_telemetry():
    if not state.config or not state.config.app or not state.config.app.should_log_telemetry or not state.telemetry:
        message = "📡 No telemetry to upload" if not state.telemetry else "📡 Telemetry logging disabled"
        logger.debug(message)
        return

    try:
        logger.debug(f"📡 Upload usage telemetry to {constants.telemetry_server}:\n{state.telemetry}")
        for log in state.telemetry:
            for field in log:
                # Check if the value for the field is JSON serializable
                try:
                    json.dumps(log[field])
                except TypeError:
                    log[field] = str(log[field])
        requests.post(constants.telemetry_server, json=state.telemetry)
    except Exception as e:
        logger.error(f"📡 Error uploading telemetry: {e}", exc_info=True)
    else:
        state.telemetry = []
