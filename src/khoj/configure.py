# Standard Packages
import sys
import logging
import json
from enum import Enum
from typing import Optional
from fastapi import Request
import requests
import os

# External Packages
import schedule
from starlette.middleware.sessions import SessionMiddleware
from starlette.middleware.authentication import AuthenticationMiddleware

from starlette.authentication import (
    AuthCredentials,
    AuthenticationBackend,
    SimpleUser,
    UnauthenticatedUser,
)

# Internal Packages
from khoj.utils import constants, state
from khoj.utils.config import (
    SearchType,
)
from khoj.utils.fs_syncer import collect_files
from khoj.utils.rawconfig import FullConfig
from khoj.routers.indexer import configure_content, load_content, configure_search
from database.models import KhojUser
from database.adapters import get_all_users


logger = logging.getLogger(__name__)


class AuthenticatedKhojUser(SimpleUser):
    def __init__(self, user):
        self.object = user
        super().__init__(user.email)


class UserAuthenticationBackend(AuthenticationBackend):
    def __init__(
        self,
    ):
        from database.models import KhojUser, KhojApiUser

        self.khojuser_manager = KhojUser.objects
        self.khojapiuser_manager = KhojApiUser.objects
        self._initialize_default_user()
        super().__init__()

    def _initialize_default_user(self):
        if not self.khojuser_manager.filter(username="default").exists():
            self.khojuser_manager.create_user(
                username="default",
                email="default@example.com",
                password="default",
            )

    async def authenticate(self, request: Request):
        current_user = request.session.get("user")
        if current_user and current_user.get("email"):
            user = await self.khojuser_manager.filter(email=current_user.get("email")).afirst()
            if user:
                return AuthCredentials(["authenticated"]), AuthenticatedKhojUser(user)
        if len(request.headers.get("Authorization", "").split("Bearer ")) == 2:
            # Get bearer token from header
            bearer_token = request.headers["Authorization"].split("Bearer ")[1]
            # Get user owning token
            user_with_token = await self.khojapiuser_manager.filter(token=bearer_token).select_related("user").afirst()
            if user_with_token:
                return AuthCredentials(["authenticated"]), AuthenticatedKhojUser(user_with_token.user)
        if state.anonymous_mode:
            user = await self.khojuser_manager.filter(username="default").afirst()
            if user:
                return AuthCredentials(["authenticated"]), AuthenticatedKhojUser(user)

        return AuthCredentials(), UnauthenticatedUser()


def initialize_server(config: Optional[FullConfig]):
    if config is None:
        logger.warning(
            f"🚨 Khoj is not configured.\nConfigure it via http://{state.host}:{state.port}/config, plugins or by editing {state.config_file}."
        )
        return None

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

    # Initialize Search Models from Config and initialize content
    try:
        state.config_lock.acquire()
        state.SearchType = configure_search_types(state.config)
        state.search_models = configure_search(state.search_models, state.config.search_type)
        initialize_content(regenerate, search_type, init, user)
    except Exception as e:
        logger.error(f"🚨 Failed to configure search models", exc_info=True)
        raise e
    finally:
        state.config_lock.release()


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
                state.content_index = configure_content(
                    state.content_index,
                    state.config.content_type,
                    all_files,
                    state.search_models,
                    regenerate,
                    search_type,
                    user=user,
                )
        except Exception as e:
            logger.error(f"🚨 Failed to index content", exc_info=True)
            raise e


def configure_routes(app):
    # Import APIs here to setup search types before while configuring server
    from khoj.routers.api import api
    from khoj.routers.api_beta import api_beta
    from khoj.routers.web_client import web_client
    from khoj.routers.indexer import indexer
    from khoj.routers.auth import auth_router

    app.include_router(api, prefix="/api")
    app.include_router(api_beta, prefix="/api/beta")
    app.include_router(indexer, prefix="/api/v1/index")
    app.include_router(web_client)
    app.include_router(auth_router, prefix="/auth")


def configure_middleware(app):
    app.add_middleware(AuthenticationMiddleware, backend=UserAuthenticationBackend())
    app.add_middleware(SessionMiddleware, secret_key=os.environ.get("KHOJ_DJANGO_SECRET_KEY", "!secret"))


if not state.demo:

    @schedule.repeat(schedule.every(61).minutes)
    def update_search_index():
        try:
            logger.info("📬 Updating content index via Scheduler")
            for user in get_all_users():
                all_files = collect_files(user=user)
                state.content_index = configure_content(
                    state.content_index, state.config.content_type, all_files, state.search_models, user=user
                )
            all_files = collect_files(user=None)
            state.content_index = configure_content(
                state.content_index, state.config.content_type, all_files, state.search_models, user=None
            )
            logger.info("📪 Content index updated via Scheduler")
        except Exception as e:
            logger.error(f"🚨 Error updating content index via Scheduler: {e}", exc_info=True)


def configure_search_types(config: FullConfig):
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
