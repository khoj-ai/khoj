"""Tests for OAuth authentication: config helpers, database adapters, and userinfo extraction."""

import os
from unittest.mock import AsyncMock, patch

os.environ["DJANGO_ALLOW_ASYNC_UNSAFE"] = "true"

import pytest
from asgiref.sync import sync_to_async

from khoj.database.adapters import (
    create_user_by_oauth,
    get_or_create_user_oauth,
    get_user_by_oauth,
    get_user_name,
    get_user_photo,
)
from khoj.database.models import KhojUser, OAuthAccount, Subscription
from khoj.routers.auth import get_oauth_userinfo
from khoj.utils.oauth_config import get_all_oauth_configs, get_oauth_config
from tests.helpers import UserFactory


# ============================================================================
# OAuth Config Tests
# ============================================================================


class TestGetOAuthConfig:
    def test_returns_none_when_disabled(self):
        with patch.dict(os.environ, {}, clear=True):
            assert get_oauth_config() is None

    def test_returns_none_when_enabled_but_missing_credentials(self):
        with patch.dict(os.environ, {"GENERIC_OAUTH_ENABLED": "True"}, clear=True):
            assert get_oauth_config() is None

    def test_returns_none_when_missing_client_secret(self):
        env = {"GENERIC_OAUTH_ENABLED": "True", "GENERIC_OAUTH_CLIENT_ID": "id"}
        with patch.dict(os.environ, env, clear=True):
            assert get_oauth_config() is None

    @patch("khoj.utils.oauth_config.httpx.get")
    def test_returns_config_with_oidc_discovery(self, mock_get):
        from unittest.mock import MagicMock
        mock_response = MagicMock()
        mock_response.raise_for_status.return_value = None
        mock_response.json.return_value = {
            "authorization_endpoint": "https://accounts.google.com/o/oauth2/v2/auth",
            "token_endpoint": "https://oauth2.googleapis.com/token",
            "userinfo_endpoint": "https://openidconnect.googleapis.com/v1/userinfo",
            "jwks_uri": "https://www.googleapis.com/oauth2/v3/certs",
        }
        mock_get.return_value = mock_response

        env = {
            "GENERIC_OAUTH_ENABLED": "True",
            "GENERIC_OAUTH_CLIENT_ID": "my-id",
            "GENERIC_OAUTH_CLIENT_SECRET": "my-secret",
            "GENERIC_OAUTH_ISSUER": "https://accounts.google.com",
            "GENERIC_OAUTH_PROVIDER_NAME": "Google",
        }
        with patch.dict(os.environ, env, clear=True):
            config = get_oauth_config()

        assert config is not None
        assert config["client_id"] == "my-id"
        assert config["authorize_url"] == "https://accounts.google.com/o/oauth2/v2/auth"
        assert config["access_token_url"] == "https://oauth2.googleapis.com/token"
        assert config["provider_name"] == "Google"
        assert "server_metadata_url" not in config

    def test_returns_config_with_manual_endpoints(self):
        env = {
            "GENERIC_OAUTH_ENABLED": "True",
            "GENERIC_OAUTH_CLIENT_ID": "my-id",
            "GENERIC_OAUTH_CLIENT_SECRET": "my-secret",
            "GENERIC_OAUTH_AUTHORIZATION_ENDPOINT": "https://example.com/authorize",
            "GENERIC_OAUTH_TOKEN_ENDPOINT": "https://example.com/token",
            "GENERIC_OAUTH_USERINFO_ENDPOINT": "https://example.com/userinfo",
            "GENERIC_OAUTH_JWKS_URI": "https://example.com/jwks",
        }
        with patch.dict(os.environ, env, clear=True):
            config = get_oauth_config()

        assert config is not None
        assert config["authorize_url"] == "https://example.com/authorize"
        assert config["access_token_url"] == "https://example.com/token"
        assert config["userinfo_endpoint"] == "https://example.com/userinfo"
        assert config["jwks_uri"] == "https://example.com/jwks"
        assert "server_metadata_url" not in config

    def test_custom_scope(self):
        env = {
            "GENERIC_OAUTH_ENABLED": "True",
            "GENERIC_OAUTH_CLIENT_ID": "id",
            "GENERIC_OAUTH_CLIENT_SECRET": "secret",
            "GENERIC_OAUTH_SCOPE": "openid email",
        }
        with patch.dict(os.environ, env, clear=True):
            config = get_oauth_config()

        assert config["scope"] == "openid email"

    def test_button_label(self):
        env = {
            "GENERIC_OAUTH_ENABLED": "True",
            "GENERIC_OAUTH_CLIENT_ID": "id",
            "GENERIC_OAUTH_CLIENT_SECRET": "secret",
            "GENERIC_OAUTH_BUTTON_LABEL": "Sign in with SSO",
        }
        with patch.dict(os.environ, env, clear=True):
            config = get_oauth_config()

        assert config["button_label"] == "Sign in with SSO"


class TestGetAllOAuthConfigs:
    def test_empty_when_disabled(self):
        with patch.dict(os.environ, {}, clear=True):
            assert get_all_oauth_configs() == {}

    def test_contains_oauth_key_when_configured(self):
        env = {
            "GENERIC_OAUTH_ENABLED": "True",
            "GENERIC_OAUTH_CLIENT_ID": "id",
            "GENERIC_OAUTH_CLIENT_SECRET": "secret",
        }
        with patch.dict(os.environ, env, clear=True):
            configs = get_all_oauth_configs()

        assert "oauth" in configs
        assert configs["oauth"]["client_id"] == "id"


# ============================================================================
# OAuth Adapter Tests
# ============================================================================


SAMPLE_USERINFO = {
    "sub": "google-12345",
    "email": "test@example.com",
    "name": "Test User",
    "given_name": "Test",
    "family_name": "User",
    "picture": "https://example.com/photo.jpg",
}


@pytest.mark.django_db(transaction=True)
class TestCreateUserByOAuth:
    @pytest.mark.asyncio
    async def test_creates_user_and_oauth_account(self):
        user = await create_user_by_oauth("google", SAMPLE_USERINFO)

        assert user is not None
        assert user.email == "test@example.com"
        assert user.verified_email is True

        oauth_account = await OAuthAccount.objects.filter(user=user).afirst()
        assert oauth_account is not None
        assert oauth_account.provider == "google"
        assert oauth_account.provider_user_id == "google-12345"
        assert oauth_account.email == "test@example.com"
        assert oauth_account.name == "Test User"
        assert oauth_account.given_name == "Test"

    @pytest.mark.asyncio
    async def test_creates_standard_subscription(self):
        user = await create_user_by_oauth("google", SAMPLE_USERINFO)

        subscription = await Subscription.objects.filter(user=user).afirst()
        assert subscription is not None
        assert subscription.type == Subscription.Type.STANDARD

    @pytest.mark.asyncio
    async def test_returns_none_when_no_email(self):
        user_info = {"sub": "no-email-user", "name": "No Email"}
        user = await create_user_by_oauth("google", user_info)
        assert user is None

    @pytest.mark.asyncio
    async def test_handles_missing_optional_fields(self):
        minimal_info = {"sub": "minimal-user", "email": "minimal@example.com"}
        user = await create_user_by_oauth("google", minimal_info)

        assert user is not None
        oauth_account = await OAuthAccount.objects.filter(user=user).afirst()
        assert oauth_account.name == ""
        assert oauth_account.given_name == ""
        assert oauth_account.family_name == ""
        assert oauth_account.picture == ""

    @pytest.mark.asyncio
    async def test_updates_existing_user_by_email(self):
        existing = await sync_to_async(UserFactory)(email="existing@example.com", username="existing@example.com")
        user_info = {**SAMPLE_USERINFO, "email": "existing@example.com", "sub": "oauth-existing"}

        user = await create_user_by_oauth("google", user_info)

        assert user.id == existing.id
        assert user.verified_email is True

    @pytest.mark.asyncio
    async def test_does_not_duplicate_subscription(self):
        user = await create_user_by_oauth("google", SAMPLE_USERINFO)
        # Call again — should not create a second subscription
        user2 = await create_user_by_oauth("google", SAMPLE_USERINFO)

        assert user.id == user2.id
        count = await Subscription.objects.filter(user=user).acount()
        assert count == 1


@pytest.mark.django_db(transaction=True)
class TestGetUserByOAuth:
    @pytest.mark.asyncio
    async def test_finds_existing_user(self):
        created = await create_user_by_oauth("google", SAMPLE_USERINFO)
        found = await get_user_by_oauth("google", SAMPLE_USERINFO)

        assert found is not None
        assert found.id == created.id

    @pytest.mark.asyncio
    async def test_returns_none_for_unknown_sub(self):
        user_info = {"sub": "nonexistent-sub"}
        found = await get_user_by_oauth("google", user_info)
        assert found is None

    @pytest.mark.asyncio
    async def test_returns_none_for_wrong_provider(self):
        await create_user_by_oauth("google", SAMPLE_USERINFO)
        found = await get_user_by_oauth("authentik", SAMPLE_USERINFO)
        assert found is None


@pytest.mark.django_db(transaction=True)
class TestGetOrCreateUserOAuth:
    @pytest.mark.asyncio
    async def test_creates_new_user(self):
        user = await get_or_create_user_oauth("google", SAMPLE_USERINFO)
        assert user is not None
        assert user.email == "test@example.com"

    @pytest.mark.asyncio
    async def test_gets_existing_user(self):
        created = await create_user_by_oauth("google", SAMPLE_USERINFO)
        found = await get_or_create_user_oauth("google", SAMPLE_USERINFO)

        assert found.id == created.id
        # Should not create a second OAuthAccount
        count = await OAuthAccount.objects.filter(user=found).acount()
        assert count == 1


# ============================================================================
# get_user_name / get_user_photo with OAuthAccount
# ============================================================================


@pytest.mark.django_db(transaction=True)
class TestUserNameAndPhoto:
    @pytest.mark.asyncio
    async def test_get_user_name_from_oauth(self):
        user = await create_user_by_oauth("google", SAMPLE_USERINFO)
        name = await sync_to_async(get_user_name)(user)
        assert name == "Test"

    @pytest.mark.asyncio
    async def test_get_user_name_prefers_full_name(self):
        user = await create_user_by_oauth("google", SAMPLE_USERINFO)
        user.first_name = "Override"
        user.last_name = "Name"
        await user.asave()

        name = await sync_to_async(get_user_name)(user)
        assert name == "Override Name"

    @pytest.mark.asyncio
    async def test_get_user_photo_from_oauth(self):
        user = await create_user_by_oauth("google", SAMPLE_USERINFO)
        photo = await sync_to_async(get_user_photo)(user)
        assert photo == "https://example.com/photo.jpg"

    @pytest.mark.asyncio
    async def test_get_user_photo_returns_none_without_oauth(self):
        user = await sync_to_async(UserFactory)()
        photo = await sync_to_async(get_user_photo)(user)
        assert photo is None


# ============================================================================
# get_oauth_userinfo Tests
# ============================================================================


class TestGetOAuthUserinfo:
    @pytest.mark.asyncio
    async def test_returns_userinfo_from_token_data(self):
        """When Authlib already parsed the ID token, use that directly."""
        oauth_client = AsyncMock()
        token_data = {
            "access_token": "abc",
            "userinfo": {"sub": "123", "email": "a@b.com", "name": "A"},
        }

        result = await get_oauth_userinfo(oauth_client, token_data)

        assert result == {"sub": "123", "email": "a@b.com", "name": "A"}
        oauth_client.userinfo.assert_not_called()

    @pytest.mark.asyncio
    async def test_fetches_userinfo_via_authlib(self):
        """When token doesn't contain userinfo, call oauth_client.userinfo()."""
        oauth_client = AsyncMock()
        oauth_client.userinfo.return_value = {"sub": "456", "email": "b@c.com"}
        token_data = {"access_token": "abc"}

        result = await get_oauth_userinfo(oauth_client, token_data)

        assert result == {"sub": "456", "email": "b@c.com"}
        oauth_client.userinfo.assert_called_once_with(token=token_data)

    @pytest.mark.asyncio
    async def test_falls_back_to_token_data_on_error(self):
        """When userinfo fetch fails, return raw token data."""
        oauth_client = AsyncMock()
        oauth_client.userinfo.side_effect = Exception("Network error")
        token_data = {"access_token": "abc", "email": "fallback@test.com"}

        result = await get_oauth_userinfo(oauth_client, token_data)

        assert result == token_data

    @pytest.mark.asyncio
    async def test_falls_back_when_userinfo_returns_none(self):
        """When userinfo returns None, fall back to token data."""
        oauth_client = AsyncMock()
        oauth_client.userinfo.return_value = None
        token_data = {"access_token": "abc"}

        result = await get_oauth_userinfo(oauth_client, token_data)

        assert result == token_data
