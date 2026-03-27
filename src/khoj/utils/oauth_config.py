"""OAuth provider configuration helpers."""
import logging
import os
from typing import Dict, Optional

import httpx

from khoj.utils.helpers import is_env_var_true

logger = logging.getLogger(__name__)


def _fetch_oidc_metadata(issuer: str) -> Dict:
    """Pre-fetch OIDC discovery metadata AND JWKS at startup.
    Resolves everything eagerly so Authlib makes zero HTTP requests at request time."""
    import time

    issuer = issuer.rstrip("/")
    metadata_url = f"{issuer}/.well-known/openid-configuration"
    resp = httpx.get(metadata_url, follow_redirects=True, timeout=10, trust_env=False)
    resp.raise_for_status()
    metadata = resp.json()

    # Pre-fetch JWKS so Authlib doesn't need to fetch it at token verification time
    jwks_uri = metadata.get("jwks_uri")
    if jwks_uri:
        jwks_resp = httpx.get(jwks_uri, follow_redirects=True, timeout=10, trust_env=False)
        jwks_resp.raise_for_status()
        metadata["jwks"] = jwks_resp.json()

    # Mark as loaded so Authlib won't try to re-fetch
    metadata["_loaded_at"] = time.time()

    return metadata


def get_oauth_config() -> Optional[Dict]:
    """Get OAuth configuration. Uses OIDC discovery if issuer provided, otherwise manual endpoints."""
    if not is_env_var_true("GENERIC_OAUTH_ENABLED"):
        return None

    client_id = os.getenv("GENERIC_OAUTH_CLIENT_ID", "").strip()
    client_secret = os.getenv("GENERIC_OAUTH_CLIENT_SECRET", "").strip()

    if not client_id or not client_secret:
        return None

    config = {
        "name": "oauth",
        "client_id": client_id,
        "client_secret": client_secret,
        "scope": os.getenv("GENERIC_OAUTH_SCOPE", "openid profile email").strip(),
        "provider_name": os.getenv("GENERIC_OAUTH_PROVIDER_NAME", "OAuth").strip(),
        "button_label": (os.getenv("GENERIC_OAUTH_BUTTON_LABEL") or "").strip() or None,
    }

    # OIDC Discovery mode (preferred) — fetch metadata eagerly at startup
    issuer = (os.getenv("GENERIC_OAUTH_ISSUER") or "").strip() or None
    if issuer:
        try:
            metadata = _fetch_oidc_metadata(issuer)
            # Pass resolved endpoints so Authlib doesn't fetch metadata at request time
            config["authorize_url"] = metadata["authorization_endpoint"]
            config["access_token_url"] = metadata["token_endpoint"]
            # Pass full metadata as server_metadata so Authlib can verify JWT tokens
            # (needs issuer, jwks_uri, id_token_signing_alg_values_supported, etc.)
            config["server_metadata"] = metadata
            logger.info(f"OIDC discovery OK: authorize={config['authorize_url']}")
        except Exception as e:
            logger.error(f"Failed to fetch OIDC metadata from {issuer}: {e}")
            return None
    else:
        # Manual endpoint mode (for non-OIDC providers)
        auth_endpoint = (os.getenv("GENERIC_OAUTH_AUTHORIZATION_ENDPOINT") or "").strip() or None
        token_endpoint = (os.getenv("GENERIC_OAUTH_TOKEN_ENDPOINT") or "").strip() or None
        userinfo_endpoint = (os.getenv("GENERIC_OAUTH_USERINFO_ENDPOINT") or "").strip() or None

        if auth_endpoint:
            config["authorize_url"] = auth_endpoint
        if token_endpoint:
            config["access_token_url"] = token_endpoint
        if userinfo_endpoint:
            config["userinfo_endpoint"] = userinfo_endpoint

        jwks_uri = (os.getenv("GENERIC_OAUTH_JWKS_URI") or "").strip() or None
        if jwks_uri:
            config["jwks_uri"] = jwks_uri

    return config


def get_all_oauth_configs() -> Dict:
    """Get all configured OAuth providers."""
    configs = {}

    oauth_config = get_oauth_config()
    if oauth_config:
        configs["oauth"] = oauth_config

    return configs
