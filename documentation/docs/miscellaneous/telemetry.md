---
sidebar_position: 1
---

# Telemetry

We collect some high level, anonymized metadata about usage of self-hosted AlphaMind. This includes:
- Client (Web, Emacs, Obsidian)
- API usage (Search, Chat)
- Configured content types (Github, Org, etc)
- Request metadata (e.g., host, referrer)

We don't send any personal information or any information from/about your content. We only send the above metadata. This helps us prioritize feature development and understand how people are using AlphaMind. Don't just take our word for it -- you can see [the code here](https://github.com/alphamind-ai/alphamind/tree/master/src/telemetry).

## Disable Telemetry

If you're self-hosting AlphaMind, you can opt out of telemetry at any time by setting the `ALPHAMIND_TELEMETRY_DISABLE` environment variable to `True` via shell or `docker-compose.yml`

If you have any questions or concerns, please reach out to us on [Discord](https://discord.gg/BDgyabRM6e).
