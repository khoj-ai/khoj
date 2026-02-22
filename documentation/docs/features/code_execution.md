---
---

# Code Execution

Khoj can generate and run simple Python code as well. This is useful if you want to have Khoj do some data analysis, generate plots and reports. LLMs by default aren't skilled at complex quantitative tasks. Code generation & execution can come in handy for such tasks.

Khoj automatically infers when to use the code tool. You can also tell it explicitly to use the code tool or use the `/code` [slash command](https://docs.khoj.dev/features/chat/#commands) in your chat.

## Setup (Self-Hosting)
### Terrarium Sandbox
Use [Cohere's Terrarium](https://github.com/cohere-ai/cohere-terrarium) to host the code sandbox locally on your machine for free.

To run with Docker, use our [docker-compose.yml](https://github.com/khoj-ai/khoj/blob/master/docker-compose.yml) to automatically setup the Terrarium code sandbox, or start it manually like this:

```bash
docker pull ghcr.io/khoj-ai/terrarium:latest
docker run -d -p 8080:8080 ghcr.io/khoj-ai/terrarium:latest
```

To run from source, check [these instructions](https://github.com/khoj-ai/cohere-terrarium?tab=readme-ov-file#development).

#### Verify
Verify that it's running, by evaluating a simple Python expression:

```bash
curl -X POST -H "Content-Type: application/json" \
--url http://localhost:8080 \
--data-raw '{"code": "1 + 1"}' \
--no-buffer
```

### E2B Sandbox
[E2B](https://e2b.dev/) allows Khoj to run code on a remote but versatile sandbox with support for more python libraries. This is [not free](https://e2b.dev/pricing).

To have Khoj use E2B as the code sandbox:
1. Generate an API key on [their dashboard](https://e2b.dev/dashboard).
2. Set the `E2B_API_KEY` environment variable to it on the machine running your Khoj server.
   - When using our [docker-compose.yml](https://github.com/khoj-ai/khoj/blob/master/docker-compose.yml), uncomment and set the `E2B_API_KEY` env var in the `docker-compose.yml` file.
3. Now restart your Khoj server to switch to using the E2B code sandbox.
