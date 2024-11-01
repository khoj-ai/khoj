---
---

# Code Execution

Khoj can generate and run very simple Python code snippets as well. This is useful if you want to generate a plot, run a simple calculation, or do some basic data manipulation. LLMs by default aren't skilled at complex quantitative tasks. Code generation & execution can come in handy for such tasks.

Just use `/code` in your chat command.

### Setup (Self-Hosting)
Run [Cohere's Terrarium](https://github.com/cohere-ai/cohere-terrarium) on your machine to enable code generation and execution.

Check the [instructions](https://github.com/cohere-ai/cohere-terrarium?tab=readme-ov-file#development) for running from source.

For running with Docker, you can use our [docker-compose.yml](https://github.com/khoj-ai/khoj/blob/master/docker-compose.yml), or start it manually like this:

```bash
docker pull ghcr.io/khoj-ai/terrarium:latest
docker run -d -p 8080:8080 ghcr.io/khoj-ai/terrarium:latest
```

#### Verify
Verify that it's running, by evaluating a simple Python expression:

```bash
curl -X POST -H "Content-Type: application/json" \
--url http://localhost:8080 \
--data-raw '{"code": "1 + 1"}' \
--no-buffer
```
