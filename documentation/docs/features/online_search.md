# Online Search

By default, Khoj will try to infer which information-sourcing tools are required to answer your question. Sometimes, you'll have a need for outside questions that the LLM's knowledge doesn't cover. In that case, it will use the `online` search feature.

For example, these queries would trigger an online search:
- What's the latest news about the Israel-Palestine war?
- Where can I find the best pizza in New York City?
- Deadline for filing taxes 2024.
- Give me a summary of this article: https://en.wikipedia.org/wiki/Haitian_Revolution

Try it out yourself! https://app.khoj.dev

## Self-Hosting

The general online search function currently requires an API key from Serper.dev. You can grab one here: https://serper.dev/, and then add it as an environment variable with the name `SERPER_DEV_API_KEY`.

Without any API keys, Khoj will use the `requests` library to directly read any webpages you give it a link to. This means that you can use Khoj to read any webpage that you have access in your local network.
