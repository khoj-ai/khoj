# Online Search

Khoj will research on the internet to ground its responses, when it determines that it would need fresh information outside its existing knowledge to answer the query. It will always show any online references it used to respond to your requests.

By default, Khoj will try to infer which information sources, it needs to read to answer your question. This can include reading your documents or researching information online. You can also explicitly trigger an online search by adding the `/online` prefix to your chat query.

Example queries that should trigger an online search:
- What's the latest news about the Israel-Palestine war?
- Where can I find the best pizza in New York City?
- /online Deadline for filing taxes 2024.
- Give me a summary of this article: https://en.wikipedia.org/wiki/Haitian_Revolution

Try it out yourself! https://app.khoj.dev

## Self-Hosting

Online search works out of the box even when self-hosting. Khoj uses [JinaAI's reader API](https://jina.ai/reader/) to search online and read webpages by default. No API key setup is necessary.

To improve online search, set the `SERPER_DEV_API_KEY` environment variable to your [Serper.dev](https://serper.dev/) API key. These search results include additional context like answer box, knowledge graph etc.

For advanced webpage reading, set the `OLOSTEP_API_KEY` environment variable to your [Olostep](https://www.olostep.com/) API key. This has a higher success rate at reading webpages than the default webpage reader.
