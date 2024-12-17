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

### Search

Online search can work even with self-hosting! You have a few options:

- If you're using Docker, online search should work out of the box with [searxng](https://github.com/searxng/searxng) using our standard `docker-compose.yml`.
- For a non-local, free solution, you can use [JinaAI's reader API](https://jina.ai/reader/) to search online and read webpages. You can get a free API key via https://jina.ai/reader. Set the `JINA_API_KEY` environment variable to your Jina AI reader API key to enable online search.
- To get production-grade, fast online search, set the `SERPER_DEV_API_KEY` environment variable to your [Serper.dev](https://serper.dev/) API key. These search results include additional context like answer box, knowledge graph etc.

### Webpage Reading

Out of the box, you **don't have to do anything to enable webpage reading**. Khoj will automatically read webpages by using the `requests` library. To get more distributed and scalable webpage reading, you can use the following options:

- If you're using Jina AI's reader API for search, it should work automatically for webpage reading as well.
- For scalable webpage scraping, you can use [Firecrawl](https://www.firecrawl.dev/). Create a new [Webscraper](http://localhost:42110/server/admin/database/webscraper/add/). Set your Firecrawl API key to the Api Key field, and set the type to Firecrawl.
- For advanced webpage reading, you can use [Olostep](https://www.olostep.com/). This has a higher success rate at reading webpages than the default webpage readers. Create a new [Webscraper](http://localhost:42110/server/admin/database/webscraper/add/). Set your Olostep API key to the Api Key field, and set the type to Olostep.
