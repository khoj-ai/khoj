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
- To get production-grade, fast online search, set the `SERPER_DEV_API_KEY` environment variable to your [Serper.dev](https://serper.dev/) API key. These search results include additional context like answer box, knowledge graph etc.
- To use open, self-hostable search provider, set the `FIRECRAWL_API_KEY` environment variable to your [Firecrawl](https://firecrawl.dev) API key. These search results do not scrape social media results.
- To use Exa search provider, set the `EXA_API_KEY` environment variable to your [Exa](https://exa.ai) API key.

### Webpage Reading

Out of the box, you **don't have to do anything to enable webpage reading**. Khoj will automatically read webpages by using the `requests` library. To get faster, more readable webpages for Khoj, you can use the following options:

- For open, self-hostable webpage reader, you can use [Firecrawl](https://www.firecrawl.dev/). Create a new [Webscraper](http://localhost:42110/server/admin/database/webscraper/add/). Set your Firecrawl API key to the Api Key field, and set the type to Firecrawl.
- For advanced webpage reading, you can use [Olostep](https://www.olostep.com/). This can read a wider variety of webpages. Create a new [Webscraper](http://localhost:42110/server/admin/database/webscraper/add/). Set your Olostep API key to the Api Key field, and set the type to Olostep.
- For fast webpage reading, you can use [Exa](https://exa.ai). Create a new [Webscraper](http://localhost:42110/server/admin/database/webscraper/add/). Set your Exa API key to the Api Key field, and set the type to Exa.
