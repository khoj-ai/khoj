This is a [Next.js](https://nextjs.org/) project.

## Getting Started

First, install the dependencies:

```bash
bun install
```

In case you run into any dependency linking issues, you can try running:

```bash
bun add next
```

### Run the development server:

```bash
bun dev
```

Make sure the `rewrites` in `next.config.mjs` are set up correctly for your environment. The rewrites are used to proxy requests to the API server.

```js
    rewrites: async () => {
        return [
            {
                source: '/api/:path*',
                destination: 'http://localhost:42110/api/:path*',
            },
        ];
    },
```

The `destination` should be the URL of the API server.

Open [http://localhost:3000](http://localhost:3000) with your browser to see the result.

You can start editing the page by modifying any of the `.tsx` pages. The page auto-updates as you edit the file.

### Testing built files

We've setup a utility command for building and serving the built files. This is useful for testing the production build locally.

1. Exporting code
   To build the files once and serve them, run:

```bash
bun export
```

If you're using Windows:

```bash
bun windowsexport
```

2. Continuously building code

To keep building the files and serving them, run:

```bash
bun watch
```

If you're using Windows:

```bash
bun windowswatch
```

Now you should be able to load your custom pages from the Khoj app at http://localhost:42110/. To server any of the built files, you should update the routes in the `web_client.py` like so, where `new_file` is the new page you've added in this repo:

```python
@web_client.post("/new_route", response_class=FileResponse)
@requires(["authenticated"], redirect="login_page")
def index_post(request: Request):

    return templates.TemplateResponse(
        "new_file/index.html",
        context={
            "request": request,
        },
    )
```

## Learn More

To learn more about Next.js, take a look at the following resources:

- [Next.js Documentation](https://nextjs.org/docs) - learn about Next.js features and API.
- [Next.js App Router](https://nextjs.org/docs/app) - learn about the Next.js router.
- [Learn Next.js](https://nextjs.org/learn) - an interactive Next.js tutorial.

You can check out [the Next.js GitHub repository](https://github.com/vercel/next.js/) - your feedback and contributions are welcome!
