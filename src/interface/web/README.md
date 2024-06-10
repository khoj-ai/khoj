This is a [Next.js](https://nextjs.org/) project.

## Getting Started

First, run the development server:

```bash
yarn dev
```

Make sure the `rewrites` in `next.config.js` are set up correctly for your environment. The rewrites are used to proxy requests to the API server.

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

## Testing built files

We've setup a utility command for building and serving the built files. This is useful for testing the production build locally.

To keep building the files and serving them, run:
```bash
yarn watch
```

To build the files once and serve them, run:
```bash
yarn export
```

## Learn More

To learn more about Next.js, take a look at the following resources:

- [Next.js Documentation](https://nextjs.org/docs) - learn about Next.js features and API.
- [Learn Next.js](https://nextjs.org/learn) - an interactive Next.js tutorial.

You can check out [the Next.js GitHub repository](https://github.com/vercel/next.js/) - your feedback and contributions are welcome!
