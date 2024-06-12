/** @type {import('next').NextConfig} */

const nextConfig = {
    // Use output: 'export' to enable building for static export. This cannot be used with rewrites. Use in Production
    output: 'export',
    // Use rewrites when doing local development. This cannot be used with output: 'export'. Use in Development.
    // rewrites: async () => {
    //     return [
    //         {
    //             source: '/api/:path*',
    //             destination: 'http://localhost:42110/api/:path*',
    //         },
    //     ];
    // },
    trailingSlash: true,
    skipTrailingSlashRedirect: true,
    // React Strict Mode can be disabled to test the functionality of the application in development.
    // reactStrictMode: false,
    distDir: 'out',
    images: {
        // Production
        loader: 'custom',
        // Production
        loaderFile: './image-loader.ts',
        remotePatterns: [
            {
                protocol: "https",
                hostname: "**.googleusercontent.com",
            },
            {
                protocol: "https",
                hostname: "generated.khoj.dev",
            },
            {
                protocol: "https",
                hostname: "assets.khoj.dev",
            },
            {
                protocol: "https",
                hostname: "khoj-web-bucket.s3.amazonaws.com",
            },
            {
                protocol: "https",
                hostname: "khoj-generated-images.s3.amazonaws.com",
            }
        ],
    }

};

export default nextConfig;
