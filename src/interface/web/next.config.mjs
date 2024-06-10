/** @type {import('next').NextConfig} */

const nextConfig = {
    // Use output: 'export' to enable building for static export. This cannot be used with rewrites
    // output: 'export',
    // Use rewrites when doing local development. This cannot be used with output: 'export'
    rewrites: async () => {
        return [
            {
                source: '/api/:path*',
                destination: 'http://localhost:42110/api/:path*',
            },
        ];
    },
    trailingSlash: true,
    skipTrailingSlashRedirect: true,
    // reactStrictMode: false,
    distDir: 'out',
    images: {
        remotePatterns: [
            {
              protocol: "https",
              hostname: "**.googleusercontent.com",
            },
            {
              protocol: "https",
              hostname: "generated.khoj.dev",
            },
        ],
    }

};

export default nextConfig;
