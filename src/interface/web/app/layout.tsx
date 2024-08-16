import type { Metadata } from "next";
import { Noto_Sans } from "next/font/google";
import "./globals.css";

const inter = Noto_Sans({ subsets: ["latin"] });

export const metadata: Metadata = {
    title: "Khoj AI - Home",
    description: "Your Second Brain.",
    icons: {
        icon: "/static/assets/icons/khoj_lantern.ico",
        apple: "/static/assets/icons/khoj_lantern_256x256.png",
    },
    manifest: "/static/khoj.webmanifest",
    openGraph: {
        siteName: "Khoj AI",
        title: "Khoj AI - Home",
        description: "Your Second Brain.",
        url: "https://app.khoj.dev",
        type: "website",
        images: [
            {
                url: "https://assets.khoj.dev/khoj_lantern_256x256.png",
                width: 256,
                height: 256,
            },
            {
                url: "https://assets.khoj.dev/khoj_lantern_logomarktype_1200x630.png",
                width: 1200,
                height: 630,
            },
        ],
    },
};

export default function RootLayout({
    children,
}: Readonly<{
    children: React.ReactNode;
}>) {
    return (
        <html lang="en">
            <meta
                httpEquiv="Content-Security-Policy"
                content="default-src 'self' https://assets.khoj.dev;
                       media-src * blob:;
                       script-src 'self' https://assets.khoj.dev 'unsafe-inline' 'unsafe-eval';
                       connect-src 'self' blob: https://ipapi.co/json ws://localhost:42110;
                       style-src 'self' https://assets.khoj.dev 'unsafe-inline' https://fonts.googleapis.com;
                       img-src 'self' data: blob: https://*.khoj.dev https://*.googleusercontent.com https://*.google.com/ https://*.gstatic.com;
                       font-src 'self' https://assets.khoj.dev https://fonts.gstatic.com;
                       child-src 'none';
                       object-src 'none';"
            ></meta>
            <body className={inter.className}>{children}</body>
        </html>
    );
}
