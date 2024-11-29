import type { Metadata } from "next";
import { noto_sans, noto_sans_arabic } from "@/app/fonts";
import "../globals.css";

export const metadata: Metadata = {
    title: "Khoj AI - Agents",
    description: "Find a specialized agent that can help you address more specific needs.",
    icons: {
        icon: "/static/assets/icons/khoj_lantern.ico",
        apple: "/static/assets/icons/khoj_lantern_256x256.png",
    },
    openGraph: {
        siteName: "Khoj AI",
        title: "Khoj AI - Agents",
        description: "Your Second Brain.",
        url: "https://app.khoj.dev/agents",
        type: "website",
        images: [
            {
                url: "https://assets.khoj.dev/khoj_lantern_256x256.png",
                width: 256,
                height: 256,
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
        <html lang="en" className={`${noto_sans.variable} ${noto_sans_arabic.variable}`}>
            <meta
                httpEquiv="Content-Security-Policy"
                content="default-src 'self' https://assets.khoj.dev;
                       media-src * blob:;
                       script-src 'self' https://assets.khoj.dev 'unsafe-inline' 'unsafe-eval';
                       connect-src 'self' https://ipapi.co/json ws://localhost:42110;
                       style-src 'self' https://assets.khoj.dev 'unsafe-inline' https://fonts.googleapis.com;
                       img-src 'self' data: https://*.khoj.dev https://*.googleusercontent.com https://*.google.com/ https://*.gstatic.com;
                       font-src 'self' https://assets.khoj.dev https://fonts.gstatic.com;
                       child-src 'none';
                       object-src 'none';"
            ></meta>
            <body>{children}</body>
        </html>
    );
}
