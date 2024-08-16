import type { Metadata } from "next";
import { Noto_Sans } from "next/font/google";
import "../globals.css";
import { Toaster } from "@/components/ui/toaster";

const inter = Noto_Sans({ subsets: ["latin"] });

export const metadata: Metadata = {
    title: "Khoj AI - Settings",
    description: "Configure Khoj to get personalized, deeper assistance.",
    icons: {
        icon: "/static/assets/icons/khoj_lantern.ico",
        apple: "/static/assets/icons/khoj_lantern_256x256.png",
    },
    openGraph: {
        siteName: "Khoj AI",
        title: "Khoj AI - Settings",
        description: "Your Second Brain.",
        url: "https://app.khoj.dev/settings",
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
        <html lang="en">
            <meta
                httpEquiv="Content-Security-Policy"
                content="default-src 'self' https://assets.khoj.dev;
                        script-src 'self' https://assets.khoj.dev 'unsafe-inline' 'unsafe-eval';
                        connect-src 'self' https://ipapi.co/json ws://localhost:42110;
                        style-src 'self' https://assets.khoj.dev 'unsafe-inline' https://fonts.googleapis.com;
                        img-src 'self' data: https://*.khoj.dev https://*.googleusercontent.com;
                        font-src 'self' https://assets.khoj.dev https://fonts.gstatic.com;
                        child-src 'none';
                        object-src 'none';"
            ></meta>
            <body className={inter.className}>
                {children}
                <Toaster />
            </body>
        </html>
    );
}
