import type { Metadata } from "next";
import { Toaster } from "@/components/ui/toaster";

import "../globals.css";
import { ContentSecurityPolicy } from "../common/layoutHelper";

export const metadata: Metadata = {
    title: "Khoj AI - Automations",
    description: "Use Automations with Khoj to simplify the process of running repetitive tasks.",
    icons: {
        icon: "/static/assets/icons/khoj_lantern.ico",
        apple: "/static/assets/icons/khoj_lantern_256x256.png",
    },
    openGraph: {
        siteName: "Khoj AI",
        title: "Khoj AI - Automations",
        description:
            "Use Automations with Khoj to simplify the process of running repetitive tasks.",
        url: "https://app.khoj.dev/automations",
        type: "website",
        images: [
            {
                url: "https://assets.khoj.dev/khoj_hero.png",
                width: 940,
                height: 525,
            },
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
        <html>
            <ContentSecurityPolicy />
            <body>
                {children}
                <Toaster />
            </body>
        </html>
    );
}
