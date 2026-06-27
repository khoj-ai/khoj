import type { Metadata } from "next";

import "../globals.css";

export const metadata: Metadata = {
    title: "AlphaMind AI - Search",
    description:
        "Find anything in documents you've shared with AlphaMind using natural language queries.",
    icons: {
        icon: "/static/assets/icons/alphamind_lantern.ico",
        apple: "/static/assets/icons/alphamind_lantern_256x256.png",
    },
    openGraph: {
        siteName: "AlphaMind AI",
        title: "AlphaMind AI - Search",
        description: "Your Second Brain.",
        url: "https://app.alphamind.dev/search",
        type: "website",
        images: [
            {
                url: "https://assets.alphamind.dev/alphamind_lantern_256x256.png",
                width: 256,
                height: 256,
            },
        ],
    },
};

export default function ChildLayout({
    children,
}: Readonly<{
    children: React.ReactNode;
}>) {
    return <>{children}</>;
}
