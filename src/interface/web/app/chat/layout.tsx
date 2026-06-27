import type { Metadata } from "next";
import "../globals.css";
import { Toaster } from "@/components/ui/toaster";

export const metadata: Metadata = {
    title: "AlphaMind AI - Chat",
    description:
        "Ask anything. Research answers from across the internet and your documents, draft messages, summarize documents, generate paintings and chat with personal agents.",
    icons: {
        icon: "/static/assets/icons/alphamind_lantern.ico",
        apple: "/static/assets/icons/alphamind_lantern_256x256.png",
    },
    openGraph: {
        siteName: "AlphaMind AI",
        title: "AlphaMind AI - Chat",
        description:
            "Ask anything. Research answers from across the internet and your documents, draft messages, summarize documents, generate paintings and chat with personal agents.",
        url: "https://app.alphamind.dev/chat",
        type: "website",
        images: [
            {
                url: "https://assets.alphamind.dev/alphamind_hero.png",
                width: 940,
                height: 525,
            },
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
    return (
        <>
            {children}
            <Toaster />
            <script
                dangerouslySetInnerHTML={{
                    __html: `window.EXCALIDRAW_ASSET_PATH = 'https://assets.alphamind.dev/@excalidraw/excalidraw/dist/';`,
                }}
            />
        </>
    );
}
