import type { Metadata } from "next";
import "../globals.css";
import { Toaster } from "@/components/ui/toaster";
import { ChatwootWidget } from "../components/chatWoot/ChatwootWidget";

export const metadata: Metadata = {
    title: "AlphaMind AI - Settings",
    description: "Configure AlphaMind to get personalized, deeper assistance.",
    icons: {
        icon: "/static/assets/icons/alphamind_lantern.ico",
        apple: "/static/assets/icons/alphamind_lantern_256x256.png",
    },
    openGraph: {
        siteName: "AlphaMind AI",
        title: "AlphaMind AI - Settings",
        description: "Setup, configure, and personalize AlphaMind, your AI research assistant.",
        url: "https://app.alphamind.dev/settings",
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
            <ChatwootWidget />
        </>
    );
}
