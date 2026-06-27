import type { Metadata } from "next";
import { Toaster } from "@/components/ui/toaster";

import "../globals.css";

export const metadata: Metadata = {
    title: "AlphaMind AI - Automations",
    description:
        "Use AlphaMind Automations to get tailored research and event based notifications directly in your inbox.",
    icons: {
        icon: "/static/assets/icons/alphamind_lantern.ico",
        apple: "/static/assets/icons/alphamind_lantern_256x256.png",
    },
    openGraph: {
        siteName: "AlphaMind AI",
        title: "AlphaMind AI - Automations",
        description:
            "Use AlphaMind Automations to get tailored research and event based notifications directly in your inbox.",
        url: "https://app.alphamind.dev/automations",
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
        </>
    );
}
