import type { Metadata } from "next";
import { Toaster } from "@/components/ui/toaster";

import "../globals.css";

export const metadata: Metadata = {
    title: "Khoj AI - Automations",
    description: "Use Autoomations with Khoj to simplify the process of running repetitive tasks.",
    icons: {
        icon: "/static/assets/icons/khoj_lantern.ico",
        apple: "/static/assets/icons/khoj_lantern_256x256.png",
    },
};

export default function RootLayout({
    children,
}: Readonly<{
    children: React.ReactNode;
}>) {
    return (
        <div>
            {children}
            <Toaster />
        </div>
    );
}
