import type { Metadata } from "next";

import "../globals.css";


export const metadata: Metadata = {
    title: "Khoj AI - Search",
    description: "Search through all the documents you've shared with Khoj AI using natural language queries.",
    icons: {
        icon: '/static/favicon.ico',
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
        </div>
    );
}
