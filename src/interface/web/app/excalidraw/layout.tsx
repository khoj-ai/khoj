import type { Metadata } from "next";

export const metadata: Metadata = {
    title: "Khoj AI - Excalidraw",
    description: "Use Excalidraw to draw diagrams, collaborate, and export to SVG, PNG, and more.",
    icons: {
        icon: "/static/assets/icons/khoj_lantern.ico",
        apple: "/static/assets/icons/khoj_lantern_256x256.png",
    },
    openGraph: {
        siteName: "Khoj AI",
        title: "Khoj AI - Excalidraw",
        description: "Your Second Brain.",
        url: "https://app.khoj.dev/excalidraw",
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
    return <div>{children}</div>;
}
