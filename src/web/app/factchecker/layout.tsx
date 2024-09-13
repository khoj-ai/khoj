import type { Metadata } from "next";

export const metadata: Metadata = {
    title: "Khoj AI - Fact Checker",
    description:
        "Use the Fact Checker with Khoj AI for verifying statements. It can research the internet for you, either refuting or confirming the statement using fresh data.",
    icons: {
        icon: "/static/assets/icons/khoj_lantern.ico",
        apple: "/static/assets/icons/khoj_lantern_256x256.png",
    },
    openGraph: {
        siteName: "Khoj AI",
        title: "Khoj AI - Fact Checker",
        description: "Your Second Brain.",
        url: "https://app.khoj.dev/factchecker",
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
