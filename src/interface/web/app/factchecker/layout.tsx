import type { Metadata } from "next";

export const metadata: Metadata = {
    title: "Khoj AI - Fact Checker",
    description:
        "Use the Fact Checker with Khoj AI for verifying statements. It can research the internet for you, either refuting or confirming the statement using fresh data.",
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
    return <div>{children}</div>;
}
