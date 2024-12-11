import type { Metadata } from "next";
import { noto_sans, noto_sans_arabic } from "@/app/fonts";
import "../../globals.css";
import { ContentSecurityPolicy } from "@/app/common/layoutHelper";

export const metadata: Metadata = {
    title: "Khoj AI - Chat",
    description: "Use this page to view a chat with Khoj AI.",
};

export default function RootLayout({
    children,
}: Readonly<{
    children: React.ReactNode;
}>) {
    return (
        <html lang="en" className={`${noto_sans.variable} ${noto_sans_arabic.variable}`}>
            <ContentSecurityPolicy />
            <body>
                {children}
                <script
                    dangerouslySetInnerHTML={{
                        __html: `window.EXCALIDRAW_ASSET_PATH = 'https://assets.khoj.dev/@excalidraw/excalidraw/dist/';`,
                    }}
                />
            </body>
        </html>
    );
}
