import type { Metadata } from "next";
import { Noto_Sans } from "next/font/google";
import "../globals.css";

const inter = Noto_Sans({ subsets: ["latin"] });

export const metadata: Metadata = {
  title: "Khoj AI - Chat",
  description: "Use this page to chat with Khoj AI.",
};

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
    <html lang="en">
      <body className={inter.className}>
        {children}
      </body>
    </html>
  );
}
