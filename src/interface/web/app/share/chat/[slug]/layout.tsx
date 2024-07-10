import type { Metadata } from "next";
import { Noto_Sans } from "next/font/google";
import "../../../globals.css";

const inter = Noto_Sans({ subsets: ["latin"] });

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
    <html lang="en">
      <meta httpEquiv="Content-Security-Policy"
        content="default-src 'self' https://assets.khoj.dev;
                       script-src 'self' https://assets.khoj.dev 'unsafe-inline' 'unsafe-eval';
                       connect-src 'self' https://ipapi.co/json ws://localhost:42110;
                       style-src 'self' https://assets.khoj.dev 'unsafe-inline' https://fonts.googleapis.com;
                       img-src 'self' data: https://*.khoj.dev https://*.googleusercontent.com https://*.google.com/ https://*.gstatic.com;
                       font-src 'self' https://assets.khoj.dev https://fonts.gstatic.com;
                       child-src 'none';
                       object-src 'none';"></meta>
      <body className={inter.className}>
        {children}
      </body>
    </html>
  );
}
