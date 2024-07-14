
import type { Metadata } from "next";
import NavMenu from '../components/navMenu/navMenu';
import styles from './automationsLayout.module.css';
import { Toaster } from "@/components/ui/toaster";

import "../globals.css";


export const metadata: Metadata = {
    title: "Khoj AI - Automations",
    description: "Use Autoomations with Khoj to simplify the process of running repetitive tasks.",
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
        <div className={`${styles.automationsLayout}`}>
            <NavMenu selected="Automations" showLogo={true} />
            {children}
            <Toaster />
        </div>
    );
}
