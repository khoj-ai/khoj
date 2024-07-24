
import type { Metadata } from "next";
import NavMenu from '../components/navMenu/navMenu';
import styles from './agentsLayout.module.css';

import "../globals.css";
import SidePanel from "../components/sidePanel/chatHistorySidePanel";
import { nullable } from "zod";

export const metadata: Metadata = {
    title: "Khoj AI - Agents",
    description: "Use Agents with Khoj AI for deeper, more personalized queries.",
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
            <div className={`${styles.agentsLayout}`}>
                <NavMenu selected="Agents" showLogo={false} />
                {children}
            </div>
        </div>
    );
}
