
import type { Metadata } from "next";
import NavMenu from '../components/navMenu/navMenu';
import styles from './agentsLayout.module.css';

export const metadata: Metadata = {
  title: "Khoj AI - Agents",
  description: "Use Agents with Khoj AI for deeper, more personalized queries.",
};

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
      <div className={`${styles.agentsLayout}`}>
        <NavMenu selected="Agents" />
        {children}
      </div>
  );
}
