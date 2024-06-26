
import type { Metadata } from "next";
import NavMenu from '../components/navMenu/navMenu';
import styles from './factCheckerLayout.module.css';
import Head from "next/head";

export const metadata: Metadata = {
  title: "Khoj AI - Fact Checker",
  description: "Use the Fact Checker with Khoj AI for verifying statements.",
};

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
      <div className={styles.factCheckerLayout}>
        <Head>
          <link rel="icon" type="image/png" sizes="128x128" href="/assets/icons/favicon-128x128.png" />
        </Head>
        <NavMenu selected="none" />
        {children}
      </div>
  );
}
