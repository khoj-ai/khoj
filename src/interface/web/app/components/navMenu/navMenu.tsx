'use client'

import styles from './navMenu.module.css';
import Image from 'next/image';
import Link from 'next/link';
import { useAuthenticatedData, UserProfile } from '@/app/common/auth';
import { useState } from 'react';


interface NavMenuProps {
    selected: string;
}

function SettingsMenu(props: UserProfile) {
    const [showSettings, setShowSettings] = useState(false);

    return (
        <div className={styles.settingsMenu}>
            <div className={styles.settingsMenuProfile} onClick={() => setShowSettings(!showSettings)}>
                <Image
                    src={props.photo || "/agents.svg"}
                    alt={props.username}
                    width={50}
                    height={50}
                />
            </div>
            {showSettings && (
                <div className={styles.settingsMenuOptions}>
                    <div className={styles.settingsMenuUsername}>{props.username}</div>
                    <Link href="/config">
                        Settings
                    </Link>
                    <Link href="https://github.com/khoj-ai/khoj">
                        Github
                    </Link>
                    <Link href="https://docs.khoj.dev">
                        Help
                    </Link>
                    <Link href="/auth/logout">
                        Logout
                    </Link>
                </div>
            )}
        </div>
    );
}
export default function NavMenu(props: NavMenuProps) {

    let userData = useAuthenticatedData();
    return (
        <div className={styles.titleBar}>
            <Link href="/">
                <Image
                    src="/khoj-logo.svg"
                    alt="Khoj Logo"
                    className={styles.logo}
                    width={100}
                    height={50}
                    priority
                />
            </Link>
            <menu className={styles.menu}>
                <a className={props.selected === "Chat" ? styles.selected : ""} href = '/chat'>
                    <Image
                        src="/chat.svg"
                        alt="Chat Logo"
                        className={styles.lgoo}
                        width={24}
                        height={24}
                        priority
                    />
                    <span>
                        Chat
                    </span>
                </a>
                <a className={props.selected === "Agents" ? styles.selected : ""} href='/agents'>
                    <Image
                        src="/agents.svg"
                        alt="Agent Logo"
                        className={styles.lgoo}
                        width={24}
                        height={24}
                        priority
                    />
                    <span>
                        Agents
                    </span>
                </a>
                <a className={props.selected === "Automations" ? styles.selected : ""} href = '/automations'>
                    <Image
                        src="/automation.svg"
                        alt="Automation Logo"
                        className={styles.lgoo}
                        width={24}
                        height={24}
                        priority
                    />
                    <span>
                        Automations
                    </span>
                </a>
                {userData && <SettingsMenu {...userData} />}
            </menu>
        </div>
    )
}
