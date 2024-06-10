import styles from './navMenu.module.css';
import Image from 'next/image';

interface NavMenuProps {
    selected: string;
}

export default function NavMenu(props: NavMenuProps) {
    return (
        <div className={styles.titleBar}>
            <menu className={styles.menu}>
                <button className={props.selected === "Chat" ? styles.selected : ""} onClick={() => window.location.href = '/chat'}>
                    <Image
                        src="/chat.svg"
                        alt="Chat Logo"
                        className={styles.lgoo}
                        width={24}
                        height={24}
                        priority
                    />
                    Chat
                </button>
                <button className={props.selected === "Agents" ? styles.selected : ""} onClick={() => window.location.href = '/agents'}>
                    <Image
                        src="/agents.svg"
                        alt="Agent Logo"
                        className={styles.lgoo}
                        width={24}
                        height={24}
                        priority
                    />
                    Agents
                </button>
                <button className={props.selected === "Automations" ? styles.selected : ""} onClick={() => window.location.href = '/automations'}>
                    <Image
                        src="/automation.svg"
                        alt="Automation Logo"
                        className={styles.lgoo}
                        width={24}
                        height={24}
                        priority
                    />
                    Automations
                </button>
            </menu>
        </div>
    )
}
