'use client'

import styles from "./sidePanel.module.css";

import { useEffect, useState } from "react";

import { UserProfile } from "@/app/common/auth";
import Link from "next/link";

interface ChatHistory {
    conversation_id: string;
    slug: string;
}

function ChatSession(prop: ChatHistory) {
    return (
        <div key={prop.conversation_id} className={styles.session}>
            <Link href={`/chat?conversationId=${prop.conversation_id}`}>
                <p className={styles.session}>{prop.slug || "New Conversation 🌱"}</p>
            </Link>
        </div>
    );
}

interface ChatSessionsModalProps {
    data: ChatHistory[];
    setIsExpanded: React.Dispatch<React.SetStateAction<boolean>>;
}

function ChatSessionsModal({data, setIsExpanded}: ChatSessionsModalProps) {
    return (
        <div className={styles.modalSessionsList}>
            <div className={styles.content}>
                {data.map((chatHistory) => (
                    <ChatSession key={chatHistory.conversation_id} conversation_id={chatHistory.conversation_id} slug={chatHistory.slug} />
                ))}
                <button className={styles.showMoreButton} onClick={() => setIsExpanded(false)}>
                    Close
                </button>
            </div>
        </div>
    );
}

export default function SidePanel() {

    const [data, setData] = useState<ChatHistory[] | null>(null);
    const [dataToShow, setDataToShow] = useState<ChatHistory[] | null>(null);
    const [isLoading, setLoading] = useState(true)
    const [enabled, setEnabled] = useState(false);
    const [isExpanded, setIsExpanded] = useState<boolean>(false);

    const [userProfile, setUserProfile] = useState<UserProfile | null>(null);

	useEffect(() => {

        fetch('/api/chat/sessions', { method: 'GET' })
            .then(response => response.json())
            .then((data: ChatHistory[]) => {
                setLoading(false);
                // Render chat options, if any
                if (data) {
                    setData(data);
                    setDataToShow(data.slice(0, 5));
                }
            })
            .catch(err => {
                console.error(err);
                return;
            });

        fetch('/api/v1/user', { method: 'GET' })
            .then(response => response.json())
            .then((data: UserProfile) => {
                setUserProfile(data);
            })
            .catch(err => {
                console.error(err);
                return;
            });
	}, []);

	return (
		<div className={`${styles.panel}`}>
            {
                enabled ?
                    <div>
                        <div className={`${styles.expanded}`}>
                            <div className={`${styles.profile}`}>
                                { userProfile &&
                                    <div className={styles.profile}>
                                        <img
                                            className={styles.profile}
                                            src={userProfile.photo}
                                            alt="profile"
                                            width={24}
                                            height={24}
                                        />
                                        <p>{userProfile?.username}</p>
                                    </div>
                                }
                            </div>
                            <button className={styles.button} onClick={() => setEnabled(false)}>
                                {/* Push Close Icon */}
                                <svg fill="#000000" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><g id="SVGRepo_bgCarrier" strokeWidth="0"></g><g id="SVGRepo_tracerCarrier" strokeLinecap="round" strokeLinejoin="round"></g><g id="SVGRepo_iconCarrier"> <path d="M8.70710678,12 L19.5,12 C19.7761424,12 20,12.2238576 20,12.5 C20,12.7761424 19.7761424,13 19.5,13 L8.70710678,13 L11.8535534,16.1464466 C12.0488155,16.3417088 12.0488155,16.6582912 11.8535534,16.8535534 C11.6582912,17.0488155 11.3417088,17.0488155 11.1464466,16.8535534 L7.14644661,12.8535534 C6.95118446,12.6582912 6.95118446,12.3417088 7.14644661,12.1464466 L11.1464466,8.14644661 C11.3417088,7.95118446 11.6582912,7.95118446 11.8535534,8.14644661 C12.0488155,8.34170876 12.0488155,8.65829124 11.8535534,8.85355339 L8.70710678,12 L8.70710678,12 Z M4,5.5 C4,5.22385763 4.22385763,5 4.5,5 C4.77614237,5 5,5.22385763 5,5.5 L5,19.5 C5,19.7761424 4.77614237,20 4.5,20 C4.22385763,20 4,19.7761424 4,19.5 L4,5.5 Z"></path> </g></svg>
                            </button>
                            <h3>Recent Conversations</h3>
                        </div>
                        <div className={styles.sessionsList}>
                            {dataToShow && dataToShow.map((chatHistory) => (
                                <ChatSession key={chatHistory.conversation_id} conversation_id={chatHistory.conversation_id} slug={chatHistory.slug} />
                            ))}
                        </div>
                        {
                            (data && data.length > 5) && (
                                (isExpanded) ?
                                    <ChatSessionsModal data={data} setIsExpanded={setIsExpanded} />
                                :
                                    <button className={styles.showMoreButton} onClick={() => {
                                        setIsExpanded(true);
                                    }}>
                                        Show All
                                    </button>
                            )
                        }
                    </div>
                :
                    <div>
                        <div className={`${styles.collapsed}`}>
                                { userProfile &&
                                    <div className={`${styles.profile}`}>
                                        <img
                                            className={styles.profile}
                                            src={userProfile.photo}
                                            alt="profile"
                                            width={24}
                                            height={24}
                                        />
                                    </div>
                                }
                            <button className={styles.button} onClick={() => setEnabled(true)}>
                                {/* Pull Open Icon */}
                                <svg fill="#000000" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><g id="SVGRepo_bgCarrier" strokeWidth="0"></g><g id="SVGRepo_tracerCarrier" strokeLinecap="round" strokeLinejoin="round"></g><g id="SVGRepo_iconCarrier"> <path d="M15.2928932,12 L12.1464466,8.85355339 C11.9511845,8.65829124 11.9511845,8.34170876 12.1464466,8.14644661 C12.3417088,7.95118446 12.6582912,7.95118446 12.8535534,8.14644661 L16.8535534,12.1464466 C17.0488155,12.3417088 17.0488155,12.6582912 16.8535534,12.8535534 L12.8535534,16.8535534 C12.6582912,17.0488155 12.3417088,17.0488155 12.1464466,16.8535534 C11.9511845,16.6582912 11.9511845,16.3417088 12.1464466,16.1464466 L15.2928932,13 L4.5,13 C4.22385763,13 4,12.7761424 4,12.5 C4,12.2238576 4.22385763,12 4.5,12 L15.2928932,12 Z M19,5.5 C19,5.22385763 19.2238576,5 19.5,5 C19.7761424,5 20,5.22385763 20,5.5 L20,19.5 C20,19.7761424 19.7761424,20 19.5,20 C19.2238576,20 19,19.7761424 19,19.5 L19,5.5 Z"></path> </g></svg>
                            </button>
                        </div>
                    </div>
            }

		</div>
	);
}
