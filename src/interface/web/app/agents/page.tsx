'use client'

import styles from './agents.module.css';

import Image from 'next/image';
import Link from 'next/link';
import useSWR from 'swr';

import { useEffect, useState } from 'react';

import { useAuthenticatedData, UserProfile } from '../common/auth';


export interface AgentData {
    slug: string;
    avatar: string;
    name: string;
    personality: string;
}

async function openChat(slug: string, userData: UserProfile | null) {

    const unauthenticatedRedirectUrl = `/login?next=/agents?agent=${slug}`;
    if (!userData) {
        window.location.href = unauthenticatedRedirectUrl;
        return;
    }

    console.log("openChat", slug, userData);

    const response = await fetch(`/api/chat/sessions?agent_slug=${slug}`, { method: "POST" });
    // const data = await response.json();
    if (response.status == 200) {
        // const conversation_id = data.conversation_id;
        // window.location.href = `/chat?conversation_id=${conversation_id}`;
        window.location.href = `/chat`;
    } else if(response.status == 403 || response.status == 401) {
        window.location.href = unauthenticatedRedirectUrl;
    } else {
        alert("Failed to start chat session");
    }
}

const agentsFetcher = () => window.fetch('/api/agents').then(res => res.json()).catch(err => console.log(err));

interface AgentModalProps {
    data: AgentData;
    setShowModal: (show: boolean) => void;
    userData: UserProfile | null;
}

interface AgentCardProps {
    data: AgentData;
    userProfile: UserProfile | null;
}

function AgentModal(props: AgentModalProps) {
    const [copiedToClipboard, setCopiedToClipboard] = useState(false);

    useEffect(() => {
        if (copiedToClipboard) {
            setTimeout(() => setCopiedToClipboard(false), 3000);
        }
    }, [copiedToClipboard]);

    return (
        <div className={styles.agentModalContainer}>
            <div className={styles.agentModal}>
                <div className={styles.agentModalContent}>
                    <div className={styles.agentModalHeader}>
                        <div className={styles.agentAvatar}>
                            <Image
                                src={props.data.avatar}
                                alt={props.data.name}
                                width={50}
                                height={50}
                            />
                            <h2>{props.data.name}</h2>
                        </div>
                        <div className={styles.agentModalActions}>
                            <button onClick={() => {
                                    navigator.clipboard.writeText(`${window.location.host}/agents?agent=${props.data.slug}`);
                                    setCopiedToClipboard(true);
                            }}>
                                {
                                    copiedToClipboard ?
                                        <Image
                                            src="copy-button-success.svg"
                                            alt="Copied"
                                            width={24}
                                            height={24} />
                                        : <Image
                                            src="share.svg"
                                            alt="Copy Link"
                                            width={24}
                                            height={24} />
                                }
                            </button>
                            <button onClick={() => props.setShowModal(false)}>
                                <Image
                                        src="Close.svg"
                                        alt="Close"
                                        width={24}
                                        height={24} />
                            </button>
                        </div>
                    </div>
                    <p>{props.data.personality}</p>
                    <div className={styles.agentInfo}>
                        <button onClick={() => openChat(props.data.slug, props.userData)}>
                            Chat
                        </button>
                    </div>
                </div>
            </div>
        </div>
    );
}

function AgentCard(props: AgentCardProps) {
    const searchParams = new URLSearchParams(window.location.search);
    const agentSlug = searchParams.get('agent');
    const [showModal, setShowModal] = useState(agentSlug === props.data.slug);

    const userData = props.userProfile;

    if (showModal) {
        window.history.pushState({}, `Khoj AI - Agent ${props.data.slug}`, `/agents?agent=${props.data.slug}`);
    }

    return (
        <div className={styles.agent}>
            {
                showModal && <AgentModal data={props.data} setShowModal={setShowModal} userData={userData} />
            }
            <Link href={`/agent/${props.data.slug}`}>
                <div className={styles.agentAvatar}>
                    <Image
                        src={props.data.avatar}
                        alt={props.data.name}
                        width={50}
                        height={50}
                    />
                </div>
            </Link>
            <div className={styles.agentInfo}>
                <button className={styles.infoButton} onClick={() => setShowModal(true)}>
                    <h2>{props.data.name}</h2>
                </button>
            </div>
            <div className={styles.agentInfo}>
                <button onClick={() => openChat(props.data.slug, userData)}>
                    <Image
                        src="send.svg"
                        alt="Chat"
                        width={40}
                        height={40}
                    />
                </button>
            </div>
            <div className={styles.agentPersonality}>
                <button className={styles.infoButton} onClick={() => setShowModal(true)}>
                    <p>{props.data.personality}</p>
                </button>
            </div>
        </div>
    );
}

export default function Agents() {
    const { data, error } = useSWR<AgentData[]>('agents', agentsFetcher, { revalidateOnFocus: false });
    const userData = useAuthenticatedData();

    if (error) {
        return (
            <main className={styles.main}>
                <div className={styles.titleBar}>
                    Talk to a Specialized Agent
                </div>
                <div className={styles.agentList}>
                    Error loading agents
                </div>
            </main>
        );
    }

    if (!data) {
        return (
            <main className={styles.main}>
                <div className={styles.titleBar}>
                    Talk to a Specialized Agent
                </div>
                <div className={styles.agentList}>
                    Loading agents...
                </div>
            </main>
        );
    }

    return (
        <main className={styles.main}>
            <div className={styles.titleBar}>
                Talk to a Specialized Agent
            </div>
            <div className={styles.agentList}>
                {data.map(agent => (
                    <AgentCard key={agent.slug} data={agent} userProfile={userData} />
                ))}
            </div>
        </main>
    );
}
