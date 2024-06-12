'use client'

import styles from './chat.module.css';
import React, { Suspense, useEffect, useState } from 'react';

import SuggestionCard from '../components/suggestions/suggestionCard';
import SidePanel from '../components/sidePanel/chatHistorySidePanel';
import ChatHistory from '../components/chatHistory/chatHistory';
import { SingleChatMessage } from '../components/chatMessage/chatMessage';
import NavMenu from '../components/navMenu/navMenu';
import { useSearchParams } from 'next/navigation'
import ReferencePanel, { hasValidReferences } from '../components/referencePanel/referencePanel';

import 'katex/dist/katex.min.css';

interface ChatOptions {
    [key: string]: string
}
const styleClassOptions = ['pink', 'blue', 'green', 'yellow', 'purple'];


function ChatBodyData({ chatOptionsData }: { chatOptionsData: ChatOptions | null }) {
	const searchParams = useSearchParams();
	const conversationId = searchParams.get('conversationId');
    const [showReferencePanel, setShowReferencePanel] = useState(true);
    const [referencePanelData, setReferencePanelData] = useState<SingleChatMessage | null>(null);

    if (!conversationId) {
        return (
            <div className={styles.suggestions}>
                {chatOptionsData && Object.entries(chatOptionsData).map(([key, value]) => (
                    <SuggestionCard
                        key={key}
                        title={`/${key}`}
                        body={value}
                        link='#' // replace with actual link if available
                        styleClass={styleClassOptions[Math.floor(Math.random() * styleClassOptions.length)]}
                        />
                ))}
            </div>
        );
    }

	return(
        <div className={(hasValidReferences(referencePanelData) && showReferencePanel) ? styles.chatBody : styles.chatBodyFull}>
            <ChatHistory conversationId={conversationId} setReferencePanelData={setReferencePanelData} setShowReferencePanel={setShowReferencePanel} />
            {
                (hasValidReferences(referencePanelData) && showReferencePanel) &&
                    <ReferencePanel referencePanelData={referencePanelData} setShowReferencePanel={setShowReferencePanel} />
            }
        </div>
	);
}

function Loading() {
    return <h2>🌀 Loading...</h2>;
}

function handleChatInput(e: React.FormEvent<HTMLInputElement>) {
    const target = e.target as HTMLInputElement;
    console.log(target.value);
}

export default function Chat() {
    const [chatOptionsData, setChatOptionsData] = useState<ChatOptions | null>(null);
    const [isLoading, setLoading] = useState(true)

	useEffect(() => {
        fetch('/api/chat/options')
            .then(response => response.json())
            .then((data: ChatOptions) => {
                setLoading(false);
                // Render chat options, if any
                if (data) {
                    console.log(data);
                    setChatOptionsData(data);
                }
            })
            .catch(err => {
                console.error(err);
                return;
            });
	}, []);


    return (
        <div className={styles.main + " " + styles.chatLayout}>
            <div className={styles.sidePanel}>
                <SidePanel />
            </div>
            <div className={styles.chatBox}>
                <title>
                    Khoj AI - Chat
                </title>
                <NavMenu selected="Chat" />
                <div>
                    <Suspense fallback={<Loading />}>
                        <ChatBodyData chatOptionsData={chatOptionsData} />
                    </Suspense>
                </div>
                <div className={styles.inputBox}>
                    <input className={styles.inputBox} type="text" placeholder="Type here..." onInput={(e) => handleChatInput(e)} />
                    <button className={styles.inputBox}>Send</button>
                </div>
            </div>
        </div>
    )
}
