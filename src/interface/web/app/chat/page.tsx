'use client'

import styles from './chat.module.css';
import { Suspense, useEffect, useState } from 'react';

import SuggestionCard from '../components/suggestions/suggestionCard';
import SidePanel from '../components/sidePanel/chatHistorySidePanel';
import ChatHistory from '../components/chatHistory/chatHistory';
import { useSearchParams } from 'next/navigation'

import 'katex/dist/katex.min.css';

interface ChatOptions {
    [key: string]: string
}
const styleClassOptions = ['pink', 'blue', 'green', 'yellow', 'purple'];


function SearchParams({ chatOptionsData }: { chatOptionsData: ChatOptions | null }) {
	const searchParams = useSearchParams();
    console.log('searchparam',searchParams);

	const conversationId = searchParams.get('conversationId')
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
        <ChatHistory conversationId={conversationId} />
	);
}

function Loading() {
    return <h2>🌀 Loading...</h2>;
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
            <div>
                <div>
                    Khoj AI
                </div>
                <div className={styles.inputBox}>
                    <input className={styles.inputBox} type="text" placeholder="Type here..." />
                    <button className={styles.inputBox}>Send</button>
                </div>
                <Suspense fallback={<Loading />}>
                    <SearchParams chatOptionsData={chatOptionsData} />
                </Suspense>
            </div>
        </div>
    )
}
