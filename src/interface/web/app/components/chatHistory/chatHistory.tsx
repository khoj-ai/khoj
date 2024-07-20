'use client'

import styles from './chatHistory.module.css';
import { useRef, useEffect, useState } from 'react';

import ChatMessage, { ChatHistoryData, SingleChatMessage } from '../chatMessage/chatMessage';

import renderMathInElement from 'katex/contrib/auto-render';
import 'katex/dist/katex.min.css';
import 'highlight.js/styles/github.css'

interface ChatResponse {
    status: string;
    response: ChatHistoryData;
}

interface ChatHistory {
    [key: string]: string
}

interface ChatHistoryProps {
    conversationId: string;
    setReferencePanelData: Function;
    setShowReferencePanel: Function;
}


export default function ChatHistory(props: ChatHistoryProps) {
    const [data, setData] = useState<ChatHistoryData | null>(null);
    const [isLoading, setLoading] = useState(true)
    const ref = useRef<HTMLDivElement>(null);
    const chatHistoryRef = useRef(null);


	useEffect(() => {

        fetch(`/api/chat/history?client=web&conversation_id=${props.conversationId}&n=10`)
            .then(response => response.json())
            .then((chatData: ChatResponse) => {
                setLoading(false);
                // Render chat options, if any
                if (chatData) {
                    console.log(chatData);
                    setData(chatData.response);
                }
            })
            .catch(err => {
                console.error(err);
                return;
            });
	}, [props.conversationId]);


    useEffect(() => {
        const observer = new MutationObserver((mutationsList, observer) => {
            // If the addedNodes property has one or more nodes
            for(let mutation of mutationsList) {
                if(mutation.type === 'childList' && mutation.addedNodes.length > 0) {
                    // Call your function here
                    renderMathInElement(document.body, {
                        delimiters: [
                            { left: '$$', right: '$$', display: true },
                            { left: '\\[', right: '\\]', display: true },
                            { left: '$', right: '$', display: false },
                            { left: '\\(', right: '\\)', display: false },
                        ],
                    });
                }
            }
        });

        if (chatHistoryRef.current) {
            observer.observe(chatHistoryRef.current, { childList: true });
        }

        // Clean up the observer on component unmount
        return () => observer.disconnect();
    }, []);

    if (isLoading) {
        return <h2>🌀 Loading...</h2>;
    }

    return (
        <div className={styles.main + " " + styles.chatLayout}>
            <div ref={ref}>
                <div className={styles.chatHistory} ref={chatHistoryRef}>
                    {(data && data.chat) && data.chat.map((chatMessage, index) => (
                        <ChatMessage
                            key={index}
                            chatMessage={chatMessage}
                            setReferencePanelData={props.setReferencePanelData}
                            setShowReferencePanel={props.setShowReferencePanel}
                        />
                    ))}
                </div>
            </div>
        </div>
    )
}
