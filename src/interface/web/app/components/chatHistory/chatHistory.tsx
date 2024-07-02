'use client'

import styles from './chatHistory.module.css';
import { useRef, useEffect, useState } from 'react';

import ChatMessage, { ChatHistoryData, SingleChatMessage } from '../chatMessage/chatMessage';

import ReferencePanel, { hasValidReferences} from '../referencePanel/referencePanel';

import { ScrollArea } from "@/components/ui/scroll-area"

import renderMathInElement from 'katex/contrib/auto-render';
import 'katex/dist/katex.min.css';
import 'highlight.js/styles/github.css'

import Loading from '../loading/loading';

import { Lightbulb } from "@phosphor-icons/react";

interface ChatResponse {
    status: string;
    response: ChatHistoryData;
}

interface ChatHistory {
    [key: string]: string
}

interface ChatHistoryProps {
    conversationId: string;
    setTitle: (title: string) => void;
}


export default function ChatHistory(props: ChatHistoryProps) {
    const [data, setData] = useState<ChatHistoryData | null>(null);
    const [isLoading, setLoading] = useState(true)
    const ref = useRef<HTMLDivElement>(null);
    const chatHistoryRef = useRef(null);

    const [showReferencePanel, setShowReferencePanel] = useState(true);
    const [referencePanelData, setReferencePanelData] = useState<SingleChatMessage | null>(null);

	useEffect(() => {

        fetch(`/api/chat/history?client=web&conversation_id=${props.conversationId}&n=10`)
            .then(response => response.json())
            .then((chatData: ChatResponse) => {
                setLoading(false);

                // Render chat options, if any
                if (chatData) {
                    console.log(chatData);
                    setData(chatData.response);
                    props.setTitle(chatData.response.slug);
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
                if (mutation.type === 'childList' && mutation.addedNodes.length > 0) {
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
        return <Loading />;
    }

    function constructAgentLink() {
        if (!data || !data.agent || !data.agent.slug) return `/agents`;
        return `/agents?agent=${data.agent.slug}`
    }

    function constructAgentAvatar() {
        if (!data || !data.agent || !data.agent.avatar) return `/avatar.png`;
        return data.agent.avatar;
    }

    function constructAgentName() {
        if (!data || !data.agent || !data.agent.name) return `Agent`;
        return data.agent.name;
    }

    return (
        <ScrollArea className={`h-[80vh]`}>
            <div ref={ref}>
                <div className={styles.chatHistory} ref={chatHistoryRef}>
                    {(data && data.chat) && data.chat.map((chatMessage, index) => (
                        <ChatMessage
                            key={index}
                            chatMessage={chatMessage}
                            setReferencePanelData={setReferencePanelData}
                            setShowReferencePanel={setShowReferencePanel}
                            customClassName='fullHistory'
                            borderLeftColor='orange-500'
                        />
                    ))}
                    {
                        (hasValidReferences(referencePanelData) && showReferencePanel) &&
                            <ReferencePanel referencePanelData={referencePanelData} setShowReferencePanel={setShowReferencePanel} />
                    }
                    <div className={`${styles.agentIndicator}`}>
                        <a className='no-underline mx-2 flex' href={constructAgentLink()} target="_blank" rel="noreferrer">
                            <Lightbulb color='orange' weight='fill' />
                            <span className='text-neutral-600'>{constructAgentName()}</span>
                        </a>
                    </div>
                </div>
            </div>
        </ScrollArea>
    )
}
