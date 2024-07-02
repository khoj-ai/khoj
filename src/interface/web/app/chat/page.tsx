'use client'

import styles from './chat.module.css';
import React, { Suspense, useEffect, useState } from 'react';

import SuggestionCard from '../components/suggestions/suggestionCard';
import SidePanel from '../components/sidePanel/chatHistorySidePanel';
import ChatHistory from '../components/chatHistory/chatHistory';
import NavMenu from '../components/navMenu/navMenu';
import { useSearchParams } from 'next/navigation'
import Loading from '../components/loading/loading';

import { setupWebSocket } from '../common/chatFunctions';

import 'katex/dist/katex.min.css';
import { Lightbulb, ArrowCircleUp, FileArrowUp, Microphone } from '@phosphor-icons/react';

import { Label } from "@/components/ui/label"
import { Textarea } from "@/components/ui/textarea"
import { Button } from '@/components/ui/button';

export function TextareaWithLabel() {
    return (
        <div className="grid w-full gap-1.5">
            {/* <Label htmlFor="message">Your message</Label> */}
            <Textarea className='border-none min-h-[60px]' placeholder="Type / to see a list of commands" id="message" />
        </div>
    )
}

interface ChatOptions {
    [key: string]: string
}
const styleClassOptions = ['pink', 'blue', 'green', 'yellow', 'purple'];

interface ChatBodyDataProps {
    chatOptionsData: ChatOptions | null;
    setTitle: (title: string) => void;
    setConversationID?: (conversationId: string) => void;
}


function ChatBodyData(props: ChatBodyDataProps) {
    const searchParams = useSearchParams();
    const conversationId = searchParams.get('conversationId');

    if (conversationId && props.setConversationID) {
        props.setConversationID(conversationId);
    }

    if (!conversationId) {
        return (
            <div className={styles.suggestions}>
                {props.chatOptionsData && Object.entries(props.chatOptionsData).map(([key, value]) => (
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

    return (
        <div className={false ? styles.chatBody : styles.chatBodyFull}>
            <ChatHistory conversationId={conversationId} setTitle={props.setTitle} />
        </div>
    );
}

function handleChatInput(e: React.FormEvent<HTMLInputElement>) {
    const target = e.target as HTMLInputElement;
    console.log(target.value);
}

export default function Chat() {
    const [chatOptionsData, setChatOptionsData] = useState<ChatOptions | null>(null);
    const [isLoading, setLoading] = useState(true);
    const [title, setTitle] = useState('Chat');
    const [conversationId, setConversationID] = useState<string | null>(null);
    const [chatWS, setChatWS] = useState<WebSocket | null>(null);

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

    useEffect(() => {
        (async () => {
            if (conversationId) {
                const newWS = await setupWebSocket(conversationId);
                setChatWS(newWS);
            }
        })();
    }, [conversationId]);


    if (isLoading) {
        return <Loading />;
    }


    return (
        <div className={styles.main + " " + styles.chatLayout}>
            <div className={styles.sidePanel}>
                <SidePanel webSocketConnected={chatWS !== null} />
            </div>
            <title>
                Khoj AI - Chat
            </title>
            <div className={styles.chatBox}>
                <NavMenu selected="Chat" title={title} />
                <div className={styles.chatBoxBody}>
                    <div>
                        <Suspense fallback={<Loading />}>
                            <ChatBodyData chatOptionsData={chatOptionsData} setTitle={setTitle} setConversationID={setConversationID} />
                        </Suspense>
                    </div>
                    {/* <div className={styles.agentIndicator}>
                        <a className='no-underline' href="/agents?agent=khoj" target="_blank" rel="noreferrer">
                            <Lightbulb color='var(--khoj-orange)' weight='fill' />
                            <span className='text-neutral-600'>Khoj</span>
                        </a>
                    </div> */}
                    <div className={`${styles.inputBox} bg-background align-middle items-center justify-center`}>
                        <Button className="!bg-transparent !hover:bg-transparent p-0 h-auto text-3xl">
                            <FileArrowUp fill="hsla(var(--secondary-foreground))"/>
                        </Button>
                        <TextareaWithLabel />
                        <Button className="!bg-transparent !hover:bg-transparent p-0 h-auto text-3xl">
                            <Microphone fill="hsla(var(--secondary-foreground))"/>
                        </Button>
                        <Button className="bg-orange-300 hover:bg-orange-500 rounded-full p-0 h-auto text-3xl">
                            <ArrowCircleUp/>
                        </Button>
                        {/* <input className={styles.inputBox} type="text" placeholder="Type / to see a list of commands" onInput={(e) => handleChatInput(e)} /> */}
                        {/* <button className={styles.inputBox}>Send</button> */}
                    </div>
                </div>
            </div>
        </div>
    )
}
