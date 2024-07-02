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

function TextareaWithLabel() {
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
    onConversationIdChange?: (conversationId: string) => void;
}


function ChatBodyData(props: ChatBodyDataProps) {
    const searchParams = useSearchParams();
    const conversationId = searchParams.get('conversationId');

    useEffect(() => {
        if (conversationId) {
            props.onConversationIdChange?.(conversationId);
        }
    }, [conversationId, props.onConversationIdChange]);

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
        <>
            <div className={false ? styles.chatBody : styles.chatBodyFull}>
                <ChatHistory conversationId={conversationId} setTitle={props.setTitle} />
            </div>
            <div className={`${styles.inputBox} bg-background align-middle items-center justify-center`}>
                <Button variant={'ghost'} className="!bg-none p-1 h-auto text-3xl rounded-full !hover:bg-accent ">
                    <FileArrowUp fill="hsl(var(--accent-foreground))" />
                </Button>
                <TextareaWithLabel />
                <Button variant={'ghost'} className="!bg-none p-1 h-auto text-3xl rounded-full !hover:bg-accent">
                    <Microphone fill="hsl(var(--accent-foreground))" />
                </Button>
                <Button className="bg-orange-300 hover:bg-orange-500 rounded-full p-0 h-auto text-3xl">
                    <ArrowCircleUp />
                </Button>
                {/* <input className={styles.inputBox} type="text" placeholder="Type / to see a list of commands" onInput={(e) => handleChatInput(e)} /> */}
                {/* <button className={styles.inputBox}>Send</button> */}
            </div>
        </>
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

    const handleConversationIdChange = (newConversationId: string) => {
        setConversationID(newConversationId);
    };


    if (isLoading) {
        return <Loading />;
    }


    return (
        <div className={styles.main + " " + styles.chatLayout}>
            <title>
                {title}
            </title>
            <Suspense fallback={<Loading />}>
                <div className={styles.sidePanel}>
                    <SidePanel webSocketConnected={chatWS !== null} />
                </div>
                <div className={styles.chatBox}>
                    <NavMenu selected="Chat" title={title} />
                    <div className={styles.chatBoxBody}>
                        <ChatBodyData
                            chatOptionsData={chatOptionsData}
                            setTitle={setTitle}
                            onConversationIdChange={handleConversationIdChange} />
                    </div>
                </div>
            </Suspense>
        </div>
    )
}
