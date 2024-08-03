'use client'

import styles from './chat.module.css';
import React, { Suspense, useEffect, useState } from 'react';

import SidePanel from '../components/sidePanel/chatHistorySidePanel';
import ChatHistory from '../components/chatHistory/chatHistory';
import NavMenu from '../components/navMenu/navMenu';
import { useSearchParams } from 'next/navigation'
import Loading from '../components/loading/loading';

import { processMessageChunk } from '../common/chatFunctions';

import 'katex/dist/katex.min.css';

import { Context, OnlineContext, StreamMessage } from '../components/chatMessage/chatMessage';
import { useIPLocationData, welcomeConsole } from '../common/utils';
import ChatInputArea, { ChatOptions } from '../components/chatInputArea/chatInputArea';
import { useAuthenticatedData } from '../common/auth';
import { AgentData } from '../agents/page';

interface ChatBodyDataProps {
    chatOptionsData: ChatOptions | null;
    setTitle: (title: string) => void;
    onConversationIdChange?: (conversationId: string) => void;
    setQueryToProcess: (query: string) => void;
    streamedMessages: StreamMessage[];
    setUploadedFiles: (files: string[]) => void;
    isMobileWidth?: boolean;
    isLoggedIn: boolean;
}

function ChatBodyData(props: ChatBodyDataProps) {
    const searchParams = useSearchParams();
    const conversationId = searchParams.get('conversationId');
    const [message, setMessage] = useState('');
    const [processingMessage, setProcessingMessage] = useState(false);
    const [agentMetadata, setAgentMetadata] = useState<AgentData | null>(null);

    const setQueryToProcess = props.setQueryToProcess;
    const onConversationIdChange = props.onConversationIdChange;

    useEffect(() => {
        const storedMessage = localStorage.getItem("message");
        if (storedMessage) {
            setProcessingMessage(true);
            setQueryToProcess(storedMessage);
        }
    }, [setQueryToProcess]);

    useEffect(() => {
        if (message) {
            setProcessingMessage(true);
            setQueryToProcess(message);
        }
    }, [message, setQueryToProcess]);

    useEffect(() => {
        if (conversationId) {
            onConversationIdChange?.(conversationId);
        }
    }, [conversationId, onConversationIdChange]);

    useEffect(() => {
        if (props.streamedMessages &&
            props.streamedMessages.length > 0 &&
            props.streamedMessages[props.streamedMessages.length - 1].completed) {
            setProcessingMessage(false);
        } else {
            setMessage('');
        }
    }, [props.streamedMessages]);

    if (!conversationId) {
        window.location.href = '/';
        return;
    }

    return (
        <>
            <div className={false ? styles.chatBody : styles.chatBodyFull}>
                <ChatHistory
                    conversationId={conversationId}
                    setTitle={props.setTitle}
                    setAgent={setAgentMetadata}
                    pendingMessage={processingMessage ? message : ''}
                    incomingMessages={props.streamedMessages}
                />
            </div>
            <div className={`${styles.inputBox} shadow-md bg-background align-middle items-center justify-center px-3 dark:bg-neutral-700 dark:border-0 dark:shadow-sm`}>
                <ChatInputArea
                    agentColor={agentMetadata?.color}
                    isLoggedIn={props.isLoggedIn}
                    sendMessage={(message) => setMessage(message)}
                    sendDisabled={processingMessage}
                    chatOptionsData={props.chatOptionsData}
                    conversationId={conversationId}
                    isMobileWidth={props.isMobileWidth}
                    setUploadedFiles={props.setUploadedFiles} />
            </div>
        </>
    );
}

export default function Chat() {
    const defaultTitle = 'Khoj AI - Chat';
    const [chatOptionsData, setChatOptionsData] = useState<ChatOptions | null>(null);
    const [isLoading, setLoading] = useState(true);
    const [title, setTitle] = useState(defaultTitle);
    const [conversationId, setConversationID] = useState<string | null>(null);
    const [messages, setMessages] = useState<StreamMessage[]>([]);
    const [queryToProcess, setQueryToProcess] = useState<string>('');
    const [processQuerySignal, setProcessQuerySignal] = useState(false);
    const [uploadedFiles, setUploadedFiles] = useState<string[]>([]);
    const [isMobileWidth, setIsMobileWidth] = useState(false);
    const locationData = useIPLocationData();
    const authenticatedData = useAuthenticatedData();

    useEffect(() => {
        fetch('/api/chat/options')
            .then(response => response.json())
            .then((data: ChatOptions) => {
                setLoading(false);
                // Render chat options, if any
                if (data) {
                    setChatOptionsData(data);
                }
            })
            .catch(err => {
                console.error(err);
                return;
            });

        welcomeConsole();

        setIsMobileWidth(window.innerWidth < 786);

        window.addEventListener('resize', () => {
            setIsMobileWidth(window.innerWidth < 786);
        });

    }, []);

    useEffect(() => {
        if (queryToProcess) {
            const newStreamMessage: StreamMessage = {
                rawResponse: "",
                trainOfThought: [],
                context: [],
                onlineContext: {},
                completed: false,
                timestamp: (new Date()).toISOString(),
                rawQuery: queryToProcess || "",
            };
            setMessages(prevMessages => [...prevMessages, newStreamMessage]);
            setProcessQuerySignal(true);
        }
    }, [queryToProcess]);

    useEffect(() => {
        if (processQuerySignal) {
            chat();
        }
    }, [processQuerySignal]);

    async function readChatStream(response: Response) {
        if (!response.ok) throw new Error(response.statusText);
        if (!response.body) throw new Error("Response body is null");

        const reader = response.body.getReader();
        const decoder = new TextDecoder();
        const eventDelimiter = '␃🔚␗';
        let buffer = "";

        // Track context used for chat response
        let context: Context[] = [];
        let onlineContext: OnlineContext = {};

        while (true) {
            const { done, value } = await reader.read();
            if (done) {
                setQueryToProcess('');
                setProcessQuerySignal(false);
                break;
            }

            const chunk = decoder.decode(value, { stream: true });
            buffer += chunk;

            let newEventIndex;
            while ((newEventIndex = buffer.indexOf(eventDelimiter)) !== -1) {
                const event = buffer.slice(0, newEventIndex);
                buffer = buffer.slice(newEventIndex + eventDelimiter.length);
                if (event) {
                    const currentMessage = messages.find(message => !message.completed);

                    if (!currentMessage) {
                        console.error("No current message found");
                        return;
                    }

                    // Track context used for chat response. References are rendered at the end of the chat
                    ({ context, onlineContext } = processMessageChunk(event, currentMessage, context, onlineContext));

                    setMessages([...messages]);
                }
            }
        }
    }

    async function chat() {
        localStorage.removeItem("message");
        if (!queryToProcess || !conversationId) return;
        let chatAPI = `/api/chat?q=${encodeURIComponent(queryToProcess)}&conversation_id=${conversationId}&stream=true&client=web`;
        if (locationData) {
            chatAPI += `&region=${locationData.region}&country=${locationData.country}&city=${locationData.city}&timezone=${locationData.timezone}`;
        }

        const response = await fetch(chatAPI);
        try {
            await readChatStream(response);
        } catch (err) {
            console.log(err);
        }
    }

    const handleConversationIdChange = (newConversationId: string) => {
        setConversationID(newConversationId);
    };

    if (isLoading) return <Loading />;

    return (
        <div className={styles.main + " " + styles.chatLayout}>
            <title>
                {`${defaultTitle}${(!!title && title !== defaultTitle)? `: ${title}` : ''}`}
            </title>
            {
                !isMobileWidth &&
                <div>
                    <SidePanel
                        conversationId={conversationId}
                        uploadedFiles={uploadedFiles}
                        isMobileWidth={isMobileWidth}
                    />
                </div>
            }
            <div className={styles.chatBox}>
                {
                    isMobileWidth &&
                    <div>
                        <SidePanel
                            conversationId={conversationId}
                            uploadedFiles={uploadedFiles}
                            isMobileWidth={isMobileWidth}
                        />
                    </div>
                }
                <div className={styles.chatBoxBody}>
                    {
                        !isMobileWidth &&
                        <div className={`text-nowrap text-ellipsis overflow-hidden max-w-screen-md grid items-top font-bold mr-8`}>
                            {title && <h2 className={`text-lg text-ellipsis whitespace-nowrap overflow-x-hidden pt-6`}>{title}</h2>}
                        </div>
                    }
                    <Suspense fallback={<Loading />}>
                        <ChatBodyData
                            isLoggedIn={authenticatedData !== null}
                            streamedMessages={messages}
                            chatOptionsData={chatOptionsData}
                            setTitle={setTitle}
                            setQueryToProcess={setQueryToProcess}
                            setUploadedFiles={setUploadedFiles}
                            isMobileWidth={isMobileWidth}
                            onConversationIdChange={handleConversationIdChange} />
                    </Suspense>
                </div>
            </div>
        </div>
    )
}
