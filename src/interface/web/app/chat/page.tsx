'use client'

import styles from './chat.module.css';
import React, { Suspense, useEffect, useState } from 'react';

import SidePanel from '../components/sidePanel/chatHistorySidePanel';
import ChatHistory from '../components/chatHistory/chatHistory';
import NavMenu from '../components/navMenu/navMenu';
import { useSearchParams } from 'next/navigation'
import Loading from '../components/loading/loading';

import { handleCompiledReferences, handleImageResponse, setupWebSocket } from '../common/chatFunctions';

import 'katex/dist/katex.min.css';

import { StreamMessage } from '../components/chatMessage/chatMessage';
import { welcomeConsole } from '../common/utils';
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

    useEffect(() => {
        const storedMessage = localStorage.getItem("message");
        if (storedMessage) {
            setMessage(storedMessage);
        }
    }, []);

    useEffect(() => {
        if (message) {
            setProcessingMessage(true);
            props.setQueryToProcess(message);
        }
    }, [message]);

    useEffect(() => {
        if (conversationId) {
            props.onConversationIdChange?.(conversationId);
        }
    }, [conversationId]);

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
    const [chatOptionsData, setChatOptionsData] = useState<ChatOptions | null>(null);
    const [isLoading, setLoading] = useState(true);
    const [title, setTitle] = useState('Khoj AI - Chat');
    const [conversationId, setConversationID] = useState<string | null>(null);
    const [chatWS, setChatWS] = useState<WebSocket | null>(null);
    const [messages, setMessages] = useState<StreamMessage[]>([]);
    const [queryToProcess, setQueryToProcess] = useState<string>('');
    const [processQuerySignal, setProcessQuerySignal] = useState(false);
    const [uploadedFiles, setUploadedFiles] = useState<string[]>([]);
    const [isMobileWidth, setIsMobileWidth] = useState(false);

    const authenticatedData = useAuthenticatedData();
    welcomeConsole();

    const handleWebSocketMessage = (event: MessageEvent) => {
        let chunk = event.data;
        let currentMessage = messages.find(message => !message.completed);
        if (!currentMessage) {
            console.error("No current message found");
            return;
        }

        // Process WebSocket streamed data
        if (chunk === "start_llm_response") {
            console.log("Started streaming", new Date());
        } else if (chunk === "end_llm_response") {
            currentMessage.completed = true;
        } else {
            // Get the current message
            // Process and update state with the new message
            if (chunk.includes("application/json")) {
                chunk = JSON.parse(chunk);
            }

            const contentType = chunk["content-type"];
            if (contentType === "application/json") {
                try {
                    if (chunk.image || chunk.detail) {
                        let responseWithReference = handleImageResponse(chunk);
                        console.log("Image response", responseWithReference);
                        if (responseWithReference.response) currentMessage.rawResponse = responseWithReference.response;
                        if (responseWithReference.online) currentMessage.onlineContext = responseWithReference.online;
                        if (responseWithReference.context) currentMessage.context = responseWithReference.context;
                    } else if (chunk.type == "status") {
                        currentMessage.trainOfThought.push(chunk.message);
                    } else if (chunk.type == "rate_limit") {
                        console.log("Rate limit message", chunk);
                        currentMessage.rawResponse = chunk.message;
                    } else {
                        console.log("any message", chunk);
                    }
                } catch (error) {
                    console.error("Error processing message", error);
                    currentMessage.completed = true;
                } finally {
                    // no-op
                }
            } else {
                // Update the current message with the new chunk
                if (chunk && chunk.includes("### compiled references:")) {
                    let responseWithReference = handleCompiledReferences(chunk, "");
                    currentMessage.rawResponse += responseWithReference.response;

                    if (responseWithReference.response) currentMessage.rawResponse = responseWithReference.response;
                    if (responseWithReference.online) currentMessage.onlineContext = responseWithReference.online;
                    if (responseWithReference.context) currentMessage.context = responseWithReference.context;
                } else {
                    // If the chunk is not a JSON object, just display it as is
                    currentMessage.rawResponse += chunk;
                }
            }
        };
        // Update the state with the new message, currentMessage
        setMessages([...messages]);
    }

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

        setIsMobileWidth(window.innerWidth < 786);

        window.addEventListener('resize', () => {
            setIsMobileWidth(window.innerWidth < 786);
        });

    }, []);

    useEffect(() => {
        if (chatWS) {
            chatWS.onmessage = handleWebSocketMessage;
        }
    }, [chatWS, messages]);

    //same as ChatBodyData for local storage message
    useEffect(() => {
        const storedMessage = localStorage.getItem("message");
        setQueryToProcess(storedMessage || '');
    }, []);

    useEffect(() => {
        if (chatWS && queryToProcess) {
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

            if (chatWS.readyState === WebSocket.OPEN) {
                chatWS.send(queryToProcess);
                setProcessQuerySignal(true);
            }
            else {
                console.error("WebSocket is not open. ReadyState:", chatWS.readyState);
            }

            setQueryToProcess('');
        }
    }, [queryToProcess, chatWS]);

    useEffect(() => {
        if (processQuerySignal && chatWS && chatWS.readyState === WebSocket.OPEN) {
            setProcessQuerySignal(false);
            chatWS.onmessage = handleWebSocketMessage;
            chatWS.send(queryToProcess);
            localStorage.removeItem("message");
        }
    }, [processQuerySignal, chatWS]);

    useEffect(() => {
        const setupWebSocketConnection = async () => {
            if (conversationId && (!chatWS || chatWS.readyState === WebSocket.CLOSED)) {
                if (queryToProcess) {
                    const newWS = await setupWebSocket(conversationId, queryToProcess);
                    localStorage.removeItem("message");
                    setChatWS(newWS);
                }
                else {
                    const newWS = await setupWebSocket(conversationId);
                    setChatWS(newWS);
                }
            }
        };
        setupWebSocketConnection();
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
            <div>
                <SidePanel
                    webSocketConnected={chatWS !== null}
                    conversationId={conversationId}
                    uploadedFiles={uploadedFiles}
                    isMobileWidth={isMobileWidth}
                />
            </div>
            <div className={styles.chatBox}>
                <NavMenu selected="Chat" title={title} />
                <div className={styles.chatBoxBody}>
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
