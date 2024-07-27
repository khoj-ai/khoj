'use client'

import styles from './sharedChat.module.css';
import React, { Suspense, useEffect, useRef, useState } from 'react';

import SidePanel from '../../components/sidePanel/chatHistorySidePanel';
import ChatHistory from '../../components/chatHistory/chatHistory';
import NavMenu from '../../components/navMenu/navMenu';
import Loading from '../../components/loading/loading';

import 'katex/dist/katex.min.css';

import { welcomeConsole } from '../../common/utils';
import { useAuthenticatedData } from '@/app/common/auth';

import ChatInputArea, { ChatOptions } from '@/app/components/chatInputArea/chatInputArea';
import { StreamMessage } from '@/app/components/chatMessage/chatMessage';
import { handleCompiledReferences, handleImageResponse, setupWebSocket } from '@/app/common/chatFunctions';
import { AgentData } from '@/app/agents/page';


interface ChatBodyDataProps {
    chatOptionsData: ChatOptions | null;
    setTitle: (title: string) => void;
    setUploadedFiles: (files: string[]) => void;
    isMobileWidth?: boolean;
    publicConversationSlug: string;
    streamedMessages: StreamMessage[];
    isLoggedIn: boolean;
    conversationId?: string;
    setQueryToProcess: (query: string) => void;
}


function ChatBodyData(props: ChatBodyDataProps) {
    const [message, setMessage] = useState('');
    const [processingMessage, setProcessingMessage] = useState(false);
    const [agentMetadata, setAgentMetadata] = useState<AgentData | null>(null);

    useEffect(() => {
        if (message) {
            setProcessingMessage(true);
            props.setQueryToProcess(message);
        }
    }, [message]);

    useEffect(() => {
        console.log("Streamed messages", props.streamedMessages);
        if (props.streamedMessages &&
            props.streamedMessages.length > 0 &&
            props.streamedMessages[props.streamedMessages.length - 1].completed) {

            setProcessingMessage(false);
        } else {
            setMessage('');
        }
    }, [props.streamedMessages]);

    if (!props.publicConversationSlug && !props.conversationId) {
        return (
            <div className={styles.suggestions}>
                Whoops, nothing to see here!
            </div>
        );
    }

    return (
        <>
            <div className={false ? styles.chatBody : styles.chatBodyFull}>
                <ChatHistory
                    publicConversationSlug={props.publicConversationSlug}
                    conversationId={props.conversationId || ''}
                    setAgent={setAgentMetadata}
                    setTitle={props.setTitle}
                    pendingMessage={processingMessage ? message : ''}
                    incomingMessages={props.streamedMessages} />
            </div>
            <div className={`${styles.inputBox} shadow-md bg-background align-middle items-center justify-center px-3`}>
                <ChatInputArea
                    isLoggedIn={props.isLoggedIn}
                    sendMessage={(message) => setMessage(message)}
                    sendDisabled={processingMessage}
                    chatOptionsData={props.chatOptionsData}
                    conversationId={props.conversationId}
                    agentColor={agentMetadata?.color}
                    isMobileWidth={props.isMobileWidth}
                    setUploadedFiles={props.setUploadedFiles} />
            </div>
        </>
    );
}

export default function SharedChat() {
    const [chatOptionsData, setChatOptionsData] = useState<ChatOptions | null>(null);
    const [isLoading, setLoading] = useState(true);
    const [title, setTitle] = useState('Khoj AI - Chat');
    const [conversationId, setConversationID] = useState<string | undefined>(undefined);
    const [chatWS, setChatWS] = useState<WebSocket | null>(null);
    const [messages, setMessages] = useState<StreamMessage[]>([]);
    const [queryToProcess, setQueryToProcess] = useState<string>('');
    const [processQuerySignal, setProcessQuerySignal] = useState(false);
    const [uploadedFiles, setUploadedFiles] = useState<string[]>([]);
    const [isMobileWidth, setIsMobileWidth] = useState(false);
    const [paramSlug, setParamSlug] = useState<string | undefined>(undefined);

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

        setParamSlug(window.location.pathname.split('/').pop() || '');

    }, []);

    useEffect(() => {
        if (queryToProcess && !conversationId) {
            fetch(`/api/chat/share/fork?public_conversation_slug=${paramSlug}`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
            })
                .then(response => response.json())
                .then(data => {
                    setConversationID(data.conversation_id);
                })
                .catch(err => {
                    console.error(err);
                    return;
                });
            return;
        }


        if (chatWS && queryToProcess) {
            // Add a new object to the state
            const newStreamMessage: StreamMessage = {
                rawResponse: "",
                trainOfThought: [],
                context: [],
                onlineContext: {},
                completed: false,
                timestamp: (new Date()).toISOString(),
                rawQuery: queryToProcess || "",
            }
            setMessages(prevMessages => [...prevMessages, newStreamMessage]);
            setProcessQuerySignal(true);
        } else {
            if (!chatWS) {
                console.error("No WebSocket connection available");
            }
            if (!queryToProcess) {
                console.error("No query to process");
            }
        }
    }, [queryToProcess]);

    useEffect(() => {
        if (processQuerySignal && chatWS) {
            setProcessQuerySignal(false);
            chatWS.onmessage = handleWebSocketMessage;
            chatWS?.send(queryToProcess);
        }
    }, [processQuerySignal]);

    useEffect(() => {
        if (chatWS) {
            chatWS.onmessage = handleWebSocketMessage;
        }
    }, [chatWS]);

    useEffect(() => {
        (async () => {
            if (conversationId) {
                const newWS = await setupWebSocket(conversationId, queryToProcess);
                if (!newWS) {
                    console.error("No WebSocket connection available");
                    return;
                }
                setChatWS(newWS);

                // Add a new object to the state
                const newStreamMessage: StreamMessage = {
                    rawResponse: "",
                    trainOfThought: [],
                    context: [],
                    onlineContext: {},
                    completed: false,
                    timestamp: (new Date()).toISOString(),
                    rawQuery: queryToProcess || "",
                }
                setMessages(prevMessages => [...prevMessages, newStreamMessage]);
            }
        })();
    }, [conversationId]);

    if (isLoading) {
        return <Loading />;
    }

    if (!paramSlug) {
        return (
            <div className={styles.suggestions}>
                Whoops, nothing to see here!
            </div>
        );
    }


    return (
        <div className={`${styles.main} ${styles.chatLayout}`}>
            <title>
                {title}
            </title>
            <div className={styles.sidePanel}>
                <SidePanel
                    webSocketConnected={!!conversationId ? (chatWS != null) : true}
                    conversationId={conversationId ?? null}
                    uploadedFiles={uploadedFiles}
                    isMobileWidth={isMobileWidth}
                />
            </div>

            <div className={styles.chatBox}>
                <NavMenu selected="Chat" title={title} />
                <div className={styles.chatBoxBody}>
                    <Suspense fallback={<Loading />}>
                        <ChatBodyData
                            conversationId={conversationId}
                            streamedMessages={messages}
                            setQueryToProcess={setQueryToProcess}
                            isLoggedIn={authenticatedData !== null}
                            publicConversationSlug={paramSlug}
                            chatOptionsData={chatOptionsData}
                            setTitle={setTitle}
                            setUploadedFiles={setUploadedFiles}
                            isMobileWidth={isMobileWidth} />
                    </Suspense>
                </div>
            </div>
        </div>
    )
}
