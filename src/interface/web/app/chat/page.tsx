'use client'

import styles from './chat.module.css';
import React, { Suspense, useEffect, useRef, useState } from 'react';

import SuggestionCard from '../components/suggestions/suggestionCard';
import SidePanel from '../components/sidePanel/chatHistorySidePanel';
import ChatHistory from '../components/chatHistory/chatHistory';
import NavMenu from '../components/navMenu/navMenu';
import { useSearchParams } from 'next/navigation'
import Loading from '../components/loading/loading';

import { handleCompiledReferences, handleImageResponse, setupWebSocket } from '../common/chatFunctions';

import 'katex/dist/katex.min.css';
import { Lightbulb, ArrowCircleUp, FileArrowUp, Microphone } from '@phosphor-icons/react';

import { Label } from "@/components/ui/label"
import { Textarea } from "@/components/ui/textarea"
import { Button } from '@/components/ui/button';
import { Context, OnlineContextData, StreamMessage } from '../components/chatMessage/chatMessage';

interface ChatInputProps {
    sendMessage: (message: string) => void;
    sendDisabled: boolean;
}

function ChatInputArea(props: ChatInputProps) {
    const [message, setMessage] = useState('');

    useEffect(() => {
        if (message.startsWith('/')) {
            const command = message.split(' ')[0].substring(1);
            console.log('Command is:', command);
        }
    }, [message]);

    function onSendMessage() {
        props.sendMessage(message);
        setMessage('');
    }

    return (
        <>
            <Button
                variant={'ghost'}
                className="!bg-none p-1 h-auto text-3xl rounded-full text-gray-300 hover:text-gray-500"
                disabled={props.sendDisabled}>
                <FileArrowUp weight='fill' />
            </Button>
            <div className="grid w-full gap-1.5">
                {/* <Label htmlFor="message">Your message</Label> */}
                <Textarea
                    className='border-none min-h-[60px]'
                    placeholder="Type / to see a list of commands"
                    id="message"
                    value={message}
                    onKeyDown={(e) => {
                        if (e.key === 'Enter' && !e.shiftKey) {
                            e.preventDefault();
                            onSendMessage();
                        }
                    }}
                    onChange={(e) => setMessage(e.target.value)}
                    disabled={props.sendDisabled} />
            </div>
            <Button
                variant={'ghost'}
                className="!bg-none p-1 h-auto text-3xl rounded-full text-gray-300 hover:text-gray-500"
                disabled={props.sendDisabled}>
                <Microphone weight='fill' />
            </Button>
            <Button
                className="bg-orange-300 hover:bg-orange-500 rounded-full p-0 h-auto text-3xl transition transform hover:-translate-y-1"
                onClick={onSendMessage}
                disabled={props.sendDisabled}>
                <ArrowCircleUp />
            </Button>
        </>
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
    setQueryToProcess: (query: string) => void;
    streamedMessages: StreamMessage[];
}


function ChatBodyData(props: ChatBodyDataProps) {
    const searchParams = useSearchParams();
    const conversationId = searchParams.get('conversationId');
    const [message, setMessage] = useState('');
    const [processingMessage, setProcessingMessage] = useState(false);

    useEffect(() => {
        if (conversationId) {
            props.onConversationIdChange?.(conversationId);
        }
    }, [conversationId, props.onConversationIdChange]);

    // useEffect(() => {
    //     // Reset the processing message whenever the streamed messages are updated
    //     if (props.streamedMessages) {
    //         setProcessingMessage(false);
    //     }
    // }, [props.streamedMessages]);

    useEffect(() => {
        if (message) {
            setProcessingMessage(true);
            props.setQueryToProcess(message);
            // setTimeout(() => {
            //     setProcessingMessage(false);
            // }, 1000);
        }
    }, [message]);

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
                <ChatHistory conversationId={conversationId} setTitle={props.setTitle} pendingMessage={processingMessage ? message : ''} incomingMessages={props.streamedMessages} />
            </div>
            <div className={`${styles.inputBox} bg-background align-middle items-center justify-center`}>
                <ChatInputArea sendMessage={(message) => setMessage(message)} sendDisabled={processingMessage} />
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
                    if (chunk.image && chunk.detail) {
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

    }, []);

    useEffect(() => {
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
            console.log("Messages", messages);
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

    // useEffect(() => {
    //     console.log("messages", messages);
    // }, [messages]);

    useEffect(() => {
        if (processQuerySignal && chatWS) {
            setProcessQuerySignal(false);
            chatWS.onmessage = handleWebSocketMessage;
            chatWS?.send(queryToProcess);
        }
    }, [processQuerySignal]);

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
            <div className={styles.sidePanel}>
                <SidePanel webSocketConnected={chatWS !== null} conversationId={conversationId} />
            </div>
            <div className={styles.chatBox}>
                <NavMenu selected="Chat" title={title} />
                <div className={styles.chatBoxBody}>
                    <Suspense fallback={<Loading />}>
                        <ChatBodyData
                            streamedMessages={messages}
                            chatOptionsData={chatOptionsData}
                            setTitle={setTitle}
                            setQueryToProcess={setQueryToProcess}
                            onConversationIdChange={handleConversationIdChange} />
                    </Suspense>
                </div>
            </div>
        </div>
    )
}
