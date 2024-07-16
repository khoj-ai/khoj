'use client'

import styles from './chat.module.css';
import React, { Suspense, useEffect, useState, useMemo } from 'react';

import SuggestionCard from '../components/suggestions/suggestionCard';
import AgentShortcut from '../components/agentShortcut/agentShortcut';
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

const suggestions: Suggestion[] = [["Automation", `${styles.blue}`, "/automate.svg", "Send me a summary of HackerNews every morning."], ["Automation", `${styles.blue}`, "/automate.svg", "Send me an exciting recipe every sunday"], ["Paint", `${styles.green}`, "/paint.svg", "Paint a picture of a sunset but it's made of stained glass tiles"], ["Online Search", `${styles.yellow}`, "/online_search.svg", "Search for the best attractions in Austria Hungary"]];
const sampleAgents = [["Khoj", "/khoj_agent.svg"], ["Teacher", "/teacher_agent.svg"], ["Doctor", "/doctor_agent.svg"], ["Sage", "/sage_agent.svg"]];

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
type Suggestion = [string, string, string, string];
function ChatBodyData(props: ChatBodyDataProps) {
    const searchParams = useSearchParams();
    const conversationId = searchParams.get('conversationId');
    const [message, setMessage] = useState('');
    const [processingMessage, setProcessingMessage] = useState(false);
    const [shuffledOptions, setShuffledOptions] = useState<Suggestion[]>([]);
    const [selectedAgent, setSelectedAgent] = useState<string | null>(null);

    function shuffleAndSetOptions() {
        const shuffled = [...suggestions].sort(() => 0.5 - Math.random());
        setShuffledOptions(shuffled.slice(0, 3));
    }

    function onButtonClick() {
        shuffleAndSetOptions();
    }
    useEffect(() => {
        if (props.chatOptionsData) {
            shuffleAndSetOptions();
        }
    }, [props.chatOptionsData]);

    useEffect(() => {
        if (conversationId) {
            props.onConversationIdChange?.(conversationId);
        }
    }, [conversationId, props.onConversationIdChange]);

    useEffect(() => {
        if (message) {
            setProcessingMessage(true);
            props.setQueryToProcess(message);
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
            <div>
            <div className="w-full text-center">
            <h1 className="white pb-8 w-4/5">What would you like to do?</h1>
            </div>
            <div className="w-full text-center">
            <div className="flex pb-8 ms-10 gap-2">
                {sampleAgents.map(([title, image]) => (
                    <AgentShortcut
                        key={title}
                        title={title}
                        link=""
                        image={image}
                        color=""
                        isSelected={selectedAgent === title}
                        onSelect={() => setSelectedAgent(title)}
                    />
                ))}
                <AgentShortcut key="See More" title="See More →" link="/agents" image="" color="" />
            </div>
            </div>
            <div className="w-4/5">
                <div className={`${styles.inputBox} bg-background align-middle items-center justify-center px-3`}>
                    <ChatInputArea
                        isLoggedIn={props.isLoggedIn}
                        sendMessage={(message) => setMessage(message)}
                        sendDisabled={processingMessage}
                        chatOptionsData={props.chatOptionsData}
                        conversationId={conversationId}
                        isMobileWidth={props.isMobileWidth}
                        setUploadedFiles={props.setUploadedFiles} />
                </div>
                <div className={`suggestions ${styles.suggestions} w-full flex`}>
                    {shuffledOptions.map(([key, styleClass, image, value]) => (
                        <SuggestionCard
                        key={key + Math.random()}
                        title={key}
                        body={value.length > 65 ? value.substring(0, 65) + '...' : value}
                        link='https://www.google.com' // replace with actual link if available
                        color={styleClass}
                        image={image}
                        />
                    ))}
                </div>
                <div className="flex items-center justify-center">
                    <button onClick={onButtonClick} className="m-2 p-1 rounded-lg dark:hover:bg-[var(--background-color)] hover:bg-stone-100 border border-stone-100">More Examples ⟳</button>
                </div>
            </div>
            </div>
        );

    }

    return (
        <>
            <div className={false ? styles.chatBody : styles.chatBodyFull}>
                <ChatHistory
                    conversationId={conversationId}
                    setTitle={props.setTitle}
                    pendingMessage={processingMessage ? message : ''}
                    incomingMessages={props.streamedMessages} />
            </div>
            <div className={`${styles.inputBox} bg-background align-middle items-center justify-center px-3`}>
                <ChatInputArea
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
