"use client";

import styles from "./sharedChat.module.css";
import React, { Suspense, useEffect, useRef, useState } from "react";

import SidePanel from "../../components/sidePanel/chatHistorySidePanel";
import ChatHistory from "../../components/chatHistory/chatHistory";
import NavMenu from "../../components/navMenu/navMenu";
import Loading from "../../components/loading/loading";

import "katex/dist/katex.min.css";

import { useIPLocationData, useIsMobileWidth, welcomeConsole } from "../../common/utils";
import { useAuthenticatedData } from "@/app/common/auth";

import ChatInputArea, { ChatOptions } from "@/app/components/chatInputArea/chatInputArea";
import { StreamMessage } from "@/app/components/chatMessage/chatMessage";
import { processMessageChunk } from "@/app/common/chatFunctions";
import { AgentData } from "@/app/agents/page";

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
    setImage64: (image64: string) => void;
}

function ChatBodyData(props: ChatBodyDataProps) {
    const [message, setMessage] = useState("");
    const [image, setImage] = useState<string | null>(null);
    const [processingMessage, setProcessingMessage] = useState(false);
    const [agentMetadata, setAgentMetadata] = useState<AgentData | null>(null);
    const setQueryToProcess = props.setQueryToProcess;
    const streamedMessages = props.streamedMessages;

    useEffect(() => {
        if (image) {
            props.setImage64(encodeURIComponent(image));
        }
    }, [image, props.setImage64]);

    useEffect(() => {
        if (message) {
            setProcessingMessage(true);
            setQueryToProcess(message);
        }
    }, [message, setQueryToProcess]);

    useEffect(() => {
        if (
            streamedMessages &&
            streamedMessages.length > 0 &&
            streamedMessages[streamedMessages.length - 1].completed
        ) {
            setProcessingMessage(false);
        } else {
            setMessage("");
        }
    }, [streamedMessages]);

    if (!props.publicConversationSlug && !props.conversationId) {
        return <div className={styles.suggestions}>Whoops, nothing to see here!</div>;
    }

    return (
        <>
            <div className={false ? styles.chatBody : styles.chatBodyFull}>
                <ChatHistory
                    publicConversationSlug={props.publicConversationSlug}
                    conversationId={props.conversationId || ""}
                    setAgent={setAgentMetadata}
                    setTitle={props.setTitle}
                    pendingMessage={processingMessage ? message : ""}
                    incomingMessages={props.streamedMessages}
                />
            </div>
            <div
                className={`${styles.inputBox} p-1 md:px-2 shadow-md bg-background align-middle items-center justify-center dark:bg-neutral-700 dark:border-0 dark:shadow-sm rounded-t-2xl rounded-b-none md:rounded-xl`}
            >
                <ChatInputArea
                    isLoggedIn={props.isLoggedIn}
                    sendMessage={(message) => setMessage(message)}
                    sendImage={(image) => setImage(image)}
                    sendDisabled={processingMessage}
                    chatOptionsData={props.chatOptionsData}
                    conversationId={props.conversationId}
                    agentColor={agentMetadata?.color}
                    isMobileWidth={props.isMobileWidth}
                    setUploadedFiles={props.setUploadedFiles}
                />
            </div>
        </>
    );
}

export default function SharedChat() {
    const [chatOptionsData, setChatOptionsData] = useState<ChatOptions | null>(null);
    const [isLoading, setLoading] = useState(true);
    const [title, setTitle] = useState("Khoj AI - Chat");
    const [conversationId, setConversationID] = useState<string | undefined>(undefined);
    const [messages, setMessages] = useState<StreamMessage[]>([]);
    const [queryToProcess, setQueryToProcess] = useState<string>("");
    const [processQuerySignal, setProcessQuerySignal] = useState(false);
    const [uploadedFiles, setUploadedFiles] = useState<string[]>([]);
    const [paramSlug, setParamSlug] = useState<string | undefined>(undefined);
    const [image64, setImage64] = useState<string>("");

    const locationData = useIPLocationData() || {
        timezone: Intl.DateTimeFormat().resolvedOptions().timeZone,
    };
    const authenticatedData = useAuthenticatedData();
    const isMobileWidth = useIsMobileWidth();

    useEffect(() => {
        fetch("/api/chat/options")
            .then((response) => response.json())
            .then((data: ChatOptions) => {
                setLoading(false);
                // Render chat options, if any
                if (data) {
                    setChatOptionsData(data);
                }
            })
            .catch((err) => {
                console.error(err);
                return;
            });

        welcomeConsole();

        setParamSlug(window.location.pathname.split("/").pop() || "");
    }, []);

    useEffect(() => {
        if (queryToProcess && !conversationId) {
            // If the user has not yet started conversing in the chat, create a new conversation
            fetch(`/api/chat/share/fork?public_conversation_slug=${paramSlug}`, {
                method: "POST",
                headers: {
                    "Content-Type": "application/json",
                },
            })
                .then((response) => response.json())
                .then((data) => {
                    setConversationID(data.conversation_id);
                })
                .catch((err) => {
                    console.error(err);
                    return;
                });
            return;
        }

        if (queryToProcess) {
            // Add a new object to the state
            const newStreamMessage: StreamMessage = {
                rawResponse: "",
                trainOfThought: [],
                context: [],
                onlineContext: {},
                completed: false,
                timestamp: new Date().toISOString(),
                rawQuery: queryToProcess || "",
                uploadedImageData: decodeURIComponent(image64),
            };
            setMessages((prevMessages) => [...prevMessages, newStreamMessage]);
            setProcessQuerySignal(true);
        }
    }, [queryToProcess, conversationId, paramSlug]);

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
        const eventDelimiter = "␃🔚␗";
        let buffer = "";

        while (true) {
            const { done, value } = await reader.read();
            if (done) {
                setQueryToProcess("");
                setProcessQuerySignal(false);
                setImage64("");
                break;
            }

            const chunk = decoder.decode(value, { stream: true });

            buffer += chunk;

            let newEventIndex;
            while ((newEventIndex = buffer.indexOf(eventDelimiter)) !== -1) {
                const event = buffer.slice(0, newEventIndex);
                buffer = buffer.slice(newEventIndex + eventDelimiter.length);
                if (event) {
                    const currentMessage = messages.find((message) => !message.completed);

                    if (!currentMessage) {
                        console.error("No current message found");
                        return;
                    }

                    processMessageChunk(event, currentMessage);

                    setMessages([...messages]);
                }
            }
        }
    }

    async function chat() {
        if (!queryToProcess || !conversationId) return;
        const chatAPI = "/api/chat?client=web";
        const chatAPIBody = {
            q: queryToProcess,
            conversation_id: conversationId,
            stream: true,
            ...(locationData && {
                region: locationData.region,
                country: locationData.country,
                city: locationData.city,
                country_code: locationData.countryCode,
                timezone: locationData.timezone,
            }),
            ...(image64 && { image: image64 }),
        };

        const response = await fetch(chatAPI, {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify(chatAPIBody),
        });

        try {
            await readChatStream(response);
        } catch (error) {
            console.error(error);
        }
    }

    if (isLoading) {
        return <Loading />;
    }

    if (!paramSlug) {
        return <div className={styles.suggestions}>Whoops, nothing to see here!</div>;
    }

    return (
        <div className={`${styles.main} ${styles.chatLayout}`}>
            <title>{title}</title>
            <div className={styles.sidePanel}>
                <SidePanel
                    conversationId={conversationId ?? null}
                    uploadedFiles={uploadedFiles}
                    isMobileWidth={isMobileWidth}
                />
            </div>

            <div className={styles.chatBox}>
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
                            isMobileWidth={isMobileWidth}
                            setImage64={setImage64}
                        />
                    </Suspense>
                </div>
            </div>
        </div>
    );
}
