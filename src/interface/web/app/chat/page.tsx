"use client";

import styles from "./chat.module.css";
import React, { Suspense, useEffect, useRef, useState } from "react";

import ChatHistory from "../components/chatHistory/chatHistory";
import { useSearchParams } from "next/navigation";
import Loading from "../components/loading/loading";

import { generateNewTitle, processMessageChunk } from "../common/chatFunctions";

import "katex/dist/katex.min.css";

import {
    CodeContext,
    Context,
    OnlineContext,
    StreamMessage,
} from "../components/chatMessage/chatMessage";
import { useIPLocationData, useIsMobileWidth, welcomeConsole } from "../common/utils";
import {
    AttachedFileText,
    ChatInputArea,
    ChatOptions,
} from "../components/chatInputArea/chatInputArea";
import { useAuthenticatedData } from "../common/auth";
import { AgentData } from "@/app/components/agentCard/agentCard";
import { ChatSessionActionMenu } from "../components/allConversations/allConversations";
import { SidebarInset, SidebarProvider, SidebarTrigger } from "@/components/ui/sidebar";
import { AppSidebar } from "../components/appSidebar/appSidebar";
import { Separator } from "@/components/ui/separator";
import { KhojLogoType } from "../components/logo/khojLogo";
import { Button } from "@/components/ui/button";
import { Joystick } from "@phosphor-icons/react";
import { ChatSidebar } from "../components/chatSidebar/chatSidebar";

interface ChatBodyDataProps {
    chatOptionsData: ChatOptions | null;
    setTitle: (title: string) => void;
    onConversationIdChange?: (conversationId: string) => void;
    setQueryToProcess: (query: string) => void;
    streamedMessages: StreamMessage[];
    setStreamedMessages: (messages: StreamMessage[]) => void;
    setUploadedFiles: (files: AttachedFileText[] | undefined) => void;
    isMobileWidth?: boolean;
    isLoggedIn: boolean;
    setImages: (images: string[]) => void;
    setTriggeredAbort: (triggeredAbort: boolean, newMessage?: string) => void;
    isChatSideBarOpen: boolean;
    setIsChatSideBarOpen: (open: boolean) => void;
    isActive?: boolean;
    isParentProcessing?: boolean;
    onRetryMessage?: (query: string, turnId?: string) => void;
}

function ChatBodyData(props: ChatBodyDataProps) {
    const searchParams = useSearchParams();
    const conversationId = searchParams.get("conversationId");
    const [message, setMessage] = useState("");
    const [images, setImages] = useState<string[]>([]);
    const [processingMessage, setProcessingMessage] = useState(false);
    const [agentMetadata, setAgentMetadata] = useState<AgentData | null>(null);
    const [isInResearchMode, setIsInResearchMode] = useState(false);
    const chatInputRef = useRef<HTMLTextAreaElement>(null);

    const setQueryToProcess = props.setQueryToProcess;
    const onConversationIdChange = props.onConversationIdChange;

    const chatHistoryCustomClassName = props.isMobileWidth ? "w-full" : "w-4/6";

    useEffect(() => {
        if (images.length > 0) {
            const encodedImages = images.map((image) => encodeURIComponent(image));
            props.setImages(encodedImages);
        }
    }, [images, props.setImages]);

    useEffect(() => {
        const storedImages = localStorage.getItem("images");
        if (storedImages) {
            const parsedImages: string[] = JSON.parse(storedImages);
            setImages(parsedImages);
            const encodedImages = parsedImages.map((img: string) => encodeURIComponent(img));
            props.setImages(encodedImages);
            localStorage.removeItem("images");
        }

        const storedMessage = localStorage.getItem("message");
        if (storedMessage) {
            setProcessingMessage(true);
            setQueryToProcess(storedMessage);

            if (storedMessage.trim().startsWith("/research")) {
                setIsInResearchMode(true);
            }
        }

        const storedUploadedFiles = localStorage.getItem("uploadedFiles");

        if (storedUploadedFiles) {
            const parsedFiles = storedUploadedFiles ? JSON.parse(storedUploadedFiles) : [];
            const uploadedFiles: AttachedFileText[] = [];
            for (const file of parsedFiles) {
                uploadedFiles.push({
                    name: file.name,
                    file_type: file.file_type,
                    content: file.content,
                    size: file.size,
                });
            }
            localStorage.removeItem("uploadedFiles");
            props.setUploadedFiles(uploadedFiles);
        }
    }, [setQueryToProcess, props.setImages, conversationId]);

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
        if (
            props.streamedMessages &&
            props.streamedMessages.length > 0 &&
            props.streamedMessages[props.streamedMessages.length - 1].completed
        ) {
            setProcessingMessage(false);
            setImages([]); // Reset images after processing
            props.setUploadedFiles(undefined); // Reset uploaded files after processing
        } else {
            setMessage("");
        }
    }, [props.streamedMessages]);

    if (!conversationId) {
        window.location.href = "/";
        return;
    }

    return (
        <div className="flex flex-row h-full w-full">
            <div className="flex flex-col h-full w-full">
                <div className={false ? styles.chatBody : styles.chatBodyFull}>
                    <ChatHistory
                        conversationId={conversationId}
                        setTitle={props.setTitle}
                        setAgent={setAgentMetadata}
                        pendingMessage={processingMessage ? message : ""}
                        incomingMessages={props.streamedMessages}
                        setIncomingMessages={props.setStreamedMessages}
                        customClassName={chatHistoryCustomClassName}
                        setIsChatSideBarOpen={props.setIsChatSideBarOpen}
                        onRetryMessage={props.onRetryMessage}
                    />
                </div>
                <div
                    className={`${styles.inputBox} p-1 md:px-2 shadow-md bg-background align-middle items-center justify-center dark:bg-neutral-700 dark:border-0 dark:shadow-sm rounded-2xl md:rounded-xl h-fit ${chatHistoryCustomClassName} mr-auto ml-auto mt-auto`}
                >
                    <ChatInputArea
                        agentColor={agentMetadata?.color}
                        isLoggedIn={props.isLoggedIn}
                        sendMessage={(message) => setMessage(message)}
                        sendImage={(image) => setImages((prevImages) => [...prevImages, image])}
                        sendDisabled={props.isParentProcessing || false}
                        chatOptionsData={props.chatOptionsData}
                        conversationId={conversationId}
                        isMobileWidth={props.isMobileWidth}
                        setUploadedFiles={props.setUploadedFiles}
                        ref={chatInputRef}
                        isResearchModeEnabled={isInResearchMode}
                        setTriggeredAbort={props.setTriggeredAbort}
                    />
                </div>
            </div>
            <ChatSidebar
                conversationId={conversationId}
                isActive={props.isActive}
                isOpen={props.isChatSideBarOpen}
                onOpenChange={props.setIsChatSideBarOpen}
                isMobileWidth={props.isMobileWidth}
            />
        </div>
    );
}

export default function Chat() {
    const defaultTitle = "Khoj AI - Chat";
    const [chatOptionsData, setChatOptionsData] = useState<ChatOptions | null>(null);
    const [isLoading, setLoading] = useState(true);
    const [title, setTitle] = useState(defaultTitle);
    const [conversationId, setConversationID] = useState<string | null>(null);
    const [messages, setMessages] = useState<StreamMessage[]>([]);
    const [queryToProcess, setQueryToProcess] = useState<string>("");
    const [processQuerySignal, setProcessQuerySignal] = useState(false);
    const [uploadedFiles, setUploadedFiles] = useState<AttachedFileText[] | undefined>(undefined);
    const [images, setImages] = useState<string[]>([]);

    const [abortMessageStreamController, setAbortMessageStreamController] =
        useState<AbortController | null>(null);
    const [triggeredAbort, setTriggeredAbort] = useState(false);
    const [interruptMessage, setInterruptMessage] = useState<string>("");
    const [shouldSendWithInterrupt, setShouldSendWithInterrupt] = useState(false);
    const socketRef = useRef<WebSocket | null>(null);
    const bufferRef = useRef("");

    const { locationData, locationDataError, locationDataLoading } = useIPLocationData() || {
        locationData: {
            timezone: Intl.DateTimeFormat().resolvedOptions().timeZone,
        },
    };
    const {
        data: authenticatedData,
        error: authenticationError,
        isLoading: authenticationLoading,
    } = useAuthenticatedData();
    const isMobileWidth = useIsMobileWidth();
    const [isChatSideBarOpen, setIsChatSideBarOpen] = useState(false);

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
    }, []);

    const handleTriggeredAbort = (value: boolean, newMessage?: string) => {
        if (value) {
            setInterruptMessage(newMessage || "");
        }
        setTriggeredAbort(value);
    };

    useEffect(() => {
        if (triggeredAbort) {
            // For WebSocket connections, send interrupt message directly
            if (socketRef.current && socketRef.current.readyState === WebSocket.OPEN) {
                const messageToSend = interruptMessage || queryToProcess;
                socketRef.current.send(
                    JSON.stringify({
                        type: "interrupt",
                        query: messageToSend,
                    }),
                );
                console.log("Sent interrupt message via WebSocket:", messageToSend);

                // Update the current message with the new query but keep it in processing state
                setMessages((prevMessages) => {
                    const newMessages = [...prevMessages];
                    const currentMessage = newMessages[newMessages.length - 1];
                    if (currentMessage && !currentMessage.completed) {
                        currentMessage.rawQuery = messageToSend;
                    }
                    return newMessages;
                });

                // Update the query being processed
                setQueryToProcess(messageToSend);
            }
            setTriggeredAbort(false);
            setInterruptMessage("");
        }
    }, [triggeredAbort, interruptMessage, queryToProcess]);

    useEffect(() => {
        if (queryToProcess) {
            const newStreamMessage: StreamMessage = {
                rawResponse: "",
                trainOfThought: [],
                context: [],
                onlineContext: {},
                codeContext: {},
                completed: false,
                timestamp: new Date().toISOString(),
                rawQuery: queryToProcess || "",
                images: images,
                queryFiles: uploadedFiles,
            };
            setMessages((prevMessages) => [...prevMessages, newStreamMessage]);
            setProcessQuerySignal(true);
        }
    }, [queryToProcess]);

    useEffect(() => {
        if (processQuerySignal) {
            if (locationDataLoading) {
                return;
            }

            chat();
        }
    }, [processQuerySignal, locationDataLoading]);

    useEffect(() => {
        if (!conversationId) return;

        const protocol = window.location.protocol === "https:" ? "wss:" : "ws:";
        const wsUrl = `${protocol}//${window.location.host}/api/chat/ws?client=web`;

        const ws = new WebSocket(wsUrl);
        socketRef.current = ws;

        ws.onopen = () => {
            console.log("WebSocket connection established");
        };

        ws.onmessage = (event) => {
            // Check if this is a control message (JSON) rather than a streaming event
            try {
                const controlMessage = JSON.parse(event.data);
                if (controlMessage.type === "interrupt_acknowledged") {
                    console.log("Interrupt acknowledged by server");
                    return;
                }
                if (controlMessage.error) {
                    console.error("WebSocket error:", controlMessage.error);
                    return;
                }
            } catch {
                // Not a JSON control message, process as streaming event
            }

            const eventDelimiter = "␃🔚␗";
            bufferRef.current += event.data;

            let newEventIndex;
            while ((newEventIndex = bufferRef.current.indexOf(eventDelimiter)) !== -1) {
                const eventChunk = bufferRef.current.slice(0, newEventIndex);
                bufferRef.current = bufferRef.current.slice(newEventIndex + eventDelimiter.length);
                if (eventChunk) {
                    setMessages((prevMessages) => {
                        const newMessages = [...prevMessages];
                        const currentMessage = newMessages[newMessages.length - 1];
                        if (!currentMessage || currentMessage.completed) {
                            return prevMessages;
                        }

                        const { context, onlineContext, codeContext } = processMessageChunk(
                            eventChunk,
                            currentMessage,
                            currentMessage.context || [],
                            currentMessage.onlineContext || {},
                            currentMessage.codeContext || {},
                        );

                        // Update the current message with the new reference data
                        currentMessage.context = context;
                        currentMessage.onlineContext = onlineContext;
                        currentMessage.codeContext = codeContext;

                        if (currentMessage.completed) {
                            setQueryToProcess("");
                            setProcessQuerySignal(false);
                            setImages([]);
                            if (conversationId) generateNewTitle(conversationId, setTitle);
                        }

                        return newMessages;
                    });
                }
            }
        };

        ws.onclose = () => {
            console.log("WebSocket connection closed");
            socketRef.current = null;
        };

        ws.onerror = (error) => {
            console.error("WebSocket error:", error);
            setMessages((prevMessages) => {
                const newMessages = [...prevMessages];
                const currentMessage = newMessages[newMessages.length - 1];
                if (currentMessage && !currentMessage.completed) {
                    currentMessage.rawResponse = `A WebSocket error occurred. Please check the connection or try again.`;
                    currentMessage.completed = true;
                }

                setQueryToProcess("");
                setProcessQuerySignal(false);

                return newMessages;
            });
            socketRef.current = null;
        };

        return () => {
            if (socketRef.current) {
                socketRef.current.close();
            }
        };
    }, [conversationId]);

    function handleAbortedMessage() {
        // TODO: Implement WebSocket abort logic
        const currentMessage = messages.find((message) => !message.completed);
        if (!currentMessage) return;

        currentMessage.completed = true;
        setMessages([...messages]);
        setProcessQuerySignal(false);
    }

    async function chat() {
        localStorage.removeItem("message");
        if (!queryToProcess || !conversationId) {
            setProcessQuerySignal(false);
            return;
        }

        // Wait for WebSocket connection to be established (with timeout)
        const maxWaitTime = 5000; // 5 seconds
        const pollInterval = 100; // 100ms
        let waitTime = 0;

        while (
            (!socketRef.current || socketRef.current.readyState !== WebSocket.OPEN) &&
            waitTime < maxWaitTime
        ) {
            await new Promise((resolve) => setTimeout(resolve, pollInterval));
            waitTime += pollInterval;
        }

        if (!socketRef.current || socketRef.current.readyState !== WebSocket.OPEN) {
            console.error("WebSocket connection timeout.");
            setMessages((prevMessages) => {
                const newMessages = [...prevMessages];
                const currentMessage = newMessages[newMessages.length - 1];
                if (currentMessage && !currentMessage.completed) {
                    currentMessage.rawResponse =
                        "Failed to connect to the server. Please check your connection and try again.";
                    currentMessage.completed = true;
                }
                return newMessages;
            });
            setProcessQuerySignal(false);
            return;
        }

        const chatAPIBody = {
            q: queryToProcess,
            conversation_id: conversationId,
            stream: true,
            interrupt: shouldSendWithInterrupt,
            ...(locationData && {
                city: locationData.city,
                region: locationData.region,
                country: locationData.country,
                country_code: locationData.countryCode,
                timezone: locationData.timezone,
            }),
            ...(images.length > 0 && { images: images }),
            ...(uploadedFiles && { files: uploadedFiles }),
        };

        // Reset the flag after using it
        setShouldSendWithInterrupt(false);

        socketRef.current.send(JSON.stringify(chatAPIBody));
    }

    const handleConversationIdChange = (newConversationId: string) => {
        setConversationID(newConversationId);
    };

    const handleRetryMessage = (query: string, turnId?: string) => {
        if (!query) {
            console.warn("No query provided for retry");
            return;
        }

        // If we have a turnId, delete the old turn first
        if (turnId) {
            // Delete from streaming messages if present
            setMessages((prevMessages) => prevMessages.filter((msg) => msg.turnId !== turnId));

            // Also call the delete API to remove from conversation history
            fetch("/api/chat/conversation/message", {
                method: "DELETE",
                headers: {
                    "Content-Type": "application/json",
                },
                body: JSON.stringify({
                    conversation_id: conversationId,
                    turn_id: turnId,
                }),
            }).catch((error) => {
                console.error("Failed to delete message for retry:", error);
            });
        }

        // Re-send the original query
        setQueryToProcess(query);
    };

    if (isLoading) return <Loading />;

    return (
        <SidebarProvider>
            <AppSidebar conversationId={conversationId || ""} />
            <SidebarInset>
                <header className="flex h-16 shrink-0 items-center gap-2 border-b px-4">
                    <SidebarTrigger className="-ml-1" />
                    <Separator orientation="vertical" className="mr-2 h-4" />
                    {conversationId && (
                        <div
                            className={`${styles.chatTitleWrapper} text-nowrap text-ellipsis overflow-hidden max-w-screen-md grid items-top font-bold mx-2 md:mr-8 col-auto h-fit`}
                        >
                            {isMobileWidth ? (
                                <a className="p-0 no-underline" href="/">
                                    <KhojLogoType className="h-auto w-16" />
                                </a>
                            ) : (
                                title && (
                                    <>
                                        <h2
                                            className={`text-lg text-ellipsis whitespace-nowrap overflow-x-hidden mr-4`}
                                        >
                                            {title}
                                        </h2>
                                        <ChatSessionActionMenu
                                            conversationId={conversationId}
                                            setTitle={setTitle}
                                            sizing={"md"}
                                        />
                                    </>
                                )
                            )}
                        </div>
                    )}
                    <div className="flex justify-end items-start gap-2 text-sm ml-auto">
                        <Button
                            variant="ghost"
                            size="icon"
                            className="h-12 w-12 data-[state=open]:bg-accent"
                            onClick={() => setIsChatSideBarOpen(!isChatSideBarOpen)}
                        >
                            <Joystick className="w-6 h-6" />
                        </Button>
                    </div>
                </header>
                <div className={`${styles.main} ${styles.chatLayout}`}>
                    <title>
                        {`${defaultTitle}${!!title && title !== defaultTitle ? `: ${title}` : ""}`}
                    </title>
                    <div className={styles.chatBox}>
                        <div className={styles.chatBoxBody}>
                            <Suspense fallback={<Loading />}>
                                <ChatBodyData
                                    isLoggedIn={authenticatedData ? true : false}
                                    streamedMessages={messages}
                                    setStreamedMessages={setMessages}
                                    chatOptionsData={chatOptionsData}
                                    setTitle={setTitle}
                                    setQueryToProcess={setQueryToProcess}
                                    setUploadedFiles={setUploadedFiles}
                                    isMobileWidth={isMobileWidth}
                                    onConversationIdChange={handleConversationIdChange}
                                    setImages={setImages}
                                    setTriggeredAbort={handleTriggeredAbort}
                                    isChatSideBarOpen={isChatSideBarOpen}
                                    setIsChatSideBarOpen={setIsChatSideBarOpen}
                                    isActive={authenticatedData?.is_active}
                                    isParentProcessing={processQuerySignal}
                                    onRetryMessage={handleRetryMessage}
                                />
                            </Suspense>
                        </div>
                    </div>
                </div>
            </SidebarInset>
        </SidebarProvider>
    );
}
