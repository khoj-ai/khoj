"use client";

import styles from "./chat.module.css";
import React, { Suspense, useCallback, useEffect, useRef, useState } from "react";
import useWebSocket from "react-use-websocket";

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
                    className={`${styles.inputBox} print-hidden p-1 md:px-2 shadow-md bg-background align-middle items-center justify-center dark:bg-neutral-700 dark:border-0 dark:shadow-sm rounded-2xl md:rounded-xl h-fit ${chatHistoryCustomClassName} mr-auto ml-auto mt-auto`}
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
            <div className="print-hidden">
                <ChatSidebar
                    conversationId={conversationId}
                    isActive={props.isActive}
                    isOpen={props.isChatSideBarOpen}
                    onOpenChange={props.setIsChatSideBarOpen}
                    isMobileWidth={props.isMobileWidth}
                />
            </div>
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

    const [triggeredAbort, setTriggeredAbort] = useState(false);
    const [interruptMessage, setInterruptMessage] = useState<string>("");
    const bufferRef = useRef("");
    const idleTimerRef = useRef<NodeJS.Timeout | null>(null);

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
    const [socketUrl, setSocketUrl] = useState<string | null>(null);

    const disconnectFromServer = useCallback(() => {
        if (idleTimerRef.current) {
            clearTimeout(idleTimerRef.current);
        }
        setSocketUrl(null);
        console.log("WebSocket disconnected due to inactivity.");
    }, []);

    const resetIdleTimer = useCallback(() => {
        const idleTimeout = 10 * 60 * 1000; // 10 minutes
        if (idleTimerRef.current) {
            clearTimeout(idleTimerRef.current);
        }
        idleTimerRef.current = setTimeout(disconnectFromServer, idleTimeout);
    }, [disconnectFromServer]);

    const { sendMessage, lastMessage } = useWebSocket(socketUrl, {
        share: true,
        shouldReconnect: (closeEvent) => true,
        reconnectAttempts: 10,
        // reconnect using exponential backoff with jitter
        reconnectInterval: (attemptNumber) => {
            const baseDelay = 1000 * Math.pow(2, attemptNumber);
            const jitter = Math.random() * 1000; // Add jitter up to 1s
            return Math.min(baseDelay + jitter, 20000); // Cap backoff at 20s
        },
        onOpen: () => {
            console.log("WebSocket connection established.");
            resetIdleTimer();
        },
        onClose: () => {
            console.log("WebSocket connection closed.");
            if (idleTimerRef.current) {
                clearTimeout(idleTimerRef.current);
            }
        },
    });

    useEffect(() => {
        if (lastMessage !== null) {
            resetIdleTimer();
            // Check if this is a control message (JSON) rather than a streaming event
            try {
                const controlMessage = JSON.parse(lastMessage.data);
                if (controlMessage.type === "interrupt_acknowledged") {
                    console.log("Interrupt acknowledged by server");
                    setProcessQuerySignal(false);
                    return;
                } else if (controlMessage.type === "interrupt_message_acknowledged") {
                    console.log("Interrupt message acknowledged by server");
                    setProcessQuerySignal(false);
                    return;
                } else if (controlMessage.error) {
                    console.error("WebSocket error:", controlMessage.error);
                    return;
                }
            } catch {
                // Not a JSON control message, process as streaming event
            }

            const eventDelimiter = "␃🔚␗";
            bufferRef.current += lastMessage.data;

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
        }
    }, [lastMessage, setMessages]);

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
            sendMessage(
                JSON.stringify({
                    type: "interrupt",
                    query: interruptMessage,
                }),
            );
            console.log("Sent interrupt message via WebSocket:", interruptMessage);

            // Mark the last message as completed
            setMessages((prevMessages) => {
                const newMessages = [...prevMessages];
                const currentMessage = newMessages[newMessages.length - 1];
                if (currentMessage) currentMessage.completed = true;
                return newMessages;
            });

            // Set the interrupt message as the new query being processed
            setQueryToProcess(interruptMessage);
            setTriggeredAbort(false); // Always set to false after processing
            setInterruptMessage("");
        }
    }, [triggeredAbort, sendMessage]);

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
        setSocketUrl(wsUrl);

        return () => {
            if (idleTimerRef.current) {
                clearTimeout(idleTimerRef.current);
            }
        };
    }, [conversationId]);

    async function chat() {
        localStorage.removeItem("message");
        if (!queryToProcess || !conversationId) {
            setProcessQuerySignal(false);
            return;
        }

        // Re-establish WebSocket connection if disconnected
        resetIdleTimer();
        if (!socketUrl) {
            const protocol = window.location.protocol === "https:" ? "wss:" : "ws:";
            const wsUrl = `${protocol}//${window.location.host}/api/chat/ws?client=web`;
            setSocketUrl(wsUrl);
        }

        const chatAPIBody = {
            q: queryToProcess,
            conversation_id: conversationId,
            stream: true,
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

        sendMessage(JSON.stringify(chatAPIBody));
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
            <div className="print-hidden">
                <AppSidebar conversationId={conversationId || ""} />
            </div>
            <SidebarInset>
                <header className="flex h-16 shrink-0 items-center gap-2 border-b px-4 print-hidden">
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
                            className="h-12 w-12 data-[state=open]:bg-accent print-hidden"
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
