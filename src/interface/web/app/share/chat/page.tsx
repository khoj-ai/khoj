"use client";

import styles from "./sharedChat.module.css";
import React, { Suspense, useEffect, useRef, useState } from "react";

import ChatHistory from "../../components/chatHistory/chatHistory";
import Loading from "../../components/loading/loading";

import "katex/dist/katex.min.css";

import { useIsMobileWidth, welcomeConsole } from "../../common/utils";
import { useAuthenticatedData } from "@/app/common/auth";

import {
    AttachedFileText,
    ChatInputArea,
    ChatOptions,
} from "@/app/components/chatInputArea/chatInputArea";
import { StreamMessage } from "@/app/components/chatMessage/chatMessage";
import { AgentData } from "@/app/components/agentCard/agentCard";
import { SidebarInset, SidebarProvider, SidebarTrigger } from "@/components/ui/sidebar";
import { AppSidebar } from "@/app/components/appSidebar/appSidebar";
import { Separator } from "@/components/ui/separator";
import { KhojLogoType } from "@/app/components/logo/khojLogo";
import { Button } from "@/components/ui/button";
import { Trash } from "@phosphor-icons/react";

interface ChatBodyDataProps {
    chatOptionsData: ChatOptions | null;
    setTitle: (title: string) => void;
    setUploadedFiles: (files: AttachedFileText[]) => void;
    isMobileWidth?: boolean;
    publicConversationSlug: string;
    streamedMessages: StreamMessage[];
    isLoggedIn: boolean;
    conversationId?: string;
    setQueryToProcess: (query: string) => void;
    setImages: (images: string[]) => void;
    setIsOwner: (isOwner: boolean) => void;
}

function UnshareButton({ slug, className }: { slug: string; className?: string }) {
    const handleUnshare = async () => {
        try {
            const response = await fetch(`/api/chat/share?public_conversation_slug=${slug}`, {
                method: "DELETE",
            });

            if (response.redirected) {
                window.location.reload();
            } else {
                console.error("Failed to unshare conversation");
            }
        } catch (error) {
            console.error("Error unsharing conversation:", error);
        }
    };

    return (
        <div className="flex items-center gap-2">
            <Button
                className="p-0 text-sm h-auto text-rose-500 hover:text-rose-600"
                variant={"ghost"}
                title="Unshare conversation"
                onClick={handleUnshare}
            >
                <Trash className={`${className}`} />
            </Button>
        </div>
    );
}

function ChatBodyData(props: ChatBodyDataProps) {
    const [message, setMessage] = useState("");
    const [images, setImages] = useState<string[]>([]);
    const [processingMessage, setProcessingMessage] = useState(false);
    const [agentMetadata, setAgentMetadata] = useState<AgentData | null>(null);
    const chatInputRef = useRef<HTMLTextAreaElement>(null);

    const setQueryToProcess = props.setQueryToProcess;
    const streamedMessages = props.streamedMessages;

    const chatHistoryCustomClassName = props.isMobileWidth ? "w-full" : "w-4/6";

    useEffect(() => {
        if (images.length > 0) {
            const encodedImages = images.map((image) => encodeURIComponent(image));
            props.setImages(encodedImages);
        }
    }, [images, props.setImages]);

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
        <div className="flex flex-row h-full w-full">
            <div className="flex flex-col h-full w-full">
                <div className={false ? styles.chatBody : styles.chatBodyFull}>
                    <ChatHistory
                        publicConversationSlug={props.publicConversationSlug}
                        conversationId={props.conversationId || ""}
                        setAgent={setAgentMetadata}
                        setTitle={props.setTitle}
                        setIsOwner={props.setIsOwner}
                        pendingMessage={processingMessage ? message : ""}
                        incomingMessages={props.streamedMessages}
                        customClassName={chatHistoryCustomClassName}
                    />
                </div>
                <div
                    className={`${styles.inputBox} p-1 md:px-2 shadow-md bg-background align-middle items-center justify-center dark:bg-neutral-700 dark:border-0 dark:shadow-sm rounded-2xl md:rounded-xl h-fit ${chatHistoryCustomClassName} mr-auto ml-auto mt-auto`}
                >
                    <ChatInputArea
                        isLoggedIn={props.isLoggedIn}
                        sendMessage={(message) => setMessage(message)}
                        sendImage={(image) => setImages((prevImages) => [...prevImages, image])}
                        sendDisabled={processingMessage}
                        chatOptionsData={props.chatOptionsData}
                        conversationId={props.conversationId}
                        agentColor={agentMetadata?.color}
                        isMobileWidth={props.isMobileWidth}
                        setUploadedFiles={props.setUploadedFiles}
                        setTriggeredAbort={() => {}}
                        ref={chatInputRef}
                    />
                </div>
            </div>
        </div>
    );
}

export default function SharedChat() {
    const [chatOptionsData, setChatOptionsData] = useState<ChatOptions | null>(null);
    const [isLoading, setLoading] = useState(true);
    const [title, setTitle] = useState("Khoj AI - Chat");
    const [conversationId, setConversationID] = useState<string | undefined>(undefined);
    const [messages, setMessages] = useState<StreamMessage[]>([]);
    const [queryToProcess, setQueryToProcess] = useState<string>("");
    const [uploadedFiles, setUploadedFiles] = useState<AttachedFileText[] | null>(null);
    const [paramSlug, setParamSlug] = useState<string | undefined>(undefined);
    const [images, setImages] = useState<string[]>([]);
    const [isOwner, setIsOwner] = useState(false);

    const {
        data: authenticatedData,
        error: authenticationError,
        isLoading: authenticationLoading,
    } = useAuthenticatedData();
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
        if (uploadedFiles) {
            localStorage.setItem("uploadedFiles", JSON.stringify(uploadedFiles));
        }
    }, [uploadedFiles]);

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
                    localStorage.setItem("message", queryToProcess);
                    if (images.length > 0) {
                        localStorage.setItem("images", JSON.stringify(images));
                    }
                    window.location.href = `/chat?conversationId=${data.conversation_id}`;
                })
                .catch((err) => {
                    console.error(err);
                    return;
                });
            return;
        }
    }, [queryToProcess, conversationId, paramSlug]);

    if (isLoading) {
        return <Loading />;
    }

    if (!paramSlug) {
        return <div className={styles.suggestions}>Whoops, nothing to see here!</div>;
    }

    return (
        <SidebarProvider>
            <AppSidebar conversationId={conversationId || ""} />
            <SidebarInset>
                <header className="flex h-16 shrink-0 items-center gap-2 border-b px-4">
                    <SidebarTrigger className="-ml-1" />
                    <Separator orientation="vertical" className="mr-2 h-4" />
                    {paramSlug && (
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
                                        {isOwner && authenticatedData && (
                                            <UnshareButton
                                                slug={paramSlug}
                                                className={"h-4 w-4 mt-1"}
                                            />
                                        )}
                                    </>
                                )
                            )}
                        </div>
                    )}
                </header>
                <div className={`${styles.main} ${styles.chatLayout}`}>
                    <title>{title}</title>
                    <div className={styles.chatBox}>
                        <div className={styles.chatBoxBody}>
                            <Suspense fallback={<Loading />}>
                                <ChatBodyData
                                    conversationId={conversationId}
                                    streamedMessages={messages}
                                    setQueryToProcess={setQueryToProcess}
                                    isLoggedIn={authenticatedData ? true : false}
                                    publicConversationSlug={paramSlug}
                                    chatOptionsData={chatOptionsData}
                                    setTitle={setTitle}
                                    setUploadedFiles={setUploadedFiles}
                                    isMobileWidth={isMobileWidth}
                                    setImages={setImages}
                                    setIsOwner={setIsOwner}
                                />
                            </Suspense>
                        </div>
                    </div>
                </div>
            </SidebarInset>
        </SidebarProvider>
    );
}
