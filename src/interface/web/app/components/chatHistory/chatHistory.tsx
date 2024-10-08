"use client";

import styles from "./chatHistory.module.css";
import { useRef, useEffect, useState } from "react";

import ChatMessage, {
    ChatHistoryData,
    StreamMessage,
    TrainOfThought,
} from "../chatMessage/chatMessage";

import { ScrollArea } from "@/components/ui/scroll-area";

import { InlineLoading } from "../loading/loading";

import { Lightbulb, ArrowDown } from "@phosphor-icons/react";

import AgentProfileCard from "../profileCard/profileCard";
import { getIconFromIconName } from "@/app/common/iconUtils";
import { AgentData } from "@/app/agents/page";
import React from "react";
import { useIsMobileWidth } from "@/app/common/utils";

interface ChatResponse {
    status: string;
    response: ChatHistoryData;
}

interface ChatHistory {
    [key: string]: string;
}

interface ChatHistoryProps {
    conversationId: string;
    setTitle: (title: string) => void;
    incomingMessages?: StreamMessage[];
    pendingMessage?: string;
    publicConversationSlug?: string;
    setAgent: (agent: AgentData) => void;
}

function constructTrainOfThought(
    trainOfThought: string[],
    lastMessage: boolean,
    agentColor: string,
    key: string,
    completed: boolean = false,
) {
    const lastIndex = trainOfThought.length - 1;
    return (
        <div className={`${styles.trainOfThought} shadow-sm`} key={key}>
            {!completed && <InlineLoading className="float-right" />}

            {trainOfThought.map((train, index) => (
                <TrainOfThought
                    key={`train-${index}`}
                    message={train}
                    primary={index === lastIndex && lastMessage && !completed}
                    agentColor={agentColor}
                />
            ))}
        </div>
    );
}

export default function ChatHistory(props: ChatHistoryProps) {
    const [data, setData] = useState<ChatHistoryData | null>(null);
    const [currentPage, setCurrentPage] = useState(0);
    const [hasMoreMessages, setHasMoreMessages] = useState(true);
    const sentinelRef = useRef<HTMLDivElement | null>(null);
    const scrollAreaRef = useRef<HTMLDivElement | null>(null);
    const latestUserMessageRef = useRef<HTMLDivElement | null>(null);
    const latestFetchedMessageRef = useRef<HTMLDivElement | null>(null);

    const [incompleteIncomingMessageIndex, setIncompleteIncomingMessageIndex] = useState<
        number | null
    >(null);
    const [fetchingData, setFetchingData] = useState(false);
    const [isNearBottom, setIsNearBottom] = useState(true);
    const isMobileWidth = useIsMobileWidth();
    const scrollAreaSelector = "[data-radix-scroll-area-viewport]";
    const fetchMessageCount = 10;

    useEffect(() => {
        const scrollAreaEl = scrollAreaRef.current?.querySelector<HTMLElement>(scrollAreaSelector);
        if (!scrollAreaEl) return;

        const detectIsNearBottom = () => {
            const { scrollTop, scrollHeight, clientHeight } = scrollAreaEl;
            const bottomThreshold = 50; // pixels from bottom
            const distanceFromBottom = scrollHeight - (scrollTop + clientHeight);
            const isNearBottom = distanceFromBottom <= bottomThreshold;
            setIsNearBottom(isNearBottom);
        };

        scrollAreaEl.addEventListener("scroll", detectIsNearBottom);
        return () => scrollAreaEl.removeEventListener("scroll", detectIsNearBottom);
    }, []);

    // Auto scroll while incoming message is streamed
    useEffect(() => {
        if (props.incomingMessages && props.incomingMessages.length > 0 && isNearBottom) {
            scrollToBottom();
        }
    }, [props.incomingMessages, isNearBottom]);

    // Scroll to most recent user message after the first page of chat messages is loaded.
    useEffect(() => {
        if (data && data.chat && data.chat.length > 0 && currentPage < 2) {
            requestAnimationFrame(() => {
                latestUserMessageRef.current?.scrollIntoView({ behavior: "auto", block: "start" });
            });
        }
    }, [data, currentPage]);

    useEffect(() => {
        if (!hasMoreMessages || fetchingData) return;

        // TODO: A future optimization would be to add a time to delay to re-enabling the intersection observer.
        const observer = new IntersectionObserver(
            (entries) => {
                if (entries[0].isIntersecting && hasMoreMessages) {
                    setFetchingData(true);
                    fetchMoreMessages(currentPage);
                }
            },
            { threshold: 1.0 },
        );

        if (sentinelRef.current) {
            observer.observe(sentinelRef.current);
        }

        return () => observer.disconnect();
    }, [hasMoreMessages, currentPage, fetchingData]);

    useEffect(() => {
        setHasMoreMessages(true);
        setFetchingData(false);
        setCurrentPage(0);
        setData(null);
    }, [props.conversationId]);

    useEffect(() => {
        if (props.incomingMessages) {
            const lastMessage = props.incomingMessages[props.incomingMessages.length - 1];
            if (lastMessage && !lastMessage.completed) {
                setIncompleteIncomingMessageIndex(props.incomingMessages.length - 1);
                props.setTitle(lastMessage.rawQuery);
            }
        }
    }, [props.incomingMessages]);

    const adjustScrollPosition = () => {
        const scrollAreaEl = scrollAreaRef.current?.querySelector<HTMLElement>(scrollAreaSelector);
        requestAnimationFrame(() => {
            // Snap scroll position to the latest fetched message ref
            latestFetchedMessageRef.current?.scrollIntoView({ behavior: "auto", block: "start" });
            // Now scroll up smoothly to render user scroll action
            scrollAreaEl?.scrollBy({ behavior: "smooth", top: -150 });
        });
    };

    function fetchMoreMessages(currentPage: number) {
        if (!hasMoreMessages || fetchingData) return;
        const nextPage = currentPage + 1;
        const maxMessagesToFetch = nextPage * fetchMessageCount;
        let conversationFetchURL = "";

        if (props.conversationId) {
            conversationFetchURL = `/api/chat/history?client=web&conversation_id=${encodeURIComponent(props.conversationId)}&n=${maxMessagesToFetch}`;
        } else if (props.publicConversationSlug) {
            conversationFetchURL = `/api/chat/share/history?client=web&public_conversation_slug=${props.publicConversationSlug}&n=${maxMessagesToFetch}`;
        } else {
            return;
        }

        fetch(conversationFetchURL)
            .then((response) => response.json())
            .then((chatData: ChatResponse) => {
                props.setTitle(chatData.response.slug);
                if (
                    chatData &&
                    chatData.response &&
                    chatData.response.chat &&
                    chatData.response.chat.length > 0
                ) {
                    setCurrentPage(Math.ceil(chatData.response.chat.length / fetchMessageCount));
                    if (chatData.response.chat.length === data?.chat.length) {
                        setHasMoreMessages(false);
                        setFetchingData(false);
                        return;
                    }
                    props.setAgent(chatData.response.agent);
                    setData(chatData.response);
                    setFetchingData(false);
                    if (currentPage === 0) {
                        scrollToBottom(true);
                    } else {
                        adjustScrollPosition();
                    }
                } else {
                    if (chatData.response.agent && chatData.response.conversation_id) {
                        const chatMetadata = {
                            chat: [],
                            agent: chatData.response.agent,
                            conversation_id: chatData.response.conversation_id,
                            slug: chatData.response.slug,
                        };
                        props.setAgent(chatData.response.agent);
                        setData(chatMetadata);
                    }

                    setHasMoreMessages(false);
                    setFetchingData(false);
                }
            })
            .catch((err) => {
                console.error(err);
                window.location.href = "/";
            });
    }

    const scrollToBottom = (instant: boolean = false) => {
        const scrollAreaEl = scrollAreaRef.current?.querySelector<HTMLElement>(scrollAreaSelector);
        requestAnimationFrame(() => {
            scrollAreaEl?.scrollTo({
                top: scrollAreaEl.scrollHeight,
                behavior: instant ? "auto" : "smooth",
            });
        });
        setIsNearBottom(true);
    };

    function constructAgentLink() {
        if (!data || !data.agent || !data.agent?.slug) return `/agents`;
        return `/agents?agent=${data.agent?.slug}`;
    }

    function constructAgentName() {
        if (!data || !data.agent || !data.agent?.name) return `Agent`;
        return data.agent?.name;
    }

    function constructAgentPersona() {
        if (!data || !data.agent || !data.agent?.persona)
            return `Your agent is no longer available. You will be reset to the default agent.`;
        return data.agent?.persona;
    }

    if (!props.conversationId && !props.publicConversationSlug) {
        return null;
    }

    return (
        <ScrollArea className={`h-[80vh] relative`} ref={scrollAreaRef}>
            <div>
                <div className={styles.chatHistory}>
                    <div ref={sentinelRef} style={{ height: "1px" }}>
                        {fetchingData && (
                            <InlineLoading message="Loading Conversation" className="opacity-50" />
                        )}
                    </div>
                    {data &&
                        data.chat &&
                        data.chat.map((chatMessage, index) => (
                            <ChatMessage
                                key={`${index}fullHistory`}
                                ref={
                                    // attach ref to the second last message to handle scroll on page load
                                    index === data.chat.length - 2
                                        ? latestUserMessageRef
                                        : // attach ref to the newest fetched message to handle scroll on fetch
                                          // note: stabilize index selection against last page having less messages than fetchMessageCount
                                          index ===
                                            data.chat.length - (currentPage - 1) * fetchMessageCount
                                          ? latestFetchedMessageRef
                                          : null
                                }
                                isMobileWidth={isMobileWidth}
                                chatMessage={chatMessage}
                                customClassName="fullHistory"
                                borderLeftColor={`${data?.agent?.color}-500`}
                                isLastMessage={index === data.chat.length - 1}
                            />
                        ))}
                    {props.incomingMessages &&
                        props.incomingMessages.map((message, index) => {
                            return (
                                <React.Fragment key={`incomingMessage${index}`}>
                                    <ChatMessage
                                        key={`${index}outgoing`}
                                        isMobileWidth={isMobileWidth}
                                        chatMessage={{
                                            message: message.rawQuery,
                                            context: [],
                                            onlineContext: {},
                                            created: message.timestamp,
                                            by: "you",
                                            automationId: "",
                                            uploadedImageData: message.uploadedImageData,
                                        }}
                                        customClassName="fullHistory"
                                        borderLeftColor={`${data?.agent?.color}-500`}
                                    />
                                    {message.trainOfThought &&
                                        constructTrainOfThought(
                                            message.trainOfThought,
                                            index === incompleteIncomingMessageIndex,
                                            data?.agent?.color || "orange",
                                            `${index}trainOfThought`,
                                            message.completed,
                                        )}
                                    <ChatMessage
                                        key={`${index}incoming`}
                                        isMobileWidth={isMobileWidth}
                                        chatMessage={{
                                            message: message.rawResponse,
                                            context: message.context,
                                            onlineContext: message.onlineContext,
                                            created: message.timestamp,
                                            by: "khoj",
                                            automationId: "",
                                            rawQuery: message.rawQuery,
                                        }}
                                        customClassName="fullHistory"
                                        borderLeftColor={`${data?.agent?.color}-500`}
                                        isLastMessage={true}
                                    />
                                </React.Fragment>
                            );
                        })}
                    {props.pendingMessage && (
                        <ChatMessage
                            key={`pendingMessage-${props.pendingMessage.length}`}
                            isMobileWidth={isMobileWidth}
                            chatMessage={{
                                message: props.pendingMessage,
                                context: [],
                                onlineContext: {},
                                created: new Date().getTime().toString(),
                                by: "you",
                                automationId: "",
                                uploadedImageData: props.pendingMessage,
                            }}
                            customClassName="fullHistory"
                            borderLeftColor={`${data?.agent?.color}-500`}
                            isLastMessage={true}
                        />
                    )}
                    {data && (
                        <div className={`${styles.agentIndicator} pb-4`}>
                            <div className="relative group mx-2 cursor-pointer">
                                <AgentProfileCard
                                    name={constructAgentName()}
                                    link={constructAgentLink()}
                                    avatar={
                                        getIconFromIconName(
                                            data.agent?.icon,
                                            data.agent?.color,
                                        ) || <Lightbulb />
                                    }
                                    description={constructAgentPersona()}
                                />
                            </div>
                        </div>
                    )}
                </div>
                {!isNearBottom && (
                    <button
                        title="Scroll to bottom"
                        className="absolute bottom-4 right-5 bg-white dark:bg-[hsl(var(--background))] text-neutral-500 dark:text-white p-2 rounded-full shadow-xl"
                        onClick={() => {
                            scrollToBottom();
                            setIsNearBottom(true);
                        }}
                    >
                        <ArrowDown size={24} />
                    </button>
                )}
            </div>
        </ScrollArea>
    );
}
