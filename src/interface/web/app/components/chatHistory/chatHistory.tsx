"use client";

import styles from "./chatHistory.module.css";
import { useRef, useEffect, useState } from "react";
import { motion, AnimatePresence } from "framer-motion";

import ChatMessage, {
    ChatHistoryData,
    StreamMessage,
    TrainOfThought,
    TrainOfThoughtObject,
} from "../chatMessage/chatMessage";
import TrainOfThoughtVideoPlayer from "../../../components/trainOfThoughtVideoPlayer/trainOfThoughtVideoPlayer";

import { ScrollArea } from "@/components/ui/scroll-area";

import { InlineLoading } from "../loading/loading";

import { Lightbulb, ArrowDown, CaretDown, CaretUp } from "@phosphor-icons/react";

import AgentProfileCard from "../profileCard/profileCard";
import { getIconFromIconName } from "@/app/common/iconUtils";
import { AgentData } from "@/app/components/agentCard/agentCard";
import React from "react";
import { useIsMobileWidth } from "@/app/common/utils";
import { Button } from "@/components/ui/button";
import { KhojLogo } from "../logo/khojLogo";

interface ChatResponse {
    status: string;
    response: ChatHistoryData;
}

interface ChatHistoryProps {
    conversationId: string;
    setTitle: (title: string) => void;
    pendingMessage?: string;
    incomingMessages?: StreamMessage[];
    setIncomingMessages?: (incomingMessages: StreamMessage[]) => void;
    publicConversationSlug?: string;
    setAgent: (agent: AgentData) => void;
    customClassName?: string;
    setIsChatSideBarOpen?: (isOpen: boolean) => void;
    setIsOwner?: (isOwner: boolean) => void;
    onRetryMessage?: (query: string, turnId?: string) => void;
}

interface TrainOfThoughtFrame {
    text: string;
    image?: string;
    timestamp: number;
}

interface TrainOfThoughtGroup {
    type: "video" | "text";
    frames?: TrainOfThoughtFrame[];
    textEntries?: TrainOfThoughtObject[];
}

interface TrainOfThoughtComponentProps {
    trainOfThought: string[] | TrainOfThoughtObject[];
    lastMessage: boolean;
    agentColor: string;
    keyId: string;
    completed?: boolean;
}

function extractTrainOfThoughtGroups(
    trainOfThought?: TrainOfThoughtObject[],
): TrainOfThoughtGroup[] {
    if (!trainOfThought) return [];

    const groups: TrainOfThoughtGroup[] = [];
    let currentVideoFrames: TrainOfThoughtFrame[] = [];
    let currentTextEntries: TrainOfThoughtObject[] = [];

    trainOfThought.forEach((thought, index) => {
        let text = thought.data;
        let hasImage = false;

        // Extract screenshot image from the thought data
        try {
            const jsonMatch = text.match(
                /\{.*(\"action\": \"screenshot\"|\"type\": \"screenshot\"|\"image\": \"data:image\/.*\").*\}/,
            );
            if (jsonMatch) {
                const jsonMessage = JSON.parse(jsonMatch[0]);
                if (jsonMessage.image) {
                    hasImage = true;
                    // Clean up the text to remove the JSON action
                    text = text.replace(`:\n**Action**: ${jsonMatch[0]}`, "");
                    if (jsonMessage.text) {
                        text += `\n\n${jsonMessage.text}`;
                    }

                    // If we have accumulated text entries, add them as a text group
                    if (currentTextEntries.length > 0) {
                        groups.push({
                            type: "text",
                            textEntries: [...currentTextEntries],
                        });
                        currentTextEntries = [];
                    }

                    // Add to current video frames
                    currentVideoFrames.push({
                        text: text,
                        image: jsonMessage.image,
                        timestamp: index,
                    });
                }
            }
        } catch (e) {
            console.error("Failed to parse screenshot data", e);
        }

        if (!hasImage) {
            // If we have accumulated video frames, add them as a video group
            if (currentVideoFrames.length > 0) {
                groups.push({
                    type: "video",
                    frames: [...currentVideoFrames],
                });
                currentVideoFrames = [];
            }

            // Add to current text entries
            currentTextEntries.push(thought);
        }
    });

    // Add any remaining frames/entries
    if (currentVideoFrames.length > 0) {
        groups.push({
            type: "video",
            frames: currentVideoFrames,
        });
    }
    if (currentTextEntries.length > 0) {
        groups.push({
            type: "text",
            textEntries: currentTextEntries,
        });
    }

    return groups;
}

function TrainOfThoughtComponent(props: TrainOfThoughtComponentProps) {
    const [collapsed, setCollapsed] = useState(props.completed);
    const [trainOfThoughtGroups, setTrainOfThoughtGroups] = useState<TrainOfThoughtGroup[]>([]);

    const variants = {
        open: {
            height: "auto",
            opacity: 1,
            transition: { duration: 0.3, ease: "easeOut" },
        },
        closed: {
            height: 0,
            opacity: 0,
            transition: { duration: 0.3, ease: "easeIn" },
        },
    } as const;

    useEffect(() => {
        if (props.completed) {
            setCollapsed(true);
        }
    }, [props.completed]);

    useEffect(() => {
        // Handle empty array case
        if (!props.trainOfThought || props.trainOfThought.length === 0) {
            setTrainOfThoughtGroups([]);
            return;
        }

        // Convert string array to TrainOfThoughtObject array if needed
        let trainOfThoughtObjects: TrainOfThoughtObject[];

        if (typeof props.trainOfThought[0] === "string") {
            trainOfThoughtObjects = (props.trainOfThought as string[]).map((data, index) => ({
                type: "text",
                data: data,
            }));
        } else {
            trainOfThoughtObjects = props.trainOfThought as TrainOfThoughtObject[];
        }

        const groups = extractTrainOfThoughtGroups(trainOfThoughtObjects);
        setTrainOfThoughtGroups(groups);
    }, [props.trainOfThought]);

    return (
        <div
            className={`${!collapsed ? styles.trainOfThought + " border" : ""} rounded-lg`}
            key={props.keyId}
        >
            {!props.completed && <InlineLoading className="float-right" />}
            {props.completed &&
                (collapsed ? (
                    <Button
                        className="w-fit text-left justify-start content-start text-xs"
                        onClick={() => setCollapsed(false)}
                        variant="ghost"
                        size="sm"
                    >
                        Thought Process <CaretDown size={16} className="ml-1" />
                    </Button>
                ) : (
                    <Button
                        className="w-fit text-left justify-start content-start text-xs p-0 h-fit"
                        onClick={() => setCollapsed(true)}
                        variant="ghost"
                        size="sm"
                    >
                        Close <CaretUp size={16} className="ml-1" />
                    </Button>
                ))}
            <AnimatePresence initial={false}>
                {!collapsed && (
                    <motion.div initial="closed" animate="open" exit="closed" variants={variants}>
                        {trainOfThoughtGroups.map((group, groupIndex) => (
                            <div key={`train-group-${groupIndex}`}>
                                {group.type === "video" &&
                                    group.frames &&
                                    group.frames.length > 0 && (
                                        <TrainOfThoughtVideoPlayer
                                            frames={group.frames}
                                            autoPlay={false}
                                            playbackSpeed={1500}
                                        />
                                    )}
                                {group.type === "text" &&
                                    group.textEntries &&
                                    group.textEntries.map((entry, entryIndex) => {
                                        const lastIndex = trainOfThoughtGroups.length - 1;
                                        const isLastGroup = groupIndex === lastIndex;
                                        const isLastEntry =
                                            entryIndex === group.textEntries!.length - 1;
                                        const isPrimaryEntry =
                                            isLastGroup &&
                                            isLastEntry &&
                                            props.lastMessage &&
                                            !props.completed;

                                        return (
                                            <TrainOfThought
                                                key={`train-text-${groupIndex}-${entryIndex}-${entry.data.length}`}
                                                message={entry.data}
                                                primary={isPrimaryEntry}
                                                agentColor={props.agentColor}
                                            />
                                        );
                                    })}
                            </div>
                        ))}
                    </motion.div>
                )}
            </AnimatePresence>
        </div>
    );
}

export default function ChatHistory(props: ChatHistoryProps) {
    const [data, setData] = useState<ChatHistoryData | null>(null);
    const [currentPage, setCurrentPage] = useState(0);
    const [hasMoreMessages, setHasMoreMessages] = useState(true);
    const [currentTurnId, setCurrentTurnId] = useState<string | null>(null);
    const sentinelRef = useRef<HTMLDivElement | null>(null);
    const scrollAreaRef = useRef<HTMLDivElement | null>(null);
    const scrollableContentWrapperRef = useRef<HTMLDivElement | null>(null);
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
    const hasStartingMessage = localStorage.getItem("message");

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
        detectIsNearBottom(); // Initial check
        return () => scrollAreaEl.removeEventListener("scroll", detectIsNearBottom);
    }, [scrollAreaRef]);

    // Auto scroll while incoming message is streamed
    useEffect(() => {
        if (props.incomingMessages && props.incomingMessages.length > 0 && isNearBottom) {
            scrollToBottom(true);
        }
    }, [props.incomingMessages, isNearBottom]);

    // ResizeObserver to handle content height changes (e.g., images loading)
    useEffect(() => {
        const contentWrapper = scrollableContentWrapperRef.current;
        const scrollViewport =
            scrollAreaRef.current?.querySelector<HTMLElement>(scrollAreaSelector);

        if (!contentWrapper || !scrollViewport) return;

        const observer = new ResizeObserver(() => {
            // Check current scroll position to decide if auto-scroll is warranted
            const { scrollTop, scrollHeight, clientHeight } = scrollViewport;
            const bottomThreshold = 50;
            const currentlyNearBottom =
                scrollHeight - (scrollTop + clientHeight) <= bottomThreshold;

            if (currentlyNearBottom) {
                // Only auto-scroll if there are incoming messages being processed
                if (props.incomingMessages && props.incomingMessages.length > 0) {
                    const lastMessage = props.incomingMessages[props.incomingMessages.length - 1];
                    // If the last message is not completed, or it just completed (indicated by incompleteIncomingMessageIndex still being set)
                    if (
                        !lastMessage.completed ||
                        (lastMessage.completed && incompleteIncomingMessageIndex !== null)
                    ) {
                        scrollToBottom(true); // Use instant scroll
                    }
                }
            }
        });

        observer.observe(contentWrapper);
        return () => observer.disconnect();
    }, [props.incomingMessages, incompleteIncomingMessageIndex, scrollAreaRef]); // Dependencies

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
                // Store the turnId when we get it
                if (lastMessage.turnId) {
                    setCurrentTurnId(lastMessage.turnId);
                }
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
                props.setIsOwner && props.setIsOwner(chatData?.response?.is_owner);
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
                            is_owner: chatData.response.is_owner,
                        };
                        props.setAgent(chatData.response.agent);
                        setData(chatMetadata);
                        if (props.setIsChatSideBarOpen) {
                            if (!hasStartingMessage) {
                                props.setIsChatSideBarOpen(true);
                            }
                        }
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
        // Optimistically set, the scroll listener will verify
        if (
            instant ||
            (scrollAreaEl &&
                scrollAreaEl.scrollHeight - (scrollAreaEl.scrollTop + scrollAreaEl.clientHeight) <
                    5)
        ) {
            setIsNearBottom(true);
        }
    };

    function constructAgentLink() {
        if (!data || !data.agent || !data.agent?.slug) return `/agents`;
        return `/agents?agent=${data.agent?.slug}`;
    }

    function constructAgentName() {
        if (!data || !data.agent || !data.agent?.name) return `Agent`;
        if (data.agent.is_hidden) return "Khoj";
        return data.agent?.name;
    }

    function constructAgentPersona() {
        if (!data || !data.agent) {
            return `Your agent is no longer available. You will be reset to the default agent.`;
        }

        if (!data.agent?.persona) {
            return `You can set a persona for your agent in the Chat Options side panel.`;
        }

        return data.agent?.persona;
    }

    const handleDeleteMessage = (turnId?: string) => {
        if (!turnId) return;

        setData((prevData) => {
            if (!prevData || !turnId) return prevData;
            return {
                ...prevData,
                chat: prevData.chat.filter((msg) => msg.turnId !== turnId),
            };
        });

        // Update incoming messages if they exist
        if (props.incomingMessages && props.setIncomingMessages) {
            props.setIncomingMessages(
                props.incomingMessages.filter((msg) => msg.turnId !== turnId),
            );
        }
    };

    const handleRetryMessage = (query: string, turnId?: string) => {
        if (!query) return;

        // Delete the message from local state first
        if (turnId) {
            handleDeleteMessage(turnId);
        }

        // Then trigger the retry
        props.onRetryMessage?.(query, turnId);
    };

    if (!props.conversationId && !props.publicConversationSlug) {
        return null;
    }

    return (
        <ScrollArea
            className={`
            h-[calc(100svh-theme(spacing.44))]
            sm:h-[calc(100svh-theme(spacing.44))]
            md:h-[calc(100svh-theme(spacing.44))]
            lg:h-[calc(100svh-theme(spacing.44))]
        `}
            ref={scrollAreaRef}
        >
            <div ref={scrollableContentWrapperRef}>
                {/* Print-only header with conversation info */}
                <div className="print-only-header">
                    <div className="print-header-content">
                        <div className="print-header-left">
                            <KhojLogo className="print-logo" />
                        </div>
                        <div className="print-header-right">
                            <h1>{data?.slug || "Conversation with Khoj"}</h1>
                            <div className="conversation-meta">
                                <p>
                                    <strong>Agent:</strong> {constructAgentName()}
                                </p>
                            </div>
                        </div>
                    </div>
                    <hr />
                </div>

                <div className={`${styles.chatHistory} ${props.customClassName}`}>
                    <div ref={sentinelRef} style={{ height: "1px" }}>
                        {fetchingData && <InlineLoading className="opacity-50" />}
                    </div>
                    {data &&
                        data.chat &&
                        data.chat.map((chatMessage, index) => (
                            <React.Fragment key={`chatMessage-${index}`}>
                                {chatMessage.trainOfThought && chatMessage.by === "khoj" && (
                                    <TrainOfThoughtComponent
                                        trainOfThought={chatMessage.trainOfThought}
                                        lastMessage={false}
                                        agentColor={data?.agent?.color || "orange"}
                                        key={`${index}trainOfThought`}
                                        keyId={`${index}trainOfThought`}
                                        completed={true}
                                    />
                                )}
                                <ChatMessage
                                    key={`${index}fullHistory`}
                                    ref={
                                        // attach ref to the second last message to handle scroll on page load
                                        index === data.chat.length - 2
                                            ? latestUserMessageRef
                                            : // attach ref to the newest fetched message to handle scroll on fetch
                                              // note: stabilize index selection against last page having less messages than fetchMessageCount
                                              index ===
                                                data.chat.length -
                                                    (currentPage - 1) * fetchMessageCount
                                              ? latestFetchedMessageRef
                                              : null
                                    }
                                    isMobileWidth={isMobileWidth}
                                    chatMessage={chatMessage}
                                    customClassName="fullHistory"
                                    borderLeftColor={`${data?.agent?.color}-500`}
                                    isLastMessage={index === data.chat.length - 1}
                                    onDeleteMessage={handleDeleteMessage}
                                    onRetryMessage={handleRetryMessage}
                                    conversationId={props.conversationId}
                                />
                            </React.Fragment>
                        ))}
                    {props.incomingMessages &&
                        props.incomingMessages.map((message, index) => {
                            const messageTurnId = message.turnId ?? currentTurnId ?? undefined;
                            return (
                                <React.Fragment key={`incomingMessage${index}`}>
                                    <ChatMessage
                                        key={`${index}outgoing`}
                                        isMobileWidth={isMobileWidth}
                                        chatMessage={{
                                            message: message.rawQuery,
                                            context: [],
                                            onlineContext: {},
                                            codeContext: {},
                                            created: message.timestamp,
                                            by: "you",
                                            automationId: "",
                                            images: message.images,
                                            conversationId: props.conversationId,
                                            turnId: messageTurnId,
                                            queryFiles: message.queryFiles,
                                        }}
                                        customClassName="fullHistory"
                                        borderLeftColor={`${data?.agent?.color}-500`}
                                        onDeleteMessage={handleDeleteMessage}
                                        onRetryMessage={handleRetryMessage}
                                        conversationId={props.conversationId}
                                        turnId={messageTurnId}
                                    />
                                    {message.trainOfThought &&
                                        message.trainOfThought.length > 0 && (
                                            <TrainOfThoughtComponent
                                                trainOfThought={message.trainOfThought}
                                                lastMessage={
                                                    index === incompleteIncomingMessageIndex
                                                }
                                                agentColor={data?.agent?.color || "orange"}
                                                key={`${index}trainOfThought-${message.trainOfThought.length}-${message.trainOfThought.map((t) => t.length).join("-")}`}
                                                keyId={`${index}trainOfThought`}
                                                completed={message.completed}
                                            />
                                        )}
                                    <ChatMessage
                                        key={`${index}incoming`}
                                        isMobileWidth={isMobileWidth}
                                        chatMessage={{
                                            message: message.rawResponse,
                                            context: message.context,
                                            onlineContext: message.onlineContext,
                                            codeContext: message.codeContext,
                                            created: message.timestamp,
                                            by: "khoj",
                                            automationId: "",
                                            rawQuery: message.rawQuery,
                                            intent: {
                                                type: message.intentType || "",
                                                query: message.rawQuery,
                                                "memory-type": "",
                                                "inferred-queries": message.inferredQueries || [],
                                            },
                                            conversationId: props.conversationId,
                                            images: message.generatedImages,
                                            queryFiles: message.generatedFiles,
                                            mermaidjsDiagram: message.generatedMermaidjsDiagram,
                                            turnId: messageTurnId,
                                        }}
                                        conversationId={props.conversationId}
                                        turnId={messageTurnId}
                                        onDeleteMessage={handleDeleteMessage}
                                        onRetryMessage={handleRetryMessage}
                                        customClassName="fullHistory"
                                        borderLeftColor={`${data?.agent?.color}-500`}
                                        isLastMessage={index === props.incomingMessages!.length - 1}
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
                                codeContext: {},
                                created: new Date().getTime().toString(),
                                by: "you",
                                automationId: "",
                                conversationId: props.conversationId,
                                turnId: undefined,
                            }}
                            conversationId={props.conversationId}
                            onDeleteMessage={handleDeleteMessage}
                            onRetryMessage={handleRetryMessage}
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
                <div className={`${props.customClassName} fixed bottom-[20%] z-10`}>
                    {!isNearBottom && (
                        <button
                            title="Scroll to bottom"
                            className="absolute bottom-0 right-0 bg-white dark:bg-[hsl(var(--background))] text-neutral-500 dark:text-white p-2 rounded-full shadow-xl"
                            onClick={() => {
                                scrollToBottom();
                            }}
                        >
                            <ArrowDown size={24} />
                        </button>
                    )}
                </div>
            </div>
        </ScrollArea>
    );
}
