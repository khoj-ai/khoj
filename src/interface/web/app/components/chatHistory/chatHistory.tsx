'use client'

import styles from './chatHistory.module.css';
import { useRef, useEffect, useState } from 'react';

import ChatMessage, { ChatHistoryData, StreamMessage, TrainOfThought } from '../chatMessage/chatMessage';

import { ScrollArea } from "@/components/ui/scroll-area"

import renderMathInElement from 'katex/contrib/auto-render';
import 'katex/dist/katex.min.css';

import { InlineLoading } from '../loading/loading';

import { Lightbulb } from "@phosphor-icons/react";

import ProfileCard from '../profileCard/profileCard';
import { getIconFromIconName } from '@/app/common/iconUtils';
import { AgentData } from '@/app/agents/page';

interface ChatResponse {
    status: string;
    response: ChatHistoryData;
}

interface ChatHistory {
    [key: string]: string
}

interface ChatHistoryProps {
    conversationId: string;
    setTitle: (title: string) => void;
    incomingMessages?: StreamMessage[];
    pendingMessage?: string;
    publicConversationSlug?: string;
    setAgent: (agent: AgentData) => void;
}


function constructTrainOfThought(trainOfThought: string[], lastMessage: boolean, agentColor: string, key: string, completed: boolean = false) {
    const lastIndex = trainOfThought.length - 1;
    return (
        <div className={`${styles.trainOfThought} shadow-sm`} key={key}>
            {
                !completed &&
                <InlineLoading className='float-right' />
            }

            {trainOfThought.map((train, index) => (
                <TrainOfThought key={`train-${index}`} message={train} primary={index === lastIndex && lastMessage && !completed} agentColor={agentColor} />
            ))}
        </div>
    )
}

export default function ChatHistory(props: ChatHistoryProps) {
    const [data, setData] = useState<ChatHistoryData | null>(null);
    const [currentPage, setCurrentPage] = useState(0);
    const [hasMoreMessages, setHasMoreMessages] = useState(true);

    const ref = useRef<HTMLDivElement>(null);
    const chatHistoryRef = useRef<HTMLDivElement | null>(null);
    const sentinelRef = useRef<HTMLDivElement | null>(null);

    const [incompleteIncomingMessageIndex, setIncompleteIncomingMessageIndex] = useState<number | null>(null);
    const [fetchingData, setFetchingData] = useState(false);
    const [isMobileWidth, setIsMobileWidth] = useState(false);

    useEffect(() => {
        window.addEventListener('resize', () => {
            setIsMobileWidth(window.innerWidth < 768);
        });

        setIsMobileWidth(window.innerWidth < 768);
    }, []);


    useEffect(() => {
        // This function ensures that scrolling to bottom happens after the data (chat messages) has been updated and rendered the first time.
        const scrollToBottomAfterDataLoad = () => {
            // Assume the data is loading in this scenario.
            if (!data?.chat.length) {
                setTimeout(() => {
                    scrollToBottom();
                }, 500);
            }
        };

        if (currentPage < 2) {
            // Call the function defined above.
            scrollToBottomAfterDataLoad();
        }

    }, [chatHistoryRef.current, data]);

    useEffect(() => {
        if (!hasMoreMessages || fetchingData) return;

        // TODO: A future optimization would be to add a time to delay to re-enabling the intersection observer.
        const observer = new IntersectionObserver(entries => {
            if (entries[0].isIntersecting && hasMoreMessages) {
                setFetchingData(true);
                fetchMoreMessages(currentPage);
                setCurrentPage((prev) => prev + 1);
            }
        }, { threshold: 1.0 });

        if (sentinelRef.current) {
            observer.observe(sentinelRef.current);
        }

        return () => observer.disconnect();
    }, [sentinelRef.current, hasMoreMessages, currentPage, fetchingData]);

    useEffect(() => {
        setHasMoreMessages(true);
        setFetchingData(false);
        setCurrentPage(0);
        setData(null);
    }, [props.conversationId]);

    useEffect(() => {
        console.log(props.incomingMessages);
        if (props.incomingMessages) {
            const lastMessage = props.incomingMessages[props.incomingMessages.length - 1];
            if (lastMessage && !lastMessage.completed) {
                setIncompleteIncomingMessageIndex(props.incomingMessages.length - 1);
            }
        }

        if (isUserAtBottom()) {
            scrollToBottom();
        }

    }, [props.incomingMessages]);

    useEffect(() => {
        const observer = new MutationObserver((mutationsList, observer) => {
            // If the addedNodes property has one or more nodes
            for (let mutation of mutationsList) {
                if (mutation.type === 'childList' && mutation.addedNodes.length > 0) {
                    // Call your function here
                    renderMathInElement(document.body, {
                        delimiters: [
                            { left: '$$', right: '$$', display: true },
                            { left: '\\[', right: '\\]', display: true },
                            { left: '$', right: '$', display: false },
                            { left: '\\(', right: '\\)', display: false },
                        ],
                    });
                }
            }
        });

        if (chatHistoryRef.current) {
            observer.observe(chatHistoryRef.current, { childList: true });
        }

        // Clean up the observer on component unmount
        return () => observer.disconnect();
    }, []);

    const fetchMoreMessages = (currentPage: number) => {
        if (!hasMoreMessages || fetchingData) return;
        const nextPage = currentPage + 1;

        let conversationFetchURL = '';

        if (props.conversationId) {
            conversationFetchURL = `/api/chat/history?client=web&conversation_id=${props.conversationId}&n=${10 * nextPage}`;
        } else if (props.publicConversationSlug) {
            conversationFetchURL = `/api/chat/share/history?client=web&public_conversation_slug=${props.publicConversationSlug}&n=${10 * nextPage}`;
        } else {
            return;
        }

        fetch(conversationFetchURL)
            .then(response => response.json())
            .then((chatData: ChatResponse) => {
                props.setTitle(chatData.response.slug);
                if (chatData && chatData.response && chatData.response.chat && chatData.response.chat.length > 0) {

                    if (chatData.response.chat.length === data?.chat.length) {
                        setHasMoreMessages(false);
                        setFetchingData(false);
                        return;
                    }
                    props.setAgent(chatData.response.agent);

                    setData(chatData.response);

                    if (currentPage < 2) {
                        scrollToBottom();
                    }
                    setFetchingData(false);
                } else {
                    if (chatData.response.agent && chatData.response.conversation_id) {
                        const chatMetadata ={
                            chat: [],
                            agent: chatData.response.agent,
                            conversation_id: chatData.response.conversation_id,
                            slug: chatData.response.slug,
                        }
                        props.setAgent(chatData.response.agent);
                        setData(chatMetadata);
                    }

                    setHasMoreMessages(false);
                    setFetchingData(false);
                }
            })
            .catch(err => {
                console.error(err);
                window.location.href = "/";
            });
    };

    const scrollToBottom = () => {
        if (chatHistoryRef.current) {
            chatHistoryRef.current.scrollIntoView(false);
        }
    }

    const isUserAtBottom = () => {
        if (!chatHistoryRef.current) return false;

        // NOTE: This isn't working. It always seems to return true. This is because

        const { scrollTop, scrollHeight, clientHeight } = chatHistoryRef.current as HTMLDivElement;
        const threshold = 25; // pixels from the bottom

        // Considered at the bottom if within threshold pixels from the bottom
        return scrollTop + clientHeight >= scrollHeight - threshold;
    }

    function constructAgentLink() {
        if (!data || !data.agent || !data.agent.slug) return `/agents`;
        return `/agents?agent=${data.agent.slug}`
    }

    function constructAgentName() {
        if (!data || !data.agent || !data.agent.name) return `Agent`;
        return data.agent.name;
    }

    function constructAgentPersona() {
        if (!data || !data.agent || !data.agent.persona) return ``;
        return data.agent.persona;
    }

    if (!props.conversationId && !props.publicConversationSlug) {
        return null;
    }
    return (
        <ScrollArea className={`h-[80vh]`}>
            <div ref={ref}>
                <div className={styles.chatHistory} ref={chatHistoryRef}>
                    <div ref={sentinelRef} style={{ height: '1px' }}>
                        {fetchingData && <InlineLoading message="Loading Conversation" className='opacity-50'/>}
                    </div>
                    {(data && data.chat) && data.chat.map((chatMessage, index) => (
                        <ChatMessage
                            key={`${index}fullHistory`}
                            isMobileWidth={isMobileWidth}
                            chatMessage={chatMessage}
                            customClassName='fullHistory'
                            borderLeftColor={`${data?.agent.color}-500`}
                            isLastMessage={index === data.chat.length - 1}
                        />
                    ))}
                    {
                        props.incomingMessages && props.incomingMessages.map((message, index) => {
                            return (
                                <>
                                    <ChatMessage
                                        key={`${index}outgoing`}
                                        isMobileWidth={isMobileWidth}
                                        chatMessage={
                                            {
                                                message: message.rawQuery,
                                                context: [],
                                                onlineContext: {},
                                                created: message.timestamp,
                                                by: "you",
                                                automationId: '',
                                            }
                                        }
                                        customClassName='fullHistory'
                                        borderLeftColor={`${data?.agent.color}-500`}
                                    />
                                    {
                                        message.trainOfThought &&
                                        constructTrainOfThought(
                                            message.trainOfThought,
                                            index === incompleteIncomingMessageIndex,
                                            data?.agent.color || 'orange',
                                            `${index}trainOfThought`, message.completed)
                                    }
                                    <ChatMessage
                                        key={`${index}incoming`}
                                        isMobileWidth={isMobileWidth}
                                        chatMessage={
                                            {
                                                message: message.rawResponse,
                                                context: message.context,
                                                onlineContext: message.onlineContext,
                                                created: message.timestamp,
                                                by: "khoj",
                                                automationId: '',
                                                rawQuery: message.rawQuery,
                                            }
                                        }
                                        customClassName='fullHistory'
                                        borderLeftColor={`${data?.agent.color}-500`}
                                        isLastMessage={true}
                                    />
                                </>
                            )
                        })
                    }
                    {
                        props.pendingMessage &&
                        <ChatMessage
                            key={`pendingMessage-${props.pendingMessage.length}`}
                            isMobileWidth={isMobileWidth}
                            chatMessage={
                                {
                                    message: props.pendingMessage,
                                    context: [],
                                    onlineContext: {},
                                    created: (new Date().getTime()).toString(),
                                    by: "you",
                                    automationId: '',
                                }
                            }
                            customClassName='fullHistory'
                            borderLeftColor={`${data?.agent.color}-500`}
                            isLastMessage={true}
                        />
                    }
                    {data &&
                        <div className={`${styles.agentIndicator} pb-4`}>
                            <div className="relative group mx-2 cursor-pointer">
                                <ProfileCard
                                    name={constructAgentName()}
                                    link={constructAgentLink()}
                                    avatar={getIconFromIconName(data.agent.icon, data.agent.color) || <Lightbulb />}
                                    description={constructAgentPersona()}
                                />
                            </div>
                        </div>
                    }
                </div>
            </div>
        </ScrollArea>
    )
}
