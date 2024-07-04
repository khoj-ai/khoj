'use client'

import styles from './chatHistory.module.css';
import { useRef, useEffect, useState } from 'react';

import ChatMessage, { ChatHistoryData, SingleChatMessage, StreamMessage, TrainOfThought } from '../chatMessage/chatMessage';

import ReferencePanel, { hasValidReferences } from '../referencePanel/referencePanel';

import { ScrollArea } from "@/components/ui/scroll-area"

import renderMathInElement from 'katex/contrib/auto-render';
import 'katex/dist/katex.min.css';
import 'highlight.js/styles/github.css'

import Loading, { InlineLoading } from '../loading/loading';

import { Lightbulb } from "@phosphor-icons/react";

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
}


function constructTrainOfThought(trainOfThought: string[], lastMessage: boolean, key: string, completed: boolean = false) {
    const lastIndex = trainOfThought.length - 1;
    return (
        <div className={`${styles.trainOfThought}`} key={key}>
            {
                !completed &&
                <InlineLoading className='float-right' />
            }

            {trainOfThought.map((train, index) => (
                <TrainOfThought message={train} primary={index === lastIndex && lastMessage && !completed} />
            ))}
        </div>
    )
}


export default function ChatHistory(props: ChatHistoryProps) {
    const [data, setData] = useState<ChatHistoryData | null>(null);
    const [isLoading, setLoading] = useState(true);
    const [currentPage, setCurrentPage] = useState(0);
    const [hasMoreMessages, setHasMoreMessages] = useState(true);

    const ref = useRef<HTMLDivElement>(null);
    const chatHistoryRef = useRef<HTMLDivElement | null>(null);
    const sentinelRef = useRef<HTMLDivElement | null>(null);

    const [showReferencePanel, setShowReferencePanel] = useState(true);
    const [referencePanelData, setReferencePanelData] = useState<SingleChatMessage | null>(null);
    const [incompleteIncomingMessageIndex, setIncompleteIncomingMessageIndex] = useState<number | null>(null);
    const [fetchingData, setFetchingData] = useState(false);

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
    }, [sentinelRef.current, hasMoreMessages, currentPage, props.conversationId, fetchingData]);

    const fetchMoreMessages = (currentPage: number) => {
        if (!hasMoreMessages || fetchingData) return;

        console.log("fetchMoreMessages", currentPage);

        const nextPage = currentPage + 1;
        fetch(`/api/chat/history?client=web&conversation_id=${props.conversationId}&n=${10 * nextPage}`)
            .then(response => response.json())
            .then((chatData: ChatResponse) => {
                console.log(chatData);
                if (chatData && chatData.response && chatData.response.chat.length > 0) {

                    if (chatData.response.chat.length === data?.chat.length) {
                        setHasMoreMessages(false);
                        return;
                    }

                    setData(chatData.response);
                    setLoading(false);

                    if (currentPage < 2) {
                        console.log("call scroll to bottom");
                        scrollToBottom();
                    }
                    setFetchingData(false);
                } else {
                    console.log("No more messages");
                    setHasMoreMessages(false);
                }
            })
            .catch(err => {
                console.error(err);
            });
    };

    useEffect(() => {
        if (props.incomingMessages) {
            const lastMessage = props.incomingMessages[props.incomingMessages.length - 1];
            if (lastMessage && !lastMessage.completed) {
                setIncompleteIncomingMessageIndex(props.incomingMessages.length - 1);
            }
        }

        console.log("isUserAtBottom", isUserAtBottom());

        if (isUserAtBottom()) {
            scrollToBottom();
        }

    }, [props.incomingMessages]);

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

    // if (isLoading) {
    //     return <Loading />;
    // }

    function constructAgentLink() {
        if (!data || !data.agent || !data.agent.slug) return `/agents`;
        return `/agents?agent=${data.agent.slug}`
    }

    function constructAgentAvatar() {
        if (!data || !data.agent || !data.agent.avatar) return `/avatar.png`;
        return data.agent.avatar;
    }

    function constructAgentName() {
        if (!data || !data.agent || !data.agent.name) return `Agent`;
        return data.agent.name;
    }

    return (
        <ScrollArea className={`h-[80vh]`}>
            <div ref={ref}>
                <div className={styles.chatHistory} ref={chatHistoryRef}>
                    <div ref={sentinelRef} style={{ height: '1px' }}></div>
                    {(data && data.chat) && data.chat.map((chatMessage, index) => (
                        <ChatMessage
                            key={`${index}fullHistory`}
                            chatMessage={chatMessage}
                            setReferencePanelData={setReferencePanelData}
                            setShowReferencePanel={setShowReferencePanel}
                            customClassName='fullHistory'
                            borderLeftColor='orange-500'
                        />
                    ))}
                    {
                        props.incomingMessages && props.incomingMessages.map((message, index) => {
                            return (
                                <>
                                    <ChatMessage
                                        key={`${index}outgoing`}
                                        chatMessage={
                                            {
                                                message: message.rawQuery,
                                                context: [],
                                                onlineContext: {},
                                                created: message.timestamp,
                                                by: "you",
                                                intent: {},
                                                automationId: '',

                                            }
                                        }
                                        setReferencePanelData={() => { }}
                                        setShowReferencePanel={() => { }}
                                        customClassName='fullHistory'
                                        borderLeftColor='orange-500' />
                                    {
                                        message.trainOfThought && constructTrainOfThought(message.trainOfThought, index === incompleteIncomingMessageIndex, `${index}trainOfThought`, message.completed)
                                    }
                                    <ChatMessage
                                        key={`${index}incoming`}
                                        chatMessage={
                                            {
                                                message: message.rawResponse,
                                                context: message.context,
                                                onlineContext: message.onlineContext,
                                                created: message.timestamp,
                                                by: "khoj",
                                                intent: {},
                                                automationId: '',
                                            }
                                        }
                                        setReferencePanelData={setReferencePanelData}
                                        setShowReferencePanel={setShowReferencePanel}
                                        customClassName='fullHistory'
                                        borderLeftColor='orange-500'
                                    />
                                </>
                            )
                        })
                    }
                    {
                        props.pendingMessage &&
                        <ChatMessage
                            key={"pendingMessage"}
                            chatMessage={
                                {
                                    message: props.pendingMessage,
                                    context: [],
                                    onlineContext: {},
                                    created: new Date().toISOString(),
                                    by: "you",
                                    intent: {},
                                    automationId: '',
                                }
                            }
                            setReferencePanelData={() => { }}
                            setShowReferencePanel={() => { }}
                            customClassName='fullHistory'
                            borderLeftColor='orange-500'
                        />
                    }
                    {
                        (hasValidReferences(referencePanelData) && showReferencePanel) &&
                        <ReferencePanel referencePanelData={referencePanelData} setShowReferencePanel={setShowReferencePanel} />
                    }
                    <div className={`${styles.agentIndicator}`}>
                        <a className='no-underline mx-2 flex' href={constructAgentLink()} target="_blank" rel="noreferrer">
                            <Lightbulb color='orange' weight='fill' />
                            <span className='text-neutral-600'>{constructAgentName()}</span>
                        </a>
                    </div>
                </div>
            </div>
        </ScrollArea>
    )
}
