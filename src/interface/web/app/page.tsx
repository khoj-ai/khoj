"use client";
import "./globals.css";
import styles from "./page.module.css";
import "katex/dist/katex.min.css";

import React, { useEffect, useRef, useState } from "react";
import useSWR from "swr";
import { ArrowCounterClockwise } from "@phosphor-icons/react";

import { Card, CardTitle } from "@/components/ui/card";
import SuggestionCard from "@/app/components/suggestions/suggestionCard";
import SidePanel from "@/app/components/sidePanel/chatHistorySidePanel";
import Loading from "@/app/components/loading/loading";
import { ChatInputArea, ChatOptions } from "@/app/components/chatInputArea/chatInputArea";
import { Suggestion, suggestionsData } from "@/app/components/suggestions/suggestionsData";
import LoginPrompt from "@/app/components/loginPrompt/loginPrompt";

import {
    isUserSubscribed,
    useAuthenticatedData,
    UserConfig,
    useUserConfig,
} from "@/app/common/auth";
import { convertColorToBorderClass } from "@/app/common/colorUtils";
import { getIconFromIconName } from "@/app/common/iconUtils";
import { AgentData } from "@/app/agents/page";
import { createNewConversation } from "./common/chatFunctions";
import { useDebounce, useIsMobileWidth } from "./common/utils";
import { useRouter, useSearchParams } from "next/navigation";
import { ScrollArea, ScrollBar } from "@/components/ui/scroll-area";
import { AgentCard } from "@/app/components/agentCard/agentCard";
import { Popover, PopoverContent, PopoverTrigger } from "@/components/ui/popover";

interface ChatBodyDataProps {
    chatOptionsData: ChatOptions | null;
    onConversationIdChange?: (conversationId: string) => void;
    setUploadedFiles: (files: string[]) => void;
    isMobileWidth?: boolean;
    isLoggedIn: boolean;
    userConfig: UserConfig | null;
    isLoadingUserConfig: boolean;
}

function FisherYatesShuffle(array: any[]) {
    for (let i = array.length - 1; i > 0; i--) {
        const j = Math.floor(Math.random() * (i + 1));
        [array[i], array[j]] = [array[j], array[i]];
    }
    return array;
}

function ChatBodyData(props: ChatBodyDataProps) {
    const [message, setMessage] = useState("");
    const [image, setImage] = useState<string | null>(null);
    const [processingMessage, setProcessingMessage] = useState(false);
    const [greeting, setGreeting] = useState("");
    const [shuffledOptions, setShuffledOptions] = useState<Suggestion[]>([]);
    const [hoveredAgent, setHoveredAgent] = useState<string | null>(null);
    const debouncedHoveredAgent = useDebounce(hoveredAgent, 500);
    const [isPopoverOpen, setIsPopoverOpen] = useState(false);
    const [selectedAgent, setSelectedAgent] = useState<string | null>("khoj");
    const [agentIcons, setAgentIcons] = useState<JSX.Element[]>([]);
    const [agents, setAgents] = useState<AgentData[]>([]);
    const chatInputRef = useRef<HTMLTextAreaElement>(null);
    const [showLoginPrompt, setShowLoginPrompt] = useState(false);
    const router = useRouter();
    const searchParams = useSearchParams();
    const queryParam = searchParams.get("q");

    useEffect(() => {
        if (queryParam) {
            setMessage(decodeURIComponent(queryParam));
        }
    }, [queryParam]);

    useEffect(() => {
        if (debouncedHoveredAgent) {
            setIsPopoverOpen(true);
        }
    }, [debouncedHoveredAgent]);

    const onConversationIdChange = props.onConversationIdChange;

    const agentsFetcher = () =>
        window
            .fetch("/api/agents")
            .then((res) => res.json())
            .catch((err) => console.log(err));
    const { data: agentsData, error } = useSWR<AgentData[]>("agents", agentsFetcher, {
        revalidateOnFocus: false,
    });

    const openAgentEditCard = (agentSlug: string) => {
        router.push(`/agents?agent=${agentSlug}`);
    };

    function shuffleAndSetOptions() {
        const shuffled = FisherYatesShuffle(suggestionsData);
        setShuffledOptions(shuffled.slice(0, 3));
    }

    useEffect(() => {
        if (props.isLoadingUserConfig) return;

        // Get today's day
        const today = new Date();
        const day = today.getDay();
        const timeOfDay =
            today.getHours() >= 17 || today.getHours() < 4
                ? "evening"
                : today.getHours() >= 12
                  ? "afternoon"
                  : "morning";
        const nameSuffix = props.userConfig?.given_name ? `, ${props.userConfig?.given_name}` : "";
        const greetings = [
            `What would you like to get done${nameSuffix}?`,
            `Hey${nameSuffix}! How can I help?`,
            `Good ${timeOfDay}${nameSuffix}! What's on your mind?`,
            `Ready to breeze through your ${["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"][day]}?`,
            `Want help navigating your ${["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"][day]} workload?`,
        ];
        const greeting = greetings[Math.floor(Math.random() * greetings.length)];
        setGreeting(greeting);
    }, [props.isLoadingUserConfig, props.userConfig]);

    useEffect(() => {
        if (props.chatOptionsData) {
            shuffleAndSetOptions();
        }
    }, [props.chatOptionsData]);

    useEffect(() => {
        const agents = (agentsData || []).filter((agent) => agent !== null && agent !== undefined);
        setAgents(agents);
        // set the selected agent to the most recently used agent, first agent is always khoj
        setSelectedAgent(agents.length > 1 ? agents[1].slug : "khoj");

        // generate colored icons for the available agents
        const agentIcons = agents.map((agent) => getIconFromIconName(agent.icon, agent.color)!);
        setAgentIcons(agentIcons);
    }, [agentsData, props.isMobileWidth]);

    function shuffleSuggestionsCards() {
        shuffleAndSetOptions();
    }

    useEffect(() => {
        const processMessage = async () => {
            if (message && !processingMessage) {
                setProcessingMessage(true);
                try {
                    const newConversationId = await createNewConversation(selectedAgent || "khoj");
                    onConversationIdChange?.(newConversationId);
                    window.location.href = `/chat?conversationId=${newConversationId}`;
                    localStorage.setItem("message", message);
                    if (image) {
                        localStorage.setItem("image", image);
                    }
                } catch (error) {
                    console.error("Error creating new conversation:", error);
                    setProcessingMessage(false);
                }
                setMessage("");
            }
        };
        processMessage();
        if (message) {
            setProcessingMessage(true);
        }
    }, [selectedAgent, message, processingMessage, onConversationIdChange]);

    function fillArea(link: string, type: string, prompt: string) {
        if (!link) {
            let message_str = "";
            prompt = prompt.charAt(0).toLowerCase() + prompt.slice(1);

            if (type === "Online Search") {
                message_str = "/online " + prompt;
            } else if (type === "Paint") {
                message_str = "/image " + prompt;
            } else {
                message_str = prompt;
            }
            // Get the textarea element
            const message_area = document.getElementById("message") as HTMLTextAreaElement;

            if (message_area) {
                // Update the value directly
                message_area.value = message_str;
                setMessage(message_str);
            }
        }
    }

    return (
        <div className={`${styles.homeGreetings} w-full md:w-auto`}>
            {showLoginPrompt && (
                <LoginPrompt
                    onOpenChange={setShowLoginPrompt}
                    loginRedirectMessage={"Login to your second brain"}
                />
            )}
            <div className={`w-full text-center justify-end content-end`}>
                <div className="items-center">
                    <h1 className="text-2xl md:text-3xl text-center w-fit pb-6 px-4 mx-auto">
                        {greeting}
                    </h1>
                </div>
                {!props.isMobileWidth && (
                    <ScrollArea className="w-full max-w-[600px] mx-auto">
                        <div className="flex pb-2 gap-2 items-center justify-center">
                            {agents.map((agent, index) => (
                                <Popover
                                    key={`${index}-${agent.slug}`}
                                    open={isPopoverOpen && debouncedHoveredAgent === agent.slug}
                                    onOpenChange={(open) => {
                                        if (!open) {
                                            setHoveredAgent(null);
                                            setIsPopoverOpen(false);
                                        }
                                    }}
                                >
                                    <PopoverTrigger asChild>
                                        <Card
                                            className={`${
                                                selectedAgent === agent.slug
                                                    ? convertColorToBorderClass(agent.color)
                                                    : "border-stone-100 dark:border-neutral-700 text-muted-foreground"
                                            }
                                            hover:cursor-pointer rounded-lg px-2 py-2`}
                                            onDoubleClick={() => openAgentEditCard(agent.slug)}
                                            onClick={() => {
                                                setSelectedAgent(agent.slug);
                                                chatInputRef.current?.focus();
                                                setHoveredAgent(null);
                                                setIsPopoverOpen(false);
                                            }}
                                            onMouseEnter={() => setHoveredAgent(agent.slug)}
                                            onMouseLeave={() => {
                                                setHoveredAgent(null);
                                                setIsPopoverOpen(false);
                                            }}
                                        >
                                            <CardTitle className="text-center text-md font-medium flex justify-center items-center">
                                                {agentIcons[index]} {agent.name}
                                            </CardTitle>
                                        </Card>
                                    </PopoverTrigger>
                                    <PopoverContent
                                        className="w-80 p-0"
                                        onMouseLeave={() => {
                                            setHoveredAgent(null);
                                            setIsPopoverOpen(false);
                                        }}
                                    >
                                        <AgentCard
                                            data={agent}
                                            userProfile={null}
                                            isMobileWidth={props.isMobileWidth || false}
                                            showChatButton={false}
                                            editCard={false}
                                            filesOptions={[]}
                                            selectedChatModelOption=""
                                            agentSlug=""
                                            isSubscribed={isUserSubscribed(props.userConfig)}
                                            setAgentChangeTriggered={() => {}}
                                            modelOptions={[]}
                                            inputToolOptions={{}}
                                            outputModeOptions={{}}
                                        />
                                    </PopoverContent>
                                </Popover>
                            ))}
                        </div>
                        <ScrollBar orientation="horizontal" />
                    </ScrollArea>
                )}
            </div>
            <div className={`mx-auto ${props.isMobileWidth ? "w-full" : "w-fit"}`}>
                {!props.isMobileWidth && (
                    <div
                        className={`w-full ${styles.inputBox} shadow-lg bg-background align-middle items-center justify-center px-3 py-1 dark:bg-neutral-700 border-stone-100 dark:border-none dark:shadow-none rounded-2xl`}
                    >
                        <ChatInputArea
                            isLoggedIn={props.isLoggedIn}
                            sendMessage={(message) => setMessage(message)}
                            sendImage={(image) => setImage(image)}
                            sendDisabled={processingMessage}
                            chatOptionsData={props.chatOptionsData}
                            conversationId={null}
                            isMobileWidth={props.isMobileWidth}
                            setUploadedFiles={props.setUploadedFiles}
                            ref={chatInputRef}
                        />
                    </div>
                )}
                <div
                    className={`${styles.suggestions} w-full ${props.isMobileWidth ? "grid" : "flex flex-row"} justify-center items-center`}
                >
                    {shuffledOptions.map((suggestion, index) => (
                        <div
                            key={`${suggestion.type} ${suggestion.description}`}
                            onClick={(event) => {
                                if (props.isLoggedIn) {
                                    fillArea(
                                        suggestion.link,
                                        suggestion.type,
                                        suggestion.description,
                                    );
                                } else {
                                    event.preventDefault();
                                    event.stopPropagation();
                                    setShowLoginPrompt(true);
                                }
                            }}
                        >
                            <SuggestionCard
                                key={suggestion.type + Math.random()}
                                title={suggestion.type}
                                body={suggestion.description}
                                link={suggestion.link}
                                color={suggestion.color}
                            />
                        </div>
                    ))}
                </div>
                <div className="flex items-center justify-center margin-auto">
                    <button
                        onClick={shuffleSuggestionsCards}
                        className="m-2 p-1.5 rounded-lg dark:hover:bg-[var(--background-color)] hover:bg-stone-100 border border-stone-100 text-sm text-stone-500 dark:text-stone-300 dark:border-neutral-700"
                    >
                        More Ideas <ArrowCounterClockwise className="h-4 w-4 inline" />
                    </button>
                </div>
            </div>
            {props.isMobileWidth && (
                <>
                    <div
                        className={`${styles.inputBox} pt-1 shadow-[0_-20px_25px_-5px_rgba(0,0,0,0.1)] dark:bg-neutral-700 bg-background align-middle items-center justify-center pb-3 mx-1 rounded-t-2xl rounded-b-none`}
                    >
                        <ScrollArea className="w-full max-w-[85vw]">
                            <div className="flex gap-2 items-center justify-left pt-1 pb-2 px-12">
                                {agentIcons.map((icon, index) => (
                                    <Card
                                        key={`${index}-${agents[index].slug}`}
                                        className={`${selectedAgent === agents[index].slug ? convertColorToBorderClass(agents[index].color) : "border-muted text-muted-foreground"} hover:cursor-pointer`}
                                    >
                                        <CardTitle
                                            className="text-center text-xs font-medium flex justify-center items-center px-1.5 py-1"
                                            onDoubleClick={() =>
                                                openAgentEditCard(agents[index].slug)
                                            }
                                            onClick={() => {
                                                setSelectedAgent(agents[index].slug);
                                                chatInputRef.current?.focus();
                                            }}
                                        >
                                            {icon} {agents[index].name}
                                        </CardTitle>
                                    </Card>
                                ))}
                            </div>
                            <ScrollBar orientation="horizontal" />
                        </ScrollArea>
                        <ChatInputArea
                            isLoggedIn={props.isLoggedIn}
                            sendMessage={(message) => setMessage(message)}
                            sendImage={(image) => setImage(image)}
                            sendDisabled={processingMessage}
                            chatOptionsData={props.chatOptionsData}
                            conversationId={null}
                            isMobileWidth={props.isMobileWidth}
                            setUploadedFiles={props.setUploadedFiles}
                            ref={chatInputRef}
                        />
                    </div>
                </>
            )}
        </div>
    );
}

export default function Home() {
    const [chatOptionsData, setChatOptionsData] = useState<ChatOptions | null>(null);
    const [isLoading, setLoading] = useState(true);
    const [conversationId, setConversationID] = useState<string | null>(null);
    const [uploadedFiles, setUploadedFiles] = useState<string[]>([]);
    const isMobileWidth = useIsMobileWidth();

    const { userConfig: initialUserConfig, isLoadingUserConfig } = useUserConfig(true);
    const [userConfig, setUserConfig] = useState<UserConfig | null>(null);

    const authenticatedData = useAuthenticatedData();

    const handleConversationIdChange = (newConversationId: string) => {
        setConversationID(newConversationId);
    };

    useEffect(() => {
        setUserConfig(initialUserConfig);
    }, [initialUserConfig]);

    useEffect(() => {
        fetch("/api/chat/options")
            .then((response) => response.json())
            .then((data: ChatOptions) => {
                setLoading(false);
                if (data) {
                    setChatOptionsData(data);
                }
            })
            .catch((err) => {
                console.error(err);
                return;
            });
    }, []);

    if (isLoading) {
        return <Loading />;
    }

    return (
        <div className={`${styles.main} ${styles.chatLayout}`}>
            <title>Khoj AI - Your Second Brain</title>
            <div className={`${styles.sidePanel}`}>
                <SidePanel
                    conversationId={conversationId}
                    uploadedFiles={uploadedFiles}
                    isMobileWidth={isMobileWidth}
                />
            </div>
            <div className={`${styles.chatBox}`}>
                <div className={`${styles.chatBoxBody}`}>
                    <ChatBodyData
                        isLoggedIn={authenticatedData !== null}
                        chatOptionsData={chatOptionsData}
                        setUploadedFiles={setUploadedFiles}
                        isMobileWidth={isMobileWidth}
                        onConversationIdChange={handleConversationIdChange}
                        userConfig={userConfig}
                        isLoadingUserConfig={isLoadingUserConfig}
                    />
                </div>
            </div>
        </div>
    );
}
