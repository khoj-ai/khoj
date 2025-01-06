"use client";
import "./globals.css";
import styles from "./page.module.css";
import "katex/dist/katex.min.css";

import React, { useEffect, useRef, useState } from "react";
import useSWR from "swr";
import { ArrowsVertical } from "@phosphor-icons/react";

import { Card, CardTitle } from "@/components/ui/card";
import {
    StepOneSuggestionCard,
    StepOneSuggestionRevertCard,
    StepTwoSuggestionCard,
} from "@/app/components/suggestions/suggestionCard";
import Loading from "@/app/components/loading/loading";
import {
    AttachedFileText,
    ChatInputArea,
    ChatInputFocus,
    ChatOptions,
} from "@/app/components/chatInputArea/chatInputArea";
import {
    StepOneSuggestion,
    stepOneSuggestions,
    StepTwoSuggestion,
    getStepTwoSuggestions,
    SuggestionType,
} from "@/app/components/suggestions/suggestionsData";
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
import LoginPopup from "./components/loginPrompt/loginPopup";
import { SidebarInset, SidebarProvider, SidebarTrigger } from "@/components/ui/sidebar";
import { AppSidebar } from "./components/appSidebar/appSidebar";
import { Separator } from "@/components/ui/separator";
import { KhojLogoType } from "./components/logo/khojLogo";

interface ChatBodyDataProps {
    chatOptionsData: ChatOptions | null;
    onConversationIdChange?: (conversationId: string) => void;
    setUploadedFiles: (files: AttachedFileText[]) => void;
    isMobileWidth?: boolean;
    isLoggedIn: boolean;
    userConfig: UserConfig | null;
    isLoadingUserConfig: boolean;
}

function ChatBodyData(props: ChatBodyDataProps) {
    const [message, setMessage] = useState("");
    const [prefillMessage, setPrefillMessage] = useState("");
    const [chatInputFocus, setChatInputFocus] = useState<ChatInputFocus>(ChatInputFocus.MESSAGE);
    const [images, setImages] = useState<string[]>([]);
    const [processingMessage, setProcessingMessage] = useState(false);
    const [greeting, setGreeting] = useState("");
    const [stepOneSuggestionOptions, setStepOneSuggestionOptions] = useState<StepOneSuggestion[]>(
        stepOneSuggestions.slice(0, 3),
    );
    const [stepTwoSuggestionOptions, setStepTwoSuggestionOptions] = useState<StepTwoSuggestion[]>(
        [],
    );
    const [selectedStepOneSuggestion, setSelectedStepOneSuggestion] =
        useState<StepOneSuggestion | null>(null);
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
            `Ready to breeze through ${["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"][day]}?`,
            `Let's navigate your ${["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"][day]} workload`,
        ];
        const greeting = greetings[Math.floor(Math.random() * greetings.length)];
        setGreeting(greeting);
    }, [props.isLoadingUserConfig, props.userConfig]);

    useEffect(() => {
        const agents = (agentsData || []).filter((agent) => agent !== null && agent !== undefined);
        setAgents(agents);
        // set the first agent, which is always the default agent, as the default for chat
        setSelectedAgent(agents.length > 1 ? agents[0].slug : "khoj");

        // generate colored icons for the available agents
        const agentIcons = agents.map((agent) => getIconFromIconName(agent.icon, agent.color)!);
        setAgentIcons(agentIcons);
    }, [agentsData]);

    function showAllSuggestionsCards() {
        setStepOneSuggestionOptions(stepOneSuggestions);
    }

    useEffect(() => {
        const processMessage = async () => {
            if (message && !processingMessage) {
                setProcessingMessage(true);
                try {
                    const newConversationId = await createNewConversation(selectedAgent || "khoj");
                    onConversationIdChange?.(newConversationId);
                    localStorage.setItem("message", message);
                    if (images.length > 0) {
                        localStorage.setItem("images", JSON.stringify(images));
                    }

                    window.location.href = `/chat?conversationId=${newConversationId}`;
                } catch (error) {
                    console.error("Error creating new conversation:", error);
                    setProcessingMessage(false);
                }
                setMessage("");
                setImages([]);
            }
        };
        processMessage();
        if (message || images.length > 0) {
            setProcessingMessage(true);
        }
    }, [selectedAgent, message, processingMessage, onConversationIdChange]);

    // Close the agent detail hover card when scroll on agent pane
    useEffect(() => {
        const scrollAreaSelector = "[data-radix-scroll-area-viewport]";
        const scrollAreaEl = document.querySelector<HTMLElement>(scrollAreaSelector);
        const handleScroll = () => {
            setHoveredAgent(null);
            setIsPopoverOpen(false);
        };

        scrollAreaEl?.addEventListener("scroll", handleScroll);

        return () => scrollAreaEl?.removeEventListener("scroll", handleScroll);
    }, []);

    function clickStepOneSuggestion(suggestion: StepOneSuggestion) {
        setPrefillMessage(suggestion.intent);
        const stepTwoSuggestions = getStepTwoSuggestions(suggestion.type);
        setSelectedStepOneSuggestion(suggestion);
        setStepTwoSuggestionOptions(stepTwoSuggestions);
        setChatInputFocus(suggestion.focus);
    }

    return (
        <div className={`${styles.homeGreetings} w-full md:w-auto`}>
            {showLoginPrompt && (
                <LoginPrompt
                    onOpenChange={setShowLoginPrompt}
                    isMobileWidth={props.isMobileWidth}
                />
            )}
            {!props.isLoggedIn && (
                <LoginPopup
                    isMobileWidth={props.isMobileWidth}
                    setShowLoginPrompt={setShowLoginPrompt}
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
                                            <CardTitle className="text-center text-md font-medium flex justify-center items-center whitespace-nowrap">
                                                {agentIcons[index]} {agent.name}
                                            </CardTitle>
                                        </Card>
                                    </PopoverTrigger>
                                    <PopoverContent
                                        className="w-80 p-0 border-none bg-transparent shadow-none"
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
            <div className={`mx-auto ${props.isMobileWidth ? "w-full" : "w-full max-w-screen-md"}`}>
                {!props.isMobileWidth && (
                    <div
                        className={`w-full ${styles.inputBox} shadow-lg bg-background align-middle items-center justify-center px-3 py-1 dark:bg-neutral-700 border-stone-100 dark:border-none dark:shadow-none rounded-2xl`}
                    >
                        <ChatInputArea
                            isLoggedIn={props.isLoggedIn}
                            prefillMessage={prefillMessage}
                            focus={chatInputFocus}
                            sendMessage={(message) => setMessage(message)}
                            sendImage={(image) => setImages((prevImages) => [...prevImages, image])}
                            sendDisabled={processingMessage}
                            chatOptionsData={props.chatOptionsData}
                            conversationId={null}
                            isMobileWidth={props.isMobileWidth}
                            setUploadedFiles={props.setUploadedFiles}
                            agentColor={agents.find((agent) => agent.slug === selectedAgent)?.color}
                            ref={chatInputRef}
                            setTriggeredAbort={() => {}}
                        />
                    </div>
                )}
                {stepTwoSuggestionOptions.length == 0 && (
                    <div
                        className={`${styles.suggestions} w-full ${props.isMobileWidth ? (stepOneSuggestions.length > 3 ? "grid grid-cols-2" : "grid grid-cols-3") : "grid grid-cols-3"} "justify-center items-center"`}
                    >
                        {stepOneSuggestionOptions.map((suggestion, index) => (
                            <div
                                key={`${suggestion.type} ${suggestion.actionTagline}`}
                                onClick={(event) => {
                                    if (props.isLoggedIn) {
                                        clickStepOneSuggestion(suggestion);
                                    } else {
                                        event.preventDefault();
                                        event.stopPropagation();
                                        setShowLoginPrompt(true);
                                    }
                                }}
                            >
                                <StepOneSuggestionCard
                                    key={suggestion.type + Math.random()}
                                    title={suggestion.type}
                                    body={suggestion.actionTagline}
                                    color={suggestion.color}
                                />
                            </div>
                        ))}
                    </div>
                )}
                {stepTwoSuggestionOptions.length == 0 &&
                    stepOneSuggestionOptions.length < stepOneSuggestions.length && (
                        <div className="flex items-center justify-center margin-auto">
                            <button
                                onClick={showAllSuggestionsCards}
                                className="m-2 p-1.5 rounded-lg dark:hover:bg-[var(--background-color)] hover:bg-stone-100 border border-stone-100 text-sm text-stone-500 dark:text-stone-300 dark:border-neutral-700"
                            >
                                Show All <ArrowsVertical className="h-4 w-4 inline" />
                            </button>
                        </div>
                    )}
                {selectedStepOneSuggestion && (
                    <StepOneSuggestionRevertCard
                        title={selectedStepOneSuggestion.type}
                        body={selectedStepOneSuggestion.actionTagline}
                        color={selectedStepOneSuggestion.color}
                        onClick={() => {
                            setPrefillMessage("");
                            setSelectedStepOneSuggestion(null);
                            setStepTwoSuggestionOptions([]);
                            setChatInputFocus(ChatInputFocus.MESSAGE);
                        }}
                    />
                )}
                {stepTwoSuggestionOptions.length > 0 && (
                    <div
                        className={`w-full ${props.isMobileWidth ? "grid" : "grid grid-cols-1"} justify-center items-center gap-2 p-2`}
                    >
                        {stepTwoSuggestionOptions.map((suggestion, index) => (
                            <div
                                key={`${suggestion.prompt} ${index}`}
                                className={`w-full cursor-pointer animate-fade-in-up`}
                                onClick={(event) => {
                                    setMessage(suggestion.prompt);
                                }}
                            >
                                <StepTwoSuggestionCard
                                    key={suggestion.prompt}
                                    prompt={suggestion.prompt}
                                />
                            </div>
                        ))}
                    </div>
                )}
            </div>
            {props.isMobileWidth && (
                <>
                    <div
                        className={`${styles.inputBox} pt-1 shadow-[0_-20px_25px_-5px_rgba(0,0,0,0.1)] dark:bg-neutral-700 bg-background align-middle items-center justify-center pb-3 mx-1 rounded-2xl mb-2`}
                    >
                        <ScrollArea className="w-full max-w-[85vw]">
                            <div className="flex gap-2 items-center justify-left pt-1 pb-2 px-12">
                                {agentIcons.map((icon, index) => (
                                    <Card
                                        key={`${index}-${agents[index].slug}`}
                                        className={`${selectedAgent === agents[index].slug ? convertColorToBorderClass(agents[index].color) : "border-muted text-muted-foreground"} hover:cursor-pointer`}
                                    >
                                        <CardTitle
                                            className="text-center text-xs font-medium flex justify-center items-center whitespace-nowrap px-1.5 py-1"
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
                            sendImage={(image) => setImages((prevImages) => [...prevImages, image])}
                            sendDisabled={processingMessage}
                            chatOptionsData={props.chatOptionsData}
                            conversationId={null}
                            isMobileWidth={props.isMobileWidth}
                            setUploadedFiles={props.setUploadedFiles}
                            agentColor={agents.find((agent) => agent.slug === selectedAgent)?.color}
                            ref={chatInputRef}
                            setTriggeredAbort={() => {}}
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
    const [uploadedFiles, setUploadedFiles] = useState<AttachedFileText[] | null>(null);
    const isMobileWidth = useIsMobileWidth();

    const { userConfig: initialUserConfig, isLoadingUserConfig } = useUserConfig(true);
    const [userConfig, setUserConfig] = useState<UserConfig | null>(null);

    const {
        data: authenticatedData,
        error: authenticationError,
        isLoading: authenticationLoading,
    } = useAuthenticatedData();

    const handleConversationIdChange = (newConversationId: string) => {
        setConversationID(newConversationId);
    };

    useEffect(() => {
        setUserConfig(initialUserConfig);
    }, [initialUserConfig]);

    useEffect(() => {
        if (uploadedFiles) {
            localStorage.setItem("uploadedFiles", JSON.stringify(uploadedFiles));
        }
    }, [uploadedFiles]);

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
        <SidebarProvider>
            <AppSidebar conversationId={conversationId} />
            <SidebarInset>
                <header className="flex h-16 shrink-0 items-center gap-2 border-b px-4">
                    <SidebarTrigger className="-ml-1" />
                    <Separator orientation="vertical" className="mr-2 h-4" />
                    {isMobileWidth ? (
                        <a className="p-0 no-underline" href="/">
                            <KhojLogoType className="h-auto w-16" />
                        </a>
                    ) : (
                        <h2 className="text-lg">Ask Anything</h2>
                    )}
                </header>
                <div className={`${styles.main} ${styles.chatLayout}`}>
                    <title>Khoj AI - Your Second Brain</title>
                    <div className={`${styles.chatBox}`}>
                        <div className={`${styles.chatBoxBody}`}>
                            {!authenticationLoading && (
                                <ChatBodyData
                                    isLoggedIn={authenticatedData ? true : false}
                                    chatOptionsData={chatOptionsData}
                                    setUploadedFiles={setUploadedFiles}
                                    isMobileWidth={isMobileWidth}
                                    onConversationIdChange={handleConversationIdChange}
                                    userConfig={userConfig}
                                    isLoadingUserConfig={isLoadingUserConfig}
                                />
                            )}
                        </div>
                    </div>
                </div>
            </SidebarInset>
        </SidebarProvider>
    );
}
