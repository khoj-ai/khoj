'use client'
import './globals.css';
import styles from './page.module.css';
import 'katex/dist/katex.min.css';

import React, { useEffect, useState } from 'react';
import useSWR from 'swr';
import Image from 'next/image';
import { ClockCounterClockwise } from '@phosphor-icons/react';

import { Card, CardTitle } from '@/components/ui/card';
import SuggestionCard from '@/app/components/suggestions/suggestionCard';
import SidePanel from '@/app/components/sidePanel/chatHistorySidePanel';
import NavMenu from '@/app/components/navMenu/navMenu';
import Loading from '@/app/components/loading/loading';
import ChatInputArea, { ChatOptions } from '@/app/components/chatInputArea/chatInputArea';
import { Suggestion, suggestionsData } from '@/app/components/suggestions/suggestionsData';

import { useAuthenticatedData, useUserConfig } from '@/app/common/auth';
import { convertColorToBorderClass } from '@/app/common/colorUtils';
import { getIconFromIconName } from '@/app/common/iconUtils';
import { AgentData } from '@/app/agents/page';



interface ChatBodyDataProps {
    chatOptionsData: ChatOptions | null;
    onConversationIdChange?: (conversationId: string) => void;
    setUploadedFiles: (files: string[]) => void;
    isMobileWidth?: boolean;
    isLoggedIn: boolean;
}

async function createNewConvo(slug: string) {
    try {
        const response = await fetch(`/api/chat/sessions?client=web&agent_slug=${slug}`, { method: "POST" });
        if (!response.ok) throw new Error(`Failed to fetch chat sessions with status: ${response.status}`);
        const data = await response.json();
        const conversationID = data.conversation_id;
        if (!conversationID) throw new Error("Conversation ID not found in response");
        return conversationID;
    } catch (error) {
        console.error("Error creating new conversation:", error);
        throw error;
    }
}

function ChatBodyData(props: ChatBodyDataProps) {
    const [message, setMessage] = useState('');
    const [processingMessage, setProcessingMessage] = useState(false);
    const [greeting, setGreeting] = useState('');
    const [shuffledOptions, setShuffledOptions] = useState<Suggestion[]>([]);
    const [selectedAgent, setSelectedAgent] = useState<string | null>("khoj");
    const [agentIcons, setAgentIcons] = useState<JSX.Element[]>([]);
    const [agents, setAgents] = useState<AgentData[]>([]);

    const userConfig = useUserConfig(true);
    const agentsFetcher = () => window.fetch('/api/agents').then(res => res.json()).catch(err => console.log(err));
    const { data: agentsData, error } = useSWR<AgentData[]>('agents', agentsFetcher, { revalidateOnFocus: false });

    function shuffleAndSetOptions() {
        const shuffled = [...suggestionsData].sort(() => 0.5 - Math.random());
        setShuffledOptions(shuffled.slice(0, 3));
    }

    useEffect(() => {
        // Get today's day
        const today = new Date();
        const day = today.getDay();
        const timeOfDay = today.getHours() > 4 && today.getHours() < 12 ? 'morning' : today.getHours() < 17 ? 'afternoon' : 'evening';
        const nameSuffix = userConfig?.given_name ? `, ${userConfig?.given_name}` : "";
        console.log(userConfig);
        const greetings = [
            `What would you like to get done${nameSuffix}?`,
            `Hey${nameSuffix}! How can I help?`,
            `Good ${timeOfDay}${nameSuffix}! What's on your mind?`,
            `Ready to breeze through your ${['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'][day]}?`,
            `Want help navigating your ${['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'][day]} workload?`
        ];
        const greeting = greetings[Math.floor(Math.random() * greetings.length)];
        setGreeting(greeting);
    }, []);

    useEffect(() => {
        if (props.chatOptionsData) {
            shuffleAndSetOptions();
        }
    }, [props.chatOptionsData]);

    useEffect(() => {
        const nSlice = props.isMobileWidth ? 3 : 4;
        const shuffledAgents = agentsData ? [...agentsData].sort(() => 0.5 - Math.random()) : [];
        const agents = agentsData ? [agentsData[0]] : []; // Always add the first/default agent.

        shuffledAgents.slice(0, nSlice - 1).forEach(agent => {
            if (!agents.find(a => a.slug === agent.slug)) {
                agents.push(agent);
            }
        });

        setAgents(agents);

        //generate colored icons for the selected agents
        const agentIcons = agents.map(
            agent => getIconFromIconName(agent.icon, agent.color) || <Image key={agent.name} src={agent.avatar} alt={agent.name} width={50} height={50} />
        );
        setAgentIcons(agentIcons);
    }, [agentsData]);

    function shuffleSuggestionsCards() {
        shuffleAndSetOptions();
    }

    useEffect(() => {
        const processMessage = async () => {
            if (message && !processingMessage) {
                setProcessingMessage(true);
                try {
                    const newConversationId = await createNewConvo(selectedAgent || "khoj");
                    props.onConversationIdChange?.(newConversationId);
                    window.location.href = `/chat?conversationId=${newConversationId}`;
                    localStorage.setItem('message', message);
                }
                catch (error) {
                    console.error("Error creating new conversation:", error);
                    setProcessingMessage(false);
                }
                setMessage('');
            }
        };
        processMessage();
        if (message) {
            setProcessingMessage(true);
        };
    }, [selectedAgent, message]);

    function fillArea(link: string, type: string, prompt: string) {
        if (!link) {
            let message_str = "";
            prompt = prompt.charAt(0).toLowerCase() + prompt.slice(1);

            if (type === "Online Search") {
                message_str = "/online " + prompt;
            } else if (type === "Paint") {
                message_str = "/paint " + prompt;
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
        <div className={`${styles.chatBoxBody}`}>
            <div className="w-full text-center">
                <div className="items-center">
                    <h1 className="text-center pb-6 px-4 w-fit ml-auto mr-auto">{greeting}</h1>
                </div>
                {
                    !props.isMobileWidth &&
                    <div className="flex pb-6 gap-2 items-center justify-center">
                        {agentIcons.map((icon, index) => (
                            <Card
                                key={`${index}-${agents[index].slug}`}
                                className={
                                    `${selectedAgent === agents[index].slug ?
                                        convertColorToBorderClass(agents[index].color) : 'border-stone-100 dark:border-neutral-700 text-muted-foreground'}
                                    hover:cursor-pointer rounded-lg px-2 py-2`}>
                                <CardTitle
                                    className='text-center text-md font-medium flex justify-center items-center'
                                    onClick={() => setSelectedAgent(agents[index].slug)}>
                                    {icon} {agents[index].name}
                                </CardTitle>
                            </Card>
                        ))}
                        <Card className='border-none shadow-none flex justify-center items-center hover:cursor-pointer' onClick={() => window.location.href = "/agents"}>
                            <CardTitle className="text-center text-md font-normal flex justify-center items-center px-1.5 py-2">See All →</CardTitle>
                        </Card>
                    </div>
                }
            </div>
            <div className={`ml-auto mr-auto ${props.isMobileWidth ? 'w-full' : 'w-fit'}`}>
                {
                    !props.isMobileWidth &&
                    <div className={`w-full ${styles.inputBox} shadow-lg bg-background align-middle items-center justify-center px-3 py-1 dark:bg-neutral-700 border-stone-100 dark:border-none dark:shadow-none`}>
                        <ChatInputArea
                            isLoggedIn={props.isLoggedIn}
                            sendMessage={(message) => setMessage(message)}
                            sendDisabled={processingMessage}
                            chatOptionsData={props.chatOptionsData}
                            conversationId={null}
                            isMobileWidth={props.isMobileWidth}
                            setUploadedFiles={props.setUploadedFiles} />
                    </div>
                }
                <div className={`${styles.suggestions} w-full ${props.isMobileWidth ? 'grid' : 'flex flex-row'} justify-center items-center`}>
                    {shuffledOptions.map((suggestion, index) => (
                        <div
                            key={`${suggestion.type} ${suggestion.description}`}
                            onClick={() => fillArea(suggestion.link, suggestion.type, suggestion.description)}>
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
                        className="m-2 p-1.5 rounded-lg dark:hover:bg-[var(--background-color)] hover:bg-stone-100 border border-stone-100 text-sm text-stone-500 dark:text-stone-300 dark:border-neutral-700">
                        More Examples <ClockCounterClockwise className='h-4 w-4 inline' />
                    </button>
                </div>
            </div>
            {
                props.isMobileWidth &&
                <div className={`${styles.inputBox} shadow-md dark:bg-neutral-700 bg-background dark: align-middle items-center justify-center py-3 px-1`}>
                    <ChatInputArea
                        isLoggedIn={props.isLoggedIn}
                        sendMessage={(message) => setMessage(message)}
                        sendDisabled={processingMessage}
                        chatOptionsData={props.chatOptionsData}
                        conversationId={null}
                        isMobileWidth={props.isMobileWidth}
                        setUploadedFiles={props.setUploadedFiles} />
                    <div className="flex gap-2 items-center justify-left pt-4">
                        {agentIcons.map((icon, index) => (
                            <Card
                                key={`${index}-${agents[index].slug}`}
                                className={
                                    `${selectedAgent === agents[index].slug ? convertColorToBorderClass(agents[index].color) : 'border-muted text-muted-foreground'} hover:cursor-pointer`
                                }>
                                <CardTitle
                                    className='text-center text-xs font-medium flex justify-center items-center px-1.5 py-2'
                                    onClick={() => setSelectedAgent(agents[index].slug)}>
                                    {icon} {agents[index].name}
                                </CardTitle>
                            </Card>
                        ))}
                        <Card className='border-none shadow-none flex justify-center items-center hover:cursor-pointer' onClick={() => window.location.href = "/agents"}>
                            <CardTitle className={`text-center ${props.isMobileWidth ? 'text-xs' : 'text-md'} font-normal flex justify-center items-center px-1.5 py-2`}>See All →</CardTitle>
                        </Card>
                    </div>
                </div>
            }
        </div>
    );
}

export default function Home() {
    const [chatOptionsData, setChatOptionsData] = useState<ChatOptions | null>(null);
    const [isLoading, setLoading] = useState(true);
    const [title, setTitle] = useState('');
    const [conversationId, setConversationID] = useState<string | null>(null);
    const [uploadedFiles, setUploadedFiles] = useState<string[]>([]);
    const [isMobileWidth, setIsMobileWidth] = useState(false);

    const authenticatedData = useAuthenticatedData();

    const handleConversationIdChange = (newConversationId: string) => {
        setConversationID(newConversationId);
    };

    useEffect(() => {
        fetch('/api/chat/options')
            .then(response => response.json())
            .then((data: ChatOptions) => {
                setLoading(false);
                if (data) {
                    setChatOptionsData(data);
                }
            })
            .catch(err => {
                console.error(err);
                return;
            });

        setIsMobileWidth(window.innerWidth < 786);

        window.addEventListener('resize', () => {
            setIsMobileWidth(window.innerWidth < 786);
        });

    }, []);

    if (isLoading) {
        return <Loading />;
    }

    return (
        <div className={`${styles.main} ${styles.chatLayout}`}>
            <title>
                {title}
            </title>
            <div className={`${styles.sidePanel}`}>
                <SidePanel
                    conversationId={conversationId}
                    uploadedFiles={uploadedFiles}
                    isMobileWidth={isMobileWidth}
                />
            </div>
            <div className={`${styles.chatBox}`}>
                <NavMenu selected="Chat" title={title}></NavMenu>
                <div className={`${styles.chatBoxBody}`}>
                    <ChatBodyData
                        isLoggedIn={authenticatedData !== null}
                        chatOptionsData={chatOptionsData}
                        setUploadedFiles={setUploadedFiles}
                        isMobileWidth={isMobileWidth}
                        onConversationIdChange={handleConversationIdChange}
                    />
                </div>
            </div>
        </div>
    );
}
