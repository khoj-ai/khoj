'use client'

import styles from './chat.module.css';
import React, { Suspense, useEffect, useState, useMemo } from 'react';

import SuggestionCard from '../components/suggestions/suggestionCard';
import AgentShortcut from '../components/agentShortcut/agentShortcut';
import SidePanel from '../components/sidePanel/chatHistorySidePanel';
import ChatHistory from '../components/chatHistory/chatHistory';
import NavMenu from '../components/navMenu/navMenu';
import { useSearchParams } from 'next/navigation'
import Loading from '../components/loading/loading';
import useSWR from 'swr';
import Image from 'next/image';
import { Button } from "@/components/ui/button"

import { handleCompiledReferences, handleImageResponse, setupWebSocket } from '../common/chatFunctions';

import 'katex/dist/katex.min.css';

import { StreamMessage } from '../components/chatMessage/chatMessage';
import { welcomeConsole } from '../common/utils';
import ChatInputArea, { ChatOptions } from '../components/chatInputArea/chatInputArea';
import { useAuthenticatedData } from '../common/auth';
import { set } from 'react-hook-form';

const suggestions: Suggestion[] = [["Automation", "blue", "/automate.svg", "Send me a summary of HackerNews every morning.", "/automations?subject=Summarizing%20Top%20Headlines%20from%20HackerNews&query=Summarize%20the%20top%20headlines%20on%20HackerNews&crontime=00%207%20*%20*%20*"], ["Automation", "blue", "/automate.svg", "Compose a bedtime story that a five-year-old might enjoy.", "/automations?subject=Daily%20Bedtime%20Story&query=Compose%20a%20bedtime%20story%20that%20a%20five-year-old%20might%20enjoy.%20It%20should%20not%20exceed%20five%20paragraphs.%20Appeal%20to%20the%20imagination%2C%20but%20weave%20in%20learnings.&crontime=0%2021%20*%20*%20*"], ["Paint", "green", "/paint.svg", "Paint a picture of a sunset but it's made of stained glass tiles", ""], ["Online Search", "yellow", "/online_search.svg", "Search for the best attractions in Austria Hungary", ""]];
const sampleAgents = [["Khoj", "/khoj_agent.svg"], ["Teacher", "/teacher_agent.svg"], ["Doctor", "/doctor_agent.svg"], ["Sage", "/sage_agent.svg"]];

import {
    Lightbulb,
    Robot,
    Aperture,
    GraduationCap,
    Jeep,
    Island,
    MathOperations,
    Asclepius,
    Couch,
    Code,
    Atom,
    ClockCounterClockwise,
    PaperPlaneTilt,
    Info,
    UserCircle,
    Globe,
    Palette,
} from "@phosphor-icons/react";
import Chat from '../page';

interface IconMap {
    [key: string]: (color: string, width: string, height: string) => JSX.Element | null;
}

const iconMap: IconMap = {
    Lightbulb: (color: string, width: string, height: string) => <Lightbulb className={`${width} ${height} ${color} mr-2`} />,
    Robot: (color: string, width: string, height: string) => <Robot className={`${width} ${height} ${color} mr-2`} />,
    Aperture: (color: string, width: string, height: string) => <Aperture className={`${width} ${height} ${color} mr-2`} />,
    GraduationCap: (color: string, width: string, height: string) => <GraduationCap className={`${width} ${height} ${color} mr-2`} />,
    Jeep: (color: string, width: string, height: string) => <Jeep className={`${width} ${height} ${color} mr-2`} />,
    Island: (color: string, width: string, height: string) => <Island className={`${width} ${height} ${color} mr-2`} />,
    MathOperations: (color: string, width: string, height: string) => <MathOperations className={`${width} ${height} ${color} mr-2`} />,
    Asclepius: (color: string, width: string, height: string) => <Asclepius className={`${width} ${height} ${color} mr-2`} />,
    Couch: (color: string, width: string, height: string) => <Couch className={`${width} ${height} ${color} mr-2`} />,
    Code: (color: string, width: string, height: string) => <Code className={`${width} ${height} ${color} mr-2`} />,
    Atom: (color: string, width: string, height: string) => <Atom className={`${width} ${height} ${color} mr-2`} />,
    ClockCounterClockwise: (color: string, width: string, height: string) => <ClockCounterClockwise className={`${width} ${height} ${color} mr-2`} />,
    Globe: (color: string, width: string, height: string) => <Globe className={`${width} ${height} ${color} mr-2`} />,
    Palette: (color: string, width: string, height: string) => <Palette className={`${width} ${height} ${color} mr-2`} />,
};

function convertColorToTextClass(color: string) {
    if (color === 'red') return `text-red-500`;
    if (color === 'yellow') return `text-yellow-500`;
    if (color === 'green') return `text-green-500`;
    if (color === 'blue') return `text-blue-500`;
    if (color === 'orange') return `text-orange-500`;
    if (color === 'purple') return `text-purple-500`;
    if (color === 'pink') return `text-pink-500`;
    if (color === 'teal') return `text-teal-500`;
    if (color === 'cyan') return `text-cyan-500`;
    if (color === 'lime') return `text-lime-500`;
    if (color === 'indigo') return `text-indigo-500`;
    if (color === 'fuschia') return `text-fuschia-500`;
    if (color === 'rose') return `text-rose-500`;
    if (color === 'sky') return `text-sky-500`;
    if (color === 'amber') return `text-amber-500`;
    if (color === 'emerald') return `text-emerald-500`;
    return `text-gray-500`;
}

function convertSuggestionColorToTextClass(color: string) {
    if (color === 'blue') return `bg-gradient-to-b from-white 50% to-sky-50`;
    if (color === 'yellow') return `bg-gradient-to-b from-white 50% to-yellow-50`;
    if (color === 'green') return `bg-gradient-to-b from-white 50% to-green-50`;
    if (color === 'pink') return `bg-gradient-to-b from-white 50% to-pink-50`;
    if (color === 'purple') return `bg-gradient-to-b from-white 50% to-purple-50`;
    return `bg-gradient-to-b from-white 50% to-orange-50`;
}

function convertSuggestionColorToIconClass(color: string) {
    if (color === 'blue') return iconMap.Robot('blue', 'w-8', 'h-8');
    if (color === 'yellow') return iconMap.Globe('yellow', 'w-8', 'h-8');
    if (color === 'green') return iconMap.Palette('green', 'w-8', 'h-8');
    else return iconMap.Lightbulb('orange', 'w-8', 'h-8');
}




function getIconFromIconName(iconName: string, color: string = 'gray', width: string = 'w-8', height: string = 'h-8') {
    const icon = iconMap[iconName];
    const colorName = color.toLowerCase();
    const colorClass = convertColorToTextClass(colorName);

    return icon ? icon(colorClass, width, height) : null;
}

function convertColorToClass(color: string) {
    // We can't dyanmically generate the classes for tailwindcss, so we have to explicitly use the whole string.
    // See models/__init__.py 's definition of the Agent model for the color choices.
    if (color === 'red') return `bg-red-500 hover:bg-red-600`;
    if (color === 'yellow') return `bg-yellow-500 hover:bg-yellow-600`;
    if (color === 'green') return `bg-green-500 hover:bg-green-600`;
    if (color === 'blue') return `bg-blue-500 hover:bg-blue-600`;
    if (color === 'orange') return `bg-orange-500 hover:bg-orange-600`;
    if (color === 'purple') return `bg-purple-500 hover:bg-purple-600`;
    if (color === 'pink') return `bg-pink-500 hover:bg-pink-600`;
    if (color === 'teal') return `bg-teal-500 hover:bg-teal-600`;
    if (color === 'cyan') return `bg-cyan-500 hover:bg-cyan-600`;
    if (color === 'lime') return `bg-lime-500 hover:bg-lime-600`;
    if (color === 'indigo') return `bg-indigo-500 hover:bg-indigo-600`;
    if (color === 'fuschia') return `bg-fuschia-500 hover:bg-fuschia-600`;
    if (color === 'rose') return `bg-rose-500 hover:bg-rose-600`;
    if (color === 'sky') return `bg-sky-500 hover:bg-sky-600`;
    if (color === 'amber') return `bg-amber-500 hover:bg-amber-600`;
    if (color === 'emerald') return `bg-emerald-500 hover:bg-emerald-600`;
    return `bg-gray-500 hover:bg-gray-600`;
}



export interface AgentData {
    slug: string;
    avatar: string;
    name: string;
    personality: string;
    color: string;
    icon: string;
}

interface ChatBodyDataProps {
    chatOptionsData: ChatOptions | null;
    setTitle: (title: string) => void;
    onConversationIdChange?: (conversationId: string) => void;
    setQueryToProcess: (query: string) => void;
    streamedMessages: StreamMessage[];
    setUploadedFiles: (files: string[]) => void;
    isMobileWidth?: boolean;
    isLoggedIn: boolean;
    conversationId: string | null; // Added this line
}
type Suggestion = [string, string, string, string, string];

async function createNewConvo() {
    try {
      const response = await fetch('/api/chat/sessions', { method: "POST" });
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      const data = await response.json();
      const conversationID = data.conversation_id;
      console.log("New conversation ID (create new convo):", conversationID);
      if (!conversationID) {
        throw new Error("Conversation ID not found in response");
      }
      return conversationID;
    } catch (error) {
      console.error("Error creating new conversation:", error);
      throw error;
    }
  }

async function navigateToNewConvo() {
    try {
        const url = await createNewConvo();
        console.log("URL: ", url);
        window.location.href = url;
    } catch (error) {
        console.error("Error navigating to new conversation:", error);
    }
}

function ChatBodyData(props: ChatBodyDataProps) {
    const [message, setMessage] = useState('');
    const [processingMessage, setProcessingMessage] = useState(false);
    const [shuffledOptions, setShuffledOptions] = useState<Suggestion[]>([]);
    const [shuffledColors, setShuffledColors] = useState<string[]>([]);
    const [selectedAgent, setSelectedAgent] = useState<string | null>(null);
    //const [localConversationId, setLocalConversationId] = useState<string | null>(conversationId);
    const agentsFetcher = () => window.fetch('/api/agents').then(res => res.json()).catch(err => console.log(err));
    const { data, error } = useSWR<AgentData[]>('agents', agentsFetcher, { revalidateOnFocus: false });

    // useEffect(() => {
    //     if (props.conversationId) {
    //       //setLocalConversationId(props.conversationId);
    //       props.onConversationIdChange?.(props.conversationId);
    //     }
    // }, [props.conversationId]);

    function shuffleAndSetOptions() {
        const shuffled = [...suggestions].sort(() => 0.5 - Math.random());
        setShuffledOptions(shuffled.slice(0, 3));
        //use the text to color function above convertSuggestionColorToTextClass
        const colors = shuffled.map(option => convertSuggestionColorToTextClass(option[1]));
        setShuffledColors(colors);
    }

    useEffect(() => {
        if (props.chatOptionsData) {
            shuffleAndSetOptions();
        }
    }, [props.chatOptionsData]);

    function onButtonClick() {
        shuffleAndSetOptions();
    }

    useEffect(() => { const processMessage = async () => {
            if (message && !processingMessage) {
                setProcessingMessage(true);
                //if (!conversationId) {
                try {
                    const newConversationId = await createNewConvo();
                    console.log("New conversation ID (useEffect):", newConversationId);
                    //setLocalConversationId(newConversationId);
                    props.onConversationIdChange?.(newConversationId);
                    window.history.pushState({}, '', `/chat?conversationId=${newConversationId}`);
                    props.setQueryToProcess(message);
                }
                catch (error) {
                    console.error("Error creating new conversation:", error);
                    setProcessingMessage(false);
                }
                // } else {
                // props.setQueryToProcess(message);
                // }
                setMessage(''); // Clear the input after sending
            }
            };
            processMessage();
            if(message){
                setProcessingMessage(true);
                props.setQueryToProcess(message);
            };
    }, [message]);

    useEffect(() => {
        if (props.streamedMessages &&
            props.streamedMessages.length > 0 &&
            props.streamedMessages[props.streamedMessages.length - 1].completed) {
            setProcessingMessage(false);
        } else {
            setMessage('');
        }
    }, [props.streamedMessages]);

    //make the agents the first four items in data
    const agents = data ? data.slice(0, 4) : [];
    const icons = agents.map(agent => getIconFromIconName(agent.icon, agent.color) || <Image src={agent.avatar} alt={agent.name} width={50} height={50} />);
    // const storedURL = storeUrlSynchronously();
    // const id = storedURL.replace("/chat?conversationId=", "");
    // console.log("id for new convo: ", id);
    return (
        <div>
        <div className="w-full text-center">
            <h1 className="white pb-8 w-4/5">What would you like to do?</h1>
        </div>
        <div className="w-fit text-center">
        <div className="flex pb-8 ms-10 gap-2">
            {icons.map((icon, index) => (
                <a className="no-underline w-200 flex pl-3 pt-1 pb-1 border border-stone-100 rounded-md">
                {icon}
                <p className="relative top-1 pr-3">{agents[index].name}</p>
                </a>
            ))}
            <a className="no-underline w-200 flex pl-3 pt-1 pb-1 ps-4" href="/agents">
                <p className="relative top-1 hover:underline">See More →</p>
            </a>
        </div>
        </div>
        <div className="w-fit">
            <div className={`${styles.inputBox} bg-background align-middle items-center justify-center px-3`}>
                <ChatInputArea
                    isLoggedIn={props.isLoggedIn}
                    sendMessage={(message) => setMessage(message)}
                    sendDisabled={processingMessage}
                    chatOptionsData={props.chatOptionsData}
                    conversationId={null}
                    isMobileWidth={props.isMobileWidth}
                    setUploadedFiles={props.setUploadedFiles} />
            </div>
            <div className={`suggestions ${styles.suggestions} w-full flex`}>
                {shuffledOptions.map(([key, styleClass, image, value, link], index) => (
                    <SuggestionCard
                    key={key + Math.random()}
                    title={key}
                    body={value.length > 65 ? value.substring(0, 65) + '...' : value}
                    link={link} // replace with actual link if available
                    color={shuffledColors[index]}
                    image={shuffledColors[index]}
                    />
                ))}
            </div>
            <div className="flex items-center justify-center">
                <button onClick={onButtonClick} className="m-2 p-1 rounded-lg dark:hover:bg-[var(--background-color)] hover:bg-stone-100 border border-stone-100">More Examples ⟳</button>
                <button onClick={navigateToNewConvo} className="m-2 p-1 rounded-lg dark:hover:bg-[var(--background-color)] hover:bg-stone-100 border border-stone-100">New Conversation</button>
            </div>
        </div>
        </div>
    );
}

export default function Home(){
    const [chatOptionsData, setChatOptionsData] = useState<ChatOptions | null>(null);
    const [isLoading, setLoading] = useState(true);
    const [title, setTitle] = useState('Khoj AI - Chat');
    const [conversationId, setConversationID] = useState<string | null>(null);
    const [chatWS, setChatWS] = useState<WebSocket | null>(null);
    const [messages, setMessages] = useState<StreamMessage[]>([]);
    const [queryToProcess, setQueryToProcess] = useState<string>('');
    const [processQuerySignal, setProcessQuerySignal] = useState(false);
    const [uploadedFiles, setUploadedFiles] = useState<string[]>([]);
    const [isMobileWidth, setIsMobileWidth] = useState(false);

    const authenticatedData = useAuthenticatedData();

    const handleConversationIdChange = (newConversationId: string) => {
        console.log("Conversation ID changed to", newConversationId);
        setConversationID(newConversationId);
    };

    useEffect(() => {
        fetch('/api/chat/options')
            .then(response => response.json())
            .then((data: ChatOptions) => {
                setLoading(false);
                // Render chat options, if any
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
        <div className={styles.main + " " + styles.chatLayout}>
            <title>
                {title}
            </title>
            <div className={styles.sidePanel}>
                <SidePanel
                    webSocketConnected={chatWS !== null}
                    conversationId={conversationId}
                    uploadedFiles={uploadedFiles}
                    isMobileWidth={isMobileWidth}
                />
            </div>
            <div className={styles.chatBox}>
                <NavMenu selected="Chat" title={title} />
                <div className={styles.chatBoxBody}>
                    <Suspense fallback={<Loading />}>
                        <ChatBodyData
                            isLoggedIn={authenticatedData !== null}
                            streamedMessages={messages}
                            chatOptionsData={chatOptionsData}
                            setTitle={setTitle}
                            setQueryToProcess={setQueryToProcess}
                            setUploadedFiles={setUploadedFiles}
                            isMobileWidth={isMobileWidth}
                            onConversationIdChange={handleConversationIdChange}
                            conversationId={conversationId}/>
                    </Suspense>
                </div>
            </div>
        </div>
    );
}
