'use client'

import styles from './agents.module.css';

import Image from 'next/image';
import useSWR from 'swr';

import { useEffect, useState } from 'react';

import { useAuthenticatedData, UserProfile } from '../common/auth';
import { Button } from '@/components/ui/button';
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
    Lightning,
    Plus,
} from "@phosphor-icons/react";
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from '@/components/ui/card';
import { Dialog, DialogClose, DialogContent, DialogFooter, DialogHeader, DialogTrigger } from '@/components/ui/dialog';
import { Drawer, DrawerClose, DrawerContent, DrawerDescription, DrawerFooter, DrawerHeader, DrawerTitle, DrawerTrigger } from '@/components/ui/drawer';
import LoginPrompt from '../components/loginPrompt/loginPrompt';
import Loading, { InlineLoading } from '../components/loading/loading';
import { Alert, AlertDescription, AlertTitle } from '@/components/ui/alert';
import { Arrow } from '@radix-ui/react-popover';
import SidePanel from '../components/sidePanel/chatHistorySidePanel';

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
};

export interface AgentData {
    slug: string;
    avatar: string;
    name: string;
    personality: string;
    color: string;
    icon: string;
}

async function openChat(slug: string, userData: UserProfile | null) {

    const unauthenticatedRedirectUrl = `/login?next=/agents?agent=${slug}`;
    if (!userData) {
        window.location.href = unauthenticatedRedirectUrl;
        return;
    }

    const response = await fetch(`/api/chat/sessions?agent_slug=${slug}`, { method: "POST" });
    const data = await response.json();
    if (response.status == 200) {
        window.location.href = `/chat?conversationId=${data.conversation_id}`;
    } else if (response.status == 403 || response.status == 401) {
        window.location.href = unauthenticatedRedirectUrl;
    } else {
        alert("Failed to start chat session");
    }
}

const agentsFetcher = () => window.fetch('/api/agents').then(res => res.json()).catch(err => console.log(err));

interface AgentCardProps {
    data: AgentData;
    userProfile: UserProfile | null;
    isMobileWidth: boolean;
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

function AgentCard(props: AgentCardProps) {
    const searchParams = new URLSearchParams(window.location.search);
    const agentSlug = searchParams.get('agent');
    const [showModal, setShowModal] = useState(agentSlug === props.data.slug);
    const [showLoginPrompt, setShowLoginPrompt] = useState(false);

    const userData = props.userProfile;

    if (showModal) {
        window.history.pushState({}, `Khoj AI - Agent ${props.data.slug}`, `/agents?agent=${props.data.slug}`);
    }

    const stylingString = convertColorToClass(props.data.color);

    return (
        <Card className={`shadow-sm bg-gradient-to-b from-white 20% to-${props.data.color ? props.data.color : "gray"}-100/50 dark:from-[hsl(var(--background))] dark:to-${props.data.color ? props.data.color : "gray"}-950/50 rounded-xl hover:shadow-md`}>
            {
                showLoginPrompt &&
                <LoginPrompt
                    loginRedirectMessage={`Sign in to start chatting with ${props.data.name}`}
                    onOpenChange={setShowLoginPrompt} />
            }
            <CardHeader>
                <CardTitle>
                    {
                        !props.isMobileWidth ?
                            <Dialog
                                open={showModal}
                                onOpenChange={() => {
                                    setShowModal(!showModal);
                                    window.history.pushState({}, `Khoj AI - Agents`, `/agents`);
                                }}>
                                <DialogTrigger>
                                    <div className='flex items-center relative top-2'>
                                        {
                                            getIconFromIconName(props.data.icon, props.data.color) || <Image
                                                src={props.data.avatar}
                                                alt={props.data.name}
                                                width={50}
                                                height={50}
                                            />
                                        }
                                        {props.data.name}
                                    </div>
                                </DialogTrigger>
                                <div className="float-right">
                                {props.userProfile ? (
                                        <Button
                                            className={`bg-[hsl(var(--background))] w-14 h-14 rounded-xl border dark:border-neutral-700 shadow-sm hover:bg-stone-100`}
                                            onClick={() => openChat(props.data.slug, userData)}>
                                            <PaperPlaneTilt className='w-6 h-6' color={props.data.color} />
                                        </Button>
                                    ) : (
                                        <Button
                                            className={`bg-[hsl(var(--background))] w-14 h-14 rounded-xl border dark:border-neutral-700 shadow-sm`}
                                            onClick={() => setShowLoginPrompt(true)}>
                                            <PaperPlaneTilt className='w-6 h-6' color={props.data.color} />
                                        </Button>
                                    )}
                                </div>
                                <DialogContent className='whitespace-pre-line max-h-[80vh]'>
                                    <DialogHeader>
                                        <div className='flex items-center'>
                                            {
                                                getIconFromIconName(props.data.icon, props.data.color) || <Image
                                                    src={props.data.avatar}
                                                    alt={props.data.name}
                                                    width={32}
                                                    height={50}
                                                />
                                            }
                                            <p className="font-bold text-lg">{props.data.name}</p>
                                        </div>
                                    </DialogHeader>
                                    <div className="max-h-[60vh] overflow-y-scroll text-neutral-500 dark:text-white">
                                    {props.data.personality}
                                    </div>
                                    <DialogFooter>
                                        <Button
                                            className={`pt-6 pb-6 ${stylingString} bg-white dark:bg-[hsl(var(--background))] text-neutral-500 dark:text-white border-2 border-stone-100 shadow-sm rounded-xl hover:bg-stone-100`}
                                            onClick={() => {
                                                openChat(props.data.slug, userData);
                                                setShowModal(false);
                                            }}>
                                            <PaperPlaneTilt className='mr-2 w-6 h-6' color={props.data.color} />
                                            Start Chatting
                                        </Button>
                                    </DialogFooter>
                                </DialogContent>
                            </Dialog>
                            :
                            <Drawer
                                open={showModal}
                                onOpenChange={(open) => {
                                    setShowModal(open);
                                    window.history.pushState({}, `Khoj AI - Agents`, `/agents`);
                                }}>
                                <DrawerTrigger>
                                    <div className='flex items-center'>
                                        {
                                            getIconFromIconName(props.data.icon, props.data.color) || <Image
                                                src={props.data.avatar}
                                                alt={props.data.name}
                                                width={50}
                                                height={50}
                                            />
                                        }
                                        {props.data.name}
                                    </div>
                                </DrawerTrigger>
                                <DrawerContent className='whitespace-pre-line p-2'>
                                    <DrawerHeader>
                                        <DrawerTitle>{props.data.name}</DrawerTitle>
                                        <DrawerDescription>Full Prompt</DrawerDescription>
                                    </DrawerHeader>
                                    {props.data.personality}
                                    <DrawerFooter>
                                        <DrawerClose>
                                            Done
                                        </DrawerClose>
                                    </DrawerFooter>
                                </DrawerContent>
                            </Drawer>
                    }
                </CardTitle>
            </CardHeader>
            <CardContent>
                <div className={styles.agentPersonality}>
                    <button className={`${styles.infoButton} text-neutral-500 dark:text-white`} onClick={() => setShowModal(true)}>
                        <p>{props.data.personality}</p>
                    </button>
                </div>
            </CardContent>
        </Card>
    )
}

export default function Agents() {
    const { data, error } = useSWR<AgentData[]>('agents', agentsFetcher, { revalidateOnFocus: false });
    const authenticatedData = useAuthenticatedData();
    const [isMobileWidth, setIsMobileWidth] = useState(false);
    const [showLoginPrompt, setShowLoginPrompt] = useState(false);

    useEffect(() => {
        if (typeof window !== 'undefined') {
            setIsMobileWidth(window.innerWidth < 768);
        }

        window.addEventListener('resize', () => {
            setIsMobileWidth(window.innerWidth < 768);
        });
    }, []);

    if (error) {
        return (
            <main className={styles.main}>
                <div className={`${styles.titleBar} text-5xl`}>
                    Agents
                </div>
                <div className={styles.agentList}>
                    Error loading agents
                </div>
            </main>
        );
    }

    if (!data) {
        return (
            <main className={styles.main}>
                <div className={`${styles.titleBar} text-5xl`}>
                    Agents
                </div>
                <div className={styles.agentList}>
                    <InlineLoading /> booting up your agents
                </div>
            </main>
        );
    }

    return (
        <main className={`${styles.main} w-full ml-auto mr-auto`}>
            {/* <div className="h-full absolute left-0 top-0">
                <SidePanel
                    webSocketConnected={true}
                    conversationId={null}
                    uploadedFiles={[]}
                    isMobileWidth={isMobileWidth}
                />
            </div> */}
            {
                showLoginPrompt &&
                <LoginPrompt
                    loginRedirectMessage="Sign in to start chatting with a specialized agent"
                    onOpenChange={setShowLoginPrompt} />
            }
            <div className="w-7/12 ml-auto mr-auto">
                <h1 className="text-3xl">Agents</h1>
                <div className="pt-8 flex">
                    <Card className="pt-1 pb-1 bg-stone-100 dark:bg-[hsl(var(--background))]">
                        <CardContent>
                            <CardDescription className="flex flex-rows">
                                <Lightning className='w-4 h-4 mr-2 relative top-3' weight="fill" color="#a068f5" />
                                <p className="relative top-3">
                                    <strong className="text-black dark:text-white pr-2">How it works</strong>
                                    Use any of these specialized agents to tune your conversation to your needs.
                                </p>
                            </CardDescription>
                        </CardContent>
                    </Card>
                    <div className="ml-auto float-right">
                        <Button
                            className={`bg-[hsl(var(--background))] rounded-xl border dark:border-neutral-700 shadow-sm h-14`}
                        >
                        <Plus className='w-6 h-6' color='gray' />
                        <p className="text-black dark:text-white ml-2">
                        <strong>Create Agent</strong>
                        </p>
                        </Button>
                    </div>
                </div>
                <div className={`${styles.agentList}`}>
                    {data.map(agent => (
                        <AgentCard key={agent.slug} data={agent} userProfile={authenticatedData} isMobileWidth={isMobileWidth} />
                    ))}
                </div>
            </div>
        </main>
    );
}
