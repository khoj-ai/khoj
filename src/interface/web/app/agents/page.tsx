'use client'

import styles from './agents.module.css';

import Image from 'next/image';
import useSWR from 'swr';

import { useEffect, useState } from 'react';

import { useAuthenticatedData, UserProfile } from '../common/auth';
import { Button } from '@/components/ui/button';
import {
    Tooltip,
    TooltipContent,
    TooltipProvider,
    TooltipTrigger,
} from "@/components/ui/tooltip"

import {
    PaperPlaneTilt,
    Lightning,
    Plus,
} from "@phosphor-icons/react";

import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from '@/components/ui/card';
import { Dialog, DialogContent, DialogFooter, DialogHeader, DialogTrigger } from '@/components/ui/dialog';
import { Drawer, DrawerClose, DrawerContent, DrawerDescription, DrawerFooter, DrawerHeader, DrawerTitle, DrawerTrigger } from '@/components/ui/drawer';
import LoginPrompt from '../components/loginPrompt/loginPrompt';
import { InlineLoading } from '../components/loading/loading';
import SidePanel from '../components/sidePanel/chatHistorySidePanel';
import NavMenu from '../components/navMenu/navMenu';
import { getIconFromIconName } from '../common/iconUtils';
import { convertColorToTextClass } from '../common/colorUtils';
import { Alert, AlertDescription } from '@/components/ui/alert';

export interface AgentData {
    slug: string;
    avatar: string;
    name: string;
    persona: string;
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


function AgentCard(props: AgentCardProps) {
    const searchParams = new URLSearchParams(window.location.search);
    const agentSlug = searchParams.get('agent');
    const [showModal, setShowModal] = useState(agentSlug === props.data.slug);
    const [showLoginPrompt, setShowLoginPrompt] = useState(false);

    const userData = props.userProfile;

    if (showModal) {
        window.history.pushState({}, `Khoj AI - Agent ${props.data.slug}`, `/agents?agent=${props.data.slug}`);
    }

    const stylingString = convertColorToTextClass(props.data.color);

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
                                            className={`bg-[hsl(var(--background))] w-14 h-14 rounded-xl border dark:border-neutral-700 shadow-sm hover:bg-stone-100 dark:hover:bg-neutral-900`}
                                            onClick={() => openChat(props.data.slug, userData)}>
                                            <PaperPlaneTilt className={`w-6 h-6 ${convertColorToTextClass(props.data.color)}`} />
                                        </Button>
                                    ) : (
                                        <Button
                                            className={`bg-[hsl(var(--background))] w-14 h-14 rounded-xl border dark:border-neutral-700 shadow-sm hover:bg-stone-100 dark:hover:bg-neutral-900`}
                                            onClick={() => setShowLoginPrompt(true)}>
                                            <PaperPlaneTilt className={`w-6 h-6 ${convertColorToTextClass(props.data.color)}`} />
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
                                        {props.data.persona}
                                    </div>
                                    <DialogFooter>
                                        <Button
                                            className={`pt-6 pb-6 ${stylingString} bg-white dark:bg-[hsl(var(--background))] text-neutral-500 dark:text-white border-2 border-stone-100 shadow-sm rounded-xl hover:bg-stone-100 dark:hover:bg-neutral-900 dark:border-neutral-700`}
                                            onClick={() => {
                                                openChat(props.data.slug, userData);
                                                setShowModal(false);
                                            }}>
                                            <PaperPlaneTilt className={`w-6 h-6 m-2 ${convertColorToTextClass(props.data.color)}`} />
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
                                <div className="float-right">
                                    {props.userProfile ? (
                                        <Button
                                            className={`bg-[hsl(var(--background))] w-14 h-14 rounded-xl border dark:border-neutral-700 shadow-sm hover:bg-stone-100`}
                                            onClick={() => openChat(props.data.slug, userData)}>
                                            <PaperPlaneTilt className={`w-6 h-6 ${convertColorToTextClass(props.data.color)}`} />
                                        </Button>
                                    ) : (
                                        <Button
                                            className={`bg-[hsl(var(--background))] w-14 h-14 rounded-xl border dark:border-neutral-700 shadow-sm`}
                                            onClick={() => setShowLoginPrompt(true)}>
                                            <PaperPlaneTilt className={`w-6 h-6 ${convertColorToTextClass(props.data.color)}`} />
                                        </Button>
                                    )}
                                </div>
                                <DrawerContent className='whitespace-pre-line p-2'>
                                    <DrawerHeader>
                                        <DrawerTitle>{props.data.name}</DrawerTitle>
                                        <DrawerDescription>Full Prompt</DrawerDescription>
                                    </DrawerHeader>
                                    {props.data.persona}
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
                        <p>{props.data.persona}</p>
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
                <div className={styles.agentList}>
                    <InlineLoading /> booting up your agents
                </div>
            </main>
        );
    }

    return (
        <main className={`${styles.main} w-full mx-auto`}>
            {
                showLoginPrompt &&
                <LoginPrompt
                    loginRedirectMessage="Sign in to start chatting with a specialized agent"
                    onOpenChange={setShowLoginPrompt} />
            }
            <div className={`${styles.pageLayout} w-full mx-auto`}>
                <div className={`${styles.sidePanel} top-0`}>
                    <SidePanel
                        conversationId={null}
                        uploadedFiles={[]}
                        isMobileWidth={isMobileWidth}
                    />
                </div>
                <div className={`mx-auto ${isMobileWidth ? "w-11/12" : "w-1/2"} pt-4`}>
                    <div className={`pt-6 md:pt-8 flex justify-between align-middle w-full`}>
                        <h1 className="text-3xl">Agents</h1>
                        <div className="ml-auto float-right border p-2 pt-3 rounded-xl font-bold hover:bg-stone-100 dark:hover:bg-neutral-900">
                            <TooltipProvider>
                                <Tooltip>
                                    <TooltipTrigger>
                                        <div className="flex flex-row">
                                            <Plus className='pr-2 w-6 h-6' />
                                            <p className="pr-2">Create Agent</p>
                                        </div>
                                    </TooltipTrigger>
                                    <TooltipContent>
                                        <p>Coming Soon!</p>
                                    </TooltipContent>
                                </Tooltip>
                            </TooltipProvider>
                        </div>
                    </div>
                    <Alert className='bg-secondary border-none my-4'>
                        <AlertDescription>
                            <Lightning weight={'fill'} className='h-4 w-4 text-purple-400 inline' />
                            <span className='font-bold'>How it works</span> Use any of these specialized personas to tune your conversation to your needs.
                        </AlertDescription>
                    </Alert>
                    <div className={`${styles.agentList}`}>
                        {data.map(agent => (
                            <AgentCard key={agent.slug} data={agent} userProfile={authenticatedData} isMobileWidth={isMobileWidth} />
                        ))}
                    </div>
                </div>
            </div>
        </main>
    );
}
