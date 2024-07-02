'use client'

import styles from "./sidePanel.module.css";

import { useEffect, useState } from "react";

import { UserProfile } from "@/app/common/auth";
import { Avatar, AvatarImage, AvatarFallback } from "@/components/ui/avatar";
import Link from "next/link";
import useSWR from "swr";

import {
    Dialog,
    DialogContent,
    DialogDescription,
    DialogHeader,
    DialogTitle,
    DialogTrigger,
} from "@/components/ui/dialog";

import { ScrollArea } from "@/components/ui/scroll-area";

interface ChatHistory {
    conversation_id: string;
    slug: string;
    agent_name: string;
    agent_avatar: string;
    compressed: boolean;
}

import {
    DropdownMenu,
    DropdownMenuContent,
    DropdownMenuItem,
    DropdownMenuTrigger,
} from "@/components/ui/dropdown-menu";

import { Pencil, Trash, Share } from "@phosphor-icons/react";

import { Button } from "@/components/ui/button";

interface GroupedChatHistory {
    [key: string]: ChatHistory[];
}

function renameConversation(conversationId: string, newTitle: string) {
    const editUrl = `/api/chat/title?client=web&conversation_id=${conversationId}&title=${newTitle}`;

    fetch(editUrl, {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
        },
    })
        .then(response => response.json())
        .then(data => {
            console.log(data);
        })
        .catch(err => {
            console.error(err);
            return;
        });
}

function shareConversation(conversationId: string) {
    const shareUrl = `/api/chat/share?client=web&conversation_id=${conversationId}`;

    fetch(shareUrl, {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
        },
    })
        .then(response => response.json())
        .then(data => {
            console.log(data);
        })
        .catch(err => {
            console.error(err);
            return;
        });
}

function deleteConversation(conversationId: string) {
    const deleteUrl = `/api/chat/delete?client=web&conversation_id=${conversationId}`;

    fetch(deleteUrl, {
        method: 'DELETE',
        headers: {
            'Content-Type': 'application/json',
        },
    })
        .then(response => response.json())
        .then(data => {
            console.log(data);
        })
        .catch(err => {
            console.error(err);
            return;
        });
}

interface ChatSessionActionMenuProps {
    conversationId: string;
}

function ChatSessionActionMenu(props: ChatSessionActionMenuProps) {

    return (
        <DropdownMenu>
            <DropdownMenuTrigger>:</DropdownMenuTrigger>
            <DropdownMenuContent>
                <DropdownMenuItem>
                    <Button className="p-0 text-sm h-auto" variant={'ghost'} onClick={() => renameConversation(props.conversationId, 'New Title')}>
                        <Pencil className="mr-2 h-4 w-4" />Rename
                    </Button>
                </DropdownMenuItem>
                <DropdownMenuItem>
                    <Button className="p-0 text-sm h-auto" variant={'ghost'} onClick={() => shareConversation(props.conversationId)}>
                        <Share className="mr-2 h-4 w-4" />Share
                    </Button>
                </DropdownMenuItem>
                <DropdownMenuItem>
                    <Button className="p-0 text-sm h-auto text-rose-300 hover:text-rose-400" variant={'ghost'} onClick={() => deleteConversation(props.conversationId)}>
                        <Trash className="mr-2 h-4 w-4" />Delete
                    </Button>
                </DropdownMenuItem>
            </DropdownMenuContent>
        </DropdownMenu>
    )
}

function ChatSession(props: ChatHistory) {
    const [isHovered, setIsHovered] = useState(false);

    return (
        <div
            onMouseEnter={() => setIsHovered(true)}
            onMouseLeave={() => setIsHovered(false)}
            key={props.conversation_id}
            className={`${styles.session} ${props.compressed ? styles.compressed : '!max-w-full'} ${isHovered ? `${styles.sessionHover}` : ''}`}>
            <Link href={`/chat?conversationId=${props.conversation_id}`}>
                <p className={styles.session}>{props.slug || "New Conversation 🌱"}</p>
            </Link>
            <ChatSessionActionMenu conversationId={props.conversation_id} />
        </div>
    );
}

interface ChatSessionsModalProps {
    data: GroupedChatHistory | null;
}


// function ConversationList()

function ChatSessionsModal({ data }: ChatSessionsModalProps) {
    return (
        <Dialog>
            <DialogTrigger
                className="flex text-left text-medium text-gray-500 hover:text-gray-900 cursor-pointer">
                Show All
            </DialogTrigger>
            <DialogContent>
                <DialogHeader>
                    <DialogTitle>All Conversations</DialogTitle>
                    <DialogDescription>
                        <ScrollArea className="h-[500px] w-[450px] rounded-md border p-4">
                            {data && Object.keys(data).map((agentName) => (
                                <div key={agentName}>
                                    <h3 className={`grid grid-flow-col auto-cols-max gap-2`}>
                                        <img src={data[agentName][0].agent_avatar} alt={agentName} width={24} height={24} />
                                        {agentName}
                                    </h3>
                                    {data[agentName].map((chatHistory) => (
                                        <ChatSession
                                            compressed={false}
                                            key={chatHistory.conversation_id}
                                            conversation_id={chatHistory.conversation_id}
                                            slug={chatHistory.slug}
                                            agent_avatar={chatHistory.agent_avatar}
                                            agent_name={chatHistory.agent_name} />
                                    ))}
                                </div>
                            ))}
                        </ScrollArea>
                    </DialogDescription>
                </DialogHeader>
            </DialogContent>
        </Dialog>
    );
}

interface UserProfileProps {
    userProfile: UserProfile;
    webSocketConnected?: boolean;
    collapsed: boolean;
}

function UserProfileComponent(props: UserProfileProps) {
    if (props.collapsed) {
        return (
            <div className={styles.profile}>
                <Avatar>
                    <AvatarImage src={props.userProfile.photo} alt="user profile" />
                    <AvatarFallback>
                        {props.userProfile.username[0]}
                    </AvatarFallback>
                </Avatar>
            </div>
        );
    }

    return (
        <div className={styles.profile}>
            <Avatar>
                <AvatarImage src={props.userProfile.photo} alt="user profile" />
                <AvatarFallback>
                    {props.userProfile.username[0]}
                </AvatarFallback>
            </Avatar>
            <div className={styles.profileDetails}>
                <p>{props.userProfile?.username}</p>
                {/* Connected Indicator */}
                <div className="flex gap-2 items-center">
                    <div className={`inline-flex h-4 w-4 rounded-full opacity-75 ${props.webSocketConnected ? 'bg-green-500' : 'bg-rose-500'}`}></div>
                    <p className="text-muted-foreground text-sm">
                        {props.webSocketConnected ? "Connected" : "Disconnected"}
                    </p>
                </div>
            </div>
        </div>
    );

}

const fetchChatHistory = async (url: string) => {
    const response = await fetch(url, {
        method: 'GET',
        headers: {
            'Content-Type': 'application/json',
        },
    });
    return response.json();
};

export const useChatHistoryRecentFetchRequest = (url: string) => {
    const { data, error } = useSWR<ChatHistory[]>(url, fetchChatHistory);

    return {
        data,
        isLoading: !error && !data,
        isError: error,
    };
};

interface SidePanelProps {
    webSocketConnected?: boolean;
}


export default function SidePanel(props: SidePanelProps) {

    const [data, setData] = useState<ChatHistory[] | null>(null);
    const [organizedData, setOrganizedData] = useState<GroupedChatHistory | null>(null);
    const [subsetOrganizedData, setSubsetOrganizedData] = useState<GroupedChatHistory | null>(null);
    const [isLoading, setLoading] = useState(true)
    const [enabled, setEnabled] = useState(false);

    const [userProfile, setUserProfile] = useState<UserProfile | null>(null);

    const { data: chatHistory } = useChatHistoryRecentFetchRequest('/api/chat/sessions');

    useEffect(() => {
        if (chatHistory) {
            setData(chatHistory);

            const groupedData: GroupedChatHistory = {};
            const subsetOrganizedData: GroupedChatHistory = {};
            let numAdded = 0;
            chatHistory.forEach((chatHistory) => {
                if (!groupedData[chatHistory.agent_name]) {
                    groupedData[chatHistory.agent_name] = [];
                }
                groupedData[chatHistory.agent_name].push(chatHistory);

                // Add to subsetOrganizedData if less than 8
                if (numAdded < 8) {
                    if (!subsetOrganizedData[chatHistory.agent_name]) {
                        subsetOrganizedData[chatHistory.agent_name] = [];
                    }
                    subsetOrganizedData[chatHistory.agent_name].push(chatHistory);
                    numAdded++;
                }
            });
            setSubsetOrganizedData(subsetOrganizedData);
            setOrganizedData(groupedData);
        }
    }, [chatHistory]);

    useEffect(() => {

        fetch('/api/v1/user', { method: 'GET' })
            .then(response => response.json())
            .then((data: UserProfile) => {
                setUserProfile(data);
            })
            .catch(err => {
                console.error(err);
                return;
            });
    }, []);

    return (
        <div className={`${styles.panel}`}>
            {
                enabled ?
                    <div className={`${styles.panelWrapper}`}>
                        <div className={`${styles.expanded}`}>
                            <button className={styles.button} onClick={() => setEnabled(false)}>
                                {/* Push Close Icon */}
                                <svg fill="#000000" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><g id="SVGRepo_bgCarrier" strokeWidth="0"></g><g id="SVGRepo_tracerCarrier" strokeLinecap="round" strokeLinejoin="round"></g><g id="SVGRepo_iconCarrier"> <path d="M8.70710678,12 L19.5,12 C19.7761424,12 20,12.2238576 20,12.5 C20,12.7761424 19.7761424,13 19.5,13 L8.70710678,13 L11.8535534,16.1464466 C12.0488155,16.3417088 12.0488155,16.6582912 11.8535534,16.8535534 C11.6582912,17.0488155 11.3417088,17.0488155 11.1464466,16.8535534 L7.14644661,12.8535534 C6.95118446,12.6582912 6.95118446,12.3417088 7.14644661,12.1464466 L11.1464466,8.14644661 C11.3417088,7.95118446 11.6582912,7.95118446 11.8535534,8.14644661 C12.0488155,8.34170876 12.0488155,8.65829124 11.8535534,8.85355339 L8.70710678,12 L8.70710678,12 Z M4,5.5 C4,5.22385763 4.22385763,5 4.5,5 C4.77614237,5 5,5.22385763 5,5.5 L5,19.5 C5,19.7761424 4.77614237,20 4.5,20 C4.22385763,20 4,19.7761424 4,19.5 L4,5.5 Z"></path> </g></svg>
                            </button>
                        </div>
                        <ScrollArea className="h-[40vh] w-[14rem]">
                            <div className={styles.sessionsList}>
                                {subsetOrganizedData && Object.keys(subsetOrganizedData).map((agentName) => (
                                    <div key={agentName} className={`my-4`}>
                                        <h3 className={`grid grid-flow-col auto-cols-max gap-2 my-4 font-bold text-sm`}>
                                            <img src={subsetOrganizedData[agentName][0].agent_avatar} alt={agentName} width={24} height={24} />
                                            {agentName}
                                        </h3>
                                        {subsetOrganizedData[agentName].map((chatHistory) => (
                                            <ChatSession
                                                compressed={true}
                                                key={chatHistory.conversation_id}
                                                conversation_id={chatHistory.conversation_id}
                                                slug={chatHistory.slug}
                                                agent_avatar={chatHistory.agent_avatar}
                                                agent_name={chatHistory.agent_name} />
                                        ))}
                                    </div>
                                ))}
                            </div>
                        </ScrollArea>
                        {
                            (data && data.length > 5) && (
                                <ChatSessionsModal data={organizedData} />
                            )
                        }
                        {userProfile &&
                            <UserProfileComponent userProfile={userProfile} webSocketConnected={props.webSocketConnected} collapsed={false} />
                        }
                    </div>
                    :
                    <div>
                        <div className={`${styles.collapsed}`}>
                            { userProfile &&
                                <UserProfileComponent userProfile={userProfile} webSocketConnected={props.webSocketConnected} collapsed={true} />
                            }
                            <button className={styles.button} onClick={() => setEnabled(true)}>
                                {/* Pull Open Icon */}
                                <svg fill="#000000" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><g id="SVGRepo_bgCarrier" strokeWidth="0"></g><g id="SVGRepo_tracerCarrier" strokeLinecap="round" strokeLinejoin="round"></g><g id="SVGRepo_iconCarrier"> <path d="M15.2928932,12 L12.1464466,8.85355339 C11.9511845,8.65829124 11.9511845,8.34170876 12.1464466,8.14644661 C12.3417088,7.95118446 12.6582912,7.95118446 12.8535534,8.14644661 L16.8535534,12.1464466 C17.0488155,12.3417088 17.0488155,12.6582912 16.8535534,12.8535534 L12.8535534,16.8535534 C12.6582912,17.0488155 12.3417088,17.0488155 12.1464466,16.8535534 C11.9511845,16.6582912 11.9511845,16.3417088 12.1464466,16.1464466 L15.2928932,13 L4.5,13 C4.22385763,13 4,12.7761424 4,12.5 C4,12.2238576 4.22385763,12 4.5,12 L15.2928932,12 Z M19,5.5 C19,5.22385763 19.2238576,5 19.5,5 C19.7761424,5 20,5.22385763 20,5.5 L20,19.5 C20,19.7761424 19.7761424,20 19.5,20 C19.2238576,20 19,19.7761424 19,19.5 L19,5.5 Z"></path> </g></svg>
                            </button>
                        </div>
                    </div>
            }

        </div>
    );
}
