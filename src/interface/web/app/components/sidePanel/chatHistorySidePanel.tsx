'use client'

import styles from "./sidePanel.module.css";

import { Suspense, useEffect, useState } from "react";

import { UserProfile } from "@/app/common/auth";
import { Avatar, AvatarImage, AvatarFallback } from "@/components/ui/avatar";
import Link from "next/link";
import useSWR from "swr";

import {
    Collapsible,
    CollapsibleContent,
    CollapsibleTrigger,
} from "@/components/ui/collapsible";

import { InlineLoading } from "../loading/loading";

import {
    Dialog,
    DialogContent,
    DialogDescription,
    DialogFooter,
    DialogHeader,
    DialogTitle,
    DialogTrigger,
} from "@/components/ui/dialog";

import { ScrollArea } from "@/components/ui/scroll-area";

import { ArrowRight, ArrowLeft, ArrowDown, Spinner, Check } from "@phosphor-icons/react";

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

import {
    Popover,
    PopoverContent,
    PopoverTrigger,
} from "@/components/ui/popover";

import { Pencil, Trash, Share } from "@phosphor-icons/react";

import { Button, buttonVariants } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import { AlertDialog, AlertDialogAction, AlertDialogCancel, AlertDialogContent, AlertDialogDescription, AlertDialogFooter, AlertDialogHeader, AlertDialogTitle, AlertDialogTrigger } from "@/components/ui/alert-dialog";

// Define a fetcher function
const fetcher = (url: string) => fetch(url).then((res) => res.json());

interface GroupedChatHistory {
    [key: string]: ChatHistory[];
}

function renameConversation(conversationId: string, newTitle: string) {
    const editUrl = `/api/chat/title?client=web&conversation_id=${conversationId}&title=${newTitle}`;

    fetch(editUrl, {
        method: 'PATCH',
        headers: {
            'Content-Type': 'application/json',
        },
    })
        .then(response => response.json())
        .then(data => {
        })
        .catch(err => {
            console.error(err);
            return;
        });
}

function shareConversation(conversationId: string, setShareUrl: (url: string) => void) {
    const shareUrl = `/api/chat/share?client=web&conversation_id=${conversationId}`;

    fetch(shareUrl, {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
        },
    })
        .then(response => response.json())
        .then(data => {
            setShareUrl(data.url);
        })
        .catch(err => {
            console.error(err);
            return;
        });
}

function deleteConversation(conversationId: string) {
    const deleteUrl = `/api/chat/history?client=web&conversation_id=${conversationId}`;

    fetch(deleteUrl, {
        method: 'DELETE',
        headers: {
            'Content-Type': 'application/json',
        },
    })
        .then(response => response.json())
        .then(data => {
        })
        .catch(err => {
            console.error(err);
            return;
        });
}

function modifyFileFilterForConversation(
    conversationId: string | null,
    filename: string,
    setAddedFiles: (files: string[]) => void,
    mode: 'add' | 'remove') {

    if (!conversationId) {
        console.error("No conversation ID provided");
        return;
    }

    const method = mode === 'add' ? 'POST' : 'DELETE';

    const body = {
        conversation_id: conversationId,
        filename: filename,
    }
    const addUrl = `/api/chat/conversation/file-filters`;

    fetch(addUrl, {
        method: method,
        headers: {
            'Content-Type': 'application/json',
        },
        body: JSON.stringify(body),
    })
        .then(response => response.json())
        .then(data => {
            setAddedFiles(data);
        })
        .catch(err => {
            console.error(err);
            return;
        });
}


interface FilesMenuProps {
    conversationId: string | null;
}

function FilesMenu(props: FilesMenuProps) {
    // Use SWR to fetch files
    const { data: files, error } = useSWR(props.conversationId ? '/api/config/data/computer' : null, fetcher);
    const { data: selectedFiles, error: selectedFilesError } = useSWR(props.conversationId ? `/api/chat/conversation/file-filters/${props.conversationId}` : null, fetcher);
    const [isOpen, setIsOpen] = useState(false);
    const [searchInput, setSearchInput] = useState('');
    const [filteredFiles, setFilteredFiles] = useState<string[]>([]);
    const [addedFiles, setAddedFiles] = useState<string[]>([]);

    useEffect(() => {
        if (!files) return;
        if (searchInput === '') {
            setFilteredFiles(files);
        } else {
            let sortedFiles = files.filter((filename: string) => filename.toLowerCase().includes(searchInput.toLowerCase()));

            if (addedFiles) {
                sortedFiles = addedFiles.concat(filteredFiles.filter((filename: string) => !addedFiles.includes(filename)));
            }

            setFilteredFiles(sortedFiles);
        }
    }, [searchInput, files, addedFiles]);

    useEffect(() => {
        if (selectedFiles) {
            setAddedFiles(selectedFiles);
        }

    }, [selectedFiles]);

    if (!props.conversationId) return (<></>);

    if (error) return <div>Failed to load files</div>;
    if (selectedFilesError) return <div>Failed to load selected files</div>;
    if (!files) return <InlineLoading />;
    if (!selectedFiles) return <InlineLoading />;

    return (
        <>
            <Popover
                open={isOpen}
                onOpenChange={setIsOpen}>
                <PopoverTrigger asChild>
                    <div
                        className="w-auto bg-background border border-muted p-4 drop-shadow-sm rounded-2xl">
                        <div className="flex items-center justify-between space-x-4">
                            <h4 className="text-sm font-semibold">
                                Manage Files
                                <p>
                                    <span className="text-muted-foreground text-xs">Using {addedFiles.length == 0 ? files.length : addedFiles.length} files</span>
                                </p>
                            </h4>
                            <Button variant="ghost" size="sm" className="w-9 p-0">
                                {
                                    isOpen ?
                                        <ArrowDown className="h-4 w-4" />
                                        :
                                        <ArrowRight className="h-4 w-4" />

                                }
                                <span className="sr-only">Toggle</span>
                            </Button>
                        </div>
                    </div>
                </PopoverTrigger>
                <PopoverContent className="w-80 mx-2">
                    <Input
                        placeholder="Find file"
                        className="rounded-md border-none py-2 text-sm text-wrap break-words my-2 bg-accent text-accent-foreground"
                        value={searchInput}
                        onChange={(e) => setSearchInput(e.target.value)} />
                    {
                        filteredFiles.length === 0 && (
                            <div className="rounded-md border-none py-2 text-sm text-wrap break-words">
                                No files found
                            </div>
                        )
                    }
                    {
                        filteredFiles.map((filename: string) => (
                            addedFiles && addedFiles.includes(filename) ?
                                <Button
                                    variant={'ghost'}
                                    key={filename}
                                    className="rounded-md border-none py-2 text-sm text-wrap break-words bg-accent text-accent-foreground text-left"
                                    onClick={() => modifyFileFilterForConversation(props.conversationId, filename, setAddedFiles, 'remove')}>
                                    {filename}
                                    <Check className="h-4 w-4 ml-2" />
                                </Button>
                                :
                                <Button
                                    variant={'ghost'}
                                    key={filename}
                                    className="rounded-md border-none py-2 text-sm text-wrap break-words text-left"
                                    onClick={() => modifyFileFilterForConversation(props.conversationId, filename, setAddedFiles, 'add')}>
                                    {filename}
                                </Button>
                        ))
                    }
                </PopoverContent>
            </Popover>
        </>
    )

}

interface SessionsAndFilesProps {
    webSocketConnected?: boolean;
    setEnabled: (enabled: boolean) => void;
    subsetOrganizedData: GroupedChatHistory | null;
    organizedData: GroupedChatHistory | null;
    data: ChatHistory[] | null;
    userProfile: UserProfile | null;
    conversationId: string | null;
}

function SessionsAndFiles(props: SessionsAndFilesProps) {
    return (
        <>
            <div className={`${styles.expanded}`}>
                <button className={styles.button} onClick={() => props.setEnabled(false)}>
                    <ArrowLeft />
                </button>
            </div>
            <ScrollArea className="h-[40vh] w-[14rem]">
                <div className={styles.sessionsList}>
                    {props.subsetOrganizedData != null && Object.keys(props.subsetOrganizedData).map((agentName) => (
                        <div key={agentName} className={`my-4`}>
                            {/* <h3 className={`grid grid-flow-col auto-cols-max gap-2 my-4 font-bold text-sm`}>
                                {
                                    props.subsetOrganizedData &&
                                    <img src={props.subsetOrganizedData[agentName][0].agent_avatar} alt={agentName} width={24} height={24} />
                                }
                                {agentName}
                            </h3> */}
                            {props.subsetOrganizedData && props.subsetOrganizedData[agentName].map((chatHistory) => (
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
                (props.data && props.data.length > 5) && (
                    <ChatSessionsModal data={props.organizedData} />
                )
            }
            <FilesMenu conversationId={props.conversationId} />
            {props.userProfile &&
                <UserProfileComponent userProfile={props.userProfile} webSocketConnected={props.webSocketConnected} collapsed={false} />
            }</>
    )
}

interface ChatSessionActionMenuProps {
    conversationId: string;
}

function ChatSessionActionMenu(props: ChatSessionActionMenuProps) {
    const [renamedTitle, setRenamedTitle] = useState('');
    const [isRenaming, setIsRenaming] = useState(false);
    const [isSharing, setIsSharing] = useState(false);
    const [isDeleting, setIsDeleting] = useState(false);
    const [shareUrl, setShareUrl] = useState('');
    const [showShareUrl, setShowShareUrl] = useState(false);

    const [isOpen, setIsOpen] = useState(false);

    useEffect(() => {
        if (isSharing) {
            shareConversation(props.conversationId, setShareUrl);
            setShowShareUrl(true);
            setIsSharing(false);
        }
    }, [isSharing]);

    if (isRenaming) {
        return (
            <Dialog
                open={isRenaming}
                onOpenChange={(open) => setIsRenaming(open)}>
                <DialogContent>
                    <DialogHeader>
                        <DialogTitle>Set a new title for the conversation</DialogTitle>
                        <DialogDescription>
                            This will help you identify the conversation easily, and also help you search for it later.
                        </DialogDescription>
                        <Input
                            value={renamedTitle}
                            onChange={(e) => setRenamedTitle(e.target.value)}
                        />
                    </DialogHeader>
                    <DialogFooter>
                        <Button
                            onClick={() => {
                                renameConversation(props.conversationId, renamedTitle);
                                setIsRenaming(false);
                            }}
                            type="submit">Rename</Button>
                    </DialogFooter>
                </DialogContent>
            </Dialog>
        )
    }

    if (isSharing || showShareUrl) {
        if (shareUrl) {
            navigator.clipboard.writeText(shareUrl);
        }
        return (
            <Dialog
                open={isSharing || showShareUrl}
                onOpenChange={(open) => {
                    setShowShareUrl(open)
                    setIsSharing(open)
                }}>
                <DialogContent>
                    <DialogHeader>
                        <DialogTitle>Conversation Share URL</DialogTitle>
                        <DialogDescription>
                            Sharing this chat session will allow anyone with a link to view the conversation.
                            <Input
                                className="w-full bg-accent text-accent-foreground rounded-md p-2 mt-2"
                                value={shareUrl}
                                readOnly={true}
                            />
                        </DialogDescription>
                    </DialogHeader>
                    <DialogFooter>
                        {
                            !showShareUrl &&
                            <Button
                                onClick={() => {
                                    shareConversation(props.conversationId, setShareUrl);
                                    setShowShareUrl(true);
                                }}
                                className="bg-orange-500"
                                disabled><Spinner className="mr-2 h-4 w-4 animate-spin" />Sharing</Button>
                        }
                        {
                            showShareUrl &&
                            <Button
                                onClick={() => {
                                    navigator.clipboard.writeText(shareUrl);
                                }}
                                variant={'default'}>Copy</Button>
                        }
                    </DialogFooter>
                </DialogContent>
            </Dialog>
        )
    }

    if (isDeleting) {
        return (
            <AlertDialog
                open={isDeleting}
                onOpenChange={(open) => setIsDeleting(open)}>
                <AlertDialogContent>
                    <AlertDialogHeader>
                        <AlertDialogTitle>Delete Conversation</AlertDialogTitle>
                        <AlertDialogDescription>
                            Are you sure you want to delete this conversation? This action cannot be undone.
                        </AlertDialogDescription>
                    </AlertDialogHeader>
                    <AlertDialogFooter>
                        <AlertDialogCancel>Cancel</AlertDialogCancel>
                        <AlertDialogAction
                            onClick={() => {
                                deleteConversation(props.conversationId);
                                setIsDeleting(false);
                            }}
                            className="bg-rose-500 hover:bg-rose-600">Delete</AlertDialogAction>
                    </AlertDialogFooter>
                </AlertDialogContent>
            </AlertDialog>
        )
    }

    return (
        <DropdownMenu
            onOpenChange={(open) => setIsOpen(open)}
            open={isOpen}>
            <DropdownMenuTrigger>:</DropdownMenuTrigger>
            <DropdownMenuContent>
                <DropdownMenuItem>
                    <Button className="p-0 text-sm h-auto" variant={'ghost'} onClick={() => setIsRenaming(true)}>
                        <Pencil className="mr-2 h-4 w-4" />Rename
                    </Button>
                </DropdownMenuItem>
                <DropdownMenuItem>
                    <Button className="p-0 text-sm h-auto" variant={'ghost'} onClick={() => setIsSharing(true)}>
                        <Share className="mr-2 h-4 w-4" />Share
                    </Button>
                </DropdownMenuItem>
                <DropdownMenuItem>
                    <Button className="p-0 text-sm h-auto text-rose-300 hover:text-rose-400" variant={'ghost'} onClick={() => setIsDeleting(true)}>
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

function ChatSessionsModal({ data }: ChatSessionsModalProps) {
    return (
        <Dialog>
            <DialogTrigger
                className="flex text-left text-medium text-gray-500 hover:text-gray-900 cursor-pointer my-4 text-sm">
                Show All
            </DialogTrigger>
            <DialogContent>
                <DialogHeader>
                    <DialogTitle>All Conversations</DialogTitle>
                    <DialogDescription>
                        <ScrollArea className="h-[500px] w-[450px] p-4">
                            {data && Object.keys(data).map((agentName) => (
                                <div key={agentName}>
                                    <div className={`grid grid-flow-col auto-cols-max gap-2`}>
                                        <img src={data[agentName][0].agent_avatar} alt={agentName} width={24} height={24} />
                                        {agentName}
                                    </div>
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
                <Avatar className="h-7 w-7">
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
        <Link href="/config" target="_blank" rel="noopener noreferrer">
                <Avatar>
                    <AvatarImage src={props.userProfile.photo} alt="user profile" />
                    <AvatarFallback>
                        {props.userProfile.username[0]}
                    </AvatarFallback>
                </Avatar>
        </Link>
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
    conversationId: string | null;
}


export default function SidePanel(props: SidePanelProps) {

    const [data, setData] = useState<ChatHistory[] | null>(null);
    const [organizedData, setOrganizedData] = useState<GroupedChatHistory | null>(null);
    const [subsetOrganizedData, setSubsetOrganizedData] = useState<GroupedChatHistory | null>(null);
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
                        <SessionsAndFiles
                            webSocketConnected={props.webSocketConnected}
                            setEnabled={setEnabled}
                            subsetOrganizedData={subsetOrganizedData}
                            organizedData={organizedData}
                            data={data}
                            userProfile={userProfile}
                            conversationId={props.conversationId}
                        />
                    </div>
                    :
                    <div>
                        <div className={`${styles.collapsed}`}>
                            <button className={styles.button} onClick={() => setEnabled(true)}>
                                <ArrowRight />
                            </button>
                            {userProfile &&
                                <UserProfileComponent userProfile={userProfile} webSocketConnected={props.webSocketConnected} collapsed={true} />
                            }
                        </div>
                    </div>
            }

        </div>
    );
}
