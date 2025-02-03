"use client";

import styles from "./sidePanel.module.css";

import { useEffect, useMemo, useState } from "react";

import { mutate } from "swr";

import { UserProfile, useAuthenticatedData } from "@/app/common/auth";
import Link from "next/link";
import useSWR from "swr";

import {
    Command,
    CommandEmpty,
    CommandGroup,
    CommandInput,
    CommandItem,
    CommandList,
} from "@/components/ui/command";

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

import {
    Drawer,
    DrawerClose,
    DrawerContent,
    DrawerDescription,
    DrawerFooter,
    DrawerHeader,
    DrawerTitle,
    DrawerTrigger,
} from "@/components/ui/drawer";

import { ScrollArea } from "@/components/ui/scroll-area";

import {
    ArrowRight,
    ArrowDown,
    Spinner,
    Check,
    FolderPlus,
    DotsThreeVertical,
    FunnelSimple,
    ChatsCircle,
    Info,
} from "@phosphor-icons/react";

interface ChatHistory {
    conversation_id: string;
    slug: string;
    agent_name: string;
    agent_icon: string;
    agent_color: string;
    agent_is_hidden: boolean;
    compressed: boolean;
    created: string;
    updated: string;
}

import {
    DropdownMenu,
    DropdownMenuCheckboxItem,
    DropdownMenuContent,
    DropdownMenuItem,
    DropdownMenuLabel,
    DropdownMenuSeparator,
    DropdownMenuTrigger,
} from "@/components/ui/dropdown-menu";

import { Popover, PopoverContent, PopoverTrigger } from "@/components/ui/popover";

import { Pencil, Trash, Share } from "@phosphor-icons/react";

import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import {
    AlertDialog,
    AlertDialogAction,
    AlertDialogCancel,
    AlertDialogContent,
    AlertDialogDescription,
    AlertDialogFooter,
    AlertDialogHeader,
    AlertDialogTitle,
} from "@/components/ui/alert-dialog";
import { modifyFileFilterForConversation } from "@/app/common/chatFunctions";
import { ScrollAreaScrollbar } from "@radix-ui/react-scroll-area";
import { getIconFromIconName } from "@/app/common/iconUtils";
import {
    SidebarGroup,
    SidebarGroupLabel,
    SidebarMenuAction,
    SidebarMenuButton,
    SidebarMenuItem,
    SidebarMenuSkeleton,
} from "@/components/ui/sidebar";

// Define a fetcher function
const fetcher = (url: string) =>
    fetch(url).then((res) => {
        if (!res.ok) throw new Error(`Failed to call API at ${url} with error ${res.statusText}`);
        return res.json();
    });

interface GroupedChatHistory {
    [key: string]: ChatHistory[];
}

function renameConversation(conversationId: string, newTitle: string) {
    const editUrl = `/api/chat/title?client=web&conversation_id=${conversationId}&title=${newTitle}`;

    fetch(editUrl, {
        method: "PATCH",
        headers: {
            "Content-Type": "application/json",
        },
    })
        .then((response) => response.json())
        .then((data) => { })
        .catch((err) => {
            console.error(err);
            return;
        });
}

function shareConversation(conversationId: string, setShareUrl: (url: string) => void) {
    const shareUrl = `/api/chat/share?client=web&conversation_id=${conversationId}`;

    fetch(shareUrl, {
        method: "POST",
        headers: {
            "Content-Type": "application/json",
        },
    })
        .then((response) => response.json())
        .then((data) => {
            setShareUrl(data.url);
        })
        .catch((err) => {
            console.error(err);
            return;
        });
}

function deleteConversation(conversationId: string) {
    const deleteUrl = `/api/chat/history?client=web&conversation_id=${conversationId}`;

    fetch(deleteUrl, {
        method: "DELETE",
        headers: {
            "Content-Type": "application/json",
        },
    })
        .then((response) => {
            response.json();
            mutate("/api/chat/sessions");
        })
        .then((data) => { })
        .catch((err) => {
            console.error(err);
            return;
        });
}

interface FilesMenuProps {
    conversationId: string | null;
    uploadedFiles: string[];
    isMobileWidth: boolean;
}

export function FilesMenu(props: FilesMenuProps) {
    // Use SWR to fetch files
    const { data: files, error } = useSWR<string[]>("/api/content/computer", fetcher);
    const { data: selectedFiles, error: selectedFilesError } = useSWR(
        props.conversationId ? `/api/chat/conversation/file-filters/${props.conversationId}` : null,
        fetcher,
    );
    const [isOpen, setIsOpen] = useState(false);
    const [unfilteredFiles, setUnfilteredFiles] = useState<string[]>([]);
    const [addedFiles, setAddedFiles] = useState<string[]>([]);
    const usingConversationContext = props.conversationId !== null;

    useEffect(() => {
        if (!files) return;

        let sortedUniqueFiles = Array.from(new Set(files)).sort();

        if (Array.isArray(addedFiles)) {
            sortedUniqueFiles = addedFiles.concat(
                sortedUniqueFiles.filter((filename: string) => !addedFiles.includes(filename)),
            );
        }

        setUnfilteredFiles(sortedUniqueFiles);
    }, [files, addedFiles]);

    useEffect(() => {
        for (const file of props.uploadedFiles) {
            setAddedFiles((addedFiles) => [...addedFiles, file]);
        }
    }, [props.uploadedFiles]);

    useEffect(() => {
        if (Array.isArray(selectedFiles)) {
            setAddedFiles(selectedFiles);
        } else {
            setAddedFiles([]);
        }
    }, [selectedFiles]);

    const removeAllFiles = () => {
        modifyFileFilterForConversation(props.conversationId, addedFiles, setAddedFiles, "remove");
    };

    const addAllFiles = () => {
        modifyFileFilterForConversation(
            props.conversationId,
            unfilteredFiles,
            setAddedFiles,
            "add",
        );
    };

    if (error || selectedFilesError) {
        return (
            <div className="w-auto bg-background border border-muted p-4 drop-shadow-sm rounded-2xl">
                <div className="flex items-center justify-between space-x-4">
                    <h4 className="text-sm font-semibold">
                        Context
                        <p>
                            <span className="text-muted-foreground text-xs">
                                {
                                    error ? "Failed to load files" : "Failed to load selected files"
                                }
                            </span>
                        </p>
                    </h4>
                    <Button variant="ghost" size="sm" className="w-9 p-0">
                        <Info className="h-4 w-4" />
                        <span className="sr-only">Error Info</span>
                    </Button>
                </div>
            </div>
        )
    }

    if (!files) return <InlineLoading />;
    if (!selectedFiles && props.conversationId) return <InlineLoading />;

    const FilesMenuCommandBox = () => {
        return (
            <Command>
                <CommandInput placeholder="Find file" />
                <CommandList>
                    <CommandEmpty>
                        No results found. <Link href="/search">Try advanced search</Link>.
                    </CommandEmpty>
                    {usingConversationContext && (
                        <CommandGroup heading="Quick">
                            <CommandItem
                                onSelect={() => {
                                    removeAllFiles();
                                }}
                            >
                                <Trash className="h-4 w-4 mr-2" />
                                <span>Clear all</span>
                            </CommandItem>
                            <CommandItem
                                onSelect={() => {
                                    addAllFiles();
                                }}
                            >
                                <FolderPlus className="h-4 w-4 mr-2" />
                                <span>Select all</span>
                            </CommandItem>
                        </CommandGroup>
                    )}
                    <CommandGroup heading="Manage files">
                        <CommandItem>
                            <Link href="/search">Settings</Link>
                        </CommandItem>
                    </CommandGroup>
                    <CommandGroup
                        heading={`${usingConversationContext ? "Configure files" : "Synced files"}`}
                    >
                        {addedFiles.length == 0 && (
                            <CommandItem>
                                <Link href="/search">Upload documents</Link>
                            </CommandItem>
                        )}
                        {unfilteredFiles.map((filename: string) =>
                            Array.isArray(addedFiles) && addedFiles.includes(filename) ? (
                                <CommandItem
                                    key={filename}
                                    value={filename}
                                    className="bg-accent text-accent-foreground mb-1"
                                    onSelect={(value) => {
                                        if (!usingConversationContext) return;

                                        modifyFileFilterForConversation(
                                            props.conversationId,
                                            [value],
                                            setAddedFiles,
                                            "remove",
                                        );
                                    }}
                                >
                                    <Check className="h-4 w-4 mr-2" />
                                    <span className="break-all">{filename}</span>
                                </CommandItem>
                            ) : (
                                <CommandItem
                                    key={filename}
                                    className="mb-1"
                                    value={filename}
                                    onSelect={(value) => {
                                        if (!usingConversationContext) return;

                                        modifyFileFilterForConversation(
                                            props.conversationId,
                                            [value],
                                            setAddedFiles,
                                            "add",
                                        );
                                    }}
                                >
                                    <span className="break-all">{filename}</span>
                                </CommandItem>
                            ),
                        )}
                    </CommandGroup>
                </CommandList>
            </Command>
        );
    };

    if (props.isMobileWidth) {
        return (
            <>
                <Drawer>
                    <DrawerTrigger className="bg-background border border-muted p-4 rounded-2xl my-8 text-left inline-flex items-center justify-between w-full">
                        Manage Files <ArrowRight className="h-4 w-4 mx-2" />
                    </DrawerTrigger>
                    <DrawerContent>
                        <DrawerHeader>
                            <DrawerTitle>Files</DrawerTitle>
                            <DrawerDescription>
                                {usingConversationContext
                                    ? "Manage files for this conversation"
                                    : "Shared files"}
                            </DrawerDescription>
                        </DrawerHeader>
                        <div className={`${styles.panelWrapper}`}>
                            <FilesMenuCommandBox />
                        </div>
                        <DrawerFooter>
                            <DrawerClose>
                                <Button variant="outline">Done</Button>
                            </DrawerClose>
                        </DrawerFooter>
                    </DrawerContent>
                </Drawer>
            </>
        );
    }

    return (
        <>
            <Popover open={isOpen} onOpenChange={setIsOpen}>
                <PopoverTrigger asChild>
                    <div className="w-auto bg-background border border-muted p-4 drop-shadow-sm rounded-2xl">
                        <div className="flex items-center justify-between space-x-4">
                            <h4 className="text-sm font-semibold">
                                {usingConversationContext ? "Manage Context" : "Files"}
                                <p>
                                    {usingConversationContext ? (
                                        <span className="text-muted-foreground text-xs">
                                            Using{" "}
                                            {addedFiles.length == 0
                                                ? files.length
                                                : addedFiles.length}{" "}
                                            files
                                        </span>
                                    ) : (
                                        <span className="text-muted-foreground text-xs">
                                            Shared{" "}
                                            {addedFiles.length == 0
                                                ? files.length
                                                : addedFiles.length}{" "}
                                            files
                                        </span>
                                    )}
                                </p>
                            </h4>
                            <Button variant="ghost" size="sm" className="w-9 p-0">
                                {isOpen ? (
                                    <ArrowDown className="h-4 w-4" />
                                ) : (
                                    <ArrowRight className="h-4 w-4" />
                                )}
                                <span className="sr-only">Toggle</span>
                            </Button>
                        </div>
                    </div>
                </PopoverTrigger>
                <PopoverContent className={`mx-2`}>
                    <FilesMenuCommandBox />
                </PopoverContent>
            </Popover>
        </>
    );
}

interface SessionsAndFilesProps {
    subsetOrganizedData: GroupedChatHistory | null;
    organizedData: GroupedChatHistory | null;
    data: ChatHistory[] | null;
    userProfile: UserProfile | null;
    conversationId: string | null;
    uploadedFiles: string[];
    isMobileWidth: boolean;
    sideBarOpen: boolean;
}

function SessionsAndFiles(props: SessionsAndFilesProps) {
    return (
        <div>
            {props.sideBarOpen && (
                <ScrollArea>
                    <ScrollAreaScrollbar
                        orientation="vertical"
                        className="h-full w-2.5"
                    />
                    <div className="p-0 m-0">
                        {props.subsetOrganizedData != null &&
                            Object.keys(props.subsetOrganizedData)
                                .filter((tg) => tg !== "All Time")
                                .map((timeGrouping) => (
                                    <div key={timeGrouping} className={`my-1`}>
                                        <div className={`text-muted-foreground text-xs p-[0.5rem]`}>
                                            {timeGrouping}
                                        </div>
                                        {props.subsetOrganizedData &&
                                            props.subsetOrganizedData[timeGrouping].map(
                                                (chatHistory) => (
                                                    <ChatSession
                                                        updated={chatHistory.updated}
                                                        created={chatHistory.created}
                                                        compressed={true}
                                                        key={chatHistory.conversation_id}
                                                        conversation_id={
                                                            chatHistory.conversation_id
                                                        }
                                                        slug={chatHistory.slug}
                                                        agent_name={chatHistory.agent_name}
                                                        agent_color={chatHistory.agent_color}
                                                        agent_icon={chatHistory.agent_icon}
                                                        agent_is_hidden={chatHistory.agent_is_hidden}
                                                    />
                                                ),
                                            )}
                                    </div>
                                ))}
                    </div>
                </ScrollArea>
            )}
            {props.data && props.data.length > 5 && (
                <ChatSessionsModal data={props.organizedData} sideBarOpen={props.sideBarOpen} />
            )}
        </div>
    );
}

export interface ChatSessionActionMenuProps {
    conversationId: string;
    setTitle: (title: string) => void;
    sizing?: "sm" | "md" | "lg";
}

export function ChatSessionActionMenu(props: ChatSessionActionMenuProps) {
    const [renamedTitle, setRenamedTitle] = useState("");
    const [isRenaming, setIsRenaming] = useState(false);
    const [isSharing, setIsSharing] = useState(false);
    const [isDeleting, setIsDeleting] = useState(false);
    const [shareUrl, setShareUrl] = useState("");
    const [showShareUrl, setShowShareUrl] = useState(false);

    const [isOpen, setIsOpen] = useState(false);

    useEffect(() => {
        if (isSharing) {
            shareConversation(props.conversationId, setShareUrl);
            setShowShareUrl(true);
            setIsSharing(false);
        }
    }, [isSharing, props.conversationId]);

    if (isRenaming) {
        return (
            <Dialog open={isRenaming} onOpenChange={(open) => setIsRenaming(open)}>
                <DialogContent>
                    <DialogHeader>
                        <DialogTitle>Set a new title for the conversation</DialogTitle>
                        <DialogDescription>
                            This will help you identify the conversation easily, and also help you
                            search for it later.
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
                                props.setTitle(renamedTitle);
                                setIsRenaming(false);
                            }}
                            type="submit"
                        >
                            Rename
                        </Button>
                    </DialogFooter>
                </DialogContent>
            </Dialog>
        );
    }

    if (isSharing || showShareUrl) {
        if (shareUrl) {
            navigator.clipboard.writeText(shareUrl);
        }
        return (
            <Dialog
                open={isSharing || showShareUrl}
                onOpenChange={(open) => {
                    setShowShareUrl(open);
                    setIsSharing(open);
                }}
            >
                <DialogContent>
                    <DialogHeader>
                        <DialogTitle>Conversation Share URL</DialogTitle>
                        <DialogDescription>
                            Sharing this chat session will allow anyone with a link to view the
                            conversation.
                            <Input
                                className="w-full bg-accent text-accent-foreground rounded-md p-2 mt-2"
                                value={shareUrl}
                                readOnly={true}
                            />
                        </DialogDescription>
                    </DialogHeader>
                    <DialogFooter>
                        {!showShareUrl && (
                            <Button
                                onClick={() => {
                                    shareConversation(props.conversationId, setShareUrl);
                                    setShowShareUrl(true);
                                }}
                                className="bg-orange-500"
                                disabled
                            >
                                <Spinner className="mr-2 h-4 w-4 animate-spin" />
                                Sharing
                            </Button>
                        )}
                        {showShareUrl && (
                            <Button
                                onClick={() => {
                                    navigator.clipboard.writeText(shareUrl);
                                }}
                                variant={"default"}
                            >
                                Copy
                            </Button>
                        )}
                    </DialogFooter>
                </DialogContent>
            </Dialog>
        );
    }

    if (isDeleting) {
        return (
            <AlertDialog open={isDeleting} onOpenChange={(open) => setIsDeleting(open)}>
                <AlertDialogContent>
                    <AlertDialogHeader>
                        <AlertDialogTitle>Delete Conversation</AlertDialogTitle>
                        <AlertDialogDescription>
                            Are you sure you want to delete this conversation? This action cannot be
                            undone.
                        </AlertDialogDescription>
                    </AlertDialogHeader>
                    <AlertDialogFooter>
                        <AlertDialogCancel>Cancel</AlertDialogCancel>
                        <AlertDialogAction
                            onClick={() => {
                                deleteConversation(props.conversationId);
                                setIsDeleting(false);
                            }}
                            className="bg-rose-500 hover:bg-rose-600"
                        >
                            Delete
                        </AlertDialogAction>
                    </AlertDialogFooter>
                </AlertDialogContent>
            </AlertDialog>
        );
    }

    function sizeClass() {
        switch (props.sizing) {
            case "sm":
                return "h-4 w-4";
            case "md":
                return "h-6 w-6";
            case "lg":
                return "h-8 w-8";
            default:
                return "h-4 w-4";
        }
    }

    const size = sizeClass();

    return (
        <div className="flex items-center gap-2">
            {(props.sizing === "lg" || props.sizing === "md") && (
                <Button
                    className="p-0 text-sm h-auto"
                    variant={"ghost"}
                    onClick={() => setIsSharing(true)}
                >
                    <Share className={`${size}`} />
                </Button>
            )}
            <DropdownMenu onOpenChange={(open) => setIsOpen(open)} open={isOpen}>
                <DropdownMenuTrigger asChild>
                    {props.sizing === "lg" || props.sizing === "md" ? (
                        <Button className="p-0 text-sm h-auto" variant={"ghost"}>
                            <DotsThreeVertical className={`${size}`} />
                        </Button>
                    ) : (
                        <SidebarMenuAction asChild>
                            <DotsThreeVertical className={`${size}`} />
                        </SidebarMenuAction>
                    )}
                </DropdownMenuTrigger>
                <DropdownMenuContent side="right" align="start">
                    <DropdownMenuItem onClick={() => setIsRenaming(true)}>
                        <span className="flex items-center">
                            <Pencil className={`mr-2 ${size}`} />
                            Rename
                        </span>
                    </DropdownMenuItem>
                    {props.sizing === "sm" && (
                        <DropdownMenuItem onClick={() => setIsSharing(true)}>
                            <span className="flex items-center">
                                <Share className={`mr-2 ${size}`} />
                                Share
                            </span>
                        </DropdownMenuItem>
                    )}
                    <DropdownMenuItem onClick={() => setIsDeleting(true)}>
                        <span className="flex items-center">
                            <Trash className={`mr-2 ${size}`} />
                            Delete
                        </span>
                    </DropdownMenuItem>
                </DropdownMenuContent>
            </DropdownMenu>
        </div>
    );
}

function ChatSession(props: ChatHistory) {
    const [isHovered, setIsHovered] = useState(false);
    const [title, setTitle] = useState(props.slug || "New Conversation 🌱");
    var currConversationId =
        new URLSearchParams(window.location.search).get("conversationId") || "-1";
    return (
        <SidebarMenuItem
            onMouseEnter={() => setIsHovered(true)}
            onMouseLeave={() => setIsHovered(false)}
            key={props.conversation_id}
            className={`${styles.session} ${props.compressed ? styles.compressed : "!max-w-full"} ${isHovered ? `${styles.sessionHover}` : ""} ${currConversationId === props.conversation_id && currConversationId != "-1" ? "dark:bg-neutral-800 bg-white" : ""} m-1 rounded-lg`}
        >
            <SidebarMenuButton asChild>
                <Link
                    href={`/chat?conversationId=${props.conversation_id}`}
                    className="flex items-center gap-2 no-underline"
                >
                    <p
                        className={`${styles.session} ${props.compressed ? styles.compressed : 'max-w-[15rem] md:max-w-[22rem]'}`}
                    >
                        {title}
                    </p>
                </Link>
            </SidebarMenuButton>
            <ChatSessionActionMenu
                conversationId={props.conversation_id}
                setTitle={setTitle}
                sizing="sm"
            />
        </SidebarMenuItem>
    );
}

interface ChatSessionsModalProps {
    data: GroupedChatHistory | null;
    sideBarOpen: boolean;
}

interface AgentStyle {
    color: string;
    icon: string;
}

function ChatSessionsModal({ data, sideBarOpen }: ChatSessionsModalProps) {
    const [agentsFilter, setAgentsFilter] = useState<string[]>([]);
    const [agentOptions, setAgentOptions] = useState<string[]>([]);
    const [searchQuery, setSearchQuery] = useState<string>("");

    const [agentNameToStyleMap, setAgentNameToStyleMap] = useState<Record<string, AgentStyle>>({});

    useEffect(() => {
        if (data) {
            const agents: string[] = [];
            let agentNameToStyleMapLocal: Record<string, AgentStyle> = {};
            Object.keys(data).forEach((timeGrouping) => {
                data[timeGrouping].forEach((chatHistory) => {
                    if (chatHistory.agent_is_hidden) return;
                    if (!chatHistory.agent_color) return;
                    if (!chatHistory.agent_name) return;
                    if (!chatHistory.agent_icon) return;

                    const agentName = chatHistory.agent_name;

                    if (agentName && !agents.includes(agentName)) {
                        agents.push(chatHistory.agent_name);

                        agentNameToStyleMapLocal = {
                            ...agentNameToStyleMapLocal,
                            [chatHistory.agent_name]: {
                                color: chatHistory.agent_color ?? "orange",
                                icon: chatHistory.agent_icon ?? "Lightbulb",
                            },
                        };
                    }
                });
            });
            setAgentNameToStyleMap(agentNameToStyleMapLocal);
            setAgentOptions(agents);
        }
    }, [data]);

    // Memoize the filtered results
    const filteredData = useMemo(() => {
        if (!data) return null;

        // Early return if no filters active
        if (agentsFilter.length === 0 && searchQuery.length === 0) {
            return data;
        }

        const filtered: GroupedChatHistory = {};
        const agentSet = new Set(agentsFilter);
        const searchLower = searchQuery.toLowerCase();

        for (const timeGrouping in data) {
            const matches = data[timeGrouping].filter((chatHistory) => {
                // Early return for agent filter
                if (agentsFilter.length > 0 && !agentSet.has(chatHistory.agent_name)) {
                    return false;
                }

                // Early return for search query
                if (searchQuery && !chatHistory.slug?.toLowerCase().includes(searchLower)) {
                    return false;
                }

                return true;
            });

            if (matches.length > 0) {
                filtered[timeGrouping] = matches;
            }
        }

        return filtered;
    }, [data, agentsFilter, searchQuery]);

    return (
        <Dialog>
            <DialogTrigger
                className={`flex text-left text-medium text-gray-500 hover:text-gray-300 cursor-pointer my-1 text-sm ${sideBarOpen ? "p-[0.5rem] " : "p-[0.1rem]"}`}
            >
                <span className="flex items-center gap-1">
                    <ChatsCircle className="inline h-4 w-4 mr-1" />
                    {sideBarOpen ? "All Conversations" : ""}
                </span>
            </DialogTrigger>
            <DialogContent>
                <DialogHeader>
                    <DialogTitle>All Conversations</DialogTitle>
                    <DialogDescription className="p-0">
                        <div className="flex flex-row justify-between mt-2 gap-2 w-fit md:w-full">
                            <Input
                                value={searchQuery}
                                onChange={(e) => {
                                    setSearchQuery(e.target.value);
                                }}
                                placeholder="Search conversations"
                            />
                            <DropdownMenu>
                                <DropdownMenuTrigger asChild>
                                    <Button
                                        variant="ghost"
                                        className={`p-0 px-1 ${agentsFilter.length > 0 ? "bg-muted text-muted-foreground" : "bg-inherit"} `}
                                    >
                                        <FunnelSimple />
                                    </Button>
                                </DropdownMenuTrigger>
                                <DropdownMenuContent>
                                    <DropdownMenuLabel>Agents</DropdownMenuLabel>
                                    <DropdownMenuSeparator />
                                    {agentOptions.map((agent) => (
                                        <DropdownMenuCheckboxItem
                                            key={agent}
                                            onSelect={(e) => e.preventDefault()}
                                            checked={agentsFilter.includes(agent)}
                                            onCheckedChange={(checked) => {
                                                if (checked) {
                                                    setAgentsFilter([...agentsFilter, agent]);
                                                } else {
                                                    setAgentsFilter(
                                                        agentsFilter.filter((a) => a !== agent),
                                                    );
                                                }
                                            }}
                                        >
                                            <div className="flex items-center justify-center px-1">
                                                {getIconFromIconName(
                                                    agentNameToStyleMap[agent]?.icon,
                                                    agentNameToStyleMap[agent]?.color,
                                                )}
                                                <div className="break-words">{agent}</div>
                                            </div>
                                        </DropdownMenuCheckboxItem>
                                    ))}
                                </DropdownMenuContent>
                            </DropdownMenu>
                        </div>
                        <ScrollArea className="h-[500px] py-4">
                            {filteredData &&
                                Object.keys(filteredData).map((timeGrouping) => (
                                    <div key={timeGrouping}>
                                        <div
                                            className={`text-muted-foreground text-sm font-bold p-[0.5rem] `}
                                        >
                                            {timeGrouping}
                                        </div>
                                        {filteredData[timeGrouping].map((chatHistory) => (
                                            <ChatSession
                                                updated={chatHistory.updated}
                                                created={chatHistory.created}
                                                compressed={false}
                                                key={chatHistory.conversation_id}
                                                conversation_id={chatHistory.conversation_id}
                                                slug={chatHistory.slug}
                                                agent_name={chatHistory.agent_name}
                                                agent_color={chatHistory.agent_color}
                                                agent_icon={chatHistory.agent_icon}
                                                agent_is_hidden={chatHistory.agent_is_hidden}
                                            />
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

const fetchChatHistory = async (url: string) => {
    const response = await fetch(url, {
        method: "GET",
        headers: {
            "Content-Type": "application/json",
        },
    });
    return response.json();
};

export const useChatSessionsFetchRequest = (url: string) => {
    const { data, isLoading, error } = useSWR<ChatHistory[]>(url, fetchChatHistory);

    return { data, isLoading, error };
};

interface SidePanelProps {
    conversationId: string | null;
    uploadedFiles: string[];
    isMobileWidth: boolean;
    sideBarOpen: boolean;
}

export default function AllConversations(props: SidePanelProps) {
    const [data, setData] = useState<ChatHistory[] | null>(null);
    const [organizedData, setOrganizedData] = useState<GroupedChatHistory | null>(null);
    const [subsetOrganizedData, setSubsetOrganizedData] = useState<GroupedChatHistory | null>(null);

    const {
        data: authenticatedData,
        error: authenticationError,
        isLoading: authenticationLoading,
    } = useAuthenticatedData();
    const { data: chatSessions, isLoading } = useChatSessionsFetchRequest(
        authenticatedData ? `/api/chat/sessions` : "",
    );

    useEffect(() => {
        if (chatSessions) {
            setData(chatSessions);

            const groupedData: GroupedChatHistory = {};
            const subsetOrganizedData: GroupedChatHistory = {};
            let numAdded = 0;

            const currentDate = new Date();

            chatSessions.forEach((chatSessionMetadata) => {
                const chatDate = new Date(chatSessionMetadata.updated);
                const diffTime = Math.abs(currentDate.getTime() - chatDate.getTime());
                const diffDays = Math.ceil(diffTime / (1000 * 60 * 60 * 24));

                const timeGrouping =
                    diffDays < 7 ? "Recent" : diffDays < 30 ? "Last Month" : "All Time";
                if (!groupedData[timeGrouping]) {
                    groupedData[timeGrouping] = [];
                }
                groupedData[timeGrouping].push(chatSessionMetadata);

                // Add to subsetOrganizedData if less than 8
                if (numAdded < 8) {
                    if (!subsetOrganizedData[timeGrouping]) {
                        subsetOrganizedData[timeGrouping] = [];
                    }
                    subsetOrganizedData[timeGrouping].push(chatSessionMetadata);
                    numAdded++;
                }
            });

            setSubsetOrganizedData(subsetOrganizedData);
            setOrganizedData(groupedData);
        }
    }, [chatSessions]);

    if (isLoading) {
        return (
            <SidebarGroup className="p-0 m-0">
                <SidebarGroupLabel className="!p-0 m-0 px-0">Conversations</SidebarGroupLabel>
                {Array.from({ length: 5 }).map((_, index) => (
                    <SidebarMenuItem key={index} className="p-0 list-none m-0">
                        <SidebarMenuSkeleton showIcon />
                    </SidebarMenuItem>
                ))}
            </SidebarGroup>
        );
    }

    return (
        <SidebarGroup>
            <div className={`flex justify-between flex-col`}>
                {authenticatedData && (
                    <>
                        <SidebarGroupLabel className="!p-0 m-0 px-0">
                            Conversations
                        </SidebarGroupLabel>
                        <div
                            className={`${props.sideBarOpen ? "border-l-2 border-light-blue-500 border-opacity-25 " : ""}`}
                        >
                            <SessionsAndFiles
                                subsetOrganizedData={subsetOrganizedData}
                                organizedData={organizedData}
                                data={data}
                                uploadedFiles={props.uploadedFiles}
                                userProfile={authenticatedData}
                                conversationId={props.conversationId}
                                isMobileWidth={props.isMobileWidth}
                                sideBarOpen={props.sideBarOpen}
                            />
                        </div>
                    </>
                )}
            </div>
        </SidebarGroup>
    );
}
