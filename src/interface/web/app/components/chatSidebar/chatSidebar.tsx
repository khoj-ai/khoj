"use client";

import {
    ArrowsDownUp,
    CaretCircleDown,
    CheckCircle,
    Circle,
    CircleNotch,
    PersonSimpleTaiChi,
    Sparkle,
} from "@phosphor-icons/react";

import { Button } from "@/components/ui/button";

import {
    Sidebar,
    SidebarContent,
    SidebarFooter,
    SidebarGroup,
    SidebarGroupContent,
    SidebarGroupLabel,
    SidebarHeader,
    SidebarMenu,
    SidebarMenuButton,
    SidebarMenuItem,
} from "@/components/ui/sidebar";
import { Textarea } from "@/components/ui/textarea";
import { ModelSelector } from "@/app/common/modelSelector";
import { FilesMenu } from "../allConversations/allConversations";
import { AgentConfigurationOptions } from "@/app/agents/page";
import useSWR from "swr";
import { mutate } from "swr";
import { Sheet, SheetContent } from "@/components/ui/sheet";
import { AgentData } from "../agentCard/agentCard";
import { useEffect, useState } from "react";
import {
    getAvailableIcons,
    getIconForSlashCommand,
    getIconFromIconName,
} from "@/app/common/iconUtils";
import { Label } from "@/components/ui/label";
import { Checkbox } from "@/components/ui/checkbox";
import { Tooltip, TooltipTrigger } from "@/components/ui/tooltip";
import { TooltipContent } from "@radix-ui/react-tooltip";
import { useAuthenticatedData } from "@/app/common/auth";
import { Popover, PopoverContent, PopoverTrigger } from "@/components/ui/popover";
import {
    Dialog,
    DialogClose,
    DialogContent,
    DialogDescription,
    DialogFooter,
    DialogHeader,
    DialogTitle,
    DialogTrigger,
} from "@/components/ui/dialog";
import {
    Select,
    SelectContent,
    SelectItem,
    SelectTrigger,
    SelectValue,
} from "@/components/ui/select";
import { convertColorToTextClass, tailwindColors } from "@/app/common/colorUtils";
import { Input } from "@/components/ui/input";
import Link from "next/link";
import { motion } from "framer-motion";

interface ChatSideBarProps {
    conversationId: string;
    isOpen: boolean;
    isMobileWidth?: boolean;
    onOpenChange: (open: boolean) => void;
    isActive?: boolean;
}

const fetcher = (url: string) => fetch(url).then((res) => res.json());

export function ChatSidebar({ ...props }: ChatSideBarProps) {
    if (props.isMobileWidth) {
        return (
            <Sheet open={props.isOpen} onOpenChange={props.onOpenChange}>
                <SheetContent className="w-[300px] bg-sidebar p-0 text-sidebar-foreground [&>button]:hidden">
                    <ChatSidebarInternal {...props} />
                </SheetContent>
            </Sheet>
        );
    }

    return (
        <div className="relative">
            <ChatSidebarInternal {...props} />
        </div>
    );
}

interface IAgentCreationProps {
    customPrompt: string;
    selectedModel: string;
    inputTools: string[];
    outputModes: string[];
}

interface AgentError {
    detail: string;
}

function AgentCreationForm(props: IAgentCreationProps) {
    const iconOptions = getAvailableIcons();
    const colorOptions = tailwindColors;

    const [isCreating, setIsCreating] = useState<boolean>(false);
    const [customAgentName, setCustomAgentName] = useState<string | undefined>();
    const [customAgentIcon, setCustomAgentIcon] = useState<string | undefined>();
    const [customAgentColor, setCustomAgentColor] = useState<string | undefined>();

    const [doneCreating, setDoneCreating] = useState<boolean>(false);
    const [createdSlug, setCreatedSlug] = useState<string | undefined>();
    const [isValid, setIsValid] = useState<boolean>(false);
    const [error, setError] = useState<string | undefined>();

    function createAgent() {
        if (isCreating) {
            return;
        }

        setIsCreating(true);

        const data = {
            name: customAgentName,
            icon: customAgentIcon,
            color: customAgentColor,
            persona: props.customPrompt,
            chat_model: props.selectedModel,
            input_tools: props.inputTools,
            output_modes: props.outputModes,
            privacy_level: "private",
        };

        const createAgentUrl = `/api/agents`;

        fetch(createAgentUrl, {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify(data),
        })
            .then((res) => res.json())
            .then((data: AgentData | AgentError) => {
                console.log("Success:", data);
                if ("detail" in data) {
                    setError(`Error creating agent: ${data.detail}`);
                    setIsCreating(false);
                    return;
                }
                setDoneCreating(true);
                setCreatedSlug(data.slug);
                setIsCreating(false);
            })
            .catch((error) => {
                console.error("Error:", error);
                setError(`Error creating agent: ${error}`);
                setIsCreating(false);
            });
    }

    useEffect(() => {
        if (customAgentName && customAgentIcon && customAgentColor) {
            setIsValid(true);
        } else {
            setIsValid(false);
        }
    }, [customAgentName, customAgentIcon, customAgentColor]);

    return (
        <Dialog>
            <DialogTrigger asChild>
                <Button className="w-full" variant="secondary">
                    Create Agent
                </Button>
            </DialogTrigger>
            <DialogContent>
                <DialogHeader>
                    {doneCreating && createdSlug ? (
                        <DialogTitle>Created {customAgentName}</DialogTitle>
                    ) : (
                        <DialogTitle>Create a New Agent</DialogTitle>
                    )}
                    <DialogClose />
                    <DialogDescription>
                        If these settings have been helpful, create a dedicated agent you can re-use
                        across conversations.
                    </DialogDescription>
                </DialogHeader>
                <div className="py-4">
                    {doneCreating && createdSlug ? (
                        <div className="flex flex-col items-center justify-center gap-4 py-8">
                            <motion.div
                                initial={{ scale: 0 }}
                                animate={{ scale: 1 }}
                                transition={{
                                    type: "spring",
                                    stiffness: 260,
                                    damping: 20,
                                }}
                            >
                                <CheckCircle className="w-16 h-16 text-green-500" weight="fill" />
                            </motion.div>
                            <motion.p
                                initial={{ opacity: 0, y: 10 }}
                                animate={{ opacity: 1, y: 0 }}
                                transition={{ delay: 0.2 }}
                                className="text-center text-lg font-medium text-accent-foreground"
                            >
                                Created successfully!
                            </motion.p>
                            <motion.div
                                initial={{ opacity: 0, y: 10 }}
                                animate={{ opacity: 1, y: 0 }}
                                transition={{ delay: 0.4 }}
                            >
                                <Link href={`/agents?agent=${createdSlug}`}>
                                    <Button variant="secondary" className="mt-2">
                                        Manage Agent
                                    </Button>
                                </Link>
                            </motion.div>
                        </div>
                    ) : (
                        <div className="flex flex-col gap-4">
                            <div>
                                <Label htmlFor="agent_name">Name</Label>
                                <Input
                                    id="agent_name"
                                    className="w-full p-2 border mt-4 border-slate-500 rounded-lg"
                                    disabled={isCreating}
                                    value={customAgentName}
                                    onChange={(e) => setCustomAgentName(e.target.value)}
                                />
                            </div>
                            <div className="flex gap-4">
                                <div className="flex-1">
                                    <Select
                                        onValueChange={setCustomAgentColor}
                                        defaultValue={customAgentColor}
                                    >
                                        <SelectTrigger
                                            className="w-full dark:bg-muted"
                                            disabled={isCreating}
                                        >
                                            <SelectValue placeholder="Color" />
                                        </SelectTrigger>
                                        <SelectContent className="items-center space-y-1 inline-flex flex-col">
                                            {colorOptions.map((colorOption) => (
                                                <SelectItem key={colorOption} value={colorOption}>
                                                    <div className="flex items-center space-x-2">
                                                        <Circle
                                                            className={`w-6 h-6 mr-2 ${convertColorToTextClass(colorOption)}`}
                                                            weight="fill"
                                                        />
                                                        {colorOption}
                                                    </div>
                                                </SelectItem>
                                            ))}
                                        </SelectContent>
                                    </Select>
                                </div>
                                <div className="flex-1">
                                    <Select
                                        onValueChange={setCustomAgentIcon}
                                        defaultValue={customAgentIcon}
                                    >
                                        <SelectTrigger
                                            className="w-full dark:bg-muted"
                                            disabled={isCreating}
                                        >
                                            <SelectValue placeholder="Icon" />
                                        </SelectTrigger>
                                        <SelectContent className="items-center space-y-1 inline-flex flex-col">
                                            {iconOptions.map((iconOption) => (
                                                <SelectItem key={iconOption} value={iconOption}>
                                                    <div className="flex items-center space-x-2">
                                                        {getIconFromIconName(
                                                            iconOption,
                                                            customAgentColor ?? "gray",
                                                            "w-6",
                                                            "h-6",
                                                        )}
                                                        {iconOption}
                                                    </div>
                                                </SelectItem>
                                            ))}
                                        </SelectContent>
                                    </Select>
                                </div>
                            </div>
                        </div>
                    )}
                </div>
                <DialogFooter>
                    {error && <div className="text-red-500 text-sm">{error}</div>}
                    {!doneCreating && (
                        <Button
                            type="submit"
                            onClick={() => createAgent()}
                            disabled={isCreating || !isValid}
                        >
                            {isCreating ? (
                                <CircleNotch className="animate-spin" />
                            ) : (
                                <PersonSimpleTaiChi />
                            )}
                            Create
                        </Button>
                    )}
                    <DialogClose />
                </DialogFooter>
            </DialogContent>
        </Dialog>
    );
}

function ChatSidebarInternal({ ...props }: ChatSideBarProps) {
    const [isEditable, setIsEditable] = useState<boolean>(false);
    const { data: agentConfigurationOptions, error: agentConfigurationOptionsError } =
        useSWR<AgentConfigurationOptions>("/api/agents/options", fetcher);

    const {
        data: agentData,
        isLoading: agentDataLoading,
        error: agentDataError,
    } = useSWR<AgentData>(
        `/api/agents/conversation?conversation_id=${props.conversationId}`,
        fetcher,
    );
    const {
        data: authenticatedData,
        error: authenticationError,
        isLoading: authenticationLoading,
    } = useAuthenticatedData();

    const [customPrompt, setCustomPrompt] = useState<string | undefined>();
    const [selectedModel, setSelectedModel] = useState<string | undefined>();
    const [inputTools, setInputTools] = useState<string[] | undefined>();
    const [outputModes, setOutputModes] = useState<string[] | undefined>();
    const [hasModified, setHasModified] = useState<boolean>(false);
    const [isDefaultAgent, setIsDefaultAgent] = useState<boolean>(
        !agentData || agentData?.slug?.toLowerCase() === "khoj",
    );
    const [displayInputTools, setDisplayInputTools] = useState<string[] | undefined>();
    const [displayOutputModes, setDisplayOutputModes] = useState<string[] | undefined>();

    const [isSaving, setIsSaving] = useState<boolean>(false);

    const isSubscribed = authenticatedData?.is_active ?? false;

    function setupAgentData() {
        if (agentData) {
            setInputTools(agentData.input_tools);
            setDisplayInputTools(agentData.input_tools);
            if (agentData.input_tools === undefined || agentData.input_tools.length === 0) {
                setDisplayInputTools(
                    agentConfigurationOptions?.input_tools
                        ? Object.keys(agentConfigurationOptions.input_tools)
                        : [],
                );
            }
            setOutputModes(agentData.output_modes);
            setDisplayOutputModes(agentData.output_modes);
            if (agentData.output_modes === undefined || agentData.output_modes.length === 0) {
                setDisplayOutputModes(
                    agentConfigurationOptions?.output_modes
                        ? Object.keys(agentConfigurationOptions.output_modes)
                        : [],
                );
            }

            if (agentData.name?.toLowerCase() === "khoj" || agentData.is_hidden === true) {
                setIsEditable(true);
            }

            if (agentData.slug?.toLowerCase() === "khoj") {
                setSelectedModel(undefined);
                setCustomPrompt(undefined);
                setIsDefaultAgent(true);
            } else {
                setIsDefaultAgent(false);
                setCustomPrompt(agentData.persona);
                setSelectedModel(agentData.chat_model);
            }
        }
    }

    useEffect(() => {
        setupAgentData();
        setHasModified(false);
    }, [agentData]);

    // Track changes to the model, prompt, input tools and output modes fields
    useEffect(() => {
        if (!agentData || agentDataLoading) return; // Don't compare until data is loaded

        const modelChanged = !!selectedModel && selectedModel !== agentData.chat_model;
        const promptChanged = !!customPrompt && customPrompt !== agentData.persona;

        // Order independent check to ensure input tools or output modes haven't been changed.
        const toolsChanged =
            JSON.stringify(inputTools?.sort() || []) !==
            JSON.stringify(agentData.input_tools?.sort());
        const modesChanged =
            JSON.stringify(outputModes?.sort() || []) !==
            JSON.stringify(agentData.output_modes?.sort());

        setHasModified(modelChanged || promptChanged || toolsChanged || modesChanged);

        // Add agentDataLoading to dependencies to ensure it runs after loading finishes
    }, [selectedModel, customPrompt, inputTools, outputModes, agentData, agentDataLoading]);

    function isValueChecked(value: string, existingSelections: string[]): boolean {
        return existingSelections.includes(value);
    }

    function handleCheckToggle(value: string, existingSelections: string[]): string[] {
        if (existingSelections.includes(value)) {
            return existingSelections.filter((v) => v !== value);
        }

        return [...existingSelections, value];
    }

    function handleCustomPromptChange(value: string) {
        setCustomPrompt(value);
    }

    function handleSave() {
        if (hasModified) {
            if (!isDefaultAgent && agentData?.is_hidden === false) {
                alert(
                    "This agent is not a hidden agent. It cannot be modified from this interface.",
                );
                return;
            }

            let mode = "PATCH";

            if (isDefaultAgent) {
                mode = "POST";
            }

            const data = {
                persona: customPrompt,
                chat_model: selectedModel,
                input_tools: inputTools,
                output_modes: outputModes,
                ...(isDefaultAgent ? {} : { slug: agentData?.slug }),
            };

            setIsSaving(true);

            const url = !isDefaultAgent
                ? `/api/agents/hidden`
                : `/api/agents/hidden?conversation_id=${props.conversationId}`;

            // There are four scenarios here.
            // 1. If the agent is a default agent, then we need to create a new agent just to associate with this conversation.
            // 2. If the agent is not a default agent, then we need to update the existing hidden agent. This will be associated using the `slug` field.
            // 3. If the agent is a "proper" agent and not a hidden agent, then it cannot be updated from this API.
            // 4. The API is being called before the new conversation has been provisioned. This is currently not supported.
            fetch(url, {
                method: mode,
                headers: {
                    "Content-Type": "application/json",
                },
                body: JSON.stringify(data),
            })
                .then((res) => {
                    setIsSaving(false);
                    res.json();
                })
                .then((data) => {
                    mutate(`/api/agents/conversation?conversation_id=${props.conversationId}`);
                    setHasModified(false);
                })
                .catch((error) => {
                    console.error("Error:", error);
                    setIsSaving(false);
                });
        }
    }

    function handleReset() {
        setupAgentData();
        setHasModified(false);
    }

    function handleModelSelect(model: string) {
        setSelectedModel(model);
    }

    return (
        <Sidebar
            collapsible="none"
            className={`ml-auto opacity-30 rounded-lg p-2 transition-all transform duration-300 ease-in-out
                ${
                    props.isOpen
                        ? "translate-x-0 opacity-100 w-[300px] relative"
                        : "translate-x-full opacity-100 w-0 p-0 m-0"
                }
                `}
            variant="floating"
        >
            <SidebarContent>
                <SidebarHeader>
                    {agentData && !isEditable ? (
                        <div className="flex items-center relative text-sm">
                            <a
                                className="text-lg font-bold flex flex-row items-center"
                                href={`/agents?agent=${agentData.slug}`}
                            >
                                {getIconFromIconName(agentData.icon, agentData.color)}
                                {agentData.name}
                            </a>
                        </div>
                    ) : (
                        <div className="flex items-center relative text-sm justify-between">
                            <p>Chat Options</p>
                        </div>
                    )}
                </SidebarHeader>
                <SidebarGroup key={"knowledge"} className="border-b last:border-none">
                    <SidebarGroupContent className="gap-0">
                        <SidebarMenu className="p-0 m-0">
                            {agentData && agentData.has_files ? (
                                <SidebarMenuItem key={"agent_knowledge"} className="list-none">
                                    <div className="flex items-center space-x-2 rounded-full">
                                        <div className="text-muted-foreground">
                                            <Sparkle />
                                        </div>
                                        <div className="text-muted-foreground text-sm">
                                            Using custom knowledge base
                                        </div>
                                    </div>
                                </SidebarMenuItem>
                            ) : null}
                        </SidebarMenu>
                    </SidebarGroupContent>
                </SidebarGroup>
                <SidebarGroup key={"instructions"}>
                    <SidebarGroupContent>
                        <SidebarGroupLabel>Instructions</SidebarGroupLabel>
                        <SidebarMenu className="p-0 m-0">
                            <SidebarMenuItem className="list-none">
                                <Textarea
                                    className="w-full h-32 resize-none hover:resize-y"
                                    value={customPrompt || ""}
                                    onChange={(e) => handleCustomPromptChange(e.target.value)}
                                    readOnly={!isEditable}
                                    disabled={!isEditable}
                                />
                            </SidebarMenuItem>
                        </SidebarMenu>
                    </SidebarGroupContent>
                </SidebarGroup>
                {!agentDataLoading && agentData && (
                    <SidebarGroup key={"model"}>
                        <SidebarGroupContent>
                            <SidebarGroupLabel>
                                Model
                                {!isSubscribed && (
                                    <a
                                        href="/settings"
                                        className="hover:font-bold text-accent-foreground m-2 bg-accent bg-opacity-10 p-1 rounded-lg"
                                    >
                                        Upgrade
                                    </a>
                                )}
                            </SidebarGroupLabel>
                            <SidebarMenu className="p-0 m-0">
                                <SidebarMenuItem key={"model"} className="list-none">
                                    <ModelSelector
                                        disabled={!isEditable}
                                        onSelect={(model) => handleModelSelect(model.name)}
                                        initialModel={
                                            isDefaultAgent ? undefined : agentData?.chat_model
                                        }
                                        isActive={props.isActive}
                                    />
                                </SidebarMenuItem>
                            </SidebarMenu>
                        </SidebarGroupContent>
                    </SidebarGroup>
                )}
                <Popover defaultOpen={false}>
                    <SidebarGroup>
                        <SidebarGroupLabel asChild>
                            <PopoverTrigger>
                                Tools
                                <CaretCircleDown className="ml-auto transition-transform group-data-[state=open]/collapsible:rotate-180" />
                            </PopoverTrigger>
                        </SidebarGroupLabel>
                        <PopoverContent>
                            <SidebarGroupContent>
                                <SidebarMenu className="p-1 m-0">
                                    {Object.entries(
                                        agentConfigurationOptions?.input_tools ?? {},
                                    ).map(([key, value]) => {
                                        return (
                                            <SidebarMenuItem key={key} className="list-none">
                                                <Tooltip>
                                                    <TooltipTrigger key={key} asChild>
                                                        <div className="flex items-center space-x-2 py-1 justify-between">
                                                            <Label
                                                                htmlFor={key}
                                                                className="flex items-center gap-2 text-accent-foreground p-1 cursor-pointer"
                                                            >
                                                                {getIconForSlashCommand(key)}
                                                                <p className="text-sm my-auto flex items-center">
                                                                    {key}
                                                                </p>
                                                            </Label>
                                                            <Checkbox
                                                                id={key}
                                                                className={`${isEditable ? "cursor-pointer" : ""}`}
                                                                checked={isValueChecked(
                                                                    key,
                                                                    displayInputTools ?? [],
                                                                )}
                                                                onCheckedChange={() => {
                                                                    let updatedInputTools =
                                                                        handleCheckToggle(
                                                                            key,
                                                                            displayInputTools ?? [],
                                                                        );
                                                                    setInputTools(
                                                                        updatedInputTools,
                                                                    );
                                                                    setDisplayInputTools(
                                                                        updatedInputTools,
                                                                    );
                                                                }}
                                                                disabled={!isEditable}
                                                            >
                                                                {key}
                                                            </Checkbox>
                                                        </div>
                                                    </TooltipTrigger>
                                                    <TooltipContent
                                                        sideOffset={5}
                                                        side="left"
                                                        align="start"
                                                        className="text-sm bg-background text-foreground shadow-sm border border-slate-500 border-opacity-20 p-2 rounded-lg"
                                                    >
                                                        {value}
                                                    </TooltipContent>
                                                </Tooltip>
                                            </SidebarMenuItem>
                                        );
                                    })}
                                    {Object.entries(
                                        agentConfigurationOptions?.output_modes ?? {},
                                    ).map(([key, value]) => {
                                        return (
                                            <SidebarMenuItem key={key} className="list-none">
                                                <Tooltip>
                                                    <TooltipTrigger key={key} asChild>
                                                        <div className="flex items-center space-x-2 py-1 justify-between">
                                                            <Label
                                                                htmlFor={key}
                                                                className="flex items-center gap-2 p-1 rounded-lg cursor-pointer"
                                                            >
                                                                {getIconForSlashCommand(key)}
                                                                <p className="text-sm my-auto flex items-center">
                                                                    {key}
                                                                </p>
                                                            </Label>
                                                            <Checkbox
                                                                id={key}
                                                                className={`${isEditable ? "cursor-pointer" : ""}`}
                                                                checked={isValueChecked(
                                                                    key,
                                                                    displayOutputModes ?? [],
                                                                )}
                                                                onCheckedChange={() => {
                                                                    let updatedOutputModes =
                                                                        handleCheckToggle(
                                                                            key,
                                                                            displayOutputModes ??
                                                                                [],
                                                                        );
                                                                    setOutputModes(
                                                                        updatedOutputModes,
                                                                    );
                                                                    setDisplayOutputModes(
                                                                        updatedOutputModes,
                                                                    );
                                                                }}
                                                                disabled={!isEditable}
                                                            >
                                                                {key}
                                                            </Checkbox>
                                                        </div>
                                                    </TooltipTrigger>
                                                    <TooltipContent
                                                        sideOffset={5}
                                                        side="left"
                                                        align="start"
                                                        className="text-sm bg-background text-foreground shadow-sm border border-slate-500 border-opacity-20 p-2 rounded-lg"
                                                    >
                                                        {value}
                                                    </TooltipContent>
                                                </Tooltip>
                                            </SidebarMenuItem>
                                        );
                                    })}
                                </SidebarMenu>
                            </SidebarGroupContent>
                        </PopoverContent>
                    </SidebarGroup>
                </Popover>
                <SidebarGroup key={"files"}>
                    <SidebarGroupContent>
                        <SidebarGroupLabel>Files</SidebarGroupLabel>
                        <SidebarMenu className="p-0 m-0">
                            <SidebarMenuItem key={"files-conversation"} className="list-none">
                                <FilesMenu
                                    conversationId={props.conversationId}
                                    uploadedFiles={[]}
                                    isMobileWidth={props.isMobileWidth ?? false}
                                />
                            </SidebarMenuItem>
                        </SidebarMenu>
                    </SidebarGroupContent>
                </SidebarGroup>
            </SidebarContent>
            {props.isOpen && (
                <SidebarFooter key={"actions"}>
                    <SidebarMenu className="p-0 m-0">
                        {agentData && !isEditable && agentData.is_creator ? (
                            <SidebarMenuItem>
                                <SidebarMenuButton asChild>
                                    <Button
                                        className="w-full"
                                        variant={"ghost"}
                                        onClick={() =>
                                            (window.location.href = `/agents?agent=${agentData?.slug}`)
                                        }
                                    >
                                        Manage
                                    </Button>
                                </SidebarMenuButton>
                            </SidebarMenuItem>
                        ) : (
                            <>
                                {!hasModified &&
                                    isEditable &&
                                    customPrompt &&
                                    !isDefaultAgent &&
                                    selectedModel && (
                                        <SidebarMenuItem>
                                            <SidebarMenuButton asChild>
                                                <AgentCreationForm
                                                    customPrompt={customPrompt}
                                                    selectedModel={selectedModel}
                                                    inputTools={displayInputTools ?? []}
                                                    outputModes={displayOutputModes ?? []}
                                                />
                                            </SidebarMenuButton>
                                        </SidebarMenuItem>
                                    )}
                                <SidebarMenuItem>
                                    <SidebarMenuButton asChild>
                                        <Button
                                            className="w-full"
                                            onClick={() => handleReset()}
                                            variant={"ghost"}
                                            disabled={!isEditable || !hasModified}
                                        >
                                            Reset
                                        </Button>
                                    </SidebarMenuButton>
                                </SidebarMenuItem>
                                <SidebarMenuItem>
                                    <SidebarMenuButton asChild>
                                        <Button
                                            className={`w-full ${hasModified ? "bg-accent-foreground text-accent" : ""}`}
                                            variant={"secondary"}
                                            onClick={() => handleSave()}
                                            disabled={!isEditable || !hasModified || isSaving}
                                        >
                                            {isSaving ? (
                                                <CircleNotch className="animate-spin" />
                                            ) : (
                                                <ArrowsDownUp />
                                            )}
                                            {isSaving ? "Saving" : "Save"}
                                        </Button>
                                    </SidebarMenuButton>
                                </SidebarMenuItem>
                            </>
                        )}
                    </SidebarMenu>
                </SidebarFooter>
            )}
        </Sidebar>
    );
}
