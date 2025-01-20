"use client"

import { ArrowsDownUp, CaretCircleDown, CircleNotch, Sparkle } from "@phosphor-icons/react";

import { Button } from "@/components/ui/button";

import { Sidebar, SidebarContent, SidebarFooter, SidebarGroup, SidebarGroupContent, SidebarGroupLabel, SidebarHeader, SidebarMenu, SidebarMenuButton, SidebarMenuItem } from "@/components/ui/sidebar";
import { Textarea } from "@/components/ui/textarea";
import { ModelSelector } from "@/app/common/modelSelector";
import { FilesMenu } from "../allConversations/allConversations";
import { AgentConfigurationOptions } from "@/app/agents/page";
import useSWR from "swr";
import { mutate } from "swr";
import { Sheet, SheetContent } from "@/components/ui/sheet";
import { AgentData } from "../agentCard/agentCard";
import { useEffect, useState } from "react";
import { getIconForSlashCommand, getIconFromIconName } from "@/app/common/iconUtils";
import { Label } from "@/components/ui/label";
import { Checkbox } from "@/components/ui/checkbox";
import { Tooltip, TooltipTrigger } from "@/components/ui/tooltip";
import { TooltipContent } from "@radix-ui/react-tooltip";
import { Collapsible, CollapsibleContent, CollapsibleTrigger } from "@/components/ui/collapsible";
import { useAuthenticatedData } from "@/app/common/auth";

interface ChatSideBarProps {
    conversationId: string;
    isOpen: boolean;
    isMobileWidth?: boolean;
    onOpenChange: (open: boolean) => void;
}

const fetcher = (url: string) => fetch(url).then((res) => res.json());

export function ChatSidebar({ ...props }: ChatSideBarProps) {

    if (props.isMobileWidth) {
        return (
            <Sheet
                open={props.isOpen}
                onOpenChange={props.onOpenChange}>
                <SheetContent
                    className="w-[300px] bg-sidebar p-0 text-sidebar-foreground [&>button]:hidden"
                >
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


function ChatSidebarInternal({ ...props }: ChatSideBarProps) {
    const [isEditable, setIsEditable] = useState<boolean>(false);
    const [isDefaultAgent, setIsDefaultAgent] = useState<boolean>(false);
    const { data: agentConfigurationOptions, error: agentConfigurationOptionsError } =
        useSWR<AgentConfigurationOptions>("/api/agents/options", fetcher);

    const { data: agentData, error: agentDataError } = useSWR<AgentData>(`/api/agents/conversation?conversation_id=${props.conversationId}`, fetcher);
    const {
        data: authenticatedData,
        error: authenticationError,
        isLoading: authenticationLoading,
    } = useAuthenticatedData();

    const [customPrompt, setCustomPrompt] = useState<string | undefined>("");
    const [selectedModel, setSelectedModel] = useState<string | undefined>();
    const [inputTools, setInputTools] = useState<string[] | undefined>();
    const [outputModes, setOutputModes] = useState<string[] | undefined>();
    const [hasModified, setHasModified] = useState<boolean>(false);

    const [isSaving, setIsSaving] = useState<boolean>(false);

    function setupAgentData() {
        if (agentData) {
            setSelectedModel(agentData.chat_model);
            setInputTools(agentData.input_tools);
            if (agentData.input_tools === undefined || agentData.input_tools.length === 0) {
                setInputTools(agentConfigurationOptions?.input_tools ? Object.keys(agentConfigurationOptions.input_tools) : []);
            }
            setOutputModes(agentData.output_modes);
            if (agentData.output_modes === undefined || agentData.output_modes.length === 0) {
                setOutputModes(agentConfigurationOptions?.output_modes ? Object.keys(agentConfigurationOptions.output_modes) : []);
            }

            if (agentData.name.toLowerCase() === "khoj" || agentData.is_hidden === true) {
                setIsEditable(true);
            }

            if (agentData.slug.toLowerCase() === "khoj") {
                setIsDefaultAgent(true);
            } else {
                setCustomPrompt(agentData.persona);
            }
        }
    }

    useEffect(() => {
        setupAgentData();
    }, [agentData]);


    function isValueChecked(value: string, existingSelections: string[]): boolean {
        return existingSelections.includes(value);
    }

    function handleCheckToggle(value: string, existingSelections: string[]): string[] {
        setHasModified(true);

        if (existingSelections.includes(value)) {
            return existingSelections.filter((v) => v !== value);
        }

        return [...existingSelections, value];
    }

    function handleCustomPromptChange(value: string) {
        setCustomPrompt(value);
        setHasModified(true);
    }

    function handleSave() {
        if (hasModified) {
            if (!isDefaultAgent && agentData?.is_hidden === false) {
                alert("This agent is not a hidden agent. It cannot be modified from this interface.");
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
                ...(isDefaultAgent ? {} : { slug: agentData?.slug })
            };

            setIsSaving(true);

            const url = !isDefaultAgent ? `/api/agents/hidden` : `/api/agents/hidden?conversation_id=${props.conversationId}`;

            // There are four scenarios here.
            // 1. If the agent is a default agent, then we need to create a new agent just to associate with this conversation.
            // 2. If the agent is not a default agent, then we need to update the existing hidden agent. This will be associated using the `slug` field.
            // 3. If the agent is a "proper" agent and not a hidden agent, then it cannot be updated from this API.
            // 4. The API is being called before the new conversation has been provisioned. If this happens, then create the agent and associate it with the conversation. Reroute the user to the new conversation page.
            fetch(url, {
                method: mode,
                headers: {
                    "Content-Type": "application/json"
                },
                body: JSON.stringify(data)
            })
                .then((res) => {
                    setIsSaving(false);
                    res.json()
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
        if (model !== agentData?.chat_model) {
            setHasModified(true);
        }
    }

    return (
        <Sidebar
            collapsible="none"
            className={`ml-auto opacity-30 rounded-lg p-2 transition-all transform duration-300 ease-in-out
                ${props.isOpen
                    ? "translate-x-0 opacity-100 w-[300px] relative"
                    : "translate-x-full opacity-100 w-0 p-0 m-0"}
                `}
            variant="floating">
            <SidebarContent>
                <SidebarHeader>
                    {
                        agentData && !isEditable ? (
                            <div className="flex items-center relative top-2">
                                <a className="text-lg font-bold flex flex-row items-center" href={`/agents?agent=${agentData.slug}`}>
                                    {getIconFromIconName(agentData.icon, agentData.color)}
                                    {agentData.name}
                                </a>
                            </div>
                        ) : (
                            <div className="flex items-center relative top-2">
                                {getIconFromIconName("lightbulb", "orange")}
                                Chat Options
                            </div>
                        )
                    }
                </SidebarHeader>
                <SidebarGroup key={"knowledge"} className="border-b last:border-none">
                    <SidebarGroupContent className="gap-0">
                        <SidebarMenu className="p-0 m-0">
                            {
                                agentData && agentData.has_files ? (
                                    <SidebarMenuItem key={"agent_knowledge"} className="list-none">
                                        <div className="flex items-center space-x-2 rounded-full">
                                            <div className="text-muted-foreground"><Sparkle /></div>
                                            <div className="text-muted-foreground text-sm">Using custom knowledge base</div>
                                        </div>
                                    </SidebarMenuItem>
                                ) : null
                            }
                        </SidebarMenu>
                    </SidebarGroupContent>
                </SidebarGroup>
                <SidebarGroup key={"instructions"}>
                    <SidebarGroupContent>
                        <SidebarGroupLabel>Custom Instructions</SidebarGroupLabel>
                        <SidebarMenu className="p-0 m-0">
                            <SidebarMenuItem className="list-none">
                                <Textarea
                                    className="w-full h-32"
                                    value={customPrompt}
                                    onChange={(e) => handleCustomPromptChange(e.target.value)}
                                    readOnly={!isEditable}
                                    disabled={!isEditable} />
                            </SidebarMenuItem>
                        </SidebarMenu>
                    </SidebarGroupContent>
                </SidebarGroup>
                <SidebarGroup key={"model"}>
                    <SidebarGroupContent>
                        <SidebarGroupLabel>Model</SidebarGroupLabel>
                        <SidebarMenu className="p-0 m-0">
                            <SidebarMenuItem key={"model"} className="list-none">
                                <ModelSelector
                                    disabled={!isEditable || !authenticatedData?.is_active}
                                    onSelect={(model) => handleModelSelect(model.name)}
                                    selectedModel={selectedModel}
                                />
                            </SidebarMenuItem>
                        </SidebarMenu>
                    </SidebarGroupContent>
                </SidebarGroup>
                <Collapsible defaultOpen className="group/collapsible">
                    <SidebarGroup>
                        <SidebarGroupLabel asChild>
                            <CollapsibleTrigger>
                                Tools
                                <CaretCircleDown className="ml-auto transition-transform group-data-[state=open]/collapsible:rotate-180" />
                            </CollapsibleTrigger>
                        </SidebarGroupLabel>
                        <CollapsibleContent>
                            <SidebarGroupContent>
                                <SidebarMenu className="p-1 m-0">
                                    {
                                        Object.entries(agentConfigurationOptions?.input_tools ?? {}).map(([key, value]) => {
                                            return (
                                                <SidebarMenuItem key={key} className="list-none">
                                                    <Tooltip>
                                                        <TooltipTrigger key={key} asChild>
                                                            <div className="flex items-center space-x-2 py-1 justify-between">
                                                                <Label htmlFor={key} className="flex items-center gap-2 text-accent-foreground p-1 cursor-pointer">
                                                                    {getIconForSlashCommand(key)}
                                                                    <p className="text-sm my-auto flex items-center">
                                                                        {key}
                                                                    </p>
                                                                </Label>
                                                                <Checkbox
                                                                    id={key}
                                                                    className={`${isEditable ? "cursor-pointer" : ""}`}
                                                                    checked={isValueChecked(key, inputTools ?? [])}
                                                                    onCheckedChange={() => setInputTools(handleCheckToggle(key, inputTools ?? []))}
                                                                    disabled={!isEditable}
                                                                >
                                                                    {key}
                                                                </Checkbox>
                                                            </div>
                                                        </TooltipTrigger>
                                                        <TooltipContent sideOffset={5} side="left" align="start" className="text-sm bg-background text-foreground shadow-sm border border-slate-500 border-opacity-20 p-2 rounded-lg">
                                                            {value}
                                                        </TooltipContent>
                                                    </Tooltip>
                                                </SidebarMenuItem>
                                            );
                                        }
                                        )
                                    }
                                    {
                                        Object.entries(agentConfigurationOptions?.output_modes ?? {}).map(([key, value]) => {
                                            return (
                                                <SidebarMenuItem key={key} className="list-none">
                                                    <Tooltip>
                                                        <TooltipTrigger key={key} asChild>
                                                            <div className="flex items-center space-x-2 py-1 justify-between">
                                                                <Label htmlFor={key} className="flex items-center gap-2 p-1 rounded-lg cursor-pointer">
                                                                    {getIconForSlashCommand(key)}
                                                                    <p className="text-sm my-auto flex items-center">
                                                                        {key}
                                                                    </p>
                                                                </Label>
                                                                <Checkbox
                                                                    id={key}
                                                                    className={`${isEditable ? "cursor-pointer" : ""}`}
                                                                    checked={isValueChecked(key, outputModes ?? [])}
                                                                    onCheckedChange={() => setOutputModes(handleCheckToggle(key, outputModes ?? []))}
                                                                    disabled={!isEditable}
                                                                >
                                                                    {key}
                                                                </Checkbox>
                                                            </div>
                                                        </TooltipTrigger>
                                                        <TooltipContent sideOffset={5} side="left" align="start" className="text-sm bg-background text-foreground shadow-sm border border-slate-500 border-opacity-20 p-2 rounded-lg">
                                                            {value}
                                                        </TooltipContent>
                                                    </Tooltip>
                                                </SidebarMenuItem>
                                            );
                                        }
                                        )
                                    }
                                </SidebarMenu>

                            </SidebarGroupContent>
                        </CollapsibleContent>
                    </SidebarGroup>
                </Collapsible>
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
            {
                props.isOpen && (
                    <SidebarFooter key={"actions"}>
                        <SidebarMenu className="p-0 m-0">

                            {
                                (agentData && !isEditable && agentData.is_creator) ? (
                                    <SidebarMenuItem>
                                        <SidebarMenuButton asChild>
                                            <Button
                                                className="w-full"
                                                variant={"ghost"}
                                                onClick={() => window.location.href = `/agents?agent=${agentData?.slug}`}
                                            >
                                                Manage
                                            </Button>
                                        </SidebarMenuButton>
                                    </SidebarMenuItem>
                                ) :
                                    <>
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
                                                    className="w-full"
                                                    variant={"secondary"}
                                                    onClick={() => handleSave()}
                                                    disabled={!isEditable || !hasModified || isSaving}
                                                >
                                                    {
                                                        isSaving ?
                                                            <CircleNotch className="animate-spin" />
                                                            :
                                                            <ArrowsDownUp />
                                                    }
                                                    {
                                                        isSaving ? "Saving" : "Save"
                                                    }
                                                </Button>
                                            </SidebarMenuButton>
                                        </SidebarMenuItem>
                                    </>
                            }
                        </SidebarMenu>
                    </SidebarFooter>
                )
            }
        </Sidebar>
    )
}
