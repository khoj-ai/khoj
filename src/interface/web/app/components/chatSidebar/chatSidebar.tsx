"use client"

import { ArrowsDownUp, Bell, CaretCircleDown, Sparkle } from "@phosphor-icons/react";

import { Button } from "@/components/ui/button";

import { Sidebar, SidebarContent, SidebarFooter, SidebarGroup, SidebarGroupContent, SidebarGroupLabel, SidebarHeader, SidebarMenu, SidebarMenuButton, SidebarMenuItem } from "@/components/ui/sidebar";
import { DropdownMenu, DropdownMenuCheckboxItem, DropdownMenuContent, DropdownMenuItem, DropdownMenuLabel, DropdownMenuSeparator, DropdownMenuTrigger } from "@/components/ui/dropdown-menu";
import { Textarea } from "@/components/ui/textarea";
import { ModelSelector } from "@/app/common/modelSelector";
import { FilesMenu } from "../allConversations/allConversations";
import { AgentConfigurationOptions } from "@/app/agents/page";
import useSWR from "swr";
import { Sheet, SheetContent } from "@/components/ui/sheet";
import { AgentData } from "../agentCard/agentCard";
import { useState } from "react";
import { getIconForSlashCommand, getIconFromIconName } from "@/app/common/iconUtils";
import { Label } from "@/components/ui/label";
import { Checkbox } from "@/components/ui/checkbox";
import { Tooltip, TooltipTrigger } from "@/components/ui/tooltip";
import { TooltipContent } from "@radix-ui/react-tooltip";
import { Collapsible, CollapsibleContent, CollapsibleTrigger } from "@/components/ui/collapsible";

interface ChatSideBarProps {
    preexistingAgent?: AgentData | null;
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
        <ChatSidebarInternal {...props} />
    );
}


function ChatSidebarInternal({ ...props }: ChatSideBarProps) {
    const isEditable = props.preexistingAgent?.name.toLowerCase() === "khoj" || props.preexistingAgent?.is_hidden === true;
    const isDefaultAgent = props.preexistingAgent?.name.toLowerCase() === "khoj";
    const { data: agentConfigurationOptions, error: agentConfigurationOptionsError } =
        useSWR<AgentConfigurationOptions>("/api/agents/options", fetcher);

    const [customPrompt, setCustomPrompt] = useState<string | undefined>(!isDefaultAgent && props.preexistingAgent ? props.preexistingAgent.persona : "always respond in spanish");
    const [selectedModel, setSelectedModel] = useState<string | undefined>(props.preexistingAgent?.chat_model);
    const [inputTools, setInputTools] = useState<string[] | undefined>(props.preexistingAgent?.input_tools);


    function isValueChecked(value: string, existingSelections: string[] | undefined): boolean {
        if (existingSelections === undefined || existingSelections === null || existingSelections.length === 0) {
            return true;
        }

        return existingSelections.includes(value);
    }

    return (
        <Sidebar
            collapsible="none"
            className={`ml-auto opacity-30 rounded-lg p-2 transition-all transform duration-300 ease-in-out
                               ${props.isOpen
                    ? "translate-x-0 opacity-100 w-[300px]"
                    : "translate-x-full opacity-0 w-0"}
                               `}
            variant="floating">
            <SidebarContent>
                <SidebarHeader>
                    {
                        props.preexistingAgent && !isEditable ? (
                            <div className="flex items-center relative top-2">
                                <a className="text-lg font-bold flex flex-row items-center" href={`/agents?agent=${props.preexistingAgent.slug}`}>
                                    {getIconFromIconName(props.preexistingAgent.icon, props.preexistingAgent.color)}
                                    {props.preexistingAgent.name}
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
                                props.preexistingAgent && props.preexistingAgent.has_files ? (
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
                                    onChange={(e) => setCustomPrompt(e.target.value)}
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
                                    disabled={!isEditable}
                                    onSelect={(model) => setSelectedModel(model.name)}
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
                                {
                                    Object.entries(agentConfigurationOptions?.input_tools ?? {}).map(([key, value]) => {
                                        return (
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
                                                            checked={isValueChecked(key, props.preexistingAgent?.input_tools)}
                                                            onCheckedChange={() => { }}
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
                                        );
                                    }
                                    )
                                }
                                {
                                    Object.entries(agentConfigurationOptions?.output_modes ?? {}).map(([key, value]) => {
                                        return (
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
                                                            checked={isValueChecked(key, props.preexistingAgent?.output_modes)}
                                                            onCheckedChange={() => { }}
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
                                        );
                                    }
                                    )
                                }
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
            <SidebarFooter key={"actions"}>
                <SidebarMenu className="p-0 m-0">

                    {
                        (props.preexistingAgent && props.preexistingAgent.is_creator) ? (
                            <SidebarMenuItem>
                                <SidebarMenuButton asChild>
                                    <Button
                                        className="w-full"
                                        variant={"ghost"}
                                        onClick={() => window.location.href = `/agents?agent=${props.preexistingAgent?.slug}`}
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
                                            onClick={() => { }}
                                            variant={"ghost"}
                                            disabled={!isEditable}
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
                                            onClick={() => { }}
                                            disabled={!isEditable}
                                        >
                                            Save
                                        </Button>
                                    </SidebarMenuButton>
                                </SidebarMenuItem>
                            </>
                    }
                </SidebarMenu>
            </SidebarFooter>
        </Sidebar>
    )
}
