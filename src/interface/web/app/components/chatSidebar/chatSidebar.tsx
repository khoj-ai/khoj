"use client"

import * as React from "react"

import { Bell } from "@phosphor-icons/react";

import { Button } from "@/components/ui/button";

import { Sidebar, SidebarContent, SidebarGroup, SidebarGroupContent, SidebarHeader, SidebarMenu, SidebarMenuButton, SidebarMenuItem } from "@/components/ui/sidebar";
import { DropdownMenu, DropdownMenuCheckboxItem, DropdownMenuContent, DropdownMenuLabel, DropdownMenuSeparator, DropdownMenuTrigger } from "@/components/ui/dropdown-menu";
import { Textarea } from "@/components/ui/textarea";
import { ModelSelector } from "@/app/common/modelSelector";
import { FilesMenu } from "../allConversations/allConversations";
import { AgentConfigurationOptions } from "@/app/agents/page";
import useSWR from "swr";
import { Sheet, SheetContent } from "@/components/ui/sheet";

interface ChatSideBarProps {
    conversationId: string;
    isOpen: boolean;
    isMobileWidth?: boolean;
    onOpenChange: (open: boolean) => void;
}

const fetcher = (url: string) => fetch(url).then((res) => res.json());

export function ChatSidebar({ ...props }: ChatSideBarProps) {
    const { data: agentConfigurationOptions, error: agentConfigurationOptionsError } =
        useSWR<AgentConfigurationOptions>("/api/agents/options", fetcher);

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
    const { data: agentConfigurationOptions, error: agentConfigurationOptionsError } =
        useSWR<AgentConfigurationOptions>("/api/agents/options", fetcher);

    return (
        <Sidebar
            collapsible="none"
            className={`ml-auto rounded-lg p-2 transition-all transform duration-300 ease-in-out
                               ${props.isOpen
                    ? "translate-x-0 opacity-100 w-[300px]"
                    : "translate-x-full opacity-0 w-0"}
                               `}
            variant="floating">
            <SidebarContent>
                <SidebarHeader>
                    Chat Options
                </SidebarHeader>
                <SidebarGroup key={"test"} className="border-b last:border-none">
                    <SidebarGroupContent className="gap-0">
                        <SidebarMenu className="p-0 m-0">
                            <SidebarMenuItem key={"item4"} className="list-none">
                                <span>Custom Instructions</span>
                                <Textarea className="w-full h-32" />
                            </SidebarMenuItem>
                            <SidebarMenuItem key={"item"} className="list-none">
                                <SidebarMenuButton>
                                    <Bell /> <span>Model</span>
                                </SidebarMenuButton>
                                <ModelSelector />
                            </SidebarMenuItem>
                            <SidebarMenuItem key={"item1"} className="list-none">
                                <SidebarMenuButton>
                                    <Bell /> <span>Input Tools</span>
                                </SidebarMenuButton>
                                <DropdownMenu>
                                    <DropdownMenuTrigger asChild>
                                        <Button variant="outline">Input Tools</Button>
                                    </DropdownMenuTrigger>
                                    <DropdownMenuContent className="w-56">
                                        <DropdownMenuLabel>Input Tool Options</DropdownMenuLabel>
                                        <DropdownMenuSeparator />
                                        {
                                            Object.entries(agentConfigurationOptions?.input_tools ?? {}).map(([key, value]) => {
                                                return (
                                                    <DropdownMenuCheckboxItem
                                                        checked={true}
                                                        onCheckedChange={() => { }}
                                                    >
                                                        {key}
                                                    </DropdownMenuCheckboxItem>
                                                );
                                            }
                                            )
                                        }
                                    </DropdownMenuContent>
                                </DropdownMenu>
                            </SidebarMenuItem>
                            <SidebarMenuItem key={"item2"} className="list-none">
                                <SidebarMenuButton>
                                    <Bell /> <span>Output Tools</span>
                                </SidebarMenuButton>
                                <DropdownMenu>
                                    <DropdownMenuTrigger asChild>
                                        <Button variant="outline">Output Tools</Button>
                                    </DropdownMenuTrigger>
                                    <DropdownMenuContent className="w-56">
                                        <DropdownMenuLabel>Output Tool Options</DropdownMenuLabel>
                                        <DropdownMenuSeparator />
                                        {
                                            Object.entries(agentConfigurationOptions?.output_modes ?? {}).map(([key, value]) => {
                                                return (
                                                    <DropdownMenuCheckboxItem
                                                        checked={true}
                                                        onCheckedChange={() => { }}
                                                    >
                                                        {key}
                                                    </DropdownMenuCheckboxItem>
                                                );
                                            }
                                            )
                                        }
                                    </DropdownMenuContent>
                                </DropdownMenu>
                            </SidebarMenuItem>
                            <SidebarMenuItem key={"item3"} className="list-none">
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
        </Sidebar>
    )
}
