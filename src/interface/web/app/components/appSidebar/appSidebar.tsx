import { Calendar, Home, Inbox, Search, Settings } from "lucide-react";

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
import Link from "next/link";
import {
    KhojAgentLogo,
    KhojAutomationLogo,
    KhojLogo,
    KhojLogoType,
    KhojSearchLogo,
} from "../logo/khojLogo";
import { Gear } from "@phosphor-icons/react/dist/ssr";
import { House, Plus } from "@phosphor-icons/react";
import { useEffect, useState } from "react";
import { useAuthenticatedData } from "@/app/common/auth";
import AllConversations from "../allConversations/allConversations";
import NavMenu from "../navMenu/navMenu";
import { useSidebar } from "@/components/ui/sidebar";

// Menu items.
const items = [
    {
        title: "New",
        url: "/",
        icon: Plus,
    },
    {
        title: "Agents",
        url: "/agents",
        icon: KhojAgentLogo,
    },
    {
        title: "Automations",
        url: "/automations",
        icon: KhojAutomationLogo,
    },
    {
        title: "Search",
        url: "/search",
        icon: KhojSearchLogo,
    },
    {
        title: "Settings",
        url: "/settings",
        icon: Gear,
    },
];

const SIDEBAR_KEYBOARD_SHORTCUT = "b";
const SIDEBAR_WIDTH = "18rem";
const SIDEBAR_WIDTH_MOBILE = "20rem";

interface AppSidebarProps {
    conversationId: string | null;
}

export function AppSidebar(props: AppSidebarProps) {
    const [isMobileWidth, setIsMobileWidth] = useState(false);

    const { state, open, setOpen, openMobile, setOpenMobile, isMobile, toggleSidebar } =
        useSidebar();

    useEffect(() => {
        const handleResize = () => {
            setIsMobileWidth(window.innerWidth < 768);
        };

        handleResize();
        window.addEventListener("resize", handleResize);
        return () => window.removeEventListener("resize", handleResize);
    }, []);

    return (
        <Sidebar collapsible={"icon"} variant="sidebar">
            <SidebarHeader>
                <SidebarMenu className="p-0 m-0">
                    <SidebarMenuItem className="p-0 m-0">
                        {open ? (
                            <SidebarMenuButton>
                                <a className="p-0 no-underline" href="/">
                                    <KhojLogoType className="h-auto w-16" />
                                </a>
                            </SidebarMenuButton>
                        ) : (
                            <SidebarMenuButton asChild>
                                <a className="flex items-center gap-2 no-underline" href="/">
                                    <KhojLogo className="w-14 h-auto" />
                                    <span className="text-lg">Khoj</span>
                                </a>
                            </SidebarMenuButton>
                        )}
                    </SidebarMenuItem>
                </SidebarMenu>
            </SidebarHeader>
            <SidebarContent>
                <SidebarGroup>
                    <SidebarGroupContent>
                        <SidebarMenu className="p-0 m-0">
                            {items.map((item) => (
                                <SidebarMenuItem key={item.title} className="p-0 list-none m-0">
                                    <SidebarMenuButton asChild>
                                        <a
                                            href={item.url}
                                            className="flex items-center gap-2 no-underline"
                                        >
                                            <item.icon />
                                            <span>{item.title}</span>
                                        </a>
                                    </SidebarMenuButton>
                                </SidebarMenuItem>
                            ))}
                        </SidebarMenu>
                    </SidebarGroupContent>
                    <AllConversations
                        isMobileWidth={isMobileWidth}
                        conversationId={props.conversationId}
                        uploadedFiles={[]}
                        sideBarOpen={open}
                    />
                </SidebarGroup>
            </SidebarContent>
            <SidebarFooter>
                <NavMenu sideBarIsOpen={open} />
            </SidebarFooter>
        </Sidebar>
    );
}
