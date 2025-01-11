import {
    Sidebar,
    SidebarContent,
    SidebarFooter,
    SidebarGroup,
    SidebarGroupContent,
    SidebarHeader,
    SidebarMenu,
    SidebarMenuButton,
    SidebarMenuItem,
} from "@/components/ui/sidebar";
import {
    KhojAgentLogo,
    KhojAutomationLogo,
    KhojLogo,
    KhojLogoType,
    KhojSearchLogo,
} from "../logo/khojLogo";
import { Gear } from "@phosphor-icons/react/dist/ssr";
import { Plus } from "@phosphor-icons/react";
import { useEffect, useState } from "react";
import AllConversations from "../allConversations/allConversations";
import FooterMenu from "../navMenu/navMenu";
import { useSidebar } from "@/components/ui/sidebar";
import { useIsMobileWidth } from "@/app/common/utils";
import { UserPlusIcon } from "lucide-react";
import { useAuthenticatedData } from "@/app/common/auth";
import LoginPrompt from "../loginPrompt/loginPrompt";
import { url } from "inspector";

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
    const isMobileWidth = useIsMobileWidth();
    const { data, isLoading, error } = useAuthenticatedData();

    const { state, open, setOpen, openMobile, setOpenMobile, isMobile, toggleSidebar } =
        useSidebar();

    const [showLoginPrompt, setShowLoginPrompt] = useState(false);

    useEffect(() => {
        if (!isLoading && !data) {
            setShowLoginPrompt(true);
        }
    }, [isLoading, data]);

    return (
        <Sidebar collapsible={"icon"} variant="sidebar" className="md:py-2">
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
                                </a>
                            </SidebarMenuButton>
                        )}
                    </SidebarMenuItem>
                </SidebarMenu>
            </SidebarHeader>
            <SidebarContent>
                {showLoginPrompt && (
                    <LoginPrompt
                        onOpenChange={(isOpen) => setShowLoginPrompt(isOpen)}
                        isMobileWidth={isMobileWidth}
                    />
                )}
                <SidebarGroup>
                    <SidebarGroupContent>
                        <SidebarMenu className="p-0 m-0">
                            {!isLoading && !data && (
                                <SidebarMenuItem className="p-0 m-0 list-none">
                                    <SidebarMenuButton
                                        asChild
                                        variant={"default"}
                                        onClick={() => setShowLoginPrompt(true)}
                                    >
                                        <div>
                                            <UserPlusIcon />
                                            <span>Sign up to get started</span>
                                        </div>
                                    </SidebarMenuButton>
                                </SidebarMenuItem>
                            )}
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
                <FooterMenu sideBarIsOpen={open} />
            </SidebarFooter>
        </Sidebar>
    );
}
