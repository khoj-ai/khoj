"use client";

import Link from "next/link";
import { useAuthenticatedData } from "@/app/common/auth";
import { useState, useEffect } from "react";
import { Avatar, AvatarImage, AvatarFallback } from "@/components/ui/avatar";

import {
    DropdownMenu,
    DropdownMenuContent,
    DropdownMenuItem,
    DropdownMenuSeparator,
    DropdownMenuTrigger,
} from "@/components/ui/dropdown-menu";
import {
    Moon,
    Sun,
    UserCircle,
    Question,
    ArrowRight,
    Code,
    BuildingOffice,
} from "@phosphor-icons/react";
import { useIsDarkMode, useIsMobileWidth } from "@/app/common/utils";
import LoginPrompt from "../loginPrompt/loginPrompt";
import { Button } from "@/components/ui/button";
import { SidebarMenu, SidebarMenuButton, SidebarMenuItem } from "@/components/ui/sidebar";
import { ChevronUp } from "lucide-react";

function SubscriptionBadge({ is_active }: { is_active: boolean }) {
    return (
        <div className="flex flex-row items-center">
            <div
                className={`w-3 h-3 rounded-full ${is_active ? "bg-yellow-500" : "bg-muted"} mr-1`}
            ></div>
            <p className="text-xs">{is_active ? "Futurist" : "Free"}</p>
        </div>
    );
}

function VersionBadge({ version }: { version: string }) {
    return (
        <div className="flex flex-row items-center">
            <div className="w-3 h-3 rounded-full bg-green-500 mr-1"></div>
            <p className="text-xs">{version}</p>
        </div>
    );
}

interface NavMenuProps {
    sideBarIsOpen: boolean;
}

export default function FooterMenu({ sideBarIsOpen }: NavMenuProps) {
    const {
        data: userData,
        error: authenticationError,
        isLoading: authenticationLoading,
    } = useAuthenticatedData();
    const [darkMode, setDarkMode] = useIsDarkMode();
    const [showLoginPrompt, setShowLoginPrompt] = useState(false);
    const isMobileWidth = useIsMobileWidth();

    const menuItems = [
        {
            title: "Help",
            icon: <Question className="w-6 h-6" />,
            link: "https://docs.khoj.dev",
        },
        {
            title: "Releases",
            icon: <Code className="w-6 h-6" />,
            link: "https://github.com/khoj-ai/khoj/releases",
        },
        {
            title: "Teams",
            icon: <BuildingOffice className="w-6 h-6" />,
            link: "https://khoj.dev/teams",
        },
    ];

    return (
        <SidebarMenu className="border-none p-0 m-0">
            <SidebarMenuItem className="p-0 m-0">
                {showLoginPrompt && (
                    <LoginPrompt
                        onOpenChange={(isOpen) => setShowLoginPrompt(isOpen)}
                        isMobileWidth={isMobileWidth}
                    />
                )}
                <DropdownMenu>
                    <DropdownMenuTrigger asChild>
                        <SidebarMenuButton className="p-0 m-0 rounded-lg" asChild>
                            {userData ? (
                                <span className="flex items-center gap-2">
                                    <Avatar
                                        className={`${sideBarIsOpen ? "h-8 w-8" : "h-6 w-6"} border-2 ${userData.is_active ? "border-yellow-500" : "border-stone-700 dark:border-stone-300"}`}
                                    >
                                        <AvatarImage src={userData.photo} alt="user profile" />
                                        <AvatarFallback className="bg-transparent hover:bg-muted">
                                            {userData.username[0].toUpperCase()}
                                        </AvatarFallback>
                                    </Avatar>
                                    {sideBarIsOpen && (
                                        <>
                                            <p>{userData?.username}</p>
                                            <ChevronUp className="w-6 h-6 ml-auto" />
                                        </>
                                    )}
                                </span>
                            ) : (
                                <UserCircle className="w-10 h-10" />
                            )}
                        </SidebarMenuButton>
                    </DropdownMenuTrigger>
                    <DropdownMenuContent align="end" className="rounded-xl gap-2">
                        <DropdownMenuItem className="w-full">
                            <div className="flex flex-col">
                                <p className="font-semibold">{userData?.email}</p>
                                <SubscriptionBadge is_active={userData?.is_active ?? false} />
                                {userData?.khoj_version && (
                                    <VersionBadge version={userData?.khoj_version} />
                                )}
                            </div>
                        </DropdownMenuItem>
                        <DropdownMenuSeparator className="dark:bg-white height-[2px] bg-black" />
                        <DropdownMenuItem
                            onClick={() => setDarkMode(!darkMode)}
                            className="w-full hover:cursor-pointer"
                        >
                            <div className="flex flex-rows">
                                {darkMode ? (
                                    <Sun className="w-6 h-6" />
                                ) : (
                                    <Moon className="w-6 h-6" />
                                )}
                                <p className="ml-3 font-semibold">
                                    {darkMode ? "Light Mode" : "Dark Mode"}
                                </p>
                            </div>
                        </DropdownMenuItem>
                        {menuItems.map((menuItem, index) => (
                            <DropdownMenuItem key={index}>
                                <Link href={menuItem.link} className="no-underline w-full">
                                    <div className="flex flex-rows">
                                        {menuItem.icon}
                                        <p className="ml-3 font-semibold">{menuItem.title}</p>
                                    </div>
                                </Link>
                            </DropdownMenuItem>
                        ))}
                        {!userData ? (
                            <DropdownMenuItem>
                                <Button
                                    variant={"ghost"}
                                    onClick={() => setShowLoginPrompt(true)}
                                    className="no-underline w-full text-left p-0 content-start justify-start items-start h-fit"
                                >
                                    <div className="flex flex-rows text-left content-start justify-start items-start p-0">
                                        <ArrowRight className="w-6 h-6" />
                                        <p className="ml-3 font-semibold">Login</p>
                                    </div>
                                </Button>
                            </DropdownMenuItem>
                        ) : userData.username !== "default" ? (
                            <DropdownMenuItem>
                                <Link href="/auth/logout" className="no-underline w-full">
                                    <div className="flex flex-rows">
                                        <ArrowRight className="w-6 h-6" />
                                        <p className="ml-3 font-semibold">Logout</p>
                                    </div>
                                </Link>
                            </DropdownMenuItem>
                        ) : null}
                    </DropdownMenuContent>
                </DropdownMenu>
            </SidebarMenuItem>
        </SidebarMenu>
    );
}
