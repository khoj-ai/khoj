'use client'

import styles from './navMenu.module.css';
import Link from 'next/link';
import { useAuthenticatedData } from '@/app/common/auth';
import { useState, useEffect } from 'react';
import { Avatar, AvatarImage, AvatarFallback } from "@/components/ui/avatar";

import {
    Menubar,
    MenubarContent,
    MenubarItem,
    MenubarMenu,
    MenubarSeparator,
    MenubarTrigger,
} from "@/components/ui/menubar";

import {
    DropdownMenu,
    DropdownMenuContent,
    DropdownMenuItem,
    DropdownMenuSeparator,
    DropdownMenuTrigger,
} from "@/components/ui/dropdown-menu";
import { Moon, Sun, UserCircle, User, Robot, MagnifyingGlass, Question, GearFine, ArrowRight } from '@phosphor-icons/react';

export default function NavMenu() {

    const userData = useAuthenticatedData();
    const [isMobileWidth, setIsMobileWidth] = useState(false);
    const [darkMode, setDarkMode] = useState(false);
    const [initialLoadDone, setInitialLoadDone] = useState(false);

    useEffect(() => {
        setIsMobileWidth(window.innerWidth < 768);

        window.addEventListener('resize', () => {
            setIsMobileWidth(window.innerWidth < 768);
        });

        if (localStorage.getItem('theme') === 'dark') {
            document.documentElement.classList.add('dark');
            setDarkMode(true);
        } else if (localStorage.getItem('theme') === 'light') {
            document.documentElement.classList.remove('dark');
            setDarkMode(false);
        } else {
            const mq = window.matchMedia(
                "(prefers-color-scheme: dark)"
            );

            if (mq.matches) {
                document.documentElement.classList.add('dark');
                setDarkMode(true);
            }
        }

        setInitialLoadDone(true);
    }, []);


    useEffect(() => {
        if (!initialLoadDone) return;
        toggleDarkMode(darkMode);
    }, [darkMode]);

    function toggleDarkMode(darkMode: boolean) {
        if (darkMode) {
            document.documentElement.classList.add('dark');
        } else {
            document.documentElement.classList.remove('dark');
        }
        localStorage.setItem('theme', darkMode ? 'dark' : 'light');
    }

    return (
        <div className={styles.titleBar}>
            {
                isMobileWidth ?
                    <DropdownMenu>
                        <DropdownMenuTrigger>
                            {
                                userData ?
                                    <Avatar className="h-8 w-8">
                                        <AvatarImage src={userData?.photo} alt="user profile" />
                                        <AvatarFallback>
                                            {userData?.username[0]}
                                        </AvatarFallback>
                                    </Avatar>
                                    :
                                    <UserCircle className="w-6 h-6" />
                            }
                        </DropdownMenuTrigger>
                        <DropdownMenuContent className='gap-2'>
                            <DropdownMenuItem>
                                <div
                                    onClick={() => {
                                        setDarkMode(!darkMode)
                                    }
                                    }
                                    className="flex flex-rows">
                                    {darkMode ? <Sun className="w-6 h-6" /> : <Moon className="w-6 h-6" />}
                                    <p className="ml-3 font-semibold">{darkMode ? 'Light Mode' : 'Dark Mode'}</p>
                                </div>
                            </DropdownMenuItem>
                            <DropdownMenuItem>
                                <Link href="/agents" className="no-underline">
                                    <div className="flex flex-rows">
                                        <User className="w-6 h-6" />
                                        <p className="ml-3 font-semibold">Agents</p>
                                    </div>
                                </Link>
                            </DropdownMenuItem>
                            <DropdownMenuItem>
                                <Link href="/automations" className="no-underline">
                                    <div className="flex flex-rows">
                                        <Robot className="w-6 h-6" />
                                        <p className="ml-3 font-semibold">Automations</p>
                                    </div>
                                </Link>
                            </DropdownMenuItem>
                            <DropdownMenuItem>
                                <Link href="/search" className="no-underline">
                                    <div className="flex flex-rows">
                                        <MagnifyingGlass className="w-6 h-6" />
                                        <p className="ml-3 font-semibold">Search</p>
                                    </div>
                                </Link>
                            </DropdownMenuItem>
                            <>
                                <DropdownMenuSeparator />
                                {userData &&
                                    <DropdownMenuItem>
                                        <Link href="/settings" className="no-underline">
                                            <div className="flex flex-rows">
                                                <GearFine className="w-6 h-6" />
                                                <p className="ml-3 font-semibold">Settings</p>
                                            </div>
                                        </Link>
                                    </DropdownMenuItem>
                                }
                                <DropdownMenuItem>
                                    <Link href="https://docs.khoj.dev" className="no-underline">
                                        <div className="flex flex-rows">
                                            <Question className="w-6 h-6" />
                                            <p className="ml-3 font-semibold">Help</p>
                                        </div>
                                    </Link>
                                </DropdownMenuItem>
                                {
                                    userData ?
                                        <DropdownMenuItem>
                                            <Link href="/auth/logout" className="no-underline">
                                                <div className="flex flex-rows">
                                                    <ArrowRight className="w-6 h-6" />
                                                    <p className="ml-3 font-semibold">Logout</p>
                                                </div>
                                            </Link>
                                        </DropdownMenuItem>
                                        :
                                        <DropdownMenuItem>
                                            <Link href="/auth/login" className="no-underline">
                                                <div className="flex flex-rows">
                                                    <ArrowRight className="w-6 h-6" />
                                                    <p className="ml-3 font-semibold">Login</p>
                                                </div>
                                            </Link>
                                        </DropdownMenuItem>
                                }
                            </>
                        </DropdownMenuContent>
                    </DropdownMenu>
                    :
                    <Menubar className='border-none hover:bg-stone-100 dark:hover:bg-neutral-900 bg-none'>
                        <MenubarMenu>
                            <MenubarTrigger>
                                {
                                    userData ?
                                        <Avatar className="h-8 w-8">
                                            <AvatarImage src={userData?.photo} alt="user profile" />
                                            <AvatarFallback>
                                                {userData?.username[0]}
                                            </AvatarFallback>
                                        </Avatar>
                                        :
                                        <UserCircle className="w-8 h-8" />
                                }
                            </MenubarTrigger>
                            <MenubarContent align="end" className="rounded-xl gap-2">
                                <MenubarItem>
                                    <div
                                        onClick={() => {
                                            setDarkMode(!darkMode)
                                        }
                                        }
                                        className="flex flex-rows">
                                        {darkMode ? <Sun className="w-6 h-6" /> : <Moon className="w-6 h-6" />}
                                        <p className="ml-3 font-semibold">{darkMode ? 'Light Mode' : 'Dark Mode'}</p>
                                    </div>
                                </MenubarItem>
                                <MenubarItem>
                                    <Link href="/agents" className="no-underline">
                                        <div className="flex flex-rows">
                                            <User className="w-6 h-6" />
                                            <p className="ml-3 font-semibold">Agents</p>
                                        </div>
                                    </Link>
                                </MenubarItem>
                                <MenubarItem>
                                    <Link href="/automations" className="no-underline">
                                        <div className="flex flex-rows">
                                            <Robot className="w-6 h-6" />
                                            <p className="ml-3 font-semibold">Automations</p>
                                        </div>
                                    </Link>
                                </MenubarItem>
                                <MenubarItem>
                                    <Link href="/search" className="no-underline">
                                        <div className="flex flex-rows">
                                            <MagnifyingGlass className="w-6 h-6" />
                                            <p className="ml-3 font-semibold">Search</p>
                                        </div>
                                    </Link>
                                </MenubarItem>
                                <>
                                    <MenubarSeparator className="dark:bg-white height-[2px] bg-black" />
                                    <MenubarItem>
                                        <Link href="https://docs.khoj.dev" className="no-underline">
                                            <div className="flex flex-rows">
                                                <Question className="w-6 h-6" />
                                                <p className="ml-3 font-semibold">Help</p>
                                            </div>
                                        </Link>
                                    </MenubarItem>
                                    {
                                        userData &&
                                        <MenubarItem>
                                            <Link href="/settings" className="no-underline">
                                                <div className="flex flex-rows">
                                                    <GearFine className="w-6 h-6" />
                                                    <p className="ml-3 font-semibold">Settings</p>
                                                </div>
                                            </Link>
                                        </MenubarItem>
                                    }
                                    {
                                        userData ?
                                            <MenubarItem>
                                                <Link href="/auth/logout" className="no-underline">
                                                    <div className="flex flex-rows">
                                                        <ArrowRight className="w-6 h-6" />
                                                        <p className="ml-3 font-semibold">Logout</p>
                                                    </div>
                                                </Link>
                                            </MenubarItem>
                                            :
                                            <MenubarItem>
                                                <Link href="/auth/login" className="no-underline">
                                                    <div className="flex flex-rows">
                                                        <ArrowRight className="w-6 h-6" />
                                                        <p className="ml-3 font-semibold">Login</p>
                                                    </div>
                                                </Link>
                                            </MenubarItem>
                                    }
                                </>
                            </MenubarContent>
                        </MenubarMenu>
                    </Menubar>
            }
        </div>
    )
}
