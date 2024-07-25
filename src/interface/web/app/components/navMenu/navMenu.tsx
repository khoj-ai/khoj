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
    DropdownMenuLabel,
    DropdownMenuSeparator,
    DropdownMenuTrigger,
} from "@/components/ui/dropdown-menu";
import { Toggle } from '@/components/ui/toggle';
import { Moon, Sun, UserCircle, User, Robot, MagnifyingGlass, Question, GearFine, ArrowRight } from '@phosphor-icons/react';
import Image from 'next/image';


interface NavMenuProps {
    selected: string;
    showLogo?: boolean;
    title?: string;
}

export default function NavMenu(props: NavMenuProps) {

    const userData = useAuthenticatedData();
    const [displayTitle, setDisplayTitle] = useState<string | undefined>(props.title);

    const [isMobileWidth, setIsMobileWidth] = useState(false);
    const [darkMode, setDarkMode] = useState(false);
    const [initialLoadDone, setInitialLoadDone] = useState(false);

    useEffect(() => {
        setIsMobileWidth(window.innerWidth < 768);
        if (props.title) {
            setDisplayTitle(props.title);
        } else if (!props.title) {
            setDisplayTitle(undefined);
        }

    }, [props.title]);

    useEffect(() => {

        const mq = window.matchMedia(
            "(prefers-color-scheme: dark)"
        );

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
            <div className={`text-nowrap text-ellipsis overflow-hidden max-w-screen-md grid items-top font-bold mr-8`}>
                {displayTitle && <h2 className={`text-lg text-ellipsis whitespace-nowrap overflow-x-hidden`} >{displayTitle}</h2>}
                {
                    !displayTitle && props.showLogo &&
                    <Link href='/'>
                        <Image
                            src="/khoj-logo.svg"
                            alt="Khoj"
                            width={52}
                            height={52} />
                    </Link>
                }
            </div>
            {
                isMobileWidth ?
                    <DropdownMenu>
                        <DropdownMenuTrigger>=</DropdownMenuTrigger>
                        <DropdownMenuContent>
                            <DropdownMenuItem>
                                <Link href='/' className={`${props.selected.toLowerCase() === 'chat' ? styles.selected : ''} hover:bg-background no-underline`}>Chat</Link>
                            </DropdownMenuItem>
                            <DropdownMenuItem>
                                <Link href='/agents' className={`${props.selected.toLowerCase() === 'agent' ? styles.selected : ''} hover:bg-background no-underline`}>Agents</Link>
                            </DropdownMenuItem>
                            <DropdownMenuItem>
                                <Link href='/automations' className={`${props.selected.toLowerCase() === 'automations' ? styles.selected : ''} hover:bg-background no-underline`}>Automations</Link>
                            </DropdownMenuItem>
                            {userData && <>
                                <DropdownMenuSeparator />
                                <DropdownMenuLabel>Profile</DropdownMenuLabel>
                                <DropdownMenuItem>
                                    <Link href="/settings">Settings</Link>
                                </DropdownMenuItem>
                                <DropdownMenuItem>
                                    <Link href="https://docs.khoj.dev">Help</Link>
                                </DropdownMenuItem>
                                <DropdownMenuItem>
                                    <Link href="/auth/logout">Logout</Link>
                                </DropdownMenuItem>
                            </>}
                        </DropdownMenuContent>
                    </DropdownMenu>
                    :
                    <Menubar className='border-none hover:bg-stone-100 dark:hover:bg-neutral-900 bg-none'>

                        <MenubarMenu>
                            <MenubarTrigger>
                                <Avatar className="h-8 w-8">
                                    <AvatarImage src={userData?.photo} alt="user profile" />
                                    <AvatarFallback>
                                        {userData?.username[0]}
                                    </AvatarFallback>
                                </Avatar>
                            </MenubarTrigger>
                            <MenubarContent align="end" className="rounded-xl">
                                <MenubarItem>
                                    <div
                                        onClick={() => {
                                            setDarkMode(!darkMode)
                                        }
                                        }
                                        className="flex flex-rows">
                                        {darkMode ? <Sun className="w-6 h-6" /> : <Moon className="w-6 h-6" />}
                                        <p className="ml-3 pt-[2px] font-semibold">{darkMode ? 'Light Mode' : 'Dark Mode'}</p>
                                    </div>
                                </MenubarItem>
                                {userData &&
                                    <>
                                        <MenubarItem>
                                            <Link href="/agents" className="no-underline">
                                                <div className="flex flex-rows">
                                                <User className="w-6 h-6"/>
                                                <p className="ml-3 pt-[2px] font-semibold">Agents</p>
                                                </div>
                                            </Link>
                                        </MenubarItem>
                                        <MenubarItem>
                                            <Link href="/automations" className="no-underline">
                                                <div className="flex flex-rows">
                                                <Robot className="w-6 h-6"/>
                                                <p className="ml-3 pt-[2px] font-semibold">Automations</p>
                                                </div>
                                            </Link>
                                        </MenubarItem>
                                        <MenubarItem>
                                            <Link href="/search" className="no-underline">
                                                <div className="flex flex-rows">
                                                <MagnifyingGlass className="w-6 h-6"/>
                                                <p className="ml-3 pt-[2px] font-semibold">Search</p>
                                                </div>
                                            </Link>
                                        </MenubarItem>
                                        <MenubarSeparator className="dark:bg-white height-[2px] bg-black" />
                                        <MenubarItem>
                                            <Link href="/settings" className="no-underline">
                                                <div className="flex flex-rows">
                                                <GearFine className="w-6 h-6"/>
                                                <p className="ml-3 pt-[2px] font-semibold">Settings</p>
                                                </div>
                                            </Link>
                                        </MenubarItem>
                                        <MenubarItem>
                                            <Link href="https://docs.khoj.dev" className="no-underline">
                                                <div className="flex flex-rows">
                                                <Question className="w-6 h-6"/>
                                                <p className="ml-3 pt-[2px] font-semibold">Help</p>
                                                </div>
                                            </Link>
                                        </MenubarItem>
                                        <MenubarItem>
                                            <Link href="/auth/logout" className="no-underline">
                                                <div className="flex flex-rows">
                                                <ArrowRight className="w-6 h-6"/>
                                                <p className="ml-3 pt-[2px] font-semibold">Logout</p>
                                                </div>
                                            </Link>
                                        </MenubarItem>
                                    </>
                                }
                            </MenubarContent>
                        </MenubarMenu>
                    </Menubar>
            }
        </div>
    )
}
