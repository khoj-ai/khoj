"use client";

import styles from "./settings.module.css";

import { Suspense, useEffect, useState } from "react";

import { useUserConfig, ModelOptions } from "../common/auth";
import { Button } from "@/components/ui/button";
import {
    Card,
    CardContent,
    CardFooter,
    CardHeader,
} from "@/components/ui/card";
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuRadioGroup,
  DropdownMenuRadioItem,
  DropdownMenuTrigger,
} from "@/components/ui/dropdown-menu"

import { ArrowRight } from "@phosphor-icons/react";

import NavMenu from "../components/navMenu/navMenu";
import SidePanel from "../components/sidePanel/chatHistorySidePanel";
import Loading from "../components/loading/loading";


interface DropdownComponentProps {
    items: ModelOptions[];
    selected: number;
}

const DropdownComponent: React.FC<DropdownComponentProps> = ({items, selected}) => {
    const [position, setPosition] = useState(selected?.toString() ?? "0");

    return !!selected && (
        <div className="overflow-hidden">
            <DropdownMenu>
                <DropdownMenuTrigger asChild className="w-full">
                    <Button variant="outline" className="justify-start">
                        {items.find(item => item.id === Number(position))?.name}
                    </Button>
                </DropdownMenuTrigger>
                <DropdownMenuContent>
                    <DropdownMenuRadioGroup value={position.toString()} onValueChange={setPosition}>
                        {items.map((item) => (
                            <DropdownMenuRadioItem value={item.id.toString()}>
                                {item.name}
                            </DropdownMenuRadioItem>
                        ))}
                    </DropdownMenuRadioGroup>
                </DropdownMenuContent>
            </DropdownMenu>
        </div>
    );
}

export default function SettingsView() {
    const [title, setTitle] = useState("Settings");
    const [isMobileWidth, setIsMobileWidth] = useState(false);
    const userConfig = useUserConfig(true);
    const cardClassName = "w-1/3 grid grid-flow-column border border-gray-300 shadow-md rounded-lg";

    useEffect(() => {
        setIsMobileWidth(window.innerWidth < 786);
        const handleResize = () => setIsMobileWidth(window.innerWidth < 786);
        window.addEventListener('resize', handleResize);
        return () => window.removeEventListener('resize', handleResize);
    }, []);

    return !!userConfig && (
        <div id="page" className={styles.page}>
            <title>
                {title}
            </title>
            <div className={styles.sidePanel}>
                <SidePanel
                    webSocketConnected={true}
                    conversationId={null}
                    uploadedFiles={[]}
                    isMobileWidth={isMobileWidth}
                />
            </div>
            <div className={styles.content}>
                <NavMenu selected="Settings" title="Settings" showLogo={true} />
                <div className={styles.contentBody}>
                    <Suspense fallback={<Loading />}>
                        <div id="content" className="grid grid-flow-column gap-16 m-8">
                            <div className="section grid gap-8">
                                <div className="text-4xl">Profile</div>
                                <div className="cards flex flex-wrap gap-16">
                                    <Card className={cardClassName}>
                                        <CardHeader className="text-xl">Name</CardHeader>
                                        <CardContent className="overflow-hidden">
                                            <input type="text" className="w-full border border-gray-300 rounded-lg p-4" defaultValue={userConfig.given_name} />
                                        </CardContent>
                                        <CardFooter className="flex flex-wrap gap-4">
                                            <Button variant="outline" size="sm" className="border-green-400">Save</Button>
                                        </CardFooter>
                                    </Card>
                                </div>
                            </div>
                            <div className="section grid gap-8">
                                <div className="text-4xl">Content</div>
                                <div className="cards flex flex-wrap gap-16">
                                    <Card className={cardClassName}>
                                        <CardHeader className="text-xl">Files</CardHeader>
                                        <CardContent className="overflow-hidden">
                                            Manage your synced files
                                        </CardContent>
                                        <CardFooter className="flex flex-wrap gap-4">
                                            <Button variant="outline" size="sm" className="border-green-400">{userConfig.enabled_content_source.computer ? "Update" : "Setup"} <ArrowRight className="inline ml-2" weight="bold"/></Button>
                                            <Button variant="outline" size="sm" className={`${userConfig.enabled_content_source.computer ? "border-red-400" : "hidden"}`}>Disable</Button>
                                        </CardFooter>
                                    </Card>
                                    <Card className={cardClassName}>
                                        <CardHeader className="text-xl">Github</CardHeader>
                                        <CardContent className="overflow-hidden">
                                            Set repositories to index
                                        </CardContent>
                                        <CardFooter className="flex flex-wrap gap-4">
                                            <Button variant="outline" size="sm" className="border-green-400">{userConfig.enabled_content_source.github ? "Update" : "Setup"} <ArrowRight className="inline ml-2" weight="bold"/></Button>
                                            <Button variant="outline" size="sm" className={`${userConfig.enabled_content_source.github ? "border-red-400" : "hidden"}`}>Disable</Button>
                                        </CardFooter>
                                    </Card>
                                    <Card className={cardClassName}>
                                        <CardHeader className="text-xl">Notion</CardHeader>
                                        <CardContent className="overflow-hidden">
                                            Sync your Notion pages
                                        </CardContent>
                                        <CardFooter className="flex flex-wrap gap-4">
                                            <Button variant="outline" size="sm" className="border-green-400">{userConfig.enabled_content_source.notion ? "Update" : "Setup"} <ArrowRight className="inline ml-2" weight="bold"/></Button>
                                            <Button variant="outline" size="sm" className={`${userConfig.enabled_content_source.notion ? "border-red-400" : "hidden"}`}>Disable</Button>
                                        </CardFooter>
                                    </Card>
                                    <Card className={cardClassName}>
                                        <CardHeader className="text-xl">Language</CardHeader>
                                        <CardContent className="overflow-hidden">
                                            <DropdownComponent
                                                items={userConfig.search_model_options}
                                                selected={userConfig.selected_search_model_config}
                                            />
                                        </CardContent>
                                        <CardFooter className="flex flex-wrap gap-4">
                                            <Button variant="outline" size="sm" className="border-green-400">Save</Button>
                                        </CardFooter>
                                    </Card>
                                </div>
                            </div>
                            <div className="section grid gap-8">
                                <div className="text-4xl">Features</div>
                                <div className="cards flex flex-wrap gap-16">
                                    <Card className={cardClassName}>
                                        <CardHeader className="text-xl">Chat</CardHeader>
                                        <CardContent className="overflow-hidden">
                                            <DropdownComponent
                                                items={userConfig.chat_model_options}
                                                selected={userConfig.selected_chat_model_config}
                                            />
                                        </CardContent>
                                        <CardFooter className="flex flex-wrap gap-4">
                                            <Button variant="outline" size="sm" className="border-green-400">Save</Button>
                                        </CardFooter>
                                    </Card>
                                    <Card className={cardClassName}>
                                        <CardHeader className="text-xl">Paint</CardHeader>
                                        <CardContent className="overflow-hidden">
                                            <DropdownComponent
                                                items={userConfig.paint_model_options}
                                                selected={userConfig.selected_paint_model_config}
                                            />
                                        </CardContent>
                                        <CardFooter className="flex flex-wrap gap-4">
                                            <Button variant="outline" size="sm" className="border-green-400">Save</Button>
                                        </CardFooter>
                                    </Card>
                                    <Card className={cardClassName}>
                                        <CardHeader className="text-xl">Voice</CardHeader>
                                        <CardContent className="overflow-hidden">
                                            <DropdownComponent
                                                items={userConfig.voice_model_options}
                                                selected={userConfig.selected_voice_model_config}
                                            />
                                        </CardContent>
                                        <CardFooter className="flex flex-wrap gap-4">
                                            <Button variant="outline" size="sm" className="border-green-400">Save</Button>
                                        </CardFooter>
                                    </Card>
                                </div>
                            </div>
                        </div>
                    </Suspense>
                </div>
            </div>
        </div>
    );
}
