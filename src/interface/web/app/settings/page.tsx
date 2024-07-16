"use client";

import styles from "./settings.module.css";

import { Suspense, useEffect, useState } from "react";

import { useUserConfig } from "../common/auth";
import { Button } from "@/components/ui/button";
import {
    Card,
    CardContent,
    CardFooter,
    CardHeader,
} from "@/components/ui/card";
import NavMenu from "../components/navMenu/navMenu";
import SidePanel from "../components/sidePanel/chatHistorySidePanel";
import Loading from "../components/loading/loading";


interface CardComponentProps {
    header: string;
    children: React.ReactNode;
    footer: React.ReactNode;
}

const CardComponent: React.FC<CardComponentProps> = ({ header, children, footer }) => {
    return (
        <Card className="w-1/3 grid grid-flow-column border border-gray-300 shadow-md rounded-lg">
            <CardHeader className="text-xl">{header}</CardHeader>
            <CardContent>
                {children}
            </CardContent>
            <CardFooter className="flex gap-4">
                {footer}
            </CardFooter>
        </Card>
    );
};

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

    return (
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
                                        <CardContent>
                                            <input type="text" className="border border-gray-300 rounded-lg p-4" defaultValue={userConfig?.given_name} />
                                        </CardContent>
                                        <CardFooter className="flex gap-4">
                                            <Button variant="outline" size="sm">Save</Button>
                                        </CardFooter>
                                    </Card>
                                </div>
                            </div>
                            <div className="section grid gap-8">
                                <div className="text-4xl">Content</div>
                                <div className="cards flex flex-wrap gap-16">
                                    <Card className={cardClassName}>
                                        <CardHeader className="text-xl">Files</CardHeader>
                                        <CardContent>
                                            Manage your synced files
                                        </CardContent>
                                        <CardFooter className="flex gap-4">
                                            <Button variant="outline" size="sm">Update</Button>
                                            <Button variant="outline" size="sm">Disable</Button>
                                        </CardFooter>
                                    </Card>
                                    <Card className={cardClassName}>
                                        <CardHeader className="text-xl">Github</CardHeader>
                                        <CardContent>
                                            Set repositories to index
                                        </CardContent>
                                        <CardFooter className="flex gap-4">
                                            <Button variant="outline" size="sm">Setup</Button>
                                            <Button variant="outline" size="sm">Sync</Button>
                                        </CardFooter>
                                    </Card>
                                    <Card className={cardClassName}>
                                        <CardHeader className="text-xl">Notion</CardHeader>
                                        <CardContent>
                                            Sync your Notion pages
                                        </CardContent>
                                        <CardFooter className="flex gap-4">
                                            <Button variant="outline" size="sm">Setup</Button>
                                            <Button variant="outline" size="sm">Sync</Button>
                                        </CardFooter>
                                    </Card>
                                    <Card className={cardClassName}>
                                        <CardHeader className="text-xl">Language</CardHeader>
                                        <CardContent>
                                            <select className="border border-gray-300 rounded-lg">
                                                {userConfig?.search_model_options.map(option => (
                                                    <option
                                                        key={option.id}
                                                        value={option.id}
                                                        selected={option.id === userConfig?.selected_search_model_config}
                                                    >
                                                        {option.name}
                                                    </option>
                                                ))}
                                            </select>
                                        </CardContent>
                                        <CardFooter className="flex gap-4">
                                            <Button variant="outline" size="sm">Save</Button>
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
