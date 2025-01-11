"use client";

import { useState, useEffect } from "react";
import { SidebarInset, SidebarProvider, SidebarTrigger } from "@/components/ui/sidebar";
import { AppSidebar } from "../components/appSidebar/appSidebar";
import { Separator } from "@/components/ui/separator";
import { KhojLogoType } from "../components/logo/khojLogo";
import { Card, CardHeader, CardTitle, CardContent } from "@/components/ui/card";
import { useIsMobileWidth } from "../common/utils";
import { InlineLoading } from "../components/loading/loading";

interface FileObject {
    file_name: string;
    raw_text: string;
}

export default function KnowledgeBase() {
    const [files, setFiles] = useState<FileObject[]>([]);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);
    const isMobileWidth = useIsMobileWidth();

    useEffect(() => {
        const fetchFiles = async () => {
            try {
                const response = await fetch("/api/content/all");
                if (!response.ok) throw new Error("Failed to fetch files");

                const filesList = await response.json();
                if (Array.isArray(filesList)) {
                    setFiles(filesList.toSorted());
                }
            } catch (error) {
                setError("Failed to load files");
                console.error("Error fetching files:", error);
            } finally {
                setLoading(false);
            }
        };

        fetchFiles();
    }, []);

    return (
        <SidebarProvider>
            <AppSidebar conversationId={""} />
            <SidebarInset>
                <header className="flex h-16 shrink-0 items-center gap-2 border-b px-4">
                    <SidebarTrigger className="-ml-1" />
                    <Separator orientation="vertical" className="mr-2 h-4" />
                    {isMobileWidth ? (
                        <a className="p-0 no-underline" href="/">
                            <KhojLogoType className="h-auto w-16" />
                        </a>
                    ) : (
                        <h2 className="text-lg">Knowledge Base</h2>
                    )}
                </header>
                <main>
                    <div className="md:w-3/4 sm:w-full mx-auto pt-6 md:pt-8">
                        {loading && (
                            <div className="mt-4 flex items-center justify-center">
                                <InlineLoading
                                    className="mt-4"
                                    message={"Loading"}
                                    iconClassName="h-5 w-5"
                                />
                            </div>
                        )}
                        {error && <div className="text-red-500">{error}</div>}

                        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
                            {files.map((file, index) => (
                                <Card key={index}>
                                    <CardHeader>
                                        <CardTitle className="text-sm font-medium">
                                            {file.file_name.split("/").pop()}
                                        </CardTitle>
                                    </CardHeader>
                                    <CardContent>
                                        <p className="text-sm text-muted-foreground">
                                            {file.raw_text.slice(0, 100)}...
                                        </p>
                                    </CardContent>
                                </Card>
                            ))}
                        </div>
                    </div>
                </main>
            </SidebarInset>
        </SidebarProvider>
    );
}
