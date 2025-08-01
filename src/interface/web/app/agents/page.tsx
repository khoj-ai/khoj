"use client";

import styles from "./agents.module.css";

import useSWR from "swr";

import { useEffect, useState } from "react";

import {
    useAuthenticatedData,
    UserProfile,
    ModelOptions,
    useUserConfig,
    isUserSubscribed,
} from "../common/auth";

import { Lightning, Plus } from "@phosphor-icons/react";
import { z } from "zod";
import { Dialog, DialogContent, DialogHeader, DialogTrigger } from "@/components/ui/dialog";
import LoginPrompt from "../components/loginPrompt/loginPrompt";
import { InlineLoading } from "../components/loading/loading";
import { Alert, AlertDescription } from "@/components/ui/alert";
import { useIsDarkMode, useIsMobileWidth } from "../common/utils";
import {
    AgentCard,
    EditAgentSchema,
    AgentModificationForm,
    AgentData,
} from "@/app/components/agentCard/agentCard";

import { useForm } from "react-hook-form";
import { zodResolver } from "@hookform/resolvers/zod";
import { SidebarInset, SidebarProvider, SidebarTrigger } from "@/components/ui/sidebar";
import { AppSidebar } from "../components/appSidebar/appSidebar";
import { Separator } from "@/components/ui/separator";
import { KhojLogoType } from "../components/logo/khojLogo";
import { DialogTitle } from "@radix-ui/react-dialog";
import Link from "next/link";

const agentsFetcher = () =>
    window
        .fetch("/api/agents")
        .then((res) => res.json())
        .catch((err) => console.log(err));

// A generic fetcher function that uses the fetch API to make a request to a given URL and returns the response as JSON.
const fetcher = (url: string) => fetch(url).then((res) => res.json());

interface CreateAgentCardProps {
    data: AgentData;
    userProfile: UserProfile | null;
    isMobileWidth: boolean;
    filesOptions: string[];
    modelOptions: ModelOptions[];
    selectedChatModelOption: string;
    isSubscribed: boolean;
    setAgentChangeTriggered: (value: boolean) => void;
    inputToolOptions: { [key: string]: string };
    outputModeOptions: { [key: string]: string };
}

function CreateAgentCard(props: CreateAgentCardProps) {
    const [showModal, setShowModal] = useState(false);
    const [errors, setErrors] = useState<string | null>(null);
    const [showLoginPrompt, setShowLoginPrompt] = useState(true);

    const form = useForm<z.infer<typeof EditAgentSchema>>({
        resolver: zodResolver(EditAgentSchema),
        defaultValues: {
            name: props.data.name,
            persona: props.data.persona,
            color: props.data.color,
            icon: props.data.icon,
            privacy_level: props.data.privacy_level,
            chat_model: props.selectedChatModelOption,
            files: [],
        },
    });

    useEffect(() => {
        form.reset({
            name: props.data.name,
            persona: props.data.persona,
            color: props.data.color,
            icon: props.data.icon,
            privacy_level: props.data.privacy_level,
            chat_model: props.selectedChatModelOption,
            files: [],
        });
    }, [props.selectedChatModelOption, props.data]);

    const onSubmit = (values: z.infer<typeof EditAgentSchema>) => {
        let agentsApiUrl = `/api/agents`;

        fetch(agentsApiUrl, {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify(values),
        })
            .then((response) => {
                if (response.status === 200) {
                    form.reset();
                    setShowModal(false);
                    setErrors(null);
                    props.setAgentChangeTriggered(true);
                } else {
                    response.json().then((data) => {
                        console.error(data);
                        if (data.error) {
                            setErrors(data.error);
                        }
                    });
                }
            })
            .catch((error) => {
                console.error("Error:", error);
                setErrors(error);
            });
    };

    return (
        <Dialog open={showModal} onOpenChange={setShowModal}>
            <DialogTrigger>
                <div className="flex items-center text-md gap-2">
                    <Plus />
                    Create Agent
                </div>
            </DialogTrigger>
            <DialogContent
                className={
                    "lg:max-w-screen-lg py-4 overflow-y-scroll h-full md:h-4/6 rounded-lg flex flex-col"
                }
            >
                <DialogHeader>
                    <DialogTitle>Create Agent</DialogTitle>
                </DialogHeader>
                {!props.userProfile && showLoginPrompt && (
                    <LoginPrompt
                        onOpenChange={setShowLoginPrompt}
                        isMobileWidth={props.isMobileWidth}
                    />
                )}
                <AgentModificationForm
                    form={form}
                    onSubmit={onSubmit}
                    create={true}
                    errors={errors}
                    filesOptions={props.filesOptions}
                    modelOptions={props.modelOptions}
                    inputToolOptions={props.inputToolOptions}
                    outputModeOptions={props.outputModeOptions}
                    isSubscribed={props.isSubscribed}
                />
            </DialogContent>
        </Dialog>
    );
}

export interface AgentConfigurationOptions {
    input_tools: { [key: string]: string };
    output_modes: { [key: string]: string };
}

export default function Agents() {
    const { data, error, mutate } = useSWR<AgentData[]>("agents", agentsFetcher, {
        revalidateOnFocus: false,
    });
    const {
        data: authenticatedData,
        error: authenticationError,
        isLoading: authenticationLoading,
    } = useAuthenticatedData();
    const { data: userConfig } = useUserConfig(true);
    const [showLoginPrompt, setShowLoginPrompt] = useState(false);
    const isMobileWidth = useIsMobileWidth();

    const [personalAgents, setPersonalAgents] = useState<AgentData[]>([]);
    const [publicAgents, setPublicAgents] = useState<AgentData[]>([]);

    const [agentSlug, setAgentSlug] = useState<string | null>(null);

    const { data: filesData, error: fileError } = useSWR<string[]>(
        userConfig ? "/api/content/computer" : null,
        fetcher,
    );

    const { data: agentConfigurationOptions, error: agentConfigurationOptionsError } =
        useSWR<AgentConfigurationOptions>("/api/agents/options", fetcher);

    const [agentChangeTriggered, setAgentChangeTriggered] = useState(false);

    useEffect(() => {
        if (agentChangeTriggered) {
            mutate();
            setAgentChangeTriggered(false);
        }
    }, [agentChangeTriggered]);

    useEffect(() => {
        if (data) {
            const personalAgents = data.filter(
                (agent) => agent.creator === authenticatedData?.username,
            );
            setPersonalAgents(personalAgents);

            // Public agents are agents that are not private and not created by the user
            const publicAgents = data.filter(
                (agent) =>
                    agent.privacy_level !== "private" &&
                    agent.creator !== authenticatedData?.username,
            );
            setPublicAgents(publicAgents);

            if (typeof window !== "undefined") {
                const searchParams = new URLSearchParams(window.location.search);
                const agentSlug = searchParams.get("agent");

                // Search for the agent with the slug in the URL
                if (agentSlug) {
                    setAgentSlug(agentSlug);
                    let selectedAgent = data.find((agent) => agent.slug === agentSlug);

                    // If the agent is not found in all the returned agents, check in the public agents. The code may be running 2x after either agent data or authenticated data is retrieved.
                    if (!selectedAgent) {
                        selectedAgent = publicAgents.find((agent) => agent.slug === agentSlug);
                    }

                    if (!selectedAgent) {
                        // See if the agent is accessible as a protected agent.
                        fetch(`/api/agents/${agentSlug}`)
                            .then((res) => {
                                if (res.status === 404) {
                                    throw new Error("Agent not found");
                                }
                                return res.json();
                            })
                            .then((agent: AgentData) => {
                                if (agent.privacy_level === "protected") {
                                    setPublicAgents((prev) => [...prev, agent]);
                                }
                            });
                    }
                }
            }
        }
    }, [data, authenticatedData]);

    if (error) {
        return (
            <main className={styles.main}>
                <div className={`${styles.titleBar} text-5xl`}>Agents</div>
                <div className={styles.agentList}>Error loading agents</div>
            </main>
        );
    }

    if (!data) {
        return (
            <main className={styles.main}>
                <div className={styles.agentList}>
                    <InlineLoading /> booting up your agents
                </div>
            </main>
        );
    }

    const modelOptions: ModelOptions[] = userConfig?.chat_model_options || [];
    const selectedChatModelOption: number = userConfig?.selected_chat_model_config || 0;
    const isSubscribed: boolean = userConfig?.is_active || false;

    // The default model option should map to the item in the modelOptions array that has the same id as the selectedChatModelOption
    const defaultModelOption = modelOptions.find(
        (modelOption) => modelOption.id === selectedChatModelOption,
    );

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
                        <h2 className="text-lg">Agents</h2>
                    )}
                </header>
                <main className={`w-full mx-auto`}>
                    <div className={`grid w-full mx-auto`}>
                        <div className={`${styles.pageLayout} w-full`}>
                            <div className={`pt-6 md:pt-8 flex justify-between`}>
                                <h1 className="text-3xl flex items-center">Agents</h1>
                                <div className="ml-auto float-right border p-2 pt-3 rounded-xl font-bold hover:bg-stone-100 dark:hover:bg-neutral-900">
                                    <CreateAgentCard
                                        data={{
                                            slug: "",
                                            name: "",
                                            persona: "",
                                            color: "",
                                            icon: "",
                                            privacy_level: "private",
                                            managed_by_admin: false,
                                            chat_model: "",
                                            input_tools: [],
                                            output_modes: [],
                                            is_hidden: false,
                                        }}
                                        userProfile={
                                            authenticationLoading
                                                ? null
                                                : (authenticatedData ?? null)
                                        }
                                        isMobileWidth={isMobileWidth}
                                        filesOptions={filesData || []}
                                        modelOptions={userConfig?.chat_model_options || []}
                                        selectedChatModelOption={defaultModelOption?.name || ""}
                                        isSubscribed={isSubscribed}
                                        setAgentChangeTriggered={setAgentChangeTriggered}
                                        inputToolOptions={
                                            agentConfigurationOptions?.input_tools || {}
                                        }
                                        outputModeOptions={
                                            agentConfigurationOptions?.output_modes || {}
                                        }
                                    />
                                </div>
                            </div>
                            {showLoginPrompt && (
                                <LoginPrompt
                                    onOpenChange={setShowLoginPrompt}
                                    isMobileWidth={isMobileWidth}
                                />
                            )}
                            <Alert className="bg-secondary border-none my-4">
                                <AlertDescription>
                                    <Lightning
                                        weight={"fill"}
                                        className="h-4 w-4 text-purple-400 inline"
                                    />
                                    <span className="font-bold">How it works</span> Use any of these
                                    specialized personas to tune your conversation to your needs.
                                    {!isSubscribed && (
                                        <span>
                                            {" "}
                                            <Link href="/settings" className="font-bold">
                                                Upgrade your plan
                                            </Link>{" "}
                                            to leverage custom models. You will fallback to the
                                            default model when chatting.
                                        </span>
                                    )}
                                </AlertDescription>
                            </Alert>
                            <div className="pt-6 md:pt-8">
                                <div className={`${styles.agentList}`}>
                                    {authenticatedData &&
                                        personalAgents.map((agent) => (
                                            <AgentCard
                                                key={agent.slug}
                                                data={agent}
                                                userProfile={authenticatedData}
                                                isMobileWidth={isMobileWidth}
                                                filesOptions={filesData ?? []}
                                                selectedChatModelOption={
                                                    defaultModelOption?.name || ""
                                                }
                                                isSubscribed={isSubscribed}
                                                setAgentChangeTriggered={setAgentChangeTriggered}
                                                modelOptions={userConfig?.chat_model_options || []}
                                                editCard={true}
                                                agentSlug={agentSlug || ""}
                                                inputToolOptions={
                                                    agentConfigurationOptions?.input_tools || {}
                                                }
                                                outputModeOptions={
                                                    agentConfigurationOptions?.output_modes || {}
                                                }
                                            />
                                        ))}
                                </div>
                            </div>
                            <div className="pt-6 md:pt-8">
                                <h2 className="text-2xl">Explore</h2>
                                <div className={`${styles.agentList}`}>
                                    {!authenticationLoading &&
                                        publicAgents.map((agent) => (
                                            <AgentCard
                                                key={agent.slug}
                                                data={agent}
                                                userProfile={authenticatedData || null}
                                                isMobileWidth={isMobileWidth}
                                                editCard={false}
                                                filesOptions={filesData ?? []}
                                                selectedChatModelOption={
                                                    defaultModelOption?.name || ""
                                                }
                                                isSubscribed={isSubscribed}
                                                setAgentChangeTriggered={setAgentChangeTriggered}
                                                modelOptions={userConfig?.chat_model_options || []}
                                                agentSlug={agentSlug || ""}
                                                inputToolOptions={
                                                    agentConfigurationOptions?.input_tools || {}
                                                }
                                                outputModeOptions={
                                                    agentConfigurationOptions?.output_modes || {}
                                                }
                                            />
                                        ))}
                                </div>
                            </div>
                        </div>
                    </div>
                </main>
            </SidebarInset>
        </SidebarProvider>
    );
}
