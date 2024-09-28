"use client";

import styles from "./agents.module.css";

import useSWR from "swr";

import { useEffect, useRef, useState } from "react";

import {
    useAuthenticatedData,
    UserProfile,
    ModelOptions,
    useUserConfig,
    UserConfig,
    SubscriptionStates,
} from "../common/auth";
import { Button } from "@/components/ui/button";

import {
    PaperPlaneTilt,
    Lightning,
    Plus,
    Circle,
    Info,
    Check,
    ShieldWarning,
    Lock,
    Book,
    Brain,
    Waveform,
    CaretUpDown,
    Globe,
    LockOpen,
    FloppyDisk,
    DotsThreeCircleVertical,
    DotsThreeVertical,
    Pencil,
    Trash,
} from "@phosphor-icons/react";
import { set, z } from "zod";
import { Card, CardContent, CardFooter, CardHeader, CardTitle } from "@/components/ui/card";
import {
    Dialog,
    DialogContent,
    DialogFooter,
    DialogHeader,
    DialogTrigger,
} from "@/components/ui/dialog";
import {
    Drawer,
    DrawerClose,
    DrawerContent,
    DrawerDescription,
    DrawerFooter,
    DrawerHeader,
    DrawerTitle,
    DrawerTrigger,
} from "@/components/ui/drawer";
import LoginPrompt from "../components/loginPrompt/loginPrompt";
import { InlineLoading } from "../components/loading/loading";
import SidePanel from "../components/sidePanel/chatHistorySidePanel";
import { getAvailableIcons, getIconFromIconName } from "../common/iconUtils";
import { convertColorToTextClass, tailwindColors } from "../common/colorUtils";
import { Alert, AlertDescription } from "@/components/ui/alert";
import { useIsMobileWidth } from "../common/utils";

import {
    Form,
    FormControl,
    FormDescription,
    FormField,
    FormItem,
    FormLabel,
    FormMessage,
} from "@/components/ui/form";
import { useForm, UseFormReturn } from "react-hook-form";
import { Input } from "@/components/ui/input";
import { zodResolver } from "@hookform/resolvers/zod";
import { Textarea } from "@/components/ui/textarea";
import {
    Select,
    SelectContent,
    SelectItem,
    SelectTrigger,
    SelectValue,
} from "@/components/ui/select";
import { DialogTitle } from "@radix-ui/react-dialog";
import { Collapsible, CollapsibleContent, CollapsibleTrigger } from "@/components/ui/collapsible";
import { cn } from "@/lib/utils";
import {
    Command,
    CommandEmpty,
    CommandGroup,
    CommandInput,
    CommandItem,
    CommandList,
} from "@/components/ui/command";
import { uploadDataForIndexing } from "../common/chatFunctions";
import {
    AlertDialog,
    AlertDialogAction,
    AlertDialogContent,
    AlertDialogDescription,
    AlertDialogHeader,
    AlertDialogTitle,
} from "@/components/ui/alert-dialog";
import { Progress } from "@/components/ui/progress";
import { Popover, PopoverContent, PopoverTrigger } from "@/components/ui/popover";
import { Sheet, SheetContent, SheetHeader, SheetTrigger } from "@/components/ui/sheet";
import ShareLink from "../components/shareLink/shareLink";

export interface AgentData {
    slug: string;
    name: string;
    persona: string;
    color: string;
    icon: string;
    privacy_level: string;
    files?: string[];
    creator?: string;
    managed_by_admin: boolean;
    chat_model: string;
}

async function openChat(slug: string, userData: UserProfile | null) {
    const unauthenticatedRedirectUrl = `/login?next=/agents?agent=${slug}`;
    if (!userData) {
        window.location.href = unauthenticatedRedirectUrl;
        return;
    }

    const response = await fetch(`/api/chat/sessions?agent_slug=${slug}`, { method: "POST" });
    const data = await response.json();
    if (response.status == 200) {
        window.location.href = `/chat?conversationId=${data.conversation_id}`;
    } else if (response.status == 403 || response.status == 401) {
        window.location.href = unauthenticatedRedirectUrl;
    } else {
        alert("Failed to start chat session");
    }
}

function Badge(props: { icon: JSX.Element; text: string }) {
    // Always convert text to proper case (e.g., "public" -> "Public")
    const displayBadgeText = props.text.replace(/^\w/, (c) => c.toUpperCase());

    return (
        <div className="flex items-center space-x-2 rounded-full border-accent-500 border p-1.5">
            <div className="text-muted-foreground">{props.icon}</div>
            <div className="text-muted-foreground">{displayBadgeText}</div>
        </div>
    );
}

const agentsFetcher = () =>
    window
        .fetch("/api/agents")
        .then((res) => res.json())
        .catch((err) => console.log(err));

// A generic fetcher function that uses the fetch API to make a request to a given URL and returns the response as JSON.
const fetcher = (url: string) => fetch(url).then((res) => res.json());

interface AgentCardProps {
    data: AgentData;
    userProfile: UserProfile | null;
    isMobileWidth: boolean;
    editCard: boolean;
    filesOptions: string[];
    modelOptions: ModelOptions[];
    selectedChatModelOption: string;
    isSubscribed: boolean;
    setAgentChangeTriggered: (value: boolean) => void;
    agentSlug: string;
}

function AgentCard(props: AgentCardProps) {
    const [showModal, setShowModal] = useState(props.agentSlug === props.data.slug);
    const [showLoginPrompt, setShowLoginPrompt] = useState(false);
    const [errors, setErrors] = useState<string | null>(null);

    let lockIcon = <Lock />;

    if (props.data.privacy_level === "public") {
        lockIcon = <Globe />;
    } else if (props.data.privacy_level === "protected") {
        lockIcon = <LockOpen />;
    }

    const userData = props.userProfile;

    const form = useForm<z.infer<typeof EditAgentSchema>>({
        resolver: zodResolver(EditAgentSchema),
        defaultValues: {
            name: props.data.name,
            persona: props.data.persona,
            color: props.data.color,
            icon: props.data.icon,
            privacy_level: props.data.privacy_level,
            chat_model: props.data.chat_model,
            files: props.data.files,
        },
    });

    if (showModal) {
        window.history.pushState(
            {},
            `Khoj AI - Agent ${props.data.slug}`,
            `/agents?agent=${props.data.slug}`,
        );
    }

    const onSubmit = (values: z.infer<typeof EditAgentSchema>) => {
        let agentsApiUrl = `/api/agents`;
        let method = props.editCard ? "PATCH" : "POST";

        fetch(agentsApiUrl, {
            method: method,
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify(values),
        })
            .then((response) => {
                console.log(response);
                if (response.status === 200) {
                    form.reset();
                    setShowModal(false);
                    setErrors(null);
                    props.setAgentChangeTriggered(true);
                } else {
                    response.json().then((data) => {
                        console.error(data);
                        form.clearErrors();
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

    const stylingString = convertColorToTextClass(props.data.color);

    return (
        <Card
            className={`shadow-sm bg-gradient-to-b from-white 20% to-${props.data.color ? props.data.color : "gray"}-100/50 dark:from-[hsl(var(--background))] dark:to-${props.data.color ? props.data.color : "gray"}-950/50 rounded-xl hover:shadow-md`}
        >
            {showLoginPrompt && (
                <LoginPrompt
                    loginRedirectMessage={`Sign in to start chatting with ${props.data.name}`}
                    onOpenChange={setShowLoginPrompt}
                />
            )}
            <CardHeader>
                <CardTitle>
                    {!props.isMobileWidth ? (
                        <Dialog
                            open={showModal}
                            onOpenChange={() => {
                                setShowModal(!showModal);
                                window.history.pushState({}, `Khoj AI - Agents`, `/agents`);
                            }}
                        >
                            <DialogTrigger>
                                <div className="flex items-center relative top-2">
                                    {getIconFromIconName(props.data.icon, props.data.color)}
                                    {props.data.name}
                                </div>
                            </DialogTrigger>
                            <div className="flex float-right">
                                {props.editCard && (
                                    <div className="float-right">
                                        <Popover>
                                            <PopoverTrigger>
                                                <Button
                                                    className={`bg-[hsl(var(--background))] w-14 h-14 rounded-xl border dark:border-neutral-700 shadow-sm hover:bg-stone-100 dark:hover:bg-neutral-900`}
                                                >
                                                    <DotsThreeVertical
                                                        className={`w-6 h-6 ${convertColorToTextClass(props.data.color)}`}
                                                    />
                                                </Button>
                                            </PopoverTrigger>
                                            <PopoverContent
                                                className="w-fit grid p-1"
                                                side={"bottom"}
                                                align={"end"}
                                            >
                                                <Button
                                                    className="items-center justify-start"
                                                    variant={"ghost"}
                                                    onClick={() => setShowModal(true)}
                                                >
                                                    <Pencil className="w-4 h-4 mr-2" />
                                                    Edit
                                                </Button>
                                                {props.editCard &&
                                                    props.data.privacy_level !== "private" && (
                                                        <ShareLink
                                                            buttonTitle="Share"
                                                            title="Share Agent"
                                                            description="Share a link to this agent with others. They'll be able to chat with it, and ask questions to all of its knowledge base."
                                                            buttonVariant={"ghost" as const}
                                                            includeIcon={true}
                                                            url={`${window.location.origin}/agents?agent=${props.data.slug}`}
                                                        />
                                                    )}
                                                {props.data.creator === userData?.username && (
                                                    <Button
                                                        className="items-center justify-start"
                                                        variant={"destructive"}
                                                        onClick={() => {
                                                            fetch(
                                                                `/api/agents/${props.data.slug}`,
                                                                {
                                                                    method: "DELETE",
                                                                },
                                                            ).then(() => {
                                                                props.setAgentChangeTriggered(true);
                                                            });
                                                        }}
                                                    >
                                                        <Trash className="w-4 h-4 mr-2" />
                                                        Delete
                                                    </Button>
                                                )}
                                            </PopoverContent>
                                        </Popover>
                                    </div>
                                )}
                                <div className="float-right">
                                    {props.userProfile ? (
                                        <Button
                                            className={`bg-[hsl(var(--background))] w-14 h-14 rounded-xl border dark:border-neutral-700 shadow-sm hover:bg-stone-100 dark:hover:bg-neutral-900`}
                                            onClick={() => openChat(props.data.slug, userData)}
                                        >
                                            <PaperPlaneTilt
                                                className={`w-6 h-6 ${convertColorToTextClass(props.data.color)}`}
                                            />
                                        </Button>
                                    ) : (
                                        <Button
                                            className={`bg-[hsl(var(--background))] w-14 h-14 rounded-xl border dark:border-neutral-700 shadow-sm hover:bg-stone-100 dark:hover:bg-neutral-900`}
                                            onClick={() => setShowLoginPrompt(true)}
                                        >
                                            <PaperPlaneTilt
                                                className={`w-6 h-6 ${convertColorToTextClass(props.data.color)}`}
                                            />
                                        </Button>
                                    )}
                                </div>
                            </div>
                            {props.editCard ? (
                                <DialogContent
                                    className={"lg:max-w-screen-lg overflow-y-scroll max-h-screen"}
                                >
                                    <DialogTitle>
                                        Edit <b>{props.data.name}</b>
                                    </DialogTitle>
                                    <AgentModificationForm
                                        form={form}
                                        onSubmit={onSubmit}
                                        create={false}
                                        errors={errors}
                                        filesOptions={props.filesOptions}
                                        modelOptions={props.modelOptions}
                                        slug={props.data.slug}
                                    />
                                </DialogContent>
                            ) : (
                                <DialogContent className="whitespace-pre-line max-h-[80vh]">
                                    <DialogHeader>
                                        <div className="flex items-center">
                                            {getIconFromIconName(props.data.icon, props.data.color)}
                                            <p className="font-bold text-lg">{props.data.name}</p>
                                        </div>
                                    </DialogHeader>
                                    <div className="max-h-[60vh] overflow-y-scroll text-neutral-500 dark:text-white">
                                        {props.data.persona}
                                    </div>
                                    <div className="flex flex-wrap items-center gap-1">
                                        {props.editCard && (
                                            <Badge
                                                icon={lockIcon}
                                                text={props.data.privacy_level}
                                            />
                                        )}
                                        {props.data.files && props.data.files.length > 0 && (
                                            <Badge icon={<Book />} text={`knowledge`} />
                                        )}
                                        <Badge icon={<Brain />} text={props.data.chat_model} />
                                    </div>
                                    <DialogFooter>
                                        <Button
                                            className={`pt-6 pb-6 ${stylingString} bg-white dark:bg-[hsl(var(--background))] text-neutral-500 dark:text-white border-2 border-stone-100 shadow-sm rounded-xl hover:bg-stone-100 dark:hover:bg-neutral-900 dark:border-neutral-700`}
                                            onClick={() => {
                                                openChat(props.data.slug, userData);
                                                setShowModal(false);
                                            }}
                                        >
                                            <PaperPlaneTilt
                                                className={`w-6 h-6 m-2 ${convertColorToTextClass(props.data.color)}`}
                                            />
                                            Start Chatting
                                        </Button>
                                    </DialogFooter>
                                </DialogContent>
                            )}
                        </Dialog>
                    ) : (
                        <Drawer
                            open={showModal}
                            onOpenChange={(open) => {
                                setShowModal(open);
                                window.history.pushState({}, `Khoj AI - Agents`, `/agents`);
                            }}
                        >
                            <DrawerTrigger>
                                <div className="flex items-center">
                                    {getIconFromIconName(props.data.icon, props.data.color)}
                                    {props.data.name}
                                </div>
                            </DrawerTrigger>
                            <div className="flex float-right">
                                {props.editCard && (
                                    <div className="float-right">
                                        <Popover>
                                            <PopoverTrigger>
                                                <Button
                                                    className={`bg-[hsl(var(--background))] w-14 h-14 rounded-xl border dark:border-neutral-700 shadow-sm hover:bg-stone-100 dark:hover:bg-neutral-900`}
                                                >
                                                    <DotsThreeVertical
                                                        className={`w-6 h-6 ${convertColorToTextClass(props.data.color)}`}
                                                    />
                                                </Button>
                                            </PopoverTrigger>
                                            <PopoverContent
                                                className="w-fit grid p-1"
                                                side={"bottom"}
                                                align={"end"}
                                            >
                                                <Button
                                                    className="items-center justify-start"
                                                    variant={"ghost"}
                                                    onClick={() => setShowModal(true)}
                                                >
                                                    <Pencil className="w-4 h-4 mr-2" />
                                                    Edit
                                                </Button>
                                                {props.editCard &&
                                                    props.data.privacy_level !== "private" && (
                                                        <ShareLink
                                                            buttonTitle="Share"
                                                            title="Share Agent"
                                                            description="Share a link to this agent with others. They'll be able to chat with it, and ask questions to all of its knowledge base."
                                                            buttonVariant={"ghost" as const}
                                                            includeIcon={true}
                                                            url={`${window.location.origin}/agents?agent=${props.data.slug}`}
                                                        />
                                                    )}
                                                {props.data.creator === userData?.username && (
                                                    <Button
                                                        className="items-center justify-start"
                                                        variant={"destructive"}
                                                        onClick={() => {
                                                            fetch(
                                                                `/api/agents/${props.data.slug}`,
                                                                {
                                                                    method: "DELETE",
                                                                },
                                                            ).then(() => {
                                                                props.setAgentChangeTriggered(true);
                                                            });
                                                        }}
                                                    >
                                                        <Trash className="w-4 h-4 mr-2" />
                                                        Delete
                                                    </Button>
                                                )}
                                            </PopoverContent>
                                        </Popover>
                                    </div>
                                )}
                                <div className="float-right">
                                    {props.userProfile ? (
                                        <Button
                                            className={`bg-[hsl(var(--background))] w-14 h-14 rounded-xl border dark:border-neutral-700 shadow-sm hover:bg-stone-100 dark:hover:bg-neutral-900`}
                                            onClick={() => openChat(props.data.slug, userData)}
                                        >
                                            <PaperPlaneTilt
                                                className={`w-6 h-6 ${convertColorToTextClass(props.data.color)}`}
                                            />
                                        </Button>
                                    ) : (
                                        <Button
                                            className={`bg-[hsl(var(--background))] w-14 h-14 rounded-xl border dark:border-neutral-700 shadow-sm hover:bg-stone-100 dark:hover:bg-neutral-900`}
                                            onClick={() => setShowLoginPrompt(true)}
                                        >
                                            <PaperPlaneTilt
                                                className={`w-6 h-6 ${convertColorToTextClass(props.data.color)}`}
                                            />
                                        </Button>
                                    )}
                                </div>
                            </div>
                            {props.editCard ? (
                                <DrawerContent className="whitespace-pre-line p-2">
                                    <AgentModificationForm
                                        form={form}
                                        onSubmit={onSubmit}
                                        create={false}
                                        errors={errors}
                                        filesOptions={props.filesOptions}
                                        modelOptions={props.modelOptions}
                                        slug={props.data.slug}
                                    />
                                </DrawerContent>
                            ) : (
                                <DrawerContent className="whitespace-pre-line p-2">
                                    <DrawerHeader>
                                        <DrawerTitle>{props.data.name}</DrawerTitle>
                                        <DrawerDescription>Persona</DrawerDescription>
                                    </DrawerHeader>
                                    {props.data.persona}
                                    <div className="flex flex-wrap items-center gap-1">
                                        {props.editCard && (
                                            <Badge
                                                icon={lockIcon}
                                                text={props.data.privacy_level}
                                            />
                                        )}
                                        {props.data.files && props.data.files.length > 0 && (
                                            <Badge icon={<Book />} text={`knowledge`} />
                                        )}
                                        <Badge icon={<Brain />} text={props.data.chat_model} />
                                    </div>
                                    <DrawerFooter>
                                        <DrawerClose>Done</DrawerClose>
                                    </DrawerFooter>
                                </DrawerContent>
                            )}
                        </Drawer>
                    )}
                </CardTitle>
            </CardHeader>
            <CardContent>
                <div className={styles.agentPersonality}>
                    <button
                        className={`${styles.infoButton} text-neutral-500 dark:text-white`}
                        onClick={() => setShowModal(true)}
                    >
                        <p>{props.data.persona}</p>
                    </button>
                </div>
            </CardContent>
            <CardFooter>
                <div className="flex flex-wrap items-center gap-1">
                    {props.editCard && <Badge icon={lockIcon} text={props.data.privacy_level} />}
                    {props.data.files && props.data.files.length > 0 && (
                        <Badge icon={<Book />} text={`knowledge`} />
                    )}
                    <Badge icon={<Brain />} text={props.data.chat_model} />
                </div>
            </CardFooter>
        </Card>
    );
}

const EditAgentSchema = z.object({
    name: z.string({ required_error: "Name is required" }).min(1, "Name is required"),
    persona: z
        .string({ required_error: "Personality is required" })
        .min(1, "Personality is required"),
    color: z.string({ required_error: "Color is required" }).min(1, "Color is required"),
    icon: z.string({ required_error: "Icon is required" }).min(1, "Icon is required"),
    privacy_level: z
        .string({ required_error: "Privacy level is required" })
        .min(1, "Privacy level is required"),
    chat_model: z
        .string({ required_error: "Chat model is required" })
        .min(1, "Chat model is required"),
    files: z.array(z.string()).default([]).optional(),
});

interface AgentModificationFormProps {
    form: UseFormReturn<z.infer<typeof EditAgentSchema>>;
    onSubmit: (values: z.infer<typeof EditAgentSchema>) => void;
    create?: boolean;
    errors?: string | null;
    modelOptions: ModelOptions[];
    filesOptions: string[];
    slug?: string;
}

function AgentModificationForm(props: AgentModificationFormProps) {
    const [isSaving, setIsSaving] = useState(false);

    const iconOptions = getAvailableIcons();
    const colorOptions = tailwindColors;
    const colorOptionClassName = convertColorToTextClass(props.form.getValues("color"));

    const [isDragAndDropping, setIsDragAndDropping] = useState(false);
    const [warning, setWarning] = useState<string | null>(null);
    const [error, setError] = useState<string | null>(null);
    const [uploading, setUploading] = useState(false);
    const [progressValue, setProgressValue] = useState(0);
    const [uploadedFiles, setUploadedFiles] = useState<string[]>([]);
    const fileInputRef = useRef<HTMLInputElement>(null);

    useEffect(() => {
        if (!uploading) {
            setProgressValue(0);
        }

        if (uploading) {
            const interval = setInterval(() => {
                setProgressValue((prev) => {
                    const increment = Math.floor(Math.random() * 5) + 1; // Generates a random number between 1 and 5
                    const nextValue = prev + increment;
                    return nextValue < 100 ? nextValue : 100; // Ensures progress does not exceed 100
                });
            }, 800);
            return () => clearInterval(interval);
        }
    }, [uploading]);

    useEffect(() => {
        if (uploadedFiles.length > 0) {
            handleAgentFileChange(uploadedFiles);
        }
    }, [uploadedFiles]);

    function handleDragOver(event: React.DragEvent<HTMLDivElement>) {
        event.preventDefault();
        setIsDragAndDropping(true);
    }

    function handleDragLeave(event: React.DragEvent<HTMLDivElement>) {
        event.preventDefault();
        setIsDragAndDropping(false);
    }

    function handleDragAndDropFiles(event: React.DragEvent<HTMLDivElement>) {
        event.preventDefault();
        setIsDragAndDropping(false);

        if (!event.dataTransfer.files) return;

        uploadFiles(event.dataTransfer.files);
    }

    function uploadFiles(files: FileList) {
        uploadDataForIndexing(files, setWarning, setUploading, setError, setUploadedFiles);
    }

    function openFileInput() {
        if (fileInputRef && fileInputRef.current) {
            fileInputRef.current.click();
        }
    }

    function handleFileChange(event: React.ChangeEvent<HTMLInputElement>) {
        if (!event.target.files) return;

        uploadFiles(event.target.files);
    }

    const handleAgentFileChange = (files: string[]) => {
        for (const file of files) {
            const currentFiles = props.form.getValues("files") || [];
            const newFiles = currentFiles.includes(file)
                ? currentFiles.filter((item) => item !== file)
                : [...currentFiles, file];
            props.form.setValue("files", newFiles);
        }
    };

    const privacyOptions = ["public", "private", "protected"];

    return (
        <Form {...props.form}>
            <form
                onSubmit={props.form.handleSubmit((values) => {
                    props.onSubmit(values);
                    setIsSaving(true);
                })}
                className="space-y-6"
            >
                <FormField
                    control={props.form.control}
                    name="name"
                    render={({ field }) => (
                        <FormItem className="space-y-1">
                            <FormLabel>Name</FormLabel>
                            <FormDescription>
                                What should this agent be called? Pick something descriptive &
                                memorable.
                            </FormDescription>
                            <FormControl>
                                <Input placeholder="Biologist" {...field} />
                            </FormControl>
                            <FormMessage />
                        </FormItem>
                    )}
                />

                <FormField
                    control={props.form.control}
                    name="persona"
                    render={({ field }) => (
                        <FormItem className="space-y-1">
                            <FormLabel>Personality</FormLabel>
                            <FormDescription>
                                What is the personality, thought process, or tuning of this agent?
                            </FormDescription>
                            <FormControl>
                                <Textarea
                                    placeholder="You are an excellent biologist, at the top of your field in marine biology."
                                    {...field}
                                />
                            </FormControl>
                            <FormMessage />
                        </FormItem>
                    )}
                />
                <FormField
                    control={props.form.control}
                    name="privacy_level"
                    render={({ field }) => (
                        <FormItem className="space-y-1">
                            <FormLabel>
                                <div>Privacy Level</div>
                            </FormLabel>
                            <FormDescription>
                                <Collapsible>
                                    <CollapsibleTrigger asChild>
                                        <Button variant={"ghost" as const} className="px-0 py-1">
                                            <span className="items-center flex gap-1 text-sm">
                                                <Info className="inline" />
                                                <p className="text-sm">What is this?</p>
                                            </span>
                                        </Button>
                                    </CollapsibleTrigger>
                                    <CollapsibleContent>
                                        <b>Private</b>: only visible to you.
                                        <br />
                                        <b>Protected</b>: visible to anyone with a link.
                                        <br />
                                        <b>Public</b>: visible to everyone.
                                        <br />
                                        Note that we will review all public agents before they are
                                        made accessible to others.
                                    </CollapsibleContent>
                                </Collapsible>
                            </FormDescription>
                            <Select onValueChange={field.onChange} defaultValue={field.value}>
                                <FormControl>
                                    <SelectTrigger className="w-[200px]">
                                        <SelectValue placeholder="private" />
                                    </SelectTrigger>
                                </FormControl>
                                <SelectContent className="items-center space-y-1 inline-flex flex-col">
                                    {privacyOptions.map((privacyOption) => (
                                        <SelectItem key={privacyOption} value={privacyOption}>
                                            <div className="flex items-center space-x-2">
                                                {privacyOption}
                                            </div>
                                        </SelectItem>
                                    ))}
                                </SelectContent>
                            </Select>
                            <FormMessage />
                        </FormItem>
                    )}
                />
                <FormField
                    control={props.form.control}
                    name="chat_model"
                    render={({ field }) => (
                        <FormItem className="space-y-1">
                            <FormLabel>Chat Model</FormLabel>
                            <FormDescription>
                                Which chat model should this agent use?
                            </FormDescription>
                            <Select onValueChange={field.onChange} defaultValue={field.value}>
                                <FormControl>
                                    <SelectTrigger className="text-left">
                                        <SelectValue />
                                    </SelectTrigger>
                                </FormControl>
                                <SelectContent className="items-start space-y-1 inline-flex flex-col">
                                    {props.modelOptions.map((modelOption) => (
                                        <SelectItem key={modelOption.id} value={modelOption.name}>
                                            <div className="flex items-center space-x-2">
                                                {modelOption.name}
                                            </div>
                                        </SelectItem>
                                    ))}
                                </SelectContent>
                            </Select>
                            <FormMessage />
                        </FormItem>
                    )}
                />
                <FormField
                    control={props.form.control}
                    name="files"
                    render={({ field }) => (
                        <FormItem className="flex flex-col">
                            <FormLabel>Knowledge Base</FormLabel>
                            <FormDescription>
                                Which files should this agent have access to?{" "}
                                <a href="/settings">Manage files</a>.
                            </FormDescription>
                            <Collapsible>
                                <CollapsibleTrigger className="flex items-center justify-between text-sm gap-2">
                                    <CaretUpDown />
                                    {field.value && field.value.length > 0
                                        ? `${field.value.length} files selected`
                                        : "Select files"}
                                </CollapsibleTrigger>
                                <CollapsibleContent>
                                    <Command>
                                        <AlertDialog open={warning !== null || error != null}>
                                            <AlertDialogContent>
                                                <AlertDialogHeader>
                                                    <AlertDialogTitle>Alert</AlertDialogTitle>
                                                </AlertDialogHeader>
                                                <AlertDialogDescription>
                                                    {warning || error}
                                                </AlertDialogDescription>
                                                <AlertDialogAction
                                                    className="bg-slate-400 hover:bg-slate-500"
                                                    onClick={() => {
                                                        setWarning(null);
                                                        setError(null);
                                                        setUploading(false);
                                                    }}
                                                >
                                                    Close
                                                </AlertDialogAction>
                                            </AlertDialogContent>
                                        </AlertDialog>
                                        <div
                                            className={`flex flex-col h-full cursor-pointer`}
                                            onDragOver={handleDragOver}
                                            onDragLeave={handleDragLeave}
                                            onDrop={handleDragAndDropFiles}
                                            onClick={openFileInput}
                                        >
                                            <input
                                                type="file"
                                                multiple
                                                ref={fileInputRef}
                                                style={{ display: "none" }}
                                                onChange={handleFileChange}
                                            />
                                            <div className="flex-none p-4">
                                                {uploading && (
                                                    <Progress
                                                        indicatorColor="bg-slate-500"
                                                        className="w-full h-2 rounded-full"
                                                        value={progressValue}
                                                    />
                                                )}
                                            </div>
                                            <div
                                                className={`flex-none p-4 bg-secondary border-b ${isDragAndDropping ? "animate-pulse" : ""} rounded-lg`}
                                            >
                                                <div className="flex items-center justify-center w-full h-16 border-2 border-dashed border-gray-300 rounded-lg">
                                                    {isDragAndDropping ? (
                                                        <div className="flex items-center justify-center w-full h-full">
                                                            <Waveform className="h-6 w-6 mr-2" />
                                                            <span>Drop files to upload</span>
                                                        </div>
                                                    ) : (
                                                        <div className="flex items-center justify-center w-full h-full">
                                                            <Plus className="h-6 w-6 mr-2" />
                                                            <span>Drag and drop files here</span>
                                                        </div>
                                                    )}
                                                </div>
                                            </div>
                                        </div>
                                        <CommandInput placeholder="Select files..." />
                                        <CommandList>
                                            <CommandEmpty>No files found.</CommandEmpty>
                                            <CommandGroup>
                                                {props.filesOptions.map((file) => (
                                                    <CommandItem
                                                        value={file}
                                                        key={file}
                                                        onSelect={() => {
                                                            const currentFiles =
                                                                props.form.getValues("files") || [];
                                                            const newFiles = currentFiles.includes(
                                                                file,
                                                            )
                                                                ? currentFiles.filter(
                                                                      (item) => item !== file,
                                                                  )
                                                                : [...currentFiles, file];
                                                            props.form.setValue("files", newFiles);
                                                        }}
                                                    >
                                                        <Check
                                                            className={cn(
                                                                "mr-2 h-4 w-4",
                                                                field.value &&
                                                                    field.value.includes(file)
                                                                    ? "opacity-100"
                                                                    : "opacity-0",
                                                            )}
                                                        />
                                                        {file}
                                                    </CommandItem>
                                                ))}
                                            </CommandGroup>
                                        </CommandList>
                                    </Command>
                                </CollapsibleContent>
                            </Collapsible>
                        </FormItem>
                    )}
                />
                <div className="grid">
                    <FormLabel className="mb-2">Look & Feel</FormLabel>
                    <div className="flex gap-1 justify-left">
                        <FormField
                            control={props.form.control}
                            name="color"
                            render={({ field }) => (
                                <FormItem className="space-y-3">
                                    <Select
                                        onValueChange={field.onChange}
                                        defaultValue={field.value}
                                    >
                                        <FormControl>
                                            <SelectTrigger className="w-[200px]">
                                                <SelectValue placeholder="Color" />
                                            </SelectTrigger>
                                        </FormControl>
                                        <SelectContent className="items-center space-y-1 inline-flex flex-col">
                                            {colorOptions.map((colorOption) => (
                                                <SelectItem key={colorOption} value={colorOption}>
                                                    <div className="flex items-center space-x-2">
                                                        <Circle
                                                            className={`w-6 h-6 mr-2 ${convertColorToTextClass(colorOption)}`}
                                                            weight="fill"
                                                        />
                                                        {colorOption}
                                                    </div>
                                                </SelectItem>
                                            ))}
                                        </SelectContent>
                                    </Select>
                                    <FormMessage />
                                </FormItem>
                            )}
                        />

                        <FormField
                            control={props.form.control}
                            name="icon"
                            render={({ field }) => (
                                <FormItem className="space-y-3">
                                    <Select
                                        onValueChange={field.onChange}
                                        defaultValue={field.value}
                                    >
                                        <FormControl>
                                            <SelectTrigger className="w-[200px]">
                                                <SelectValue placeholder="Icon" />
                                            </SelectTrigger>
                                        </FormControl>
                                        <SelectContent className="items-center space-y-1 inline-flex flex-col">
                                            {iconOptions.map((iconOption) => (
                                                <SelectItem key={iconOption} value={iconOption}>
                                                    <div className="flex items-center space-x-2">
                                                        {getIconFromIconName(
                                                            iconOption,
                                                            props.form.getValues("color"),
                                                            "w-6",
                                                            "h-6",
                                                        )}
                                                        {iconOption}
                                                    </div>
                                                </SelectItem>
                                            ))}
                                        </SelectContent>
                                    </Select>
                                    <FormMessage />
                                </FormItem>
                            )}
                        />
                    </div>
                </div>
                {props.errors && (
                    <Alert className="bg-secondary border-none my-4">
                        <AlertDescription>
                            <ShieldWarning
                                weight="fill"
                                className="h-4 w-4 text-yellow-400 inline"
                            />
                            <span>{props.errors}</span>
                        </AlertDescription>
                    </Alert>
                )}
                <fieldset>
                    <Button
                        type="submit"
                        variant={"ghost"}
                        disabled={isSaving}
                        className={`items-center ${isSaving ? "bg-stone-100 dark:bg-neutral-900" : ""} text-white ${colorOptionClassName}`}
                    >
                        <FloppyDisk className="h-4 w-4 mr-2" />
                        {isSaving ? "Booting..." : "Save"}
                    </Button>
                    {!!!props.create && props.form.getValues("privacy_level") !== "private" && (
                        <ShareLink
                            buttonTitle="Share"
                            title="Share Agent"
                            description="Share a link to this agent with others. They'll be able to chat with it, and ask questions to all of its knowledge base."
                            buttonVariant={"ghost" as const}
                            buttonClassName={`${colorOptionClassName}`}
                            includeIcon={true}
                            url={`${window.location.origin}/agents?agent=${props.slug}`}
                        />
                    )}
                </fieldset>
            </form>
        </Form>
    );
}

interface CreateAgentCardProps {
    data: AgentData;
    userProfile: UserProfile | null;
    isMobileWidth: boolean;
    filesOptions: string[];
    modelOptions: ModelOptions[];
    selectedChatModelOption: string;
    isSubscribed: boolean;
    setAgentChangeTriggered: (value: boolean) => void;
}

function CreateAgentCard(props: CreateAgentCardProps) {
    const [showModal, setShowModal] = useState(false);
    const [errors, setErrors] = useState<string | null>(null);

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
        console.log(JSON.stringify(values));

        let agentsApiUrl = `/api/agents`;

        fetch(agentsApiUrl, {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify(values),
        })
            .then((response) => {
                console.log(response);
                if (response.status === 200) {
                    form.reset();
                    setShowModal(false);
                    setErrors(null);
                    props.setAgentChangeTriggered(true);
                } else {
                    response.json().then((data) => {
                        console.error(data);
                        form.clearErrors();
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

    if (props.isMobileWidth) {
        return (
            <Drawer open={showModal} onOpenChange={setShowModal}>
                <DrawerTrigger>
                    <div className="flex items-center">
                        <Plus />
                        Create Agent
                    </div>
                </DrawerTrigger>
                <DrawerContent className="p-2">
                    <DrawerHeader>
                        <DrawerTitle>Create Agent</DrawerTitle>
                    </DrawerHeader>
                    <AgentModificationForm
                        form={form}
                        onSubmit={onSubmit}
                        create={true}
                        errors={errors}
                        filesOptions={props.filesOptions}
                        modelOptions={props.modelOptions}
                    />
                    <DrawerFooter>
                        <DrawerClose>Dismiss</DrawerClose>
                    </DrawerFooter>
                </DrawerContent>
            </Drawer>
        );
    }

    return (
        <Dialog open={showModal} onOpenChange={setShowModal}>
            <DialogTrigger>
                <div className="flex items-center text-md gap-2">
                    <Plus />
                    Create Agent
                </div>
            </DialogTrigger>
            <DialogContent className={"lg:max-w-screen-lg overflow-y-scroll max-h-screen"}>
                <DialogHeader>Create Agent</DialogHeader>
                <AgentModificationForm
                    form={form}
                    onSubmit={onSubmit}
                    create={true}
                    errors={errors}
                    filesOptions={props.filesOptions}
                    modelOptions={props.modelOptions}
                />
            </DialogContent>
        </Dialog>
    );
}

export default function Agents() {
    const { data, error, mutate } = useSWR<AgentData[]>("agents", agentsFetcher, {
        revalidateOnFocus: false,
    });
    const authenticatedData = useAuthenticatedData();
    const { userConfig } = useUserConfig(true);
    const [showLoginPrompt, setShowLoginPrompt] = useState(false);
    const isMobileWidth = useIsMobileWidth();

    const [personalAgents, setPersonalAgents] = useState<AgentData[]>([]);
    const [publicAgents, setPublicAgents] = useState<AgentData[]>([]);

    const [agentSlug, setAgentSlug] = useState<string | null>(null);

    const { data: filesData, error: fileError } = useSWR<string[]>(
        "/api/content/computer",
        fetcher,
    );

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
                    const selectedAgent = data.find((agent) => agent.slug === agentSlug);
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
    }, [data]);

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
    const isSubscribed: boolean =
        (userConfig?.subscription_state &&
            userConfig?.subscription_state in
                [SubscriptionStates.SUBSCRIBED, SubscriptionStates.TRIAL]) ||
        false;

    // The default model option should map to the item in the modelOptions array that has the same id as the selectedChatModelOption
    const defaultModelOption = modelOptions.find(
        (modelOption) => modelOption.id === selectedChatModelOption,
    );

    return (
        <main className={`w-full mx-auto`}>
            <div className={`grid w-full mx-auto`}>
                <div className={`${styles.sidePanel} top-0`}>
                    <SidePanel
                        conversationId={null}
                        uploadedFiles={[]}
                        isMobileWidth={isMobileWidth}
                    />
                </div>
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
                                }}
                                userProfile={authenticatedData}
                                isMobileWidth={isMobileWidth}
                                filesOptions={filesData || []}
                                modelOptions={userConfig?.chat_model_options || []}
                                selectedChatModelOption={defaultModelOption?.name || ""}
                                isSubscribed={isSubscribed}
                                setAgentChangeTriggered={setAgentChangeTriggered}
                            />
                        </div>
                    </div>
                    {showLoginPrompt && (
                        <LoginPrompt
                            loginRedirectMessage="Sign in to start chatting with a specialized agent"
                            onOpenChange={setShowLoginPrompt}
                        />
                    )}
                    <Alert className="bg-secondary border-none my-4">
                        <AlertDescription>
                            <Lightning weight={"fill"} className="h-4 w-4 text-purple-400 inline" />
                            <span className="font-bold">How it works</span> Use any of these
                            specialized personas to tune your conversation to your needs.
                        </AlertDescription>
                    </Alert>
                    <div className="pt-6 md:pt-8">
                        <div className={`${styles.agentList}`}>
                            {personalAgents.map((agent) => (
                                <AgentCard
                                    key={agent.slug}
                                    data={agent}
                                    userProfile={authenticatedData}
                                    isMobileWidth={isMobileWidth}
                                    filesOptions={filesData ?? []}
                                    selectedChatModelOption={defaultModelOption?.name || ""}
                                    isSubscribed={isSubscribed}
                                    setAgentChangeTriggered={setAgentChangeTriggered}
                                    modelOptions={userConfig?.chat_model_options || []}
                                    editCard={true}
                                    agentSlug={agentSlug || ""}
                                />
                            ))}
                        </div>
                    </div>
                    <div className="pt-6 md:pt-8">
                        <h2 className="text-2xl">Explore</h2>
                        <div className={`${styles.agentList}`}>
                            {publicAgents.map((agent) => (
                                <AgentCard
                                    key={agent.slug}
                                    data={agent}
                                    userProfile={authenticatedData}
                                    isMobileWidth={isMobileWidth}
                                    editCard={false}
                                    filesOptions={filesData ?? []}
                                    selectedChatModelOption={defaultModelOption?.name || ""}
                                    isSubscribed={isSubscribed}
                                    setAgentChangeTriggered={setAgentChangeTriggered}
                                    modelOptions={userConfig?.chat_model_options || []}
                                    agentSlug={agentSlug || ""}
                                />
                            ))}
                        </div>
                    </div>
                </div>
            </div>
        </main>
    );
}
