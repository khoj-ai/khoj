import styles from "./agentCard.module.css";

import { useEffect, useRef, useState } from "react";

import { UserProfile, ModelOptions, UserConfig } from "@/app/common/auth";
import { Button } from "@/components/ui/button";

import {
    PaperPlaneTilt,
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
    DotsThreeVertical,
    Pencil,
    Trash,
    ArrowRight,
    ArrowLeft,
} from "@phosphor-icons/react";
import { z, ZodError } from "zod";
import { Card, CardContent, CardFooter, CardHeader, CardTitle } from "@/components/ui/card";
import {
    Dialog,
    DialogContent,
    DialogFooter,
    DialogHeader,
    DialogTrigger,
} from "@/components/ui/dialog";
import LoginPrompt from "@/app/components/loginPrompt/loginPrompt";
import {
    getAvailableIcons,
    getIconForSlashCommand,
    getIconFromIconName,
} from "@/app/common/iconUtils";
import { convertColorToTextClass, tailwindColors } from "@/app/common/colorUtils";
import { Alert, AlertDescription } from "@/components/ui/alert";

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
import { uploadDataForIndexing } from "@/app/common/chatFunctions";
import {
    AlertDialog,
    AlertDialogAction,
    AlertDialogCancel,
    AlertDialogContent,
    AlertDialogDescription,
    AlertDialogFooter,
    AlertDialogHeader,
    AlertDialogTitle,
} from "@/components/ui/alert-dialog";
import { Progress } from "@/components/ui/progress";
import { Popover, PopoverContent, PopoverTrigger } from "@/components/ui/popover";
import ShareLink from "@/app/components/shareLink/shareLink";
import { Tooltip, TooltipContent, TooltipProvider, TooltipTrigger } from "@/components/ui/tooltip";

import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { ScrollArea } from "@/components/ui/scroll-area";

export interface AgentData {
    slug: string;
    name: string;
    persona: string;
    color: string;
    icon: string;
    privacy_level: string;
    files?: string[];
    creator?: string;
    is_creator?: boolean;
    managed_by_admin: boolean;
    chat_model: string;
    input_tools: string[];
    output_modes: string[];
    is_hidden: boolean;
    has_files?: boolean;
}

async function openChat(slug: string, userData: UserProfile | null) {
    const unauthenticatedRedirectUrl = `/login?next=/agents?agent=${slug}`;
    if (!userData) {
        window.location.href = unauthenticatedRedirectUrl;
        return;
    }

    const response = await fetch(`/api/chat/sessions?agent_slug=${encodeURIComponent(slug)}`, {
        method: "POST",
    });
    const data = await response.json();
    if (response.status == 200) {
        window.location.href = `/chat?conversationId=${data.conversation_id}`;
    } else if (response.status == 403 || response.status == 401) {
        window.location.href = unauthenticatedRedirectUrl;
    } else {
        alert("Failed to start chat session");
    }
}

function Badge(props: { icon: JSX.Element; text?: string; hoverText?: string }) {
    // Always convert text to proper case (e.g., "public" -> "Public")
    const displayBadgeText = props.text?.replace(/^\w/, (c) => c.toUpperCase()) || "";

    return (
        <TooltipProvider>
            <Tooltip>
                <TooltipContent asChild>
                    <div className="text-sm">{props.hoverText || displayBadgeText}</div>
                </TooltipContent>
                <TooltipTrigger className="cursor-text">
                    <div className="flex items-center space-x-2 rounded-full border-accent-500 border p-1.5">
                        <div className="text-muted-foreground">{props.icon}</div>
                        {displayBadgeText && displayBadgeText.length > 0 && (
                            <div className="text-muted-foreground text-sm">{displayBadgeText}</div>
                        )}
                    </div>
                </TooltipTrigger>
            </Tooltip>
        </TooltipProvider>
    );
}

export const EditAgentSchema = z.object({
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
    input_tools: z.array(z.string()).default([]).optional(),
    output_modes: z.array(z.string()).default([]).optional(),
});

interface AgentCardProps {
    data: AgentData;
    userProfile: UserProfile | null;
    isMobileWidth: boolean;
    editCard: boolean;
    showChatButton?: boolean;
    filesOptions: string[];
    modelOptions: ModelOptions[];
    selectedChatModelOption: string;
    isSubscribed: boolean;
    setAgentChangeTriggered: (value: boolean) => void;
    agentSlug: string;
    inputToolOptions: { [key: string]: string };
    outputModeOptions: { [key: string]: string };
}

export function AgentCard(props: AgentCardProps) {
    const [showModal, setShowModal] = useState(props.agentSlug === props.data.slug);
    const [showLoginPrompt, setShowLoginPrompt] = useState(false);
    const [errors, setErrors] = useState<string | null>(null);

    let lockIcon = <Lock />;
    let privacyHoverText = "Private agents are only visible to you.";

    if (props.data.privacy_level === "public") {
        lockIcon = <Globe />;
        privacyHoverText = "Public agents are visible to everyone.";
    } else if (props.data.privacy_level === "protected") {
        lockIcon = <LockOpen />;
        privacyHoverText = "Protected agents are visible to anyone with a direct link.";
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
            input_tools: props.data.input_tools,
            output_modes: props.data.output_modes,
        },
    });

    useEffect(() => {
        form.reset({
            name: props.data.name,
            persona: props.data.persona,
            color: props.data.color,
            icon: props.data.icon,
            privacy_level: props.data.privacy_level,
            chat_model: props.data.chat_model,
            files: props.data.files,
            input_tools: props.data.input_tools,
            output_modes: props.data.output_modes,
        });
    }, [props.data]);

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

        let valuesToSend: any = values;

        if (props.editCard) {
            valuesToSend = { ...values, slug: props.data.slug };
        }

        fetch(agentsApiUrl, {
            method: method,
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify(valuesToSend),
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
                form.clearErrors();
            });
    };

    const stylingString = convertColorToTextClass(props.data.color);

    function makeBadgeFooter() {
        return (
            <div className="flex flex-wrap items-center gap-1">
                {props.editCard && (
                    <Badge
                        icon={lockIcon}
                        text={props.data.privacy_level}
                        hoverText={privacyHoverText}
                    />
                )}
                {props.data.files && props.data.files.length > 0 && (
                    <Badge
                        icon={<Book />}
                        text={`knowledge`}
                        hoverText={`The agent has a custom knowledge base with ${props.data.files.length} documents. It can use them to give you answers.`}
                    />
                )}
                <Badge
                    icon={<Brain />}
                    text={props.data.chat_model}
                    hoverText={`The agent uses the ${props.data.chat_model} model to chat with you.`}
                />
                {props.data.output_modes.map((outputMode) => (
                    <Badge
                        key={outputMode}
                        icon={getIconForSlashCommand(outputMode)}
                        hoverText={`${outputMode}: ${props.outputModeOptions[outputMode]}`}
                    />
                ))}
                {props.data.input_tools.map((inputTool) => (
                    <Badge
                        key={inputTool}
                        icon={getIconForSlashCommand(inputTool)}
                        hoverText={`${inputTool}: ${props.inputToolOptions[inputTool]}`}
                    />
                ))}
            </div>
        );
    }

    return (
        <Card className={`shadow-md rounded-xl hover:shadow-lg dark:bg-muted`}>
            {showLoginPrompt && (
                <LoginPrompt
                    onOpenChange={setShowLoginPrompt}
                    isMobileWidth={props.isMobileWidth}
                />
            )}
            <CardHeader>
                <CardTitle>
                    <Dialog
                        open={showModal}
                        onOpenChange={() => {
                            setShowModal(!showModal);
                            window.history.pushState({}, `Khoj AI - Agents`, `/agents`);
                        }}
                    >
                        <DialogTrigger className="focus-visible:outline-none">
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
                                                className={`bg-[hsl(var(--background))] w-10 h-10 p-0 rounded-xl border dark:border-neutral-700 shadow-sm hover:bg-stone-100 dark:hover:bg-neutral-900`}
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
                                                        fetch(`/api/agents/${props.data.slug}`, {
                                                            method: "DELETE",
                                                        }).then(() => {
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
                            {(props.showChatButton ?? true) && (
                                <div className="float-right ml-2">
                                    {props.userProfile ? (
                                        <Button
                                            className={`bg-[hsl(var(--background))] w-10 h-10 p-0 rounded-xl border dark:border-neutral-700 shadow-sm hover:bg-stone-100 dark:hover:bg-neutral-900`}
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
                            )}
                        </div>
                        {props.editCard ? (
                            <DialogContent
                                className={
                                    "lg:max-w-screen-lg py-4 overflow-y-scroll h-full md:h-4/6 rounded-lg flex flex-col"
                                }
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
                                    inputToolOptions={props.inputToolOptions}
                                    isSubscribed={props.isSubscribed}
                                    outputModeOptions={props.outputModeOptions}
                                />
                            </DialogContent>
                        ) : (
                            <DialogContent className="whitespace-pre-line max-h-[80vh] max-w-[90vw] md:max-w-[50vw] rounded-lg">
                                <DialogHeader>
                                    <div className="flex items-center">
                                        {getIconFromIconName(props.data.icon, props.data.color)}
                                        <p className="font-bold text-lg">
                                            <DialogTitle>{props.data.name}</DialogTitle>
                                        </p>
                                    </div>
                                </DialogHeader>
                                <div className="max-h-[60vh] overflow-y-scroll text-neutral-500 dark:text-white">
                                    {props.data.persona}
                                </div>
                                <div className="flex flex-wrap items-center gap-1">
                                    {makeBadgeFooter()}
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
                <div className="flex flex-wrap items-center gap-1">{makeBadgeFooter()}</div>
            </CardFooter>
        </Card>
    );
}

interface AgentModificationFormProps {
    form: UseFormReturn<z.infer<typeof EditAgentSchema>>;
    onSubmit: (values: z.infer<typeof EditAgentSchema>) => void;
    userConfig?: UserConfig;
    create?: boolean;
    errors?: string | null;
    modelOptions: ModelOptions[];
    filesOptions: string[];
    inputToolOptions: { [key: string]: string };
    outputModeOptions: { [key: string]: string };
    slug?: string;
    isSubscribed: boolean;
}

export function AgentModificationForm(props: AgentModificationFormProps) {
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
    const [allFileOptions, setAllFileOptions] = useState<string[]>([]);
    const [currentStep, setCurrentStep] = useState(0);
    const [fileSearchValue, setFileSearchValue] = useState("");

    const [showSubscribeDialog, setShowSubscribeDialog] = useState(false);

    const privacyOptions = ["public", "private", "protected"];

    const basicFields = [
        { name: "name", label: "Name" },
        { name: "persona", label: "Personality" },
        { name: "color", label: "Color" },
        { name: "icon", label: "Icon" },
    ];

    const toolsFields = [
        { name: "input_tools", label: "Input Tools" },
        { name: "output_modes", label: "Output Modes" },
    ];

    const knowledgeBaseFields = [{ name: "files", label: "Knowledge Base" }];

    const customizationFields = [
        { name: "chat_model", label: "Chat Model" },
        { name: "privacy_level", label: "Privacy Level" },
    ];

    const formGroups = [
        { fields: basicFields, label: "1. Basic Settings", tabName: "basic" },
        { fields: customizationFields, label: "2. Model & Privacy", tabName: "customize" },
        { fields: knowledgeBaseFields, label: "3. Knowledge Base", tabName: "knowledge" },
        { fields: toolsFields, label: "4. Tools", tabName: "tools" },
    ];

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
        const currentFiles = props.form.getValues("files") || [];
        const fileOptions = props.filesOptions || [];
        const concatenatedFiles = [...currentFiles, ...fileOptions];
        const fullAllFileOptions = [...allFileOptions, ...concatenatedFiles];
        const dedupedAllFileOptions = Array.from(new Set(fullAllFileOptions));
        setAllFileOptions(dedupedAllFileOptions);
    }, []);

    useEffect(() => {
        if (uploadedFiles.length > 0) {
            handleAgentFileChange(uploadedFiles);
            setAllFileOptions((prev) => [...prev, ...uploadedFiles]);
        }
    }, [uploadedFiles]);

    useEffect(() => {
        if (props.errors) {
            setIsSaving(false);
        }
    }, [props.errors]);

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

    const handleNext = (event: React.MouseEvent<HTMLButtonElement>) => {
        event.preventDefault();
        if (currentStep < formGroups.length - 1) {
            setCurrentStep(currentStep + 1);
        }
    };

    const handlePrevious = (event: React.MouseEvent<HTMLButtonElement>) => {
        event.preventDefault();
        if (currentStep > 0) {
            setCurrentStep(currentStep - 1);
        }
    };

    const handleSubmit = (values: any) => {
        console.log("Submitting", values);
        props.onSubmit(values);
        setIsSaving(true);
    };

    const handleAgentFileChange = (files: string[]) => {
        for (const file of files) {
            const currentFiles = props.form.getValues("files") || [];
            const newFiles = currentFiles.includes(file)
                ? currentFiles.filter((item) => item !== file)
                : [...currentFiles, file];
            props.form.setValue("files", newFiles);
        }
    };

    const areRequiredFieldsCompletedForCurrentStep = (formGroup: {
        fields: { name: string }[];
    }) => {
        try {
            EditAgentSchema.parse(props.form.getValues());
            return true;
        } catch (error) {
            const errors: { [key: string]: string } = (error as ZodError).errors.reduce(
                (acc: any, curr: any) => {
                    acc[curr.path[0]] = curr.message;
                    return acc;
                },
                {},
            );

            for (const field of formGroup.fields) {
                if (errors[field.name]) {
                    return false;
                }
            }

            return true;
        }
    };

    if (showSubscribeDialog) {
        return (
            <AlertDialog open={true}>
                <AlertDialogContent>
                    <AlertDialogHeader>
                        <AlertDialogTitle>Upgrade to Futurist</AlertDialogTitle>
                    </AlertDialogHeader>
                    <AlertDialogDescription>
                        You need to be a Futurist subscriber to create more agents.{" "}
                        <a href="/settings">Upgrade now</a>.
                    </AlertDialogDescription>
                    <AlertDialogFooter>
                        <AlertDialogCancel
                            onClick={() => {
                                setShowSubscribeDialog(false);
                            }}
                        >
                            Cancel
                        </AlertDialogCancel>
                        <AlertDialogAction
                            onClick={() => {
                                window.location.href = "/settings";
                            }}
                        >
                            Continue
                        </AlertDialogAction>
                    </AlertDialogFooter>
                </AlertDialogContent>
            </AlertDialog>
        );
    }

    const renderFormField = (fieldName: string) => {
        switch (fieldName) {
            case "name":
                return (
                    <FormField
                        key={fieldName}
                        control={props.form.control}
                        name="name"
                        render={({ field }) => (
                            <FormItem className="space-y-0 grid gap-2">
                                <FormLabel>Name</FormLabel>
                                <FormDescription>
                                    What should this agent be called? Pick something descriptive &
                                    memorable.
                                </FormDescription>
                                <FormControl>
                                    <Input
                                        className="dark:bg-muted"
                                        placeholder="Biologist"
                                        {...field}
                                    />
                                </FormControl>
                                <FormMessage />
                            </FormItem>
                        )}
                    />
                );
            case "chat_model":
                return (
                    <FormField
                        key={fieldName}
                        control={props.form.control}
                        name="chat_model"
                        render={({ field }) => (
                            <FormItem className="my-2 grid gap-2">
                                <FormLabel>Chat Model</FormLabel>
                                <FormDescription>
                                    {!props.isSubscribed ? (
                                        <p className="text-secondary-foreground">
                                            Upgrade to the <a href="/settings">Futurist plan</a> to
                                            access all models.
                                        </p>
                                    ) : (
                                        <p>Which chat model would you like to use?</p>
                                    )}
                                </FormDescription>
                                <Select onValueChange={field.onChange} defaultValue={field.value}>
                                    <FormControl>
                                        <SelectTrigger className="text-left dark:bg-muted">
                                            <SelectValue />
                                        </SelectTrigger>
                                    </FormControl>
                                    <SelectContent className="items-start space-y-1 inline-flex flex-col">
                                        {props.modelOptions.map((modelOption) => (
                                            <SelectItem
                                                key={modelOption.id}
                                                value={modelOption.name}
                                                disabled={
                                                    !props.isSubscribed &&
                                                    modelOption.tier !== "free"
                                                }
                                            >
                                                <div className="flex items-center space-x-2">
                                                    {modelOption.name}{" "}
                                                    {modelOption.tier === "standard" && (
                                                        <span className="text-green-500 ml-2">
                                                            (Futurist)
                                                        </span>
                                                    )}
                                                </div>
                                            </SelectItem>
                                        ))}
                                    </SelectContent>
                                </Select>
                                <FormMessage />
                            </FormItem>
                        )}
                    />
                );
            case "privacy_level":
                return (
                    <FormField
                        key={fieldName}
                        control={props.form.control}
                        name="privacy_level"
                        render={({ field }) => (
                            <FormItem className="my-2 grid gap-2">
                                <FormLabel>
                                    <div>Privacy Level</div>
                                </FormLabel>
                                <FormDescription>
                                    <Popover>
                                        <PopoverTrigger asChild>
                                            <Button
                                                variant={"ghost" as const}
                                                className="p-0 h-fit"
                                            >
                                                <span className="items-center flex gap-1 text-sm">
                                                    <Info className="inline" />
                                                    <p className="text-sm">Learn more</p>
                                                </span>
                                            </Button>
                                        </PopoverTrigger>
                                        <PopoverContent>
                                            <b>Private</b>: only visible to you.
                                            <br />
                                            <b>Protected</b>: visible to anyone with a link.
                                            <br />
                                            <b>Public</b>: visible to everyone.
                                            <br />
                                            All public agents will be reviewed by us before they are
                                            launched.
                                        </PopoverContent>
                                    </Popover>
                                </FormDescription>
                                <Select onValueChange={field.onChange} defaultValue={field.value}>
                                    <FormControl>
                                        <SelectTrigger className="w-[200px] dark:bg-muted">
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
                );
            case "color":
                return (
                    <FormField
                        key={fieldName}
                        control={props.form.control}
                        name="color"
                        render={({ field }) => (
                            <FormItem className="space-y-3 my-2">
                                <FormLabel>Color</FormLabel>
                                <Select onValueChange={field.onChange} defaultValue={field.value}>
                                    <FormControl>
                                        <SelectTrigger className="w-[200px] dark:bg-muted">
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
                );
            case "icon":
                return (
                    <FormField
                        key={fieldName}
                        control={props.form.control}
                        name="icon"
                        render={({ field }) => (
                            <FormItem className="space-y-3 my-2">
                                <FormLabel>Icon</FormLabel>
                                <Select onValueChange={field.onChange} defaultValue={field.value}>
                                    <FormControl>
                                        <SelectTrigger className="w-[200px] dark:bg-muted">
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
                );
            case "persona":
                return (
                    <FormField
                        key={fieldName}
                        control={props.form.control}
                        name="persona"
                        render={({ field }) => (
                            <FormItem className="space-y-1 my-2 grid gap-2">
                                <FormLabel>Personality</FormLabel>
                                <FormDescription>
                                    What is the personality, thought process, or tuning of this
                                    agent? This is where you can provide any instructions to the
                                    agent.
                                </FormDescription>
                                <FormControl>
                                    <Textarea
                                        className="dark:bg-muted focus:outline-none focus-visible:border-orange-500 focus-visible:border-2"
                                        placeholder="You are an excellent biologist, at the top of your field in marine biology."
                                        {...field}
                                    />
                                </FormControl>
                                <FormMessage />
                            </FormItem>
                        )}
                    />
                );
            case "files":
                return (
                    <FormField
                        key={fieldName}
                        control={props.form.control}
                        name="files"
                        render={({ field }) => (
                            <FormItem className="my-2 flex flex-col gap-2">
                                <FormLabel>Knowledge Base</FormLabel>
                                <FormDescription>
                                    Which information should be part of its digital brain?{" "}
                                    <a href="/settings">Manage data</a>.
                                </FormDescription>
                                <Collapsible>
                                    <CollapsibleTrigger className="flex items-center justify-between text-sm gap-2 bg-muted p-2 rounded-lg">
                                        <CaretUpDown />
                                        {field.value && field.value.length > 0
                                            ? `${field.value.length} files selected`
                                            : "Select files"}
                                    </CollapsibleTrigger>
                                    <CollapsibleContent className="m-1">
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
                                                                <span>
                                                                    Drag and drop files here
                                                                </span>
                                                            </div>
                                                        )}
                                                    </div>
                                                </div>
                                            </div>
                                            <CommandInput
                                                placeholder="Select files..."
                                                value={fileSearchValue}
                                                onValueChange={setFileSearchValue}
                                            />
                                            <CommandList>
                                                <CommandEmpty>No files found.</CommandEmpty>
                                                <CommandGroup>
                                                    <div className="flex gap-2 px-2 py-1 border-b">
                                                        <Button
                                                            type="button"
                                                            variant="ghost"
                                                            size="sm"
                                                            className="h-6 px-2 text-xs"
                                                            onClick={(e) => {
                                                                e.stopPropagation();
                                                                const filteredFiles =
                                                                    allFileOptions.filter((file) =>
                                                                        file
                                                                            .toLowerCase()
                                                                            .includes(
                                                                                fileSearchValue.toLowerCase(),
                                                                            ),
                                                                    );
                                                                const currentFiles =
                                                                    props.form.getValues("files") ||
                                                                    [];
                                                                const newFiles = [
                                                                    ...new Set([
                                                                        ...currentFiles,
                                                                        ...filteredFiles,
                                                                    ]),
                                                                ];
                                                                props.form.setValue(
                                                                    "files",
                                                                    newFiles,
                                                                );
                                                            }}
                                                        >
                                                            Select All
                                                        </Button>
                                                        <Button
                                                            type="button"
                                                            variant="ghost"
                                                            size="sm"
                                                            className="h-6 px-2 text-xs"
                                                            onClick={(e) => {
                                                                e.stopPropagation();
                                                                const filteredFiles =
                                                                    allFileOptions.filter((file) =>
                                                                        file
                                                                            .toLowerCase()
                                                                            .includes(
                                                                                fileSearchValue.toLowerCase(),
                                                                            ),
                                                                    );
                                                                const currentFiles =
                                                                    props.form.getValues("files") ||
                                                                    [];
                                                                const newFiles =
                                                                    currentFiles.filter(
                                                                        (file) =>
                                                                            !filteredFiles.includes(
                                                                                file,
                                                                            ),
                                                                    );
                                                                props.form.setValue(
                                                                    "files",
                                                                    newFiles,
                                                                );
                                                            }}
                                                        >
                                                            Deselect All
                                                        </Button>
                                                    </div>
                                                    {allFileOptions.map((file) => (
                                                        <CommandItem
                                                            value={file}
                                                            key={file}
                                                            className="break-all"
                                                            onSelect={() => {
                                                                const currentFiles =
                                                                    props.form.getValues("files") ||
                                                                    [];
                                                                const newFiles =
                                                                    currentFiles.includes(file)
                                                                        ? currentFiles.filter(
                                                                              (item) =>
                                                                                  item !== file,
                                                                          )
                                                                        : [...currentFiles, file];
                                                                props.form.setValue(
                                                                    "files",
                                                                    newFiles,
                                                                );
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
                );
            case "input_tools":
                return (
                    <FormField
                        key={fieldName}
                        control={props.form.control}
                        name="input_tools"
                        render={({ field }) => (
                            <FormItem className="flex flex-col gap-2 my-2">
                                <FormLabel>Restrict Input Tools</FormLabel>
                                <FormDescription>
                                    Which knowledge retrieval tools should this agent be limited to?
                                    <br />
                                    <b>Default:</b> No limitations.
                                </FormDescription>
                                <Collapsible>
                                    <CollapsibleTrigger className="flex items-center justify-between text-sm gap-2 bg-muted p-2 rounded-lg">
                                        <CaretUpDown />
                                        {field.value && field.value.length > 0
                                            ? `${field.value.length} tools selected`
                                            : "All tools"}
                                    </CollapsibleTrigger>
                                    <CollapsibleContent>
                                        <Command>
                                            <CommandList>
                                                <CommandGroup>
                                                    {Object.entries(props.inputToolOptions).map(
                                                        ([key, value]) => (
                                                            <CommandItem
                                                                value={key}
                                                                key={key}
                                                                onSelect={() => {
                                                                    const currentInputTools =
                                                                        props.form.getValues(
                                                                            "input_tools",
                                                                        ) || [];
                                                                    const newInputTools =
                                                                        currentInputTools.includes(
                                                                            key,
                                                                        )
                                                                            ? currentInputTools.filter(
                                                                                  (item) =>
                                                                                      item !== key,
                                                                              )
                                                                            : [
                                                                                  ...currentInputTools,
                                                                                  key,
                                                                              ];
                                                                    props.form.setValue(
                                                                        "input_tools",
                                                                        newInputTools,
                                                                    );
                                                                }}
                                                            >
                                                                <Check
                                                                    className={cn(
                                                                        "mr-2 h-4 w-4",
                                                                        field.value &&
                                                                            field.value.includes(
                                                                                key,
                                                                            )
                                                                            ? "opacity-100"
                                                                            : "opacity-0",
                                                                    )}
                                                                />
                                                                <div
                                                                    className={cn(
                                                                        "flex items-center space-x-2",
                                                                    )}
                                                                >
                                                                    <p>
                                                                        <b>{key}</b>
                                                                    </p>
                                                                    <p>{value}</p>
                                                                </div>
                                                            </CommandItem>
                                                        ),
                                                    )}
                                                </CommandGroup>
                                            </CommandList>
                                        </Command>
                                    </CollapsibleContent>
                                </Collapsible>
                            </FormItem>
                        )}
                    />
                );
            case "output_modes":
                return (
                    <FormField
                        key={fieldName}
                        control={props.form.control}
                        name="output_modes"
                        render={({ field }) => (
                            <FormItem className="flex flex-col gap-2 my-2">
                                <FormLabel>Restrict Output Modes</FormLabel>
                                <FormDescription>
                                    Which output modes should this agent be limited to?
                                    <br />
                                    <b>Default:</b> No limitations.
                                </FormDescription>
                                <Collapsible>
                                    <CollapsibleTrigger className="flex items-center justify-between text-sm gap-2 bg-muted p-2 rounded-lg">
                                        <CaretUpDown />
                                        {field.value && field.value.length > 0
                                            ? `${field.value.length} modes selected`
                                            : "All modes"}
                                    </CollapsibleTrigger>
                                    <CollapsibleContent>
                                        <Command>
                                            <CommandList>
                                                <CommandGroup>
                                                    {Object.entries(props.outputModeOptions).map(
                                                        ([key, value]) => (
                                                            <CommandItem
                                                                value={key}
                                                                key={key}
                                                                onSelect={() => {
                                                                    const currentOutputModes =
                                                                        props.form.getValues(
                                                                            "output_modes",
                                                                        ) || [];
                                                                    const newOutputModes =
                                                                        currentOutputModes.includes(
                                                                            key,
                                                                        )
                                                                            ? currentOutputModes.filter(
                                                                                  (item) =>
                                                                                      item !== key,
                                                                              )
                                                                            : [
                                                                                  ...currentOutputModes,
                                                                                  key,
                                                                              ];
                                                                    props.form.setValue(
                                                                        "output_modes",
                                                                        newOutputModes,
                                                                    );
                                                                }}
                                                            >
                                                                <Check
                                                                    className={cn(
                                                                        "mr-2 h-4 w-4",
                                                                        field.value &&
                                                                            field.value.includes(
                                                                                key,
                                                                            )
                                                                            ? "opacity-100"
                                                                            : "opacity-0",
                                                                    )}
                                                                />
                                                                <div
                                                                    className={cn(
                                                                        "flex items-center space-x-2",
                                                                    )}
                                                                >
                                                                    <p>
                                                                        <b>{key}</b>
                                                                    </p>
                                                                    <p>{value}</p>
                                                                </div>
                                                            </CommandItem>
                                                        ),
                                                    )}
                                                </CommandGroup>
                                            </CommandList>
                                        </Command>
                                    </CollapsibleContent>
                                </Collapsible>
                            </FormItem>
                        )}
                    />
                );
            default:
                return null;
        }
    };

    return (
        <Form {...props.form}>
            <ScrollArea className="h-full">
                <form
                    onSubmit={props.form.handleSubmit(handleSubmit)}
                    className="space-y-6 pb-4 px-4 h-full flex flex-col justify-between"
                >
                    <Tabs defaultValue="basic" value={formGroups[currentStep].tabName}>
                        <TabsList className="grid grid-cols-2 md:grid-cols-4 gap-2 h-fit">
                            {formGroups.map((group) => (
                                <TabsTrigger
                                    key={group.tabName}
                                    value={group.tabName}
                                    className={`text-center ${areRequiredFieldsCompletedForCurrentStep(group) ? "" : "text-red-500"}`}
                                    onClick={() =>
                                        setCurrentStep(
                                            formGroups.findIndex(
                                                (g) => g.tabName === group.tabName,
                                            ),
                                        )
                                    }
                                >
                                    {group.label}{" "}
                                    {!areRequiredFieldsCompletedForCurrentStep(group) && "*"}
                                </TabsTrigger>
                            ))}
                        </TabsList>
                        {formGroups.map((group) => (
                            <TabsContent key={group.tabName} value={group.tabName}>
                                {group.fields.map((field) => renderFormField(field.name))}
                            </TabsContent>
                        ))}
                    </Tabs>

                    {props.errors && (
                        <Alert className="bg-secondary border-none my-4">
                            <AlertDescription className="flex items-center gap-1">
                                <ShieldWarning
                                    weight="fill"
                                    className="h-4 w-4 text-yellow-400 inline"
                                />
                                <span>{props.errors}</span>
                            </AlertDescription>
                        </Alert>
                    )}
                </form>
            </ScrollArea>
            <div className="flex justify-between mt-1 left-0 right-0 bg-background p-1">
                <Button
                    type="button"
                    variant={"outline"}
                    onClick={handlePrevious}
                    disabled={currentStep === 0}
                    className={`items-center ${isSaving ? "bg-stone-100 dark:bg-neutral-900" : ""} text-white ${colorOptionClassName}`}
                >
                    <ArrowLeft className="mr-2 h-4 w-4" />
                    Previous
                </Button>
                {currentStep < formGroups.length - 1 ? (
                    <Button
                        type="button"
                        variant={"outline"}
                        onClick={handleNext}
                        disabled={
                            !areRequiredFieldsCompletedForCurrentStep(formGroups[currentStep])
                        }
                        className={`items-center ${isSaving ? "bg-stone-100 dark:bg-neutral-900" : ""} text-white ${colorOptionClassName}`}
                    >
                        Continue
                        <ArrowRight className="ml-2 h-4 w-4" />
                    </Button>
                ) : (
                    <Button
                        type="submit"
                        variant={"outline"}
                        disabled={isSaving}
                        onClick={props.form.handleSubmit(handleSubmit)}
                        className={`items-center ${isSaving ? "bg-stone-100 dark:bg-neutral-900" : ""} text-white ${colorOptionClassName}`}
                    >
                        <FloppyDisk className="h-4 w-4 mr-2" />
                        {isSaving ? "Booting..." : "Save"}
                    </Button>
                )}
            </div>
        </Form>
    );
}
