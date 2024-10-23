import styles from "./chatInputArea.module.css";
import React, { useEffect, useRef, useState, forwardRef } from "react";

import DOMPurify from "dompurify";
import "katex/dist/katex.min.css";
import { ArrowUp, Microphone, Paperclip, X, Stop } from "@phosphor-icons/react";

import {
    Command,
    CommandEmpty,
    CommandGroup,
    CommandInput,
    CommandItem,
    CommandList,
    CommandSeparator,
} from "@/components/ui/command";

import {
    AlertDialog,
    AlertDialogAction,
    AlertDialogContent,
    AlertDialogDescription,
    AlertDialogHeader,
    AlertDialogTitle,
} from "@/components/ui/alert-dialog";
import { Button } from "@/components/ui/button";
import { Progress } from "@/components/ui/progress";
import { Popover, PopoverContent } from "@/components/ui/popover";
import { PopoverTrigger } from "@radix-ui/react-popover";
import { Textarea } from "@/components/ui/textarea";
import { Tooltip, TooltipContent, TooltipProvider, TooltipTrigger } from "@/components/ui/tooltip";
import { convertToBGClass } from "@/app/common/colorUtils";

import LoginPrompt from "../loginPrompt/loginPrompt";
import { uploadDataForIndexing } from "../../common/chatFunctions";
import { InlineLoading } from "../loading/loading";
import { getIconForSlashCommand } from "@/app/common/iconUtils";

export interface ChatOptions {
    [key: string]: string;
}

interface ChatInputProps {
    sendMessage: (message: string) => void;
    sendImage: (image: string) => void;
    sendDisabled: boolean;
    setUploadedFiles?: (files: string[]) => void;
    conversationId?: string | null;
    chatOptionsData?: ChatOptions | null;
    isMobileWidth?: boolean;
    isLoggedIn: boolean;
    agentColor?: string;
}

export const ChatInputArea = forwardRef<HTMLTextAreaElement, ChatInputProps>((props, ref) => {
    const [message, setMessage] = useState("");
    const fileInputRef = useRef<HTMLInputElement>(null);

    const [warning, setWarning] = useState<string | null>(null);
    const [error, setError] = useState<string | null>(null);
    const [uploading, setUploading] = useState(false);
    const [loginRedirectMessage, setLoginRedirectMessage] = useState<string | null>(null);
    const [showLoginPrompt, setShowLoginPrompt] = useState(false);

    const [imageUploaded, setImageUploaded] = useState(false);
    const [imagePaths, setImagePaths] = useState<string[]>([]);
    const [imageData, setImageData] = useState<string[]>([]);

    const [recording, setRecording] = useState(false);
    const [mediaRecorder, setMediaRecorder] = useState<MediaRecorder | null>(null);

    const [progressValue, setProgressValue] = useState(0);
    const [isDragAndDropping, setIsDragAndDropping] = useState(false);

    const chatInputRef = ref as React.MutableRefObject<HTMLTextAreaElement>;
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
        async function fetchImageData() {
            if (imagePaths.length > 0) {
                const newImageData = await Promise.all(
                    imagePaths.map(async (path) => {
                        const response = await fetch(path);
                        const blob = await response.blob();
                        return new Promise<string>((resolve) => {
                            const reader = new FileReader();
                            reader.onload = () => resolve(reader.result as string);
                            reader.readAsDataURL(blob);
                        });
                    }),
                );
                setImageData(newImageData);
            }
            setUploading(false);
        }
        setUploading(true);
        fetchImageData();
    }, [imagePaths]);

    function onSendMessage() {
        if (imageUploaded) {
            setImageUploaded(false);
            setImagePaths([]);
            imageData.forEach((data) => props.sendImage(data));
        }
        if (!message.trim()) return;

        if (!props.isLoggedIn) {
            setLoginRedirectMessage(
                "Hey there, you need to be signed in to send messages to Khoj AI",
            );
            setShowLoginPrompt(true);
            return;
        }

        props.sendMessage(message.trim());
        setMessage("");
    }

    function handleSlashCommandClick(command: string) {
        setMessage(`/${command} `);
    }

    function handleFileButtonClick() {
        if (!fileInputRef.current) return;
        fileInputRef.current.click();
    }

    function handleFileChange(event: React.ChangeEvent<HTMLInputElement>) {
        if (!event.target.files) return;

        uploadFiles(event.target.files);
    }

    function handleDragAndDropFiles(event: React.DragEvent<HTMLDivElement>) {
        event.preventDefault();
        setIsDragAndDropping(false);

        if (!event.dataTransfer.files) return;

        uploadFiles(event.dataTransfer.files);
    }

    function uploadFiles(files: FileList) {
        if (!props.isLoggedIn) {
            setLoginRedirectMessage("Please login to chat with your files");
            setShowLoginPrompt(true);
            return;
        }
        // check for image files
        const image_endings = ["jpg", "jpeg", "png", "webp"];
        const newImagePaths: string[] = [];
        for (let i = 0; i < files.length; i++) {
            const file = files[i];
            const file_extension = file.name.split(".").pop();
            if (image_endings.includes(file_extension || "")) {
                newImagePaths.push(DOMPurify.sanitize(URL.createObjectURL(file)));
            }
        }

        if (newImagePaths.length > 0) {
            setImageUploaded(true);
            setImagePaths((prevPaths) => [...prevPaths, ...newImagePaths]);
            // Set focus to the input for user message after uploading files
            chatInputRef?.current?.focus();
            return;
        }

        uploadDataForIndexing(
            files,
            setWarning,
            setUploading,
            setError,
            props.setUploadedFiles,
            props.conversationId,
        );

        // Set focus to the input for user message after uploading files
        chatInputRef?.current?.focus();
    }

    // Assuming this function is added within the same context as the provided excerpt
    async function startRecordingAndTranscribe() {
        try {
            const microphone = await navigator.mediaDevices.getUserMedia({ audio: true });
            const mediaRecorder = new MediaRecorder(microphone, { mimeType: "audio/webm" });

            const audioChunks: Blob[] = [];

            mediaRecorder.ondataavailable = async (event) => {
                audioChunks.push(event.data);
                const audioBlob = new Blob(audioChunks, { type: "audio/webm" });
                const formData = new FormData();
                formData.append("file", audioBlob);

                // Send the incremental audio blob to the server
                try {
                    const response = await fetch("/api/transcribe", {
                        method: "POST",
                        body: formData,
                    });

                    if (!response.ok) {
                        throw new Error("Network response was not ok");
                    }

                    const transcription = await response.json();
                    setMessage(transcription.text.trim());
                } catch (error) {
                    console.error("Error sending audio to server:", error);
                }
            };

            // Send an audio blob every 1.5 seconds
            mediaRecorder.start(1500);

            mediaRecorder.onstop = async () => {
                const audioBlob = new Blob(audioChunks, { type: "audio/webm" });
                const formData = new FormData();
                formData.append("file", audioBlob);

                // Send the audio blob to the server
                try {
                    const response = await fetch("/api/transcribe", {
                        method: "POST",
                        body: formData,
                    });

                    if (!response.ok) {
                        throw new Error("Network response was not ok");
                    }

                    const transcription = await response.json();
                    mediaRecorder.stream.getTracks().forEach((track) => track.stop());
                    setMediaRecorder(null);
                    setMessage(transcription.text.trim());
                } catch (error) {
                    console.error("Error sending audio to server:", error);
                }
            };

            setMediaRecorder(mediaRecorder);
        } catch (error) {
            console.error("Error getting microphone", error);
        }
    }

    useEffect(() => {
        if (!recording && mediaRecorder) {
            mediaRecorder.stop();
        }

        if (recording && !mediaRecorder) {
            startRecordingAndTranscribe();
        }
    }, [recording, mediaRecorder]);

    useEffect(() => {
        if (!chatInputRef?.current) return;
        chatInputRef.current.style.height = "auto";
        chatInputRef.current.style.height =
            Math.max(chatInputRef.current.scrollHeight - 24, 64) + "px";
    }, [message]);

    function handleDragOver(event: React.DragEvent<HTMLDivElement>) {
        event.preventDefault();
        setIsDragAndDropping(true);
    }

    function handleDragLeave(event: React.DragEvent<HTMLDivElement>) {
        event.preventDefault();
        setIsDragAndDropping(false);
    }

    function removeImageUpload(index: number) {
        setImagePaths((prevPaths) => prevPaths.filter((_, i) => i !== index));
        setImageData((prevData) => prevData.filter((_, i) => i !== index));
        if (imagePaths.length === 1) {
            setImageUploaded(false);
        }
    }

    return (
        <>
            {showLoginPrompt && loginRedirectMessage && (
                <LoginPrompt
                    onOpenChange={setShowLoginPrompt}
                    loginRedirectMessage={loginRedirectMessage}
                />
            )}
            {uploading && (
                <AlertDialog open={uploading}>
                    <AlertDialogContent>
                        <AlertDialogHeader>
                            <AlertDialogTitle>Uploading data. Please wait.</AlertDialogTitle>
                        </AlertDialogHeader>
                        <AlertDialogDescription>
                            <Progress
                                indicatorColor="bg-slate-500"
                                className="w-full h-2 rounded-full"
                                value={progressValue}
                            />
                        </AlertDialogDescription>
                        <AlertDialogAction
                            className="bg-slate-400 hover:bg-slate-500"
                            onClick={() => setUploading(false)}
                        >
                            Dismiss
                        </AlertDialogAction>
                    </AlertDialogContent>
                </AlertDialog>
            )}
            {warning && (
                <AlertDialog open={warning !== null}>
                    <AlertDialogContent>
                        <AlertDialogHeader>
                            <AlertDialogTitle>Data Upload Warning</AlertDialogTitle>
                        </AlertDialogHeader>
                        <AlertDialogDescription>{warning}</AlertDialogDescription>
                        <AlertDialogAction
                            className="bg-slate-400 hover:bg-slate-500"
                            onClick={() => setWarning(null)}
                        >
                            Close
                        </AlertDialogAction>
                    </AlertDialogContent>
                </AlertDialog>
            )}
            {error && (
                <AlertDialog open={error !== null}>
                    <AlertDialogContent>
                        <AlertDialogHeader>
                            <AlertDialogTitle>Oh no!</AlertDialogTitle>
                            <AlertDialogDescription>
                                Something went wrong while uploading your data
                            </AlertDialogDescription>
                        </AlertDialogHeader>
                        <AlertDialogDescription>{error}</AlertDialogDescription>
                        <AlertDialogAction
                            className="bg-slate-400 hover:bg-slate-500"
                            onClick={() => setError(null)}
                        >
                            Close
                        </AlertDialogAction>
                    </AlertDialogContent>
                </AlertDialog>
            )}
            {message.startsWith("/") && message.split(" ").length === 1 && (
                <div className="flex justify-center text-center">
                    <Popover open={message.startsWith("/")}>
                        <PopoverTrigger className="flex justify-center text-center"></PopoverTrigger>
                        <PopoverContent
                            onOpenAutoFocus={(e) => e.preventDefault()}
                            className={`${props.isMobileWidth ? "w-[100vw]" : "w-full"} rounded-md`}
                        >
                            <Command className="max-w-full">
                                <CommandInput
                                    placeholder="Type a command or search..."
                                    value={message}
                                    className="hidden"
                                />
                                <CommandList>
                                    <CommandEmpty>No matching commands.</CommandEmpty>
                                    <CommandGroup heading="Agent Tools">
                                        {props.chatOptionsData &&
                                            Object.entries(props.chatOptionsData).map(
                                                ([key, value]) => (
                                                    <CommandItem
                                                        key={key}
                                                        className={`text-md`}
                                                        onSelect={() =>
                                                            handleSlashCommandClick(key)
                                                        }
                                                    >
                                                        <div className="grid grid-cols-1 gap-1">
                                                            <div className="font-bold flex items-center">
                                                                {getIconForSlashCommand(
                                                                    key,
                                                                    "h-4 w-4 mr-2",
                                                                )}
                                                                /{key}
                                                            </div>
                                                            <div>{value}</div>
                                                        </div>
                                                    </CommandItem>
                                                ),
                                            )}
                                    </CommandGroup>
                                    <CommandSeparator />
                                </CommandList>
                            </Command>
                        </PopoverContent>
                    </Popover>
                </div>
            )}
            <div
                className={`${styles.actualInputArea} justify-between dark:bg-neutral-700 relative ${isDragAndDropping && "animate-pulse"}`}
                onDragOver={handleDragOver}
                onDragLeave={handleDragLeave}
                onDrop={handleDragAndDropFiles}
            >
                <input
                    type="file"
                    multiple={true}
                    ref={fileInputRef}
                    onChange={handleFileChange}
                    style={{ display: "none" }}
                />
                <div className="flex items-end pb-4">
                    <Button
                        variant={"ghost"}
                        className="!bg-none p-0 m-2 h-auto text-3xl rounded-full text-gray-300 hover:text-gray-500"
                        disabled={props.sendDisabled}
                        onClick={handleFileButtonClick}
                    >
                        <Paperclip className="w-8 h-8" />
                    </Button>
                </div>
                <div className="flex-grow flex flex-col w-full gap-1.5 relative pb-2">
                    <div className="flex items-center gap-2 overflow-x-auto">
                        {imageUploaded &&
                            imagePaths.map((path, index) => (
                                <div key={index} className="relative flex-shrink-0 pb-3 pt-2 group">
                                    <img
                                        src={path}
                                        alt={`img-${index}`}
                                        className="w-auto h-16 object-cover rounded-xl"
                                    />
                                    <Button
                                        variant="ghost"
                                        size="icon"
                                        className="absolute -top-0 -right-2 h-5 w-5 rounded-full bg-neutral-200 dark:bg-neutral-600 hover:bg-neutral-300 dark:hover:bg-neutral-500 opacity-0 group-hover:opacity-100 transition-opacity"
                                        onClick={() => removeImageUpload(index)}
                                    >
                                        <X className="h-3 w-3" />
                                    </Button>
                                </div>
                            ))}
                    </div>
                    <Textarea
                        ref={chatInputRef}
                        className={`border-none w-full h-16 min-h-16 max-h-[128px] md:py-4 rounded-lg resize-none dark:bg-neutral-700 ${props.isMobileWidth ? "text-md" : "text-lg"}`}
                        placeholder="Type / to see a list of commands"
                        id="message"
                        autoFocus={true}
                        value={message}
                        onKeyDown={(e) => {
                            if (e.key === "Enter" && !e.shiftKey && !props.isMobileWidth) {
                                setImageUploaded(false);
                                setImagePaths([]);
                                e.preventDefault();
                                onSendMessage();
                            }
                        }}
                        onChange={(e) => setMessage(e.target.value)}
                        disabled={props.sendDisabled || recording}
                    />
                </div>
                <div className="flex items-end pb-4">
                    {recording ? (
                        <TooltipProvider>
                            <Tooltip>
                                <TooltipTrigger asChild>
                                    <Button
                                        variant="default"
                                        className={`${!recording && "hidden"} ${props.agentColor ? convertToBGClass(props.agentColor) : "bg-orange-300 hover:bg-orange-500"} rounded-full p-1 m-2 h-auto text-3xl transition transform md:hover:-translate-y-1`}
                                        onClick={() => {
                                            setRecording(!recording);
                                        }}
                                        disabled={props.sendDisabled}
                                    >
                                        <Stop weight="fill" className="w-6 h-6" />
                                    </Button>
                                </TooltipTrigger>
                                <TooltipContent>
                                    Click to stop recording and transcribe your voice.
                                </TooltipContent>
                            </Tooltip>
                        </TooltipProvider>
                    ) : mediaRecorder ? (
                        <InlineLoading />
                    ) : (
                        <TooltipProvider>
                            <Tooltip>
                                <TooltipTrigger asChild>
                                    <Button
                                        variant="default"
                                        className={`${!message || recording || "hidden"} ${props.agentColor ? convertToBGClass(props.agentColor) : "bg-orange-300 hover:bg-orange-500"} rounded-full p-1 m-2 h-auto text-3xl transition transform md:hover:-translate-y-1`}
                                        onClick={() => {
                                            setMessage("Listening...");
                                            setRecording(!recording);
                                        }}
                                        disabled={props.sendDisabled}
                                    >
                                        <Microphone weight="fill" className="w-6 h-6" />
                                    </Button>
                                </TooltipTrigger>
                                <TooltipContent>
                                    Click to transcribe your message with voice.
                                </TooltipContent>
                            </Tooltip>
                        </TooltipProvider>
                    )}
                    <Button
                        className={`${(!message || recording) && "hidden"} ${props.agentColor ? convertToBGClass(props.agentColor) : "bg-orange-300 hover:bg-orange-500"} rounded-full p-1 m-2 h-auto text-3xl transition transform md:hover:-translate-y-1`}
                        onClick={onSendMessage}
                        disabled={props.sendDisabled}
                    >
                        <ArrowUp className="w-6 h-6" weight="bold" />
                    </Button>
                </div>
            </div>
        </>
    );
});

ChatInputArea.displayName = "ChatInputArea";
