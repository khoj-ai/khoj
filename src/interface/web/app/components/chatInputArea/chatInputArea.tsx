import styles from "./chatInputArea.module.css";
import React, { useEffect, useRef, useState, forwardRef } from "react";
import QueryFiltersPopover, { getTriggerAtCaret } from "./query-filters";

import DOMPurify from "dompurify";
import "katex/dist/katex.min.css";
import {
    ArrowUp,
    Microphone,
    Paperclip,
    X,
    Stop,
    ToggleLeft,
    ToggleRight,
} from "@phosphor-icons/react";

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
import { convertColorToTextClass, convertToBGClass } from "@/app/common/colorUtils";

import LoginPrompt from "../loginPrompt/loginPrompt";
import { InlineLoading } from "../loading/loading";
import { getIconForSlashCommand, getIconFromFilename } from "@/app/common/iconUtils";
import { packageFilesForUpload } from "@/app/common/chatFunctions";
import { convertBytesToText } from "@/app/common/utils";
import {
    Dialog,
    DialogContent,
    DialogDescription,
    DialogHeader,
    DialogTitle,
    DialogTrigger,
} from "@/components/ui/dialog";
import { ScrollArea } from "@/components/ui/scroll-area";

export interface ChatOptions {
    [key: string]: string;
}

export interface AttachedFileText {
    name: string;
    content: string;
    file_type: string;
    size: number;
}

export enum ChatInputFocus {
    MESSAGE = "message",
    FILE = "file",
    RESEARCH = "research",
}

interface ChatInputProps {
    sendMessage: (message: string) => void;
    sendImage: (image: string) => void;
    sendDisabled: boolean;
    setUploadedFiles: (files: AttachedFileText[]) => void;
    conversationId?: string | null;
    chatOptionsData?: ChatOptions | null;
    isMobileWidth?: boolean;
    isLoggedIn: boolean;
    agentColor?: string;
    isResearchModeEnabled?: boolean;
    setTriggeredAbort: (value: boolean, newMessage?: string) => void;
    prefillMessage?: string;
    focus?: ChatInputFocus;
}

export const ChatInputArea = forwardRef<HTMLTextAreaElement, ChatInputProps>((props, ref) => {
    const [message, setMessage] = useState("");
    const [allFiles, setAllFiles] = useState<string[]>([]);
    const [showFileSuggestions, setShowFileSuggestions] = useState(false);
    const [fileQuery, setFileQuery] = useState("");
    const [suggestions, setSuggestions] = useState<string[]>([]);
    const [menuLevel, setMenuLevel] = useState<"main" | "file" | "date" | "word" | "dateMode" | "wordMode" | null>(null);
    const [dateMode, setDateMode] = useState<"exact" | ">=" | "<=" | null>(null);
    const [wordMode, setWordMode] = useState<"include" | "exclude" | null>(null);
    const [dateOperatorInfo, setDateOperatorInfo] = useState<{
        insertStart: number;
        insertToken: string;
        operator: "exact" | ">=" | "<=";
    } | null>(null);
    const [wordOperatorInfo, setWordOperatorInfo] = useState<{
        insertStart: number;
        insertToken: string;
        sign: "+" | "-";
    } | null>(null);
    const [highlightIndex, setHighlightIndex] = useState(0);
    const triggerRef = useRef<{ kind: "at" | "file" | "date" | "word" | "dateMode" | "wordMode"; triggerLen: number; query?: string; sign?: "+" | "-" } | null>(null);
    const suggestionsContainerRef = useRef<HTMLDivElement | null>(null);
    const fileInputRef = useRef<HTMLInputElement>(null);
    const fileInputButtonRef = useRef<HTMLButtonElement>(null);
    const researchModeRef = useRef<HTMLButtonElement>(null);

    const [warning, setWarning] = useState<string | null>(null);
    const [error, setError] = useState<string | null>(null);
    const [uploading, setUploading] = useState(false);
    const [loginRedirectMessage, setLoginRedirectMessage] = useState<string | null>(null);
    const [showLoginPrompt, setShowLoginPrompt] = useState(false);

    const [imageUploaded, setImageUploaded] = useState(false);
    const [imagePaths, setImagePaths] = useState<string[]>([]);
    const [imageData, setImageData] = useState<string[]>([]);

    const [attachedFiles, setAttachedFiles] = useState<FileList | null>(null);
    const [convertedAttachedFiles, setConvertedAttachedFiles] = useState<AttachedFileText[]>([]);

    const [recording, setRecording] = useState(false);
    const [mediaRecorder, setMediaRecorder] = useState<MediaRecorder | null>(null);

    const [progressValue, setProgressValue] = useState(0);
    const [isDragAndDropping, setIsDragAndDropping] = useState(false);

    const [showCommandList, setShowCommandList] = useState(false);
    const [useResearchMode, setUseResearchMode] = useState<boolean>(
        props.isResearchModeEnabled || false,
    );

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
        if (props.prefillMessage === undefined) return;
        setMessage(props.prefillMessage);
        chatInputRef?.current?.focus();
    }, [props.prefillMessage]);

    useEffect(() => {
        // Load all files for suggestions
        fetch("/api/content/computer", { method: "GET", headers: { "Content-Type": "application/json" } })
            .then((r) => r.json())
            .then((data) => setAllFiles(Array.isArray(data) ? data : []))
            .catch((e) => console.error("Failed to load files for suggestions", e));
    }, []);

    useEffect(() => {
        if (props.focus === ChatInputFocus.MESSAGE) {
            chatInputRef?.current?.focus();
        } else if (props.focus === ChatInputFocus.FILE) {
            fileInputButtonRef.current?.focus();
        } else if (props.focus === ChatInputFocus.RESEARCH) {
            researchModeRef.current?.focus();
        }
    }, [props.focus]);

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

    useEffect(() => {
        if (props.isResearchModeEnabled) {
            setUseResearchMode(props.isResearchModeEnabled);
        }
    }, [props.isResearchModeEnabled]);

    function onSendMessage() {
        if (!message.trim() && imageData.length === 0) return;
        if (!props.isLoggedIn) {
            setLoginRedirectMessage(
                "Hey there, you need to be signed in to send messages to Khoj AI",
            );
            setShowLoginPrompt(true);
            return;
        }

        // If currently processing, handle interrupt first
        if (props.sendDisabled) {
            props.setTriggeredAbort(true, message.trim());
            setMessage(""); // Clear the input
            return; // Don't continue with regular message sending
        }

        if (imageUploaded) {
            setImageUploaded(false);
            setImagePaths([]);
            imageData.forEach((data) => props.sendImage(data));
        }

        let messageToSend = message.trim();
        // Check if message starts with an explicit slash command
        const startsWithSlashCommand =
            props.chatOptionsData &&
            Object.keys(props.chatOptionsData).some((cmd) => messageToSend.startsWith(`/${cmd}`));
        // Only add /research if useResearchMode is enabled and message doesn't already use a slash command
        if (useResearchMode && !startsWithSlashCommand) {
            messageToSend = `/research ${messageToSend}`;
        }

        props.sendMessage(messageToSend);
        setAttachedFiles(null);
        setConvertedAttachedFiles([]);
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
        }

        // Process all non-image files
        const nonImageFiles = Array.from(files).filter(
            (file) => !image_endings.includes(file.name.split(".").pop() || ""),
        );

        // Concatenate attachedFiles and files
        const newFiles = nonImageFiles
            ? Array.from(nonImageFiles).concat(Array.from(attachedFiles || []))
            : Array.from(attachedFiles || []);

        if (newFiles.length > 0) {
            // Ensure files are below size limit (10 MB)
            for (let i = 0; i < newFiles.length; i++) {
                if (newFiles[i].size > 10 * 1024 * 1024) {
                    setWarning(
                        `File ${newFiles[i].name} is too large. Please upload files smaller than 10 MB.`,
                    );
                    return;
                }
            }

            const dataTransfer = new DataTransfer();
            newFiles.forEach((file) => dataTransfer.items.add(file));

            // Extract text from files
            extractTextFromFiles(dataTransfer.files).then((data) => {
                props.setUploadedFiles(data);
                setAttachedFiles(dataTransfer.files);
                setConvertedAttachedFiles(data);
            });
        }

        // Set focus to the input for user message after uploading files
        chatInputRef?.current?.focus();
    }

    async function extractTextFromFiles(files: FileList): Promise<AttachedFileText[]> {
        const formData = await packageFilesForUpload(files);
        setUploading(true);

        try {
            const response = await fetch("/api/content/convert", {
                method: "POST",
                body: formData,
            });
            setUploading(false);

            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }

            return await response.json();
        } catch (error) {
            setError(
                "Error converting files. " +
                error +
                ". Please try again, or contact team@khoj.dev if the issue persists.",
            );
            console.error("Error converting files:", error);
            return [];
        }
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

        if (message.startsWith("/") && message.split(" ").length === 1) {
            setShowCommandList(true);
        } else {
            setShowCommandList(false);
        }

        // Also detect trigger even when message is set programmatically
        try {
            const ta = chatInputRef.current as HTMLTextAreaElement | null;
            if (ta) {
                const caret = ta.selectionStart;
                const trig = getTriggerAtCaret(message, caret);
                if (trig) {
                    triggerRef.current = { kind: trig.kind, triggerLen: trig.triggerLen };
                    setShowFileSuggestions(true);
                    // Map trig.kind directly to menuLevel (handles dateMode/wordMode too)
                    if (trig.kind === "at") {
                        // If user typed characters immediately after '@', open file selector filtered by that query
                        if (trig.query && trig.query.length > 0) {
                            setMenuLevel("file");
                            updateSuggestions(trig.query, "file");
                        } else {
                            setMenuLevel("main");
                            updateSuggestions("", "main");
                        }
                    } else {
                        setMenuLevel(trig.kind as any);
                        updateSuggestions(trig.query || "", trig.kind as any);
                        // prepare operator info when dateMode/wordMode is detected
                        if (trig.kind === "dateMode") {
                            const op = (trig as any).operator || ":";
                            const insertStart = ta.selectionStart - trig.triggerLen;
                            const insertToken = op === ">=" ? 'dt>="' : op === "<=" ? 'dt<="' : 'dt:"';
                            setDateOperatorInfo({ insertStart, insertToken, operator: op });
                        }
                        if (trig.kind === "wordMode") {
                            const sign = (trig as any).sign || "+";
                            const insertStart = ta.selectionStart - trig.triggerLen;
                            const insertToken = sign + '"';
                            setWordOperatorInfo({ insertStart, insertToken, sign });
                        }
                    }
                }
            }
        } catch (err) {
            // ignore
        }
    }, [message]);

    // getTriggerAtCaret is now imported from query-filters

    function updateSuggestions(query: string, levelOverride?: typeof menuLevel) {
        setFileQuery(query);
        const q = query.toLowerCase();
        const level = levelOverride || menuLevel;
        if (level === "file") {
            const filtered = allFiles.filter((f) => f.toLowerCase().includes(q)).slice(0, 20);
            setSuggestions(filtered);
        } else if (level === "main") {
            setSuggestions(["file", "date", "word"]);
        } else if (level === "date") {
            setSuggestions(["exact", "after", "before"]);
        } else if (level === "dateMode") {
            // show current typed date as single suggestion
            const ta = chatInputRef.current;
            if (dateOperatorInfo && ta) {
                const start = dateOperatorInfo.insertStart + dateOperatorInfo.insertToken.length;
                const caret = ta.selectionStart;
                const typed = message.slice(start, caret);
                setSuggestions([typed || ""]);
            } else {
                setSuggestions([""]);
            }
        } else if (level === "word") {
            setSuggestions(["include", "exclude"]);
        } else if (level === "wordMode") {
            const ta = chatInputRef.current;
            if (wordOperatorInfo && ta) {
                const start = wordOperatorInfo.insertStart + wordOperatorInfo.insertToken.length;
                const caret = ta.selectionStart;
                const typed = message.slice(start, caret);
                setSuggestions([typed || ""]);
            } else {
                setSuggestions([""]);
            }
        } else {
            setSuggestions([]);
        }
        setHighlightIndex(0);
    }

    function insertSuggestionAtCaret(textarea: HTMLTextAreaElement, suggestion: string) {
        const start = textarea.selectionStart;
        const end = textarea.selectionEnd;
        const trigger = triggerRef.current;
        if (!trigger) return;
        const insertStart = start - trigger.triggerLen;
        let insertion = suggestion;
        if (menuLevel === "file" || trigger.kind === "file") {
            insertion = `file:\"${suggestion}\" `;
        } else if (menuLevel === "main") {
            // selecting main menu item should not insert directly
            insertion = `@${suggestion} `;
        } else if (menuLevel === "wordMode") {
            const quoted = suggestion.includes(" ") ? `\"${suggestion}\"` : suggestion;
            insertion = wordMode === "include" ? `+${quoted} ` : `-${quoted} `;
        } else if (menuLevel === "dateMode") {
            // suggestion here is expected to be the date string
            if (!dateMode) return;
            const d = suggestion;
            if (dateMode === "exact") insertion = `dt:\"${d}\" `;
            else if (dateMode === ">=") insertion = `dt>=\"${d}\" `;
            else insertion = `dt<=\"${d}\" `;
        } else {
            insertion = `@${suggestion} `;
        }

        // Ensure there's a space before insertion if previous char isn't whitespace
        const needsLeadingSpace = insertStart > 0 && !/\s/.test(message.charAt(insertStart - 1));
        if (needsLeadingSpace) {
            insertion = " " + insertion;
        }

        // Ensure there's a space after insertion if the following char isn't whitespace
        const needsTrailingSpace = end < message.length && !/\s/.test(message.charAt(end));
        if (needsTrailingSpace) {
            insertion = insertion + " ";
        }

        const newMessage = message.slice(0, insertStart) + insertion + message.slice(end);
        setMessage(newMessage);

        // Restore caret after insertion
        requestAnimationFrame(() => {
            const pos = insertStart + insertion.length;
            textarea.selectionStart = textarea.selectionEnd = pos;
            textarea.focus();
        });

        setShowFileSuggestions(false);
        triggerRef.current = null;
        setMenuLevel(null);
        setDateMode(null);
        setWordMode(null);
    }

    function insertTriggerToken(textarea: HTMLTextAreaElement, token: string, newMenuLevel: typeof menuLevel, levelOverride?: typeof menuLevel) {
        // replace the original trigger (e.g. '@') with the token (like 'file:')
        const caret = textarea.selectionStart;
        const trig = getTriggerAtCaret(textarea.value, caret);
        let insertStart = caret;
        let insertEnd = caret;
        if (trig) {
            insertStart = caret - trig.triggerLen;
            insertEnd = caret;
        }

        // add space before token if needed
        const needsLeadingSpace = insertStart > 0 && !/\s/.test(message.charAt(insertStart - 1));
        const tokenToInsert = needsLeadingSpace ? " " + token : token;

        // ensure trailing space after token if needed
        const needsTrailingSpaceToken = insertEnd < message.length && !/\s/.test(message.charAt(insertEnd));
        const finalToken = needsTrailingSpaceToken ? tokenToInsert + " " : tokenToInsert;

        const newMessage = message.slice(0, insertStart) + finalToken + message.slice(insertEnd);
        setMessage(newMessage);

        requestAnimationFrame(() => {
            const pos = insertStart + (needsLeadingSpace ? token.length + 1 : token.length) + (needsTrailingSpaceToken ? 1 : 0);
            textarea.selectionStart = textarea.selectionEnd = pos;
            textarea.focus();
        });

        setMenuLevel(newMenuLevel);
        setShowFileSuggestions(true);
        updateSuggestions("", levelOverride || newMenuLevel);
    }

    function prepareDateOperatorInfo(choice: string) {
        const ta = chatInputRef.current!;
        const caret = ta.selectionStart;
        const trig = getTriggerAtCaret(ta.value, caret);
        let insertStart = trig ? caret - trig.triggerLen : caret;
        // If there's a space immediately before the operator token in the message, include it
        if (insertStart > 0 && /\s/.test(message.charAt(insertStart - 1))) {
            insertStart = insertStart - 1;
        }
        const insertToken = choice === "exact" ? 'dt:"' : choice === "after" ? 'dt>="' : 'dt<="';
        setDateOperatorInfo({ insertStart, insertToken, operator: choice as any });
    }

    function prepareWordOperatorInfo(sign: "+" | "-") {
        const ta = chatInputRef.current!;
        const caret = ta.selectionStart;
        const trig = getTriggerAtCaret(ta.value, caret);
        let insertStart = trig ? caret - trig.triggerLen : caret;
        if (insertStart > 0 && /\s/.test(message.charAt(insertStart - 1))) {
            insertStart = insertStart - 1;
        }
        const insertToken = sign + '"';
        setWordOperatorInfo({ insertStart, insertToken, sign });
    }

    function insertDateFilterFromCaret(textarea: HTMLTextAreaElement) {
        const trig = getTriggerAtCaret(textarea.value, textarea.selectionStart);
        if (!trig) return;
        let dateText = trig.query;
        const dateRegex = /^\d{4}-\d{2}-\d{2}$/;
        // If the user hasn't typed a closing quote, try to include typed text up to caret
        const ta = textarea;
        const caret = ta.selectionStart;
        if (trig.triggerLen && dateText === "") {
            // nothing typed after operator
            setWarning("Please enter a date in YYYY-MM-DD format");
            return;
        }
        // If there's no closing quote, but user typed characters, ensure we capture them
        if (!dateRegex.test(dateText)) {
            // try to get the typed slice from operator position
            if (dateOperatorInfo) {
                const start = dateOperatorInfo.insertStart + dateOperatorInfo.insertToken.length;
                dateText = textarea.value.slice(start, caret);
            }
        }
        if (!dateMode || !dateRegex.test(dateText)) {
            setWarning("Please enter a date in YYYY-MM-DD format");
            return;
        }
        // Prefer operator info insertStart if available (accounts for added leading space)
        const insertStart = dateOperatorInfo ? dateOperatorInfo.insertStart : textarea.selectionStart - trig.triggerLen;
        let insertion = "";
        if (dateMode === "exact") insertion = `dt:\"${dateText}\" `;
        else if (dateMode === ">=") insertion = `dt>=\"${dateText}\" `;
        else insertion = `dt<=\"${dateText}\" `;

        // Ensure leading/trailing spaces are preserved
        const needsLeadingSpace = insertStart > 0 && !/\s/.test(message.charAt(insertStart - 1));
        if (needsLeadingSpace) insertion = " " + insertion;
        const needsTrailingSpace = textarea.selectionEnd < message.length && !/\s/.test(message.charAt(textarea.selectionEnd));
        if (needsTrailingSpace) insertion = insertion + " ";

        const newMessage = message.slice(0, insertStart) + insertion + message.slice(textarea.selectionEnd);
        setMessage(newMessage);
        requestAnimationFrame(() => {
            const pos = insertStart + insertion.length;
            textarea.selectionStart = textarea.selectionEnd = pos;
            textarea.focus();
        });

        setShowFileSuggestions(false);
        triggerRef.current = null;
        setMenuLevel(null);
        setDateMode(null);
        setDateOperatorInfo(null);
    }

    function insertWordFilterFromCaret(textarea: HTMLTextAreaElement) {
        const trig = getTriggerAtCaret(textarea.value, textarea.selectionStart);
        if (!trig) return;
        const wordText = trig.query;
        if (!wordMode || wordText.length === 0) {
            setWarning("Please enter a word or phrase");
            return;
        }
        const insertStart = wordOperatorInfo ? wordOperatorInfo.insertStart : textarea.selectionStart - trig.triggerLen;
        // Always quote the word/phrase to match filter syntax
        const quoted = `\"${wordText}\"`;
        let insertion = wordMode === "include" ? `+${quoted} ` : `-${quoted} `;
        // Preserve surrounding spaces
        const needsLeadingSpaceW = insertStart > 0 && !/\s/.test(message.charAt(insertStart - 1));
        if (needsLeadingSpaceW) insertion = " " + insertion;
        const needsTrailingSpaceW = textarea.selectionEnd < message.length && !/\s/.test(message.charAt(textarea.selectionEnd));
        if (needsTrailingSpaceW) insertion = insertion + " ";

        const newMessage = message.slice(0, insertStart) + insertion + message.slice(textarea.selectionEnd);
        setMessage(newMessage);
        requestAnimationFrame(() => {
            const pos = insertStart + insertion.length;
            textarea.selectionStart = textarea.selectionEnd = pos;
            textarea.focus();
        });

        setShowFileSuggestions(false);
        triggerRef.current = null;
        setMenuLevel(null);
        setWordMode(null);
        setWordOperatorInfo(null);
    }

    useEffect(() => {
        if (!showFileSuggestions) return;
        const container = suggestionsContainerRef.current;
        if (!container) return;
        const children = container.children;
        if (highlightIndex >= 0 && highlightIndex < children.length) {
            const el = children[highlightIndex] as HTMLElement | undefined;
            if (el && typeof el.scrollIntoView === "function") {
                el.scrollIntoView({ block: "nearest" });
            }
        }
    }, [highlightIndex, showFileSuggestions, suggestions]);

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
                    isMobileWidth={props.isMobileWidth}
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
            {showCommandList && (
                <div className="flex justify-center text-center">
                    <Popover open={showCommandList} onOpenChange={setShowCommandList}>
                        <PopoverTrigger className="flex justify-center text-center"></PopoverTrigger>
                        <PopoverContent
                            onOpenAutoFocus={(e) => e.preventDefault()}
                            className={`${props.isMobileWidth ? "w-[100vw]" : "w-full"} rounded-md`}
                            side="bottom"
                            align="center"
                            /* Offset below text area on home page (i.e where conversationId is unset) */
                            sideOffset={props.conversationId ? 0 : 80}
                            alignOffset={0}
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
            <div>
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
                    {convertedAttachedFiles &&
                        Array.from(convertedAttachedFiles).map((file, index) => (
                            <Dialog key={index}>
                                <DialogTrigger asChild>
                                    <div key={index} className="relative flex-shrink-0 p-2 group">
                                        <div
                                            className={`w-auto h-16 object-cover rounded-xl ${props.agentColor ? convertToBGClass(props.agentColor) : "bg-orange-300 hover:bg-orange-500"} bg-opacity-15`}
                                        >
                                            <div className="flex p-2 flex-col justify-start items-start h-full">
                                                <span className="text-sm font-bold text-neutral-500 dark:text-neutral-400 text-ellipsis truncate max-w-[200px] break-words">
                                                    {file.name}
                                                </span>
                                                <span className="flex items-center gap-1">
                                                    {getIconFromFilename(file.file_type)}
                                                    <span className="text-xs text-neutral-500 dark:text-neutral-400">
                                                        {convertBytesToText(file.size)}
                                                    </span>
                                                </span>
                                            </div>
                                        </div>
                                        <Button
                                            variant="ghost"
                                            size="icon"
                                            className="absolute -top-0 -right-2 h-5 w-5 rounded-full bg-neutral-200 dark:bg-neutral-600 hover:bg-neutral-300 dark:hover:bg-neutral-500 opacity-0 group-hover:opacity-100 transition-opacity"
                                            onClick={() => {
                                                setAttachedFiles((prevFiles) => {
                                                    const removeFile = file.name;
                                                    if (!prevFiles) return null;
                                                    const updatedFiles = Array.from(
                                                        prevFiles,
                                                    ).filter((file) => file.name !== removeFile);
                                                    const dataTransfer = new DataTransfer();
                                                    updatedFiles.forEach((file) =>
                                                        dataTransfer.items.add(file),
                                                    );

                                                    const filteredConvertedAttachedFiles =
                                                        convertedAttachedFiles.filter(
                                                            (file) => file.name !== removeFile,
                                                        );

                                                    props.setUploadedFiles(
                                                        filteredConvertedAttachedFiles,
                                                    );
                                                    setConvertedAttachedFiles(
                                                        filteredConvertedAttachedFiles,
                                                    );
                                                    return dataTransfer.files;
                                                });
                                            }}
                                        >
                                            <X className="h-3 w-3" />
                                        </Button>
                                    </div>
                                </DialogTrigger>
                                <DialogContent>
                                    <DialogHeader>
                                        <DialogTitle>{file.name}</DialogTitle>
                                    </DialogHeader>
                                    <DialogDescription>
                                        <ScrollArea className="h-72 w-full rounded-md">
                                            {file.content}
                                        </ScrollArea>
                                    </DialogDescription>
                                </DialogContent>
                            </Dialog>
                        ))}
                </div>
                <div
                    className={`${styles.actualInputArea} justify-between dark:bg-neutral-700 relative ${isDragAndDropping && "animate-pulse"}`}
                    onDragOver={handleDragOver}
                    onDragLeave={handleDragLeave}
                    onDrop={handleDragAndDropFiles}
                >
                    <input
                        type="file"
                        accept=".pdf,.doc,.docx,.txt,.md,.org,.jpg,.jpeg,.png,.webp,.py,.tsx,.js,.json,.html,.css,.ipynb"
                        multiple={true}
                        ref={fileInputRef}
                        onChange={handleFileChange}
                        style={{ display: "none" }}
                    />

                    <div className="flex items-center">
                        <TooltipProvider>
                            <Tooltip>
                                <TooltipTrigger asChild>
                                    <Button
                                        variant={"ghost"}
                                        className="!bg-none p-0 m-2 h-auto text-3xl rounded-full text-gray-300 hover:text-gray-500"
                                        disabled={!props.isLoggedIn}
                                        onClick={handleFileButtonClick}
                                        ref={fileInputButtonRef}
                                    >
                                        <Paperclip className="w-8 h-8" />
                                    </Button>
                                </TooltipTrigger>
                                <TooltipContent>
                                    Attach a PDF, plain text file, or image
                                </TooltipContent>
                            </Tooltip>
                        </TooltipProvider>
                    </div>
                    <div className="flex-grow flex flex-col w-full gap-1.5 relative">
                        <Textarea
                            ref={chatInputRef}
                            className={`border-none focus:border-none
                                focus:outline-none focus-visible:ring-transparent
                                w-full h-16 min-h-16 max-h-[128px] md:py-4 rounded-lg resize-none dark:bg-neutral-700
                                ${props.isMobileWidth ? "text-md" : "text-lg"}`}
                            placeholder="Type / for commands and @ for filters"
                            id="message"
                            autoFocus={true}
                            value={message}
                            onKeyDown={(e: React.KeyboardEvent<HTMLTextAreaElement>) => {
                                // Keyboard handling for file/menu suggestions
                                if (showFileSuggestions) {
                                    if (e.key === "ArrowDown") {
                                        e.preventDefault();
                                        setHighlightIndex((i) => Math.min(i + 1, suggestions.length - 1));
                                        return;
                                    }
                                    if (e.key === "ArrowUp") {
                                        e.preventDefault();
                                        setHighlightIndex((i) => Math.max(i - 1, 0));
                                        return;
                                    }
                                    if (e.key === "Enter") {
                                        e.preventDefault();
                                        const selected = suggestions[highlightIndex];
                                        if (!selected) return;
                                        const choice = String(selected).toLowerCase();
                                        if (menuLevel === "main") {
                                            if (choice === "file") {
                                                insertTriggerToken(chatInputRef.current!, "file:", "file", "file");
                                            } else if (choice === "date") {
                                                insertTriggerToken(chatInputRef.current!, "dt", "date", "date");
                                            } else if (choice === "word") {
                                                // go to word submenu (include/exclude)
                                                setMenuLevel("word");
                                                updateSuggestions("", "word");
                                            }
                                            return;
                                        }
                                        if (menuLevel === "file") {
                                            insertSuggestionAtCaret(chatInputRef.current!, selected);
                                            return;
                                        }
                                        if (menuLevel === "date") {
                                            if (choice === "exact") setDateMode("exact");
                                            else if (choice === "after") setDateMode(">=");
                                            else setDateMode("<=");
                                            // insert corresponding operator token (dt:" or dt>=" or dt<=" ) and enter dateMode
                                            const token = choice === "exact" ? 'dt:"' : choice === "after" ? 'dt>="' : 'dt<="';
                                            insertTriggerToken(chatInputRef.current!, token, "dateMode", "dateMode");
                                            setTimeout(() => prepareDateOperatorInfo(choice), 0);
                                            return;
                                        }
                                        if (menuLevel === "dateMode") {
                                            insertDateFilterFromCaret(chatInputRef.current!);
                                            return;
                                        }
                                        if (menuLevel === "word") {
                                            // Insert the operator token (+" or -") and enter wordMode
                                            const sign = choice === "include" ? "+" : "-";
                                            const token = sign + '"';
                                            insertTriggerToken(chatInputRef.current!, token, "wordMode", "wordMode");
                                            setWordMode(choice === "include" ? "include" : "exclude");
                                            // prepare operator info after insertion so mirror works
                                            setTimeout(() => prepareWordOperatorInfo(sign as "+" | "-"), 0);
                                            return;
                                        }
                                        if (menuLevel === "wordMode") {
                                            insertWordFilterFromCaret(chatInputRef.current!);
                                            return;
                                        }
                                    }
                                    if (e.key === "Escape") {
                                        setShowFileSuggestions(false);
                                        triggerRef.current = null;
                                        setMenuLevel(null);
                                        return;
                                    }
                                }

                                if (
                                    e.key === "Enter" &&
                                    !e.shiftKey &&
                                    !props.isMobileWidth &&
                                    !recording &&
                                    message
                                ) {
                                    setImageUploaded(false);
                                    setImagePaths([]);
                                    e.preventDefault();
                                    onSendMessage();
                                }
                            }}
                            onChange={(e: React.ChangeEvent<HTMLTextAreaElement>) => {
                                setMessage(e.target.value);

                                // Check caret trigger
                                const ta = e.target as HTMLTextAreaElement;
                                const caret = ta.selectionStart;
                                const trig = getTriggerAtCaret(ta.value, caret);
                                if (trig) {
                                    triggerRef.current = { kind: trig.kind, triggerLen: trig.triggerLen };
                                    setShowFileSuggestions(true);
                                    if (trig.kind === "at") {
                                        setMenuLevel("main");
                                        updateSuggestions("", "main");
                                    } else if (trig.kind === "file") {
                                        setMenuLevel("file");
                                        updateSuggestions(trig.query || "", "file");
                                    } else if (trig.kind === "date") {
                                        // if there's anything after dt/ date (like dt:2023 or dt>=), go to dateMode
                                        if (trig.query && trig.query.length > 0) {
                                            setMenuLevel("dateMode");
                                            updateSuggestions(trig.query || "", "dateMode");
                                        } else {
                                            setMenuLevel("date");
                                            updateSuggestions("", "date");
                                        }
                                    } else if (trig.kind === "wordMode") {
                                        // typed +"... or -"... -> open wordMode (single mirror suggestion)
                                        setMenuLevel("wordMode");
                                        updateSuggestions(trig.query || "", "wordMode");
                                    } else {
                                        setMenuLevel("main");
                                        updateSuggestions(trig.query || "", "main");
                                    }
                                } else {
                                    setShowFileSuggestions(false);
                                    triggerRef.current = null;
                                    setMenuLevel(null);
                                }
                            }}
                            disabled={recording}
                        />
                        {showFileSuggestions && (
                            <div ref={suggestionsContainerRef}>
                                <QueryFiltersPopover
                                    menuLevel={menuLevel}
                                    suggestions={suggestions}
                                    highlightIndex={highlightIndex}
                                    setHighlightIndex={setHighlightIndex}
                                    chatInputRef={chatInputRef}
                                    insertSuggestionAtCaret={(s: string) => insertSuggestionAtCaret(chatInputRef.current!, s)}
                                    insertTriggerToken={(token: string, newMenuLevel: any, levelOverride?: any) => insertTriggerToken(chatInputRef.current!, token, newMenuLevel, levelOverride)}
                                    prepareDateOperatorInfo={(c: string) => prepareDateOperatorInfo(c)}
                                    prepareWordOperatorInfo={(s: any) => prepareWordOperatorInfo(s)}
                                    setDateMode={(m: any) => setDateMode(m)}
                                    setWordMode={(m: any) => setWordMode(m)}
                                    setMenuLevel={(m: any) => setMenuLevel(m)}
                                    insertDateFilterFromCaret={() => insertDateFilterFromCaret(chatInputRef.current!)}
                                    insertWordFilterFromCaret={() => insertWordFilterFromCaret(chatInputRef.current!)}
                                />
                            </div>
                        )}
                    </div>
                    <div className="flex items-end pb-2">
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
                                            disabled={props.sendDisabled || !props.isLoggedIn}
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
                                        {props.sendDisabled && !message ? (
                                            <Button
                                                variant="default"
                                                className={`${props.agentColor ? convertToBGClass(props.agentColor) : "bg-orange-300 hover:bg-orange-500"} rounded-full p-1 m-2 h-auto text-3xl transition transform md:hover:-translate-y-1`}
                                                onClick={() => {
                                                    props.setTriggeredAbort(true);
                                                }}
                                            >
                                                <Stop weight="fill" className="w-6 h-6" />
                                            </Button>
                                        ) : (
                                            <Button
                                                variant="default"
                                                className={`${!message || recording || "hidden"} ${props.agentColor ? convertToBGClass(props.agentColor) : "bg-orange-300 hover:bg-orange-500"} rounded-full p-1 m-2 h-auto text-3xl transition transform md:hover:-translate-y-1`}
                                                disabled={props.sendDisabled || !props.isLoggedIn}
                                                onClick={() => {
                                                    setMessage("Listening...");
                                                    setRecording(!recording);
                                                }}
                                            >
                                                <Microphone weight="fill" className="w-6 h-6" />
                                            </Button>
                                        )}
                                    </TooltipTrigger>
                                    <TooltipContent>
                                        {props.sendDisabled
                                            ? "Click here to stop the streaming."
                                            : "Click to transcribe your message with voice."}
                                    </TooltipContent>
                                </Tooltip>
                            </TooltipProvider>
                        )}
                        <Button
                            className={`${(!message || recording) && "hidden"} ${props.agentColor ? convertToBGClass(props.agentColor) : "bg-orange-300 hover:bg-orange-500"} rounded-full p-1 m-2 h-auto text-3xl transition transform md:hover:-translate-y-1`}
                            disabled={!message || recording || !props.isLoggedIn}
                            onClick={onSendMessage}
                        >
                            <ArrowUp className="w-6 h-6" weight="bold" />
                        </Button>
                    </div>
                </div>
                <TooltipProvider>
                    <Tooltip>
                        <TooltipTrigger asChild>
                            <Button
                                variant="ghost"
                                className="float-right justify-center gap-1 flex items-center p-1.5 mr-2 h-fit"
                                disabled={props.sendDisabled || !props.isLoggedIn}
                                ref={researchModeRef}
                                onClick={() => {
                                    setUseResearchMode(!useResearchMode);
                                    chatInputRef?.current?.focus();
                                }}
                            >
                                <span className="text-muted-foreground text-sm">Research Mode</span>
                                {useResearchMode ? (
                                    <ToggleRight
                                        weight="fill"
                                        className={`w-6 h-6 inline-block ${props.agentColor ? convertColorToTextClass(props.agentColor) : convertColorToTextClass("orange")} rounded-full`}
                                    />
                                ) : (
                                    <ToggleLeft
                                        weight="fill"
                                        className={`w-6 h-6 inline-block ${convertColorToTextClass("gray")} rounded-full`}
                                    />
                                )}
                            </Button>
                        </TooltipTrigger>
                        <TooltipContent className="text-xs">
                            Research Mode allows you to get more deeply researched, detailed
                            responses. Response times may be longer.
                        </TooltipContent>
                    </Tooltip>
                </TooltipProvider>
            </div>
        </>
    );
});

ChatInputArea.displayName = "ChatInputArea";
