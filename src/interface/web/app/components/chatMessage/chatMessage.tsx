"use client";

import styles from "./chatMessage.module.css";

import markdownIt from "markdown-it";
import mditHljs from "markdown-it-highlightjs";
import React, { useEffect, useRef, useState, forwardRef } from "react";
import { createRoot } from "react-dom/client";
import { createPortal } from "react-dom";

import "katex/dist/katex.min.css";

import {
    TeaserReferencesSection,
    constructAllReferences,
} from "@/app/components/referencePanel/referencePanel";
import { renderCodeGenImageInline } from "@/app/common/chatFunctions";
import { fileLinksPlugin } from "@/app/components/chatMessage/fileLinksPlugin";
import FileContentSnippet from "@/app/components/chatMessage/FileContentSnippet";
import { useFileContent } from "@/app/components/chatMessage/useFileContent";

import {
    ThumbsUp,
    ThumbsDown,
    Copy,
    Brain,
    Cloud,
    Folder,
    Book,
    Aperture,
    SpeakerHigh,
    MagnifyingGlass,
    Pause,
    Palette,
    ClipboardText,
    Check,
    Code,
    Shapes,
    Trash,
    Toolbox,
    Browser,
    ArrowClockwise,
} from "@phosphor-icons/react";

import DOMPurify from "dompurify";
import { InlineLoading } from "../loading/loading";
import { convertColorToTextClass } from "@/app/common/colorUtils";
import { AgentData } from "@/app/components/agentCard/agentCard";

import renderMathInElement from "katex/contrib/auto-render";
import ExcalidrawComponent from "../excalidraw/excalidraw";
import { AttachedFileText } from "../chatInputArea/chatInputArea";
import {
    Dialog,
    DialogContent,
    DialogDescription,
    DialogHeader,
    DialogTrigger,
} from "@/components/ui/dialog";
import { DialogTitle } from "@radix-ui/react-dialog";
import { convertBytesToText } from "@/app/common/utils";
import { ScrollArea } from "@/components/ui/scroll-area";
import { getIconFromFilename } from "@/app/common/iconUtils";
import Mermaid from "../mermaid/mermaid";

const md = new markdownIt({
    html: true,
    linkify: true,
    typographer: true,
});

md.use(mditHljs, {
    inline: true,
    code: true,
});

md.use(fileLinksPlugin);

export interface Context {
    compiled: string;
    file: string;
}

export interface OnlineContext {
    [key: string]: OnlineContextData;
}

export interface WebPage {
    link: string;
    query: string;
    snippet: string;
}

interface OrganicContext {
    snippet: string;
    title: string;
    link: string;
}

interface PeopleAlsoAsk {
    link: string;
    question: string;
    snippet: string;
    title: string;
}

export interface OnlineContextData {
    webpages: WebPage[];
    answerBox: {
        answer: string;
        source: string;
        title: string;
    };
    knowledgeGraph: {
        attributes: {
            [key: string]: string;
        };
        description: string;
        descriptionLink: string;
        descriptionSource: string;
        imageUrl: string;
        title: string;
        type: string;
    };
    organic: OrganicContext[];
    peopleAlsoAsk: PeopleAlsoAsk[];
}

export interface CodeContext {
    [key: string]: CodeContextData;
}

export interface CodeContextData {
    code: string;
    results: {
        success: boolean;
        output_files: CodeContextFile[];
        std_out: string;
        std_err: string;
        code_runtime?: number;
    };
}

export interface CodeContextFile {
    filename: string;
    b64_data: string;
}

interface Intent {
    type: string;
    query: string;
    "memory-type": string;
    "inferred-queries": string[];
}

export interface TrainOfThoughtObject {
    type: string;
    data: string;
}

export interface SingleChatMessage {
    automationId: string;
    by: string;
    message: string;
    created: string;
    context: Context[];
    onlineContext: OnlineContext;
    codeContext: CodeContext;
    trainOfThought?: TrainOfThoughtObject[];
    rawQuery?: string;
    intent?: Intent;
    agent?: AgentData;
    images?: string[];
    conversationId: string;
    turnId?: string;
    queryFiles?: AttachedFileText[];
    excalidrawDiagram?: string;
    mermaidjsDiagram?: string;
}

export interface StreamMessage {
    rawResponse: string;
    trainOfThought: string[];
    context: Context[];
    onlineContext: OnlineContext;
    codeContext: CodeContext;
    completed: boolean;
    rawQuery: string;
    timestamp: string;
    agent?: AgentData;
    images?: string[];
    intentType?: string;
    inferredQueries?: string[];
    turnId?: string;
    queryFiles?: AttachedFileText[];
    excalidrawDiagram?: string;
    mermaidjsDiagram?: string;
    generatedFiles?: AttachedFileText[];
    generatedImages?: string[];
    generatedExcalidrawDiagram?: string;
    generatedMermaidjsDiagram?: string;
}

export interface ChatHistoryData {
    chat: SingleChatMessage[];
    agent: AgentData;
    conversation_id: string;
    slug: string;
    is_owner: boolean;
}

function sendFeedback(uquery: string, kquery: string, sentiment: string) {
    fetch("/api/chat/feedback", {
        method: "POST",
        headers: {
            "Content-Type": "application/json",
        },
        body: JSON.stringify({ uquery: uquery, kquery: kquery, sentiment: sentiment }),
    });
}

function FeedbackButtons({ uquery, kquery }: { uquery: string; kquery: string }) {
    // Tri-state feedback state.
    // Null = no feedback, true = positive feedback, false = negative feedback.
    const [feedbackState, setFeedbackState] = useState<boolean | null>(null);

    useEffect(() => {
        if (feedbackState !== null) {
            setTimeout(() => {
                setFeedbackState(null);
            }, 2000);
        }
    }, [feedbackState]);

    return (
        <div className={`${styles.feedbackButtons} flex align-middle justify-center items-center`}>
            <button
                title="Like"
                className={styles.thumbsUpButton}
                disabled={feedbackState !== null}
                onClick={() => {
                    sendFeedback(uquery, kquery, "positive");
                    setFeedbackState(true);
                }}
            >
                {feedbackState === true ? (
                    <ThumbsUp alt="Liked Message" className="text-green-500" weight="fill" />
                ) : (
                    <ThumbsUp
                        alt="Like Message"
                        className="hsl(var(--muted-foreground)) hover:text-green-500"
                    />
                )}
            </button>
            <button
                title="Dislike"
                className={styles.thumbsDownButton}
                disabled={feedbackState !== null}
                onClick={() => {
                    sendFeedback(uquery, kquery, "negative");
                    setFeedbackState(false);
                }}
            >
                {feedbackState === false ? (
                    <ThumbsDown alt="Disliked Message" className="text-red-500" weight="fill" />
                ) : (
                    <ThumbsDown
                        alt="Dislike Message"
                        className="hsl(var(--muted-foreground)) hover:text-red-500"
                    />
                )}
            </button>
        </div>
    );
}

interface ChatMessageProps {
    chatMessage: SingleChatMessage;
    isMobileWidth: boolean;
    customClassName?: string;
    borderLeftColor?: string;
    isLastMessage?: boolean;
    agent?: AgentData;
    onDeleteMessage: (turnId?: string) => void;
    onRetryMessage?: (query: string, turnId?: string) => void;
    conversationId: string;
    turnId?: string;
    generatedImage?: string;
    excalidrawDiagram?: string;
    mermaidjsDiagram?: string;
    generatedFiles?: AttachedFileText[];
}

interface TrainOfThoughtProps {
    message: string;
    primary: boolean;
    agentColor: string;
}

function chooseIconFromHeader(header: string, iconColor: string) {
    const compareHeader = header.toLowerCase();
    const classNames = `inline mt-1 mr-2 ${iconColor} h-4 w-4`;
    if (compareHeader.includes("understanding")) {
        return <Brain className={`${classNames}`} />;
    }

    if (compareHeader.includes("generating")) {
        return <Cloud className={`${classNames}`} />;
    }

    if (compareHeader.includes("tools")) {
        return <Toolbox className={`${classNames}`} />;
    }

    if (compareHeader.includes("notes")) {
        return <Folder className={`${classNames}`} />;
    }

    if (compareHeader.includes("read")) {
        return <Book className={`${classNames}`} />;
    }

    if (compareHeader.includes("search")) {
        return <MagnifyingGlass className={`${classNames}`} />;
    }

    if (
        compareHeader.includes("summary") ||
        compareHeader.includes("summarize") ||
        compareHeader.includes("enhanc")
    ) {
        return <Aperture className={`${classNames}`} />;
    }

    if (compareHeader.includes("diagram")) {
        return <Shapes className={`${classNames}`} />;
    }

    if (compareHeader.includes("paint")) {
        return <Palette className={`${classNames}`} />;
    }

    if (compareHeader.includes("code")) {
        return <Code className={`${classNames}`} />;
    }

    if (compareHeader.includes("operating")) {
        return <Browser className={`${classNames}`} />;
    }

    return <Brain className={`${classNames}`} />;
}

export function TrainOfThought(props: TrainOfThoughtProps) {
    // The train of thought comes in as a markdown-formatted string. It starts with a heading delimited by two asterisks at the start and end and a colon, followed by the message. Example: **header**: status. This function will parse the message and render it as a div.
    let extractedHeader = props.message.match(/\*\*(.*)\*\*/);
    let header = extractedHeader ? extractedHeader[1] : "";
    const iconColor = props.primary ? convertColorToTextClass(props.agentColor) : "text-gray-500";
    const icon = chooseIconFromHeader(header, iconColor);
    let message = props.message;

    // Render screenshot image in screenshot action message
    let jsonMessage = null;
    try {
        const jsonMatch = message.match(
            /\{.*("action": "screenshot"|"type": "screenshot"|"image": "data:image\/.*").*\}/,
        );
        if (jsonMatch) {
            jsonMessage = JSON.parse(jsonMatch[0]);
            const screenshotHtmlString = `<img src="${jsonMessage.image}" alt="State of environment" class="max-w-full" />`;
            message = message.replace(
                `:\n**Action**: ${jsonMatch[0]}`,
                `\n\n- ${jsonMessage.text}\n${screenshotHtmlString}`,
            );
        }
    } catch (e) {
        console.error("Failed to parse screenshot data", e);
    }

    // Render the sanitized train of thought as markdown
    let markdownRendered = DOMPurify.sanitize(md.render(message));

    // Remove any header tags from the rendered markdown
    markdownRendered = markdownRendered.replace(/<h[1-6].*?<\/h[1-6]>/g, "");

    return (
        <div
            className={`${styles.trainOfThoughtElement} break-words items-center ${props.primary ? "text-gray-400" : "text-gray-300"} ${styles.trainOfThought} ${props.primary ? styles.primary : ""}`}
        >
            {icon}
            <div dangerouslySetInnerHTML={{ __html: markdownRendered }} className="break-words" />
        </div>
    );
}

const ChatMessage = forwardRef<HTMLDivElement, ChatMessageProps>((props, ref) => {
    const [copySuccess, setCopySuccess] = useState<boolean>(false);
    const [isHovering, setIsHovering] = useState<boolean>(false);
    const [textRendered, setTextRendered] = useState<string>("");
    const [markdownRendered, setMarkdownRendered] = useState<string>("");
    const [isPlaying, setIsPlaying] = useState<boolean>(false);
    const [interrupted, setInterrupted] = useState<boolean>(false);
    const [excalidrawData, setExcalidrawData] = useState<string>("");
    const [mermaidjsData, setMermaidjsData] = useState<string>("");

    // State for file content preview on file link click, hover
    const [previewOpen, setPreviewOpen] = useState<boolean>(false);
    const [previewFilePath, setPreviewFilePath] = useState<string>("");
    const [previewLineNumber, setPreviewLineNumber] = useState<number | undefined>(undefined);
    const [previewLoading, setPreviewLoading] = useState<boolean>(false);
    const [previewError, setPreviewError] = useState<string | null>(null);
    const [previewContent, setPreviewContent] = useState<string>("");

    const [hoverOpen, setHoverOpen] = useState<boolean>(false);
    const [hoverFilePath, setHoverFilePath] = useState<string>("");
    const [hoverLineNumber, setHoverLineNumber] = useState<number | undefined>(undefined);
    const [hoverLoading, setHoverLoading] = useState<boolean>(false);
    const [hoverError, setHoverError] = useState<string | null>(null);
    const [hoverContent, setHoverContent] = useState<string>("");
    const [hoverPos, setHoverPos] = useState<{ x: number; y: number }>({ x: 0, y: 0 });
    const hoverCloseTimeoutRef = useRef<number | null>(null);

    const interruptedRef = useRef<boolean>(false);
    const messageRef = useRef<HTMLDivElement>(null);
    const selectionActiveRef = useRef<boolean>(false);

    useEffect(() => {
        interruptedRef.current = interrupted;
    }, [interrupted]);

    useEffect(() => {
        const observer = new MutationObserver((mutationsList, observer) => {
            // If the addedNodes property has one or more nodes
            if (messageRef.current) {
                for (let mutation of mutationsList) {
                    if (mutation.type === "childList" && mutation.addedNodes.length > 0) {
                        // Call your function here
                        renderMathInElement(messageRef.current, {
                            delimiters: [
                                { left: "$$", right: "$$", display: true },
                                { left: "\\[", right: "\\]", display: true },
                                { left: "\\(", right: "\\)", display: false },
                            ],
                        });
                    }
                }
            }
        });

        if (messageRef.current) {
            observer.observe(messageRef.current, { childList: true });
        }

        // Clean up the observer on component unmount
        return () => observer.disconnect();
    }, [messageRef.current]);

    useEffect(() => {
        // Prepare initial message for rendering
        let message = props.chatMessage.message;

        if (props.chatMessage.excalidrawDiagram) {
            setExcalidrawData(props.chatMessage.excalidrawDiagram);
        }

        if (props.chatMessage.mermaidjsDiagram) {
            setMermaidjsData(props.chatMessage.mermaidjsDiagram);
        }

        // Replace file links with base64 data
        message = renderCodeGenImageInline(message, props.chatMessage.codeContext);

        // Add code context files to the message
        if (props.chatMessage.codeContext) {
            Object.entries(props.chatMessage.codeContext).forEach(([key, value]) => {
                value.results?.output_files?.forEach((file) => {
                    if (file.filename.endsWith(".png") || file.filename.endsWith(".jpg")) {
                        // Don't add the image again if it's already in the message!
                        if (!message.includes(`![${file.filename}](`)) {
                            message += `\n\n![${file.filename}](data:image/png;base64,${file.b64_data})`;
                        }
                    }
                });
            });
        }

        // Handle rendering user attached or khoj generated images
        let messageForClipboard = message;
        let messageToRender = message;
        if (props.chatMessage.images && props.chatMessage.images.length > 0) {
            const sanitizedImages = props.chatMessage.images.map((image) => {
                const decodedImage = image.startsWith("data%3Aimage")
                    ? decodeURIComponent(image)
                    : image;
                return DOMPurify.sanitize(decodedImage);
            });
            const imagesInMd = sanitizedImages
                .map((sanitizedImage, index) => {
                    return `![rendered image ${index + 1}](${sanitizedImage})`;
                })
                .join("\n");
            const imagesInHtml = sanitizedImages
                .map((sanitizedImage, index) => {
                    return `<div class="${styles.imageWrapper}"><img src="${sanitizedImage}" alt="rendered image ${index + 1}" /></div>`;
                })
                .join("");
            const userImagesInHtml = `<div class="${styles.imagesContainer}">${imagesInHtml}</div>`;
            messageForClipboard = `${imagesInMd}\n\n${messageForClipboard}`;
            messageToRender = `${userImagesInHtml}${messageToRender}`;
        }

        // Set the message text
        setTextRendered(messageForClipboard);

        // Replace LaTeX delimiters with placeholders
        messageToRender = messageToRender
            .replace(/\\\(/g, "LEFTPAREN")
            .replace(/\\\)/g, "RIGHTPAREN")
            .replace(/\\\[/g, "LEFTBRACKET")
            .replace(/\\\]/g, "RIGHTBRACKET");

        // Preprocess file:// links so markdown-it processes them
        // We convert them to a custom scheme (filelink://) and handle in the plugin
        messageToRender = messageToRender.replace(
            /\[([^\]]+)\]\(file:\/\/([^)]+)\)/g,
            (match, text, path) => {
                // Use a special scheme that markdown-it will process
                return `[${text}](filelink://${path})`;
            },
        );

        // Render the markdown
        let markdownRendered = md.render(messageToRender);

        // Revert placeholders with LaTeX delimiters
        markdownRendered = markdownRendered
            .replace(/LEFTPAREN/g, "\\(")
            .replace(/RIGHTPAREN/g, "\\)")
            .replace(/LEFTBRACKET/g, "\\[")
            .replace(/RIGHTBRACKET/g, "\\]");

        // Sanitize and set the rendered markdown
        // Configure DOMPurify to allow file link attributes
        const cleanMarkdown = DOMPurify.sanitize(markdownRendered, {
            ADD_ATTR: ["data-file-path", "data-line-number"],
        });

        setMarkdownRendered(cleanMarkdown);
    }, [props.chatMessage.message, props.chatMessage.images, props.chatMessage.intent]);

    useEffect(() => {
        if (copySuccess) {
            setTimeout(() => {
                setCopySuccess(false);
            }, 2000);
        }
    }, [copySuccess]);

    useEffect(() => {
        if (messageRef.current) {
            const preElements = messageRef.current.querySelectorAll("pre > .hljs");
            preElements.forEach((preElement) => {
                if (!preElement.querySelector(`${styles.codeCopyButton}`)) {
                    const copyButton = document.createElement("button");
                    const copyIcon = <ClipboardText size={24} />;
                    createRoot(copyButton).render(copyIcon);

                    copyButton.className = `hljs ${styles.codeCopyButton}`;
                    copyButton.addEventListener("click", () => {
                        let textContent = preElement.textContent || "";
                        // Strip any leading $ characters
                        textContent = textContent.replace(/^\$+/, "");
                        // Remove 'Copy' if it's at the start of the string
                        textContent = textContent.replace(/^Copy/, "");
                        textContent = textContent.trim();
                        navigator.clipboard.writeText(textContent);

                        // Replace the copy icon with a checkmark
                        const copiedIcon = <Check size={24} />;
                        createRoot(copyButton).render(copiedIcon);
                    });
                    preElement.prepend(copyButton);
                }
            });

            // Event delegation on the message container for reliability
            const container = messageRef.current;

            const delegatedPointerDown = (ev: Event) => {
                const e = ev as MouseEvent;
                const target = e.target as HTMLElement | null;
                const anchor = target?.closest?.("a.file-link") as HTMLAnchorElement | null;
                if (!anchor) return;
                e.preventDefault();
                e.stopPropagation();
                const path = anchor.getAttribute("data-file-path") || "";
                const line = anchor.getAttribute("data-line-number") || undefined;
                if (!path) return;
                // Close hover popover if open
                setHoverOpen(false);
                setPreviewFilePath(path);
                setPreviewLineNumber(line ? parseInt(line) : undefined);
                setPreviewOpen(true);
            };

            let currentHoverAnchor: HTMLAnchorElement | null = null;
            const delegatedMouseOver = (ev: Event) => {
                const e = ev as MouseEvent;
                const target = e.target as HTMLElement | null;
                const anchor = target?.closest?.("a.file-link") as HTMLAnchorElement | null;
                if (!anchor) return;
                if (currentHoverAnchor === anchor) return;
                currentHoverAnchor = anchor;
                const rect = anchor.getBoundingClientRect();
                const path = anchor.getAttribute("data-file-path") || "";
                const line = anchor.getAttribute("data-line-number") || undefined;
                if (!path) return;
                setHoverPos({ x: Math.max(8, rect.left), y: rect.bottom + 6 });
                setHoverFilePath(path);
                setHoverLineNumber(line ? parseInt(line) : undefined);
                if (hoverCloseTimeoutRef.current) {
                    window.clearTimeout(hoverCloseTimeoutRef.current);
                    hoverCloseTimeoutRef.current = null;
                }
                // Open immediately for reliability
                setHoverOpen(true);
            };

            const delegatedMouseOut = (ev: Event) => {
                const e = ev as MouseEvent;
                const target = e.target as HTMLElement | null;
                const related = e.relatedTarget as HTMLElement | null;
                const anchor = target?.closest?.("a.file-link") as HTMLAnchorElement | null;
                const stillInsideAnchor = !!(related && anchor && anchor.contains(related));
                // If moving between descendants of the same anchor, ignore
                if (stillInsideAnchor) return;
                // Schedule close; will be canceled if we move into the popover
                if (hoverCloseTimeoutRef.current) {
                    window.clearTimeout(hoverCloseTimeoutRef.current);
                }
                hoverCloseTimeoutRef.current = window.setTimeout(() => {
                    setHoverOpen(false);
                    currentHoverAnchor = null;
                    hoverCloseTimeoutRef.current = null;
                }, 200);
            };

            const delegatedKeyDown = (ev: Event) => {
                const e = ev as KeyboardEvent;
                const target = e.target as HTMLElement | null;
                const anchor = target?.closest?.("a.file-link") as HTMLAnchorElement | null;
                if (!anchor) return;
                if (e.key !== "Enter" && e.key !== " ") return;
                e.preventDefault();
                e.stopPropagation();
                const path = anchor.getAttribute("data-file-path") || "";
                const line = anchor.getAttribute("data-line-number") || undefined;
                if (!path) return;
                setHoverOpen(false);
                setPreviewFilePath(path);
                setPreviewLineNumber(line ? parseInt(line) : undefined);
                setPreviewOpen(true);
            };

            container.addEventListener("pointerdown", delegatedPointerDown);
            container.addEventListener("keydown", delegatedKeyDown);
            container.addEventListener("mouseover", delegatedMouseOver);
            container.addEventListener("mouseout", delegatedMouseOut);

            renderMathInElement(messageRef.current, {
                delimiters: [
                    { left: "$$", right: "$$", display: true },
                    { left: "\\[", right: "\\]", display: true },
                    { left: "\\(", right: "\\)", display: false },
                ],
            });
            // Cleanup old listeners when content changes
            return () => {
                container.removeEventListener("pointerdown", delegatedPointerDown);
                container.removeEventListener("keydown", delegatedKeyDown);
                container.removeEventListener("mouseover", delegatedMouseOver);
                container.removeEventListener("mouseout", delegatedMouseOut);
                if (hoverCloseTimeoutRef.current) {
                    window.clearTimeout(hoverCloseTimeoutRef.current);
                    hoverCloseTimeoutRef.current = null;
                }
            };
        }
    }, [markdownRendered, messageRef]);

    // Track if there is an active text selection inside this message (no React re-render)
    useEffect(() => {
        const onSelectionChange = () => {
            try {
                const sel = document.getSelection();
                selectionActiveRef.current = !!(
                    sel && sel.rangeCount > 0 && sel.toString() &&
                    messageRef.current &&
                    (messageRef.current.contains(sel.anchorNode) || messageRef.current.contains(sel.focusNode))
                );
            } catch {
                selectionActiveRef.current = false;
            }
        };

        // Attach scroll handler to the chat viewport only, not window
        const scrollAreaViewport = document.querySelector<HTMLElement>("[data-radix-scroll-area-viewport]");
        const onScroll = () => {
            // De-bounce lightly to read selection after scroll applies
            setTimeout(() => {
                const sel = document.getSelection();
                if (!sel || !sel.toString() || !messageRef.current) {
                    selectionActiveRef.current = false;
                }
            }, 30);
        };

        document.addEventListener("selectionchange", onSelectionChange);
        if (scrollAreaViewport) scrollAreaViewport.addEventListener("scroll", onScroll, { passive: true });

        return () => {
            document.removeEventListener("selectionchange", onSelectionChange);
            if (scrollAreaViewport) scrollAreaViewport.removeEventListener("scroll", onScroll);
        };
    }, []);

    // Fetch file content for dialog and hover using shared hook
    const {
        content: previewContentHook,
        loading: previewLoadingHook,
        error: previewErrorHook,
    } = useFileContent(previewFilePath, previewOpen);
    const {
        content: hoverContentHook,
        loading: hoverLoadingHook,
        error: hoverErrorHook,
    } = useFileContent(hoverFilePath, hoverOpen);

    useEffect(() => {
        setPreviewContent(previewContentHook);
    }, [previewContentHook]);
    useEffect(() => {
        setPreviewLoading(previewLoadingHook);
    }, [previewLoadingHook]);
    useEffect(() => {
        setPreviewError(previewErrorHook);
    }, [previewErrorHook]);

    useEffect(() => {
        setHoverContent(hoverContentHook);
    }, [hoverContentHook]);
    useEffect(() => {
        setHoverLoading(hoverLoadingHook);
    }, [hoverLoadingHook]);
    useEffect(() => {
        setHoverError(hoverErrorHook);
    }, [hoverErrorHook]);

    function formatDate(timestamp: string) {
        // Format date in HH:MM, DD MMM YYYY format
        let date = new Date(timestamp + "Z");
        let time_string = date
            .toLocaleTimeString("en-US", { hour: "2-digit", minute: "2-digit", hour12: true })
            .toUpperCase();
        let date_string = date
            .toLocaleString("en-US", { year: "numeric", month: "short", day: "2-digit" })
            .replaceAll("-", " ");
        return `${time_string} on ${date_string}`;
    }

    function renderTimeStamp(timestamp: string) {
        if (!timestamp.endsWith("Z")) {
            timestamp = timestamp + "Z";
        }
        const messageDateTime = new Date(timestamp);
        const currentDateTime = new Date();
        const timeDiff = currentDateTime.getTime() - messageDateTime.getTime();

        if (timeDiff < 60e3) {
            return "Just now";
        }

        if (timeDiff < 3600e3) {
            // Using Math.round for closer to actual time representation
            return `${Math.round(timeDiff / 60e3)}m ago`;
        }

        if (timeDiff < 86400e3) {
            return `${Math.round(timeDiff / 3600e3)}h ago`;
        }

        return `${Math.round(timeDiff / 86400e3)}d ago`;
    }

    function constructClasses(chatMessage: SingleChatMessage) {
        let classes = [styles.chatMessageContainer];
        if (chatMessage.by === "khoj") {
            classes.push("shadow-md");
        }
        classes.push(styles[chatMessage.by]);
        if (!chatMessage.message) {
            classes.push(styles.emptyChatMessage);
        }

        if (props.customClassName) {
            classes.push(styles[`${chatMessage.by}${props.customClassName}`]);
        }

        return classes.join(" ");
    }

    function chatMessageWrapperClasses(chatMessage: SingleChatMessage) {
        let classes = [styles.chatMessageWrapper];
        classes.push(styles[chatMessage.by]);
        if (chatMessage.by === "khoj") {
            classes.push(
                `border-l-4 border-opacity-50 ${"border-l-" + props.borderLeftColor || "border-l-orange-400"}`,
            );
        }
        return classes.join(" ");
    }

    async function playTextToSpeech() {
        // Browser native speech API
        // const utterance = new SpeechSynthesisUtterance(props.chatMessage.message);
        // speechSynthesis.speak(utterance);

        // Using the Khoj speech API
        // Break the message up into chunks of sentences
        const sentenceRegex = /[^.!?]+[.!?]*/g;
        const chunks = props.chatMessage.message.match(sentenceRegex) || [];

        if (!chunks || chunks.length === 0 || !chunks[0]) return;

        setIsPlaying(true);

        let nextBlobPromise = fetchBlob(chunks[0]);

        for (let i = 0; i < chunks.length; i++) {
            if (interruptedRef.current) {
                break; // Exit the loop if interrupted
            }

            const currentBlobPromise = nextBlobPromise;
            if (i < chunks.length - 1) {
                nextBlobPromise = fetchBlob(chunks[i + 1]);
            }

            try {
                const blob = await currentBlobPromise;
                const url = URL.createObjectURL(blob);
                await playAudio(url);
            } catch (error) {
                console.error("Error:", error);
                break; // Exit the loop on error
            }
        }

        setIsPlaying(false);
        setInterrupted(false); // Reset interrupted state after playback
    }

    async function fetchBlob(text: string) {
        const response = await fetch(`/api/chat/speech?text=${encodeURIComponent(text)}`, {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
            },
        });

        if (!response.ok) {
            throw new Error("Network response was not ok");
        }

        return await response.blob();
    }

    function playAudio(url: string) {
        return new Promise((resolve, reject) => {
            const audio = new Audio(url);
            audio.onended = resolve;
            audio.onerror = reject;
            audio.play();
        });
    }

    const deleteMessage = async (message: SingleChatMessage) => {
        const turnId = message.turnId || props.turnId;
        const response = await fetch("/api/chat/conversation/message", {
            method: "DELETE",
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify({
                conversation_id: props.conversationId,
                turn_id: turnId,
            }),
        });

        if (response.ok) {
            // Update the UI after successful deletion
            props.onDeleteMessage(turnId);
        } else {
            console.error("Failed to delete message");
        }
    };

    const allReferences = constructAllReferences(
        props.chatMessage.context,
        props.chatMessage.onlineContext,
        props.chatMessage.codeContext,
    );

    function handleMouseEnter(event: React.MouseEvent) {
        setIsHovering(true);
    }
    function handleMouseLeave(event: React.MouseEvent) {
        // Prevent hover collapse while a selection inside this message is active
        if (selectionActiveRef.current) return;
        setIsHovering(false);
    }

    return (
        <div
            ref={ref}
            className={constructClasses(props.chatMessage)}
            onMouseLeave={handleMouseLeave}
            onMouseEnter={handleMouseEnter}
            data-created={formatDate(props.chatMessage.created)}
        >
            <div className={chatMessageWrapperClasses(props.chatMessage)}>
                {props.chatMessage.queryFiles && props.chatMessage.queryFiles.length > 0 && (
                    <div className="flex flex-wrap flex-col mb-2 max-w-full">
                        {props.chatMessage.queryFiles.map((file, index) => (
                            <Dialog key={index}>
                                <DialogTrigger asChild>
                                    <div
                                        className="flex items-center space-x-2 cursor-pointer bg-gray-500 bg-opacity-25 rounded-lg p-2 w-full
                                "
                                    >
                                        <div className="flex-shrink-0">
                                            {getIconFromFilename(file.file_type)}
                                        </div>
                                        <span className="truncate flex-1 min-w-0 max-w-[200px]">
                                            {file.name}
                                        </span>
                                        {file.size && (
                                            <span className="text-gray-400 flex-shrink-0">
                                                ({convertBytesToText(file.size)})
                                            </span>
                                        )}
                                    </div>
                                </DialogTrigger>
                                <DialogContent>
                                    <DialogHeader>
                                        <DialogTitle>
                                            <div className="truncate min-w-0 break-words break-all text-wrap max-w-full whitespace-normal">
                                                {file.name}
                                            </div>
                                        </DialogTitle>
                                    </DialogHeader>
                                    <DialogDescription>
                                        <ScrollArea className="h-72 w-full rounded-md break-words break-all text-wrap">
                                            {file.content}
                                        </ScrollArea>
                                    </DialogDescription>
                                </DialogContent>
                            </Dialog>
                        ))}
                    </div>
                )}
                <div
                    ref={messageRef}
                    className={styles.chatMessage}
                    dangerouslySetInnerHTML={{ __html: markdownRendered }}
                />
                {/* File preview hover dialog */}
                {hoverOpen &&
                    typeof window !== "undefined" &&
                    createPortal(
                        <div
                            onMouseEnter={() => {
                                if (hoverCloseTimeoutRef.current) {
                                    window.clearTimeout(hoverCloseTimeoutRef.current);
                                    hoverCloseTimeoutRef.current = null;
                                }
                                setHoverOpen(true);
                            }}
                            onMouseDown={(e) => {
                                // If user clicks the hover preview, open the dialog for the same file
                                e.preventDefault();
                                e.stopPropagation();
                                setHoverOpen(false);
                                if (hoverFilePath) {
                                    setPreviewFilePath(hoverFilePath);
                                    setPreviewLineNumber(hoverLineNumber);
                                    setPreviewOpen(true);
                                }
                            }}
                            onMouseLeave={() => {
                                if (hoverCloseTimeoutRef.current) {
                                    window.clearTimeout(hoverCloseTimeoutRef.current);
                                }
                                hoverCloseTimeoutRef.current = window.setTimeout(() => {
                                    setHoverOpen(false);
                                    hoverCloseTimeoutRef.current = null;
                                }, 200);
                            }}
                            style={{
                                position: "fixed",
                                left: hoverPos.x,
                                top: hoverPos.y,
                                zIndex: 9999,
                            }}
                            className="w-96 max-h-80 rounded-md border bg-popover p-4 text-popover-foreground shadow-md"
                        >
                            <div className="space-y-2">
                                <div className="flex items-center text-sm font-medium">
                                    <span className="truncate">
                                        {hoverFilePath.split("/").pop() || hoverFilePath}
                                    </span>
                                    {hoverLineNumber && (
                                        <span className="text-gray-500 ml-2">
                                            - Line {hoverLineNumber}
                                        </span>
                                    )}
                                </div>
                                <ScrollArea className="max-h-60">
                                    {hoverLoading && (
                                        <div className="flex items-center justify-center p-4">
                                            <InlineLoading />
                                        </div>
                                    )}
                                    {!hoverLoading && hoverError && (
                                        <div className="p-3 text-red-500 text-sm">
                                            Error: {hoverError}
                                        </div>
                                    )}
                                    {!hoverLoading && !hoverError && (
                                        <div className="text-sm">
                                            <FileContentSnippet
                                                content={hoverContent}
                                                targetLine={hoverLineNumber}
                                                maxLines={8}
                                            />
                                        </div>
                                    )}
                                </ScrollArea>
                            </div>
                        </div>,
                        document.body,
                    )}
                {/* File preview popup dialog */}
                <Dialog open={previewOpen} onOpenChange={setPreviewOpen}>
                    <DialogContent className="max-w-2xl">
                        <DialogHeader>
                            <DialogTitle>
                                <div className="truncate min-w-0 break-words break-all text-wrap max-w-full whitespace-normal">
                                    {previewFilePath.split("/").pop() || previewFilePath}
                                    <span className="text-gray-500 ml-2">
                                        {previewLineNumber ? `- Line ${previewLineNumber}` : ""}
                                    </span>
                                </div>
                            </DialogTitle>
                        </DialogHeader>
                        <div className="text-left">
                            <ScrollArea className="h-80 w-full rounded-md">
                                {previewLoading && (
                                    <div className="flex items-center justify-center p-4">
                                        <InlineLoading />
                                    </div>
                                )}
                                {!previewLoading && previewError && (
                                    <div className="p-3 text-red-500 text-sm">
                                        Error: {previewError}
                                    </div>
                                )}
                                {!previewLoading && !previewError && (
                                    <div className="text-sm">
                                        <FileContentSnippet
                                            content={previewContent}
                                            targetLine={previewLineNumber}
                                            maxLines={20}
                                        />
                                    </div>
                                )}
                            </ScrollArea>
                        </div>
                    </DialogContent>
                </Dialog>
                {excalidrawData && <ExcalidrawComponent data={excalidrawData} />}
                {mermaidjsData && <Mermaid chart={mermaidjsData} />}
            </div>
            <div className={styles.teaserReferencesContainer}>
                <TeaserReferencesSection
                    isMobileWidth={props.isMobileWidth}
                    notesReferenceCardData={allReferences.notesReferenceCardData}
                    onlineReferenceCardData={allReferences.onlineReferenceCardData}
                    codeReferenceCardData={allReferences.codeReferenceCardData}
                />
            </div>
            <div className={styles.chatFooter}>
                <div className={`${styles.chatFooterInner} ${isHovering || props.isMobileWidth || isPlaying
                    ? styles.visibleControls
                    : styles.hiddenControls
                    }`}>
                    <div
                        title={formatDate(props.chatMessage.created)}
                        className={`text-gray-400 relative top-0 left-4`}
                    >
                        {renderTimeStamp(props.chatMessage.created)}
                    </div>
                    <div className={`${styles.chatButtons} shadow-sm`}>
                        {props.chatMessage.by === "khoj" &&
                            (isPlaying ? (
                                interrupted ? (
                                    <InlineLoading iconClassName="p-0" className="m-0" />
                                ) : (
                                    <button
                                        title="Pause Speech"
                                        onClick={(event) => setInterrupted(true)}
                                    >
                                        <Pause
                                            alt="Pause Message"
                                            className="hsl(var(--muted-foreground))"
                                        />
                                    </button>
                                )
                            ) : (
                                <button title="Speak" onClick={(event) => playTextToSpeech()}>
                                    <SpeakerHigh
                                        alt="Speak Message"
                                        className="hsl(var(--muted-foreground)) hover:text-green-500"
                                    />
                                </button>
                            ))}
                        {props.chatMessage.turnId && (
                            <button
                                title="Delete"
                                className={`${styles.deleteButton}`}
                                onClick={() => deleteMessage(props.chatMessage)}
                            >
                                <Trash
                                    alt="Delete Message"
                                    className="hsl(var(--muted-foreground)) hover:text-red-500"
                                />
                            </button>
                        )}
                        {props.chatMessage.by === "khoj" &&
                            props.onRetryMessage &&
                            props.isLastMessage && (
                                <button
                                    title="Retry"
                                    className={`${styles.retryButton}`}
                                    onClick={() => {
                                        const turnId = props.chatMessage.turnId || props.turnId;
                                        const query =
                                            props.chatMessage.rawQuery ||
                                            props.chatMessage.intent?.query;
                                        if (query) {
                                            props.onRetryMessage?.(query, turnId);
                                        } else {
                                            console.error("No original query found for retry");
                                            const fallbackQuery = prompt(
                                                "Enter the original query to retry:",
                                            );
                                            if (fallbackQuery) {
                                                props.onRetryMessage?.(fallbackQuery, turnId);
                                            }
                                        }
                                    }}
                                >
                                    <ArrowClockwise
                                        alt="Retry Message"
                                        className="hsl(var(--muted-foreground)) hover:text-blue-500"
                                    />
                                </button>
                            )}
                        <button
                            title="Copy"
                            className={`${styles.copyButton}`}
                            onClick={() => {
                                navigator.clipboard.writeText(textRendered);
                                setCopySuccess(true);
                            }}
                        >
                            {copySuccess ? (
                                <Copy
                                    alt="Copied Message"
                                    weight="fill"
                                    className="text-green-500"
                                />
                            ) : (
                                <Copy
                                    alt="Copy Message"
                                    className="hsl(var(--muted-foreground)) hover:text-green-500"
                                />
                            )}
                        </button>
                        {props.chatMessage.by === "khoj" &&
                            (props.chatMessage.intent ? (
                                <FeedbackButtons
                                    uquery={props.chatMessage.intent.query}
                                    kquery={props.chatMessage.message}
                                />
                            ) : (
                                <FeedbackButtons
                                    uquery={
                                        props.chatMessage.rawQuery || props.chatMessage.message
                                    }
                                    kquery={props.chatMessage.message}
                                />
                            ))}
                    </div>
                </div>
            </div>
        </div>
    );
});

ChatMessage.displayName = "ChatMessage";

export default ChatMessage;
