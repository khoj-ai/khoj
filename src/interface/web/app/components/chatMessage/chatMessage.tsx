"use client"

import styles from './chatMessage.module.css';

import markdownIt from 'markdown-it';
import mditHljs from "markdown-it-highlightjs";
import React, { useEffect, useRef, useState } from 'react';
import Image from 'next/image';

import 'katex/dist/katex.min.css';
import 'highlight.js/styles/github.css'

import { hasValidReferences } from '../referencePanel/referencePanel';

import { ThumbsUp, ThumbsDown, Copy, Brain, Cloud, Folder, Book, Aperture } from '@phosphor-icons/react';
import { MagnifyingGlass } from '@phosphor-icons/react/dist/ssr';
import { compare } from 'swr/_internal';

const md = new markdownIt({
    html: true,
    linkify: true,
    typographer: true
});

md.use(mditHljs, {
    inline: true,
    code: true
});

export interface Context {
    compiled: string;
    file: string;
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
    }
    knowledgeGraph: {
        attributes: {
            [key: string]: string;
        }
        description: string;
        descriptionLink: string;
        descriptionSource: string;
        imageUrl: string;
        title: string;
        type: string;
    }
    organic: OrganicContext[];
    peopleAlsoAsk: PeopleAlsoAsk[];
}

interface AgentData {
    name: string;
    avatar: string;
    slug: string;
}

interface Intent {
    type: string;
    "inferred-queries": string[];
}

export interface SingleChatMessage {
    automationId: string;
    by: string;
    intent: {
        [key: string]: string
    }
    message: string;
    context: Context[];
    created: string;
    onlineContext: {
        [key: string]: OnlineContextData
    }
}

export interface StreamMessage {
    rawResponse: string;
    trainOfThought: string[];
    context: Context[];
    onlineContext: {
        [key: string]: OnlineContextData
    }
    completed: boolean;
    rawQuery: string;
    timestamp: string;
}


export interface ChatHistoryData {
    chat: SingleChatMessage[];
    agent: AgentData;
    conversation_id: string;
    slug: string;
}

function FeedbackButtons() {
    return (
        <div className={`${styles.feedbackButtons} flex align-middle justify-center items-center`}>
            <button className={styles.thumbsUpButton}>
                <ThumbsUp color='hsl(var(--muted-foreground))' />
            </button>
            <button className={styles.thumbsDownButton}>
                <ThumbsDown color='hsl(var(--muted-foreground))' />
            </button>
        </div>
    )
}

function onClickMessage(event: React.MouseEvent<any>, chatMessage: SingleChatMessage, setReferencePanelData: Function, setShowReferencePanel: Function) {
    // console.log("Clicked on message", chatMessage);
    setReferencePanelData(chatMessage);
    setShowReferencePanel(true);
}

interface ChatMessageProps {
    chatMessage: SingleChatMessage;
    setReferencePanelData: Function;
    setShowReferencePanel: Function;
    customClassName?: string;
    borderLeftColor?: string;
}

interface TrainOfThoughtProps {
    message: string;
    primary: boolean;
}

function chooseIconFromHeader(header: string, iconColor: string) {
    const compareHeader = header.toLowerCase();
    if (compareHeader.includes("understanding")) {
        return <Brain className={`inline mr-2 ${iconColor}`} />
    }

    if (compareHeader.includes("generating")) {
        return <Cloud className={`inline mr-2 ${iconColor}`} />;
    }

    if (compareHeader.includes("data sources")) {
        return <Folder className={`inline mr-2 ${iconColor}`} />;
    }

    if (compareHeader.includes("notes")) {
        return <Folder className={`inline mr-2 ${iconColor}`} />;
    }

    if (compareHeader.includes("read")) {
        return <Book className={`inline mr-2 ${iconColor}`} />;
    }

    if (compareHeader.includes("search")) {
        return <MagnifyingGlass className={`inline mr-2 ${iconColor}`} />;
    }

    if (compareHeader.includes("summary") || compareHeader.includes("summarize")) {
        return <Aperture className={`inline mr-2 ${iconColor}`} />;
    }

    return <Brain className={`inline mr-2 ${iconColor}`} />;
}

export function TrainOfThought(props: TrainOfThoughtProps) {
    // The train of thought comes in as a markdown-formatted string. It starts with a heading delimited by two asterisks at the start and end and a colon, followed by the message. Example: **header**: status. This function will parse the message and render it as a div.
    let extractedHeader = props.message.match(/\*\*(.*)\*\*/);
    let header = extractedHeader ? extractedHeader[1] : "";
    const iconColor = props.primary ? 'text-orange-400' : 'text-gray-500';
    const icon = chooseIconFromHeader(header, iconColor);
    let markdownRendered = md.render(props.message);
    return (
        <div className={`flex items-center ${props.primary ? 'text-gray-400' : 'text-gray-300'} ${styles.trainOfThought} ${props.primary ? styles.primary : ''}`} >
            {icon}
            <div dangerouslySetInnerHTML={{ __html: markdownRendered }} />
        </div>
    )
}

export default function ChatMessage(props: ChatMessageProps) {
    const [copySuccess, setCopySuccess] = useState<boolean>(false);

    let message = props.chatMessage.message;

    // Replace LaTeX delimiters with placeholders
    message = message.replace(/\\\(/g, 'LEFTPAREN').replace(/\\\)/g, 'RIGHTPAREN')
                        .replace(/\\\[/g, 'LEFTBRACKET').replace(/\\\]/g, 'RIGHTBRACKET');

    if (props.chatMessage.intent && props.chatMessage.intent.type == "text-to-image2") {
        message = `![generated_image](${message})\n\n${props.chatMessage.intent["inferred-queries"][0]}`
    }

    let markdownRendered = md.render(message);

    // Replace placeholders with LaTeX delimiters
    markdownRendered = markdownRendered.replace(/LEFTPAREN/g, '\\(').replace(/RIGHTPAREN/g, '\\)')
                        .replace(/LEFTBRACKET/g, '\\[').replace(/RIGHTBRACKET/g, '\\]');

    const messageRef = useRef<HTMLDivElement>(null);

    useEffect(() => {
        if (messageRef.current) {
            const preElements = messageRef.current.querySelectorAll('pre > .hljs');
            preElements.forEach((preElement) => {
                const copyButton = document.createElement('button');
                const copyImage = document.createElement('img');
                copyImage.src = '/copy-button.svg';
                copyImage.alt = 'Copy';
                copyImage.width = 24;
                copyImage.height = 24;
                copyButton.appendChild(copyImage);
                copyButton.className = `hljs ${styles.codeCopyButton}`
                copyButton.addEventListener('click', () => {
                    let textContent = preElement.textContent || '';
                    // Strip any leading $ characters
                    textContent = textContent.replace(/^\$+/, '');
                    // Remove 'Copy' if it's at the start of the string
                    textContent = textContent.replace(/^Copy/, '');
                    textContent = textContent.trim();
                    navigator.clipboard.writeText(textContent);
                });
                preElement.prepend(copyButton);
            });
        }
    }, [markdownRendered]);

    function renderTimeStamp(timestamp: string) {
        var dateObject = new Date(timestamp);
        var month = dateObject.getMonth() + 1;
        var date = dateObject.getDate();
        var year = dateObject.getFullYear();
        const formattedDate = `${month}/${date}/${year}`;
        return `${formattedDate} ${dateObject.toLocaleTimeString()}`;
    }

    useEffect(() => {
        if (copySuccess) {
            setTimeout(() => {
                setCopySuccess(false);
            }, 2000);
        }
    }, [copySuccess]);

    let referencesValid = hasValidReferences(props.chatMessage);

    function constructClasses(chatMessage: SingleChatMessage) {
        let classes = [styles.chatMessageContainer];
        classes.push(styles[chatMessage.by]);

        if (props.customClassName) {
            classes.push(styles[`${chatMessage.by}${props.customClassName}`])
        }

        return classes.join(' ');
    }

    function chatMessageWrapperClasses(chatMessage: SingleChatMessage) {
        let classes = [styles.chatMessageWrapper];
        classes.push(styles[chatMessage.by]);

        if (chatMessage.by === "khoj") {
            const dynamicBorderColor = `border-l-${props.borderLeftColor}`;
            classes.push(`border-l-4 border-opacity-50 border-l-orange-400 ${dynamicBorderColor}`);
        }

        return classes.join(' ');
    }

    return (
        <div
            className={constructClasses(props.chatMessage)}
            onClick={props.chatMessage.by === "khoj" ? (event) => onClickMessage(event, props.chatMessage, props.setReferencePanelData, props.setShowReferencePanel) : undefined}>
                {/* <div className={styles.chatFooter}> */}
                    {/* {props.chatMessage.by} */}
                {/* </div> */}
                <div className={chatMessageWrapperClasses(props.chatMessage)}>
                    <div ref={messageRef} className={styles.chatMessage} dangerouslySetInnerHTML={{ __html: markdownRendered }} />
                    {/* Add a copy button, thumbs up, and thumbs down buttons */}
                    <div className={styles.chatFooter}>
                        <div className={styles.chatTimestamp}>
                            {renderTimeStamp(props.chatMessage.created)}
                        </div>
                        <div className={styles.chatButtons}>
                            {
                                referencesValid &&
                                <div className={styles.referenceButton}>
                                    <button onClick={(event) => onClickMessage(event, props.chatMessage, props.setReferencePanelData, props.setShowReferencePanel)}>
                                        References
                                    </button>
                                </div>
                            }
                            <button className={`${styles.copyButton}`} onClick={() => {
                                    navigator.clipboard.writeText(props.chatMessage.message);
                                    setCopySuccess(true);
                            }}>
                                {
                                    copySuccess ?
                                        <Copy color='green' />
                                        : <Copy color='hsl(var(--muted-foreground))' />
                                }
                            </button>
                            {
                                props.chatMessage.by === "khoj" && <FeedbackButtons />
                            }
                        </div>
                    </div>
                </div>
        </div>
    )
}
