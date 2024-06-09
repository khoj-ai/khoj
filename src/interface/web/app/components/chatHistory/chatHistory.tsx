'use client'

import styles from './chatHistory.module.css';
import { useRef, useEffect, useState } from 'react';

import markdownIt from 'markdown-it';
import mditHljs from "markdown-it-highlightjs";

import renderMathInElement from 'katex/contrib/auto-render';
import 'katex/dist/katex.min.css';
import 'highlight.js/styles/github.css'

const md = new markdownIt({
    html: true,
    linkify: true,
    typographer: true
});

md.use(mditHljs, {
    inline: true,
    code: true
});

interface ChatHistory {
    [key: string]: string
}

interface ChatHistoryProps {
    conversationId: string;
}

interface Context {
    compiled: string;
    file: string;
}

interface WebPage {
    link: string;
    query: string;
    snippet: string;
}

interface OnlineContextData {
    webpages: WebPage[];
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

interface ChatMessage {
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

interface ChatHistoryData {
    chat: ChatMessage[];
    agent: AgentData;
    conversation_id: string;
    slug: string;
}

interface ChatResponse {
    status: string;
    response: ChatHistoryData;
}

function ChatMessage({ chatmessage: chatMessage }: { chatmessage: ChatMessage}) {
    let message = chatMessage.message;
    console.log("original chat message", message);
    // Replace LaTeX delimiters with placeholders
    message = message.replace(/\\\(/g, 'LEFTPAREN').replace(/\\\)/g, 'RIGHTPAREN')
                        .replace(/\\\[/g, 'LEFTBRACKET').replace(/\\\]/g, 'RIGHTBRACKET');

    if (chatMessage.intent && chatMessage.intent.type == "text-to-image2") {
        console.log("TEXT TO IMAGE")
        message = `![generated_image](${message})\n\n${chatMessage.intent["inferred-queries"][0]}`
    }

    let markdownRendered = md.render(message);

    // Replace placeholders with LaTeX delimiters
    markdownRendered = markdownRendered.replace(/LEFTPAREN/g, '\\(').replace(/RIGHTPAREN/g, '\\)')
                        .replace(/LEFTBRACKET/g, '\\[').replace(/RIGHTBRACKET/g, '\\]');

    // Render LaTeX
    // useEffect(() => {
    //     console.log("in the latex render")
    //     renderMathInElement(document.body, {
    //         delimiters: [
    //             { left: "$$", right: "$$", display: true },
    //             { left: "\\[", right: "\\]", display: true },
    //             { left: "\\(", right: "\\)", display: false },
    //             { left: "$", right: "$", display: false }
    //         ]
    //     });

    //     console.log("new document.body", document.body);
    // }, [markdownRendered]);

    return (
        <div className={`${styles.chatMessageContainer} ${styles[chatMessage.by]}`}>
            <div className={styles.chatFooter}>
                {chatMessage.by}
            </div>
            <div className={styles.chatMessage} dangerouslySetInnerHTML={{ __html: markdownRendered }} />
        </div>
    )
}

export default function ChatHistory(props: ChatHistoryProps) {
    const [data, setData] = useState<ChatHistoryData | null>(null);
    const [isLoading, setLoading] = useState(true)
    const ref = useRef<HTMLDivElement>(null);
    const chatHistoryRef = useRef(null);


	useEffect(() => {

        fetch(`/api/chat/history?client=web&conversation_id=${props.conversationId}`)
            .then(response => response.json())
            .then((chatData: ChatResponse) => {
                setLoading(false);
                // Render chat options, if any
                if (chatData) {
                    console.log(chatData);
                    setData(chatData.response);
                }
            })
            .catch(err => {
                console.error(err);
                return;
            });
	}, [props.conversationId]);


    useEffect(() => {
        const observer = new MutationObserver((mutationsList, observer) => {

            console.log("mutation observed");
            // If the addedNodes property has one or more nodes
            for(let mutation of mutationsList) {
                if(mutation.type === 'childList' && mutation.addedNodes.length > 0) {
                    console.log('A child node has been added.');
                    // Call your function here
                    renderMathInElement(document.body, {
                        delimiters: [
                            { left: '$$', right: '$$', display: true },
                            { left: '\\[', right: '\\]', display: true },
                            { left: '$', right: '$', display: false },
                            { left: '\\(', right: '\\)', display: false },
                        ],
                    });
                }
            }
        });

        if (chatHistoryRef.current) {
            observer.observe(chatHistoryRef.current, { childList: true });
        }

        // Clean up the observer on component unmount
        return () => observer.disconnect();
    }, []);

    return (
        <div className={styles.main + " " + styles.chatLayout}>
            <div ref={ref}>
                <div className={styles.chatHistory} ref={chatHistoryRef}>
                    {data && data.chat.map((chatMessage, index) => (
                        <ChatMessage key={index} chatmessage={chatMessage} />
                    ))}
                    {/* {data && ChatMessageRenderWrapper(data)} */}
                </div>
            </div>
        </div>
    )
}
