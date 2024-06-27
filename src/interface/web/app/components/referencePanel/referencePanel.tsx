'use client'

import styles from "./referencePanel.module.css";

import { useState } from "react";

import markdownIt from "markdown-it";
const md = new markdownIt({
    html: true,
    linkify: true,
    typographer: true
});

import { SingleChatMessage, Context, WebPage, OnlineContextData } from "../chatMessage/chatMessage";

interface ReferencePanelProps {
    referencePanelData: SingleChatMessage | null;
    setShowReferencePanel: (showReferencePanel: boolean) => void;
}

export function hasValidReferences(referencePanelData: SingleChatMessage | null) {
    return (
        referencePanelData &&
        (
            (referencePanelData.context && referencePanelData.context.length > 0) ||
            (referencePanelData.onlineContext && Object.keys(referencePanelData.onlineContext).length > 0 &&
                Object.values(referencePanelData.onlineContext).some(
                    (onlineContextData) =>
                        (onlineContextData.webpages && onlineContextData.webpages.length > 0)|| onlineContextData.answerBox || onlineContextData.peopleAlsoAsk || onlineContextData.knowledgeGraph))
        )
    );
}

function CompiledReference(props: { context: (Context | string) }) {

    let snippet = "";
    let file = "";
    if (typeof props.context === "string") {
        // Treat context as a string and get the first line for the file name
        const lines = props.context.split("\n");
        file = lines[0];
        snippet = lines.slice(1).join("\n");
    } else {
        const context = props.context as Context;
        snippet = context.compiled;
        file = context.file;
    }

    const [showSnippet, setShowSnippet] = useState(false);

    return (
        <div className={styles.singleReference}>
            <div className={styles.contextReference} onClick={() => setShowSnippet(!showSnippet)}>
                <div className={styles.referencePanelTitle}>
                    {file}
                </div>
                <div className={styles.referencePanelContent} style={{ display: showSnippet ? "block" : "none" }}>
                    <div>
                        {snippet}
                    </div>
                </div>
            </div>
        </div>
    )
}

function WebPageReference(props: { webpages: WebPage, query: string | null }) {

    let snippet = md.render(props.webpages.snippet);

    const [showSnippet, setShowSnippet] = useState(false);

    return (
        <div className={styles.onlineReference} onClick={() => setShowSnippet(!showSnippet)}>
            <div className={styles.onlineReferenceTitle}>
                <a href={props.webpages.link} target="_blank" rel="noreferrer">
                    {
                        props.query ? (
                            <span>
                                {props.query}
                            </span>
                        ) : <span>
                            {props.webpages.query}
                        </span>
                    }
                </a>
            </div>
            <div className={styles.onlineReferenceContent} style={{ display: showSnippet ? "block" : "none" }}>
                <div dangerouslySetInnerHTML={{ __html: snippet }}></div>
            </div>
        </div>
    )
}

function OnlineReferences(props: { onlineContext: OnlineContextData, query: string}) {

    const webpages = props.onlineContext.webpages;
    const answerBox = props.onlineContext.answerBox;
    const peopleAlsoAsk = props.onlineContext.peopleAlsoAsk;
    const knowledgeGraph = props.onlineContext.knowledgeGraph;

    return (
        <div className={styles.singleReference}>
            {
                webpages && (
                    !Array.isArray(webpages) ? (
                        <WebPageReference webpages={webpages} query={props.query} />
                    ) : (
                        webpages.map((webpage, index) => {
                            return <WebPageReference webpages={webpage} key={index} query={null} />
                        })
                    )
                )
            }
            {
                answerBox && (
                    <div className={styles.onlineReference}>
                        <div className={styles.onlineReferenceTitle}>
                            {answerBox.title}
                        </div>
                        <div className={styles.onlineReferenceContent}>
                            <div>
                                {answerBox.answer}
                            </div>
                        </div>
                    </div>
                )
            }
            {
                peopleAlsoAsk && peopleAlsoAsk.map((people, index) => {
                    return (
                        <div className={styles.onlineReference} key={index}>
                            <div className={styles.onlineReferenceTitle}>
                                <a href={people.link} target="_blank" rel="noreferrer">
                                    {people.question}
                                </a>
                            </div>
                            <div className={styles.onlineReferenceContent}>
                                <div>
                                    {people.snippet}
                                </div>
                            </div>
                        </div>
                    )
                })
            }
            {
                knowledgeGraph && (
                    <div className={styles.onlineReference}>
                        <div className={styles.onlineReferenceTitle}>
                            <a href={knowledgeGraph.descriptionLink} target="_blank" rel="noreferrer">
                                {knowledgeGraph.title}
                            </a>
                        </div>
                        <div className={styles.onlineReferenceContent}>
                            <div>
                                {knowledgeGraph.description}
                            </div>
                        </div>
                    </div>
                )
            }
        </div>

    )
}

export default function ReferencePanel(props: ReferencePanelProps) {

    if (!props.referencePanelData) {
        return null;
    }

    if (!hasValidReferences(props.referencePanelData)) {
        return null;
    }

	return (
		<div className={`${styles.panel}`}>
            References <button onClick={() => props.setShowReferencePanel(false)}>Hide</button>
            {
                props.referencePanelData?.context.map((context, index) => {
                    return <CompiledReference context={context} key={index} />
                })
            }
            {
                Object.entries(props.referencePanelData?.onlineContext || {}).map(([key, onlineContextData], index) => {
                    return <OnlineReferences onlineContext={onlineContextData} query={key} key={index} />
                })
            }
		</div>
	);
}
