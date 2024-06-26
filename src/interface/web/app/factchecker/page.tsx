'use client'

import styles from './factChecker.module.css';
import { useAuthenticatedData } from '@/app/common/auth';
import { useState, useEffect } from 'react';
import ChatMessage, { Context, OnlineContextData, WebPage } from '../components/chatMessage/chatMessage';
import { ModelPicker } from '../components/modelPicker/modelPicker';


const chatURL = "/api/chat";
const verificationPrecursor = "Limit your search to reputable sources. Search the internet for relevant supporting or refuting information. Do not reference my notes. Refuse to answer any queries that are not falsifiable by informing me that you will not answer the question. You're not permitted to ask follow-up questions, so do the best with what you have. Respond with **TRUE** or **FALSE** or **INCONCLUSIVE**, then provide your justification. Fact Check:"


interface ResponseWithReferences {
    context?: Context[];
    online?: {
        [key: string]: OnlineContextData
    }
    response?: string;
}

function handleCompiledReferences(chunk: string, currentResponse: string) {
    const rawReference = chunk.split("### compiled references:")[1];
    const rawResponse = chunk.split("### compiled references:")[0];
    let references: ResponseWithReferences = {};

    // Set the initial response
    references.response = currentResponse + rawResponse;

    const rawReferenceAsJson = JSON.parse(rawReference);
    if (rawReferenceAsJson instanceof Array) {
        references.context = rawReferenceAsJson;
    } else if (typeof rawReferenceAsJson === "object" && rawReferenceAsJson !== null) {
        references.online = rawReferenceAsJson;
    }

    console.log(references);
    return references;
}


async function verifyStatement(
    message: string,
    conversationId: string,
    setIsLoading: (loading: boolean) => void,
    setInitialResponse: (response: string) => void,
    setInitialReferences: (references: ResponseWithReferences) => void) {
        setIsLoading(true);
        // Send a message to the chat server to verify the fact
        console.log("Verifying statement: ", message);
        let verificationMessage = `${verificationPrecursor} ${message}`;
        const apiURL = `${chatURL}?q=${encodeURIComponent(verificationMessage)}&client=web&stream=true&conversation_id=${conversationId}`;
        try {
            const response = await fetch(apiURL);
            if (!response.body) throw new Error("No response body found");

            const reader = response.body?.getReader();
            let decoder = new TextDecoder();
            let result = "";

            while (true) {
                const { done, value } = await reader.read();
                if (done) break;

                let chunk = decoder.decode(value, { stream: true });

                if (chunk.includes("### compiled references:")) {
                    const references = handleCompiledReferences(chunk, result);
                    if (references.response) {
                        result = references.response;
                        setInitialResponse(references.response);
                        setInitialReferences(references);
                    }
                } else {
                    result += chunk;
                    setInitialResponse(result);
                }
            }
        } catch (error) {
            console.error("Error verifying statement: ", error);
        } finally {
            setIsLoading(false);
        }
}


async function spawnNewConversation(setConversationID: (conversationID: string) => void) {

    const agentSlug = "correct";

    let createURL = `/api/chat/sessions?client=web`;

    // if (agentSlug) {
    //     createURL += `&agent_slug=${agentSlug}`;
    // }

    const response = await fetch(createURL, { method: "POST" });

    const data = await response.json();
    console.log("Spawned new conversation: ", data);
    setConversationID(data.conversation_id);
}


interface ReferenceVerificationProps {
    message: string;
    additionalLink: string;
    conversationId: string;
}

function ReferenceVerification(props: ReferenceVerificationProps) {
    const [initialResponse, setInitialResponse] = useState("");
    const [isLoading, setIsLoading] = useState(true);
    const verificationStatement = `${props.message}. Use this link for reference: ${props.additionalLink}`;

    useEffect(() => {
        verifyStatement(verificationStatement, props.conversationId, setIsLoading, setInitialResponse, () => {});
    }, [verificationStatement, props.conversationId]);

    return (
        <div>
            {isLoading &&
                <div className={styles.loadingVerification}>
                    Checking... {props.additionalLink}
                    <div className={styles.spinner}>
                        <div className={styles.dot1}></div>
                        <div className={styles.dot2}></div>
                    </div>
                </div>}
            <div className={styles.responseText}>
                <ChatMessage chatMessage={
                    {
                        automationId: "",
                        by: "AI",
                        intent: {},
                        message: initialResponse,
                        context: [],
                        created: (new Date()).toISOString(),
                        onlineContext: {}
                    }
                } setReferencePanelData={() => {}} setShowReferencePanel={() => {}} />
            </div>
        </div>
    )
}


export default function FactChecker() {
    const [factToVerify, setFactToVerify] = useState("");
    const [officialFactToVerify, setOfficialFactToVerify] = useState("");
    const [isLoading, setIsLoading] = useState(false);
    const [initialResponse, setInitialResponse] = useState("");
    const [clickedVerify, setClickedVerify] = useState(false);
    const [initialReferences, setInitialReferences] = useState<ResponseWithReferences>();
    const [conversationID, setConversationID] = useState("");

    let userData = useAuthenticatedData();

    useEffect(() => {
        const storedFact = localStorage.getItem('factToVerify');
        if (storedFact) {
            setFactToVerify(storedFact);
        }
    }, []);

    useEffect(() => {
        if (!clickedVerify) return;
        if (!userData) {
            let currentURL = window.location.href;
            window.location.href = `/login?next=${currentURL}`;
        }

        setInitialReferences(undefined);
        setInitialResponse("");

        spawnNewConversation(setConversationID);
        setOfficialFactToVerify(factToVerify);
        setClickedVerify(false);
    }, [clickedVerify]);

    useEffect(() => {
        if (!conversationID) return;
        verifyStatement(officialFactToVerify, conversationID, setIsLoading, setInitialResponse, setInitialReferences);
    }, [conversationID]);

    // Store factToVerify in localStorage whenever it changes
    useEffect(() => {
        localStorage.setItem('factToVerify', factToVerify);
    }, [factToVerify]);

    const renderReferences = (conversationId: string, initialReferences: ResponseWithReferences, officialFactToVerify: string) => {
        const seenLinks = new Set();

        // Any links that are present in webpages should not be searched again
        Object.entries(initialReferences.online || {}).map(([key, onlineData], index) => {
            const webpages = onlineData?.webpages || [];
            // Webpage can be a list or a single object
            if (webpages instanceof Array) {
                for (let i = 0; i < webpages.length; i++) {
                    const webpage = webpages[i];
                    const additionalLink = webpage.link || '';
                    if (seenLinks.has(additionalLink)) {
                        console.log("collision on link: ", additionalLink);
                        return null;
                    }
                    seenLinks.add(additionalLink);
                }
            } else {
                let singleWebpage = webpages as WebPage;
                const additionalLink = singleWebpage.link || '';
                if (seenLinks.has(additionalLink)) {
                    console.log("collision on link: ", additionalLink);
                    return null;
                }
                seenLinks.add(additionalLink);
            }
        });

        return Object.entries(initialReferences.online || {}).map(([key, onlineData], index) => {
            let additionalLink = '';

            // Loop through organic links until we find one that hasn't been searched
            for (let i = 0; i < onlineData?.organic?.length; i++) {
                const webpage = onlineData?.organic?.[i];
                additionalLink = webpage.link || '';

                if (!seenLinks.has(additionalLink)) {
                    break;
                }
            }

            seenLinks.add(additionalLink);

            if (additionalLink === '') return null;

            return (
              <div key={key + index} className={styles.reference}>
                <div>
                    <a className={styles.titleLink} href={additionalLink} target="_blank" rel="noreferrer">
                        {onlineData?.organic?.[0]?.title}
                    </a>
                </div>
                <ReferenceVerification additionalLink={additionalLink} message={officialFactToVerify} conversationId={conversationId} />
              </div>
            );
          }).filter(Boolean);
    };

    const renderWebpages = (webpages: WebPage[] | WebPage) => {
        if (webpages instanceof Array) {
            return webpages.map((webpage, index) => {
                const webpageDomain = new URL(webpage.link).hostname;
                return (
                    <div key={index} className={styles.subLinks}>
                        <a className={styles.subLinks} href={webpage.link} target="_blank" rel="noreferrer">
                            {webpageDomain}
                        </a>
                    </div>
                )
            });
        } else {
            const webpageDomain = new URL(webpages.link).hostname;
            return (
                <div className={styles.subLinks}>
                    <a className={styles.subLinks} href={webpages.link} target="_blank" rel="noreferrer">
                        {webpageDomain}
                    </a>
                </div>
            )
        }
    };

    return (
        <div className={styles.factCheckerContainer}>
            <h1>AI Fact Checker</h1>

            <footer className={styles.footer}>
                This is an experimental AI tool. It may make mistakes.
            </footer>
            <div className={styles.inputFields}>
                <input
                    className={styles.factVerification}
                    type="text"
                    placeholder="Enter a falsifiable statement to verify"
                    disabled={isLoading}
                    onChange={(e) => setFactToVerify(e.target.value)}
                    value={factToVerify}
                    onKeyDown={(e) => {
                        if (e.key === "Enter") {
                            setClickedVerify(!clickedVerify);
                        }
                    }}
                    onFocus={(e) => e.target.placeholder = ""}
                    onBlur={(e) => e.target.placeholder = "Enter a falsifiable statement to verify"} />
                <button disabled={clickedVerify} className={styles.factCheckButton} onClick={() => setClickedVerify(!clickedVerify)}>Verify</button>
            </div>
            <h3>Try with a particular model. You must be <a href="/config">subscribed</a> to use this feature.</h3>
            <ModelPicker disabled={isLoading} />
            {isLoading && <div className={styles.loading}>
                <div className={styles.loadingVerification}>
                    Researching...
                    <div className={styles.spinner}>
                        <div className={styles.dot1}></div>
                        <div className={styles.dot2}></div>
                    </div>
                </div>
            </div>}
            {
                initialResponse &&
                <div className={styles.response}>
                    <h2>Results</h2>
                    <div className={styles.responseText}>
                        <ChatMessage chatMessage={
                            {
                                automationId: "",
                                by: "AI",
                                intent: {},
                                message: initialResponse,
                                context: [],
                                created: (new Date()).toISOString(),
                                onlineContext: {}
                            }
                        } setReferencePanelData={() => {}} setShowReferencePanel={() => {}} />
                        {
                            initialReferences && initialReferences.online && Object.keys(initialReferences.online).length > 0 && (
                                <div className={styles.subLinks}>
                                    {
                                        Object.entries(initialReferences.online).map(([key, onlineData], index) => {
                                            const webpages = onlineData?.webpages || [];
                                            return renderWebpages(webpages);
                                        })
                                    }
                                </div>
                        )}
                    </div>
                </div>
            }
            {
                initialReferences &&
                <div className={styles.referenceContainer}>
                    Supplements
                    <div className={styles.references}>
                        {initialReferences.online !== undefined && renderReferences(conversationID, initialReferences, officialFactToVerify)}
                    </div>
                </div>
            }
        </div>
    )
}
