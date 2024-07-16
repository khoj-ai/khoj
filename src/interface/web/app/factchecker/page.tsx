'use client'

import styles from './factChecker.module.css';
import { useAuthenticatedData } from '@/app/common/auth';
import { useState, useEffect } from 'react';

import ChatMessage, { Context, OnlineContextData, WebPage } from '../components/chatMessage/chatMessage';
import { ModelPicker, Model } from '../components/modelPicker/modelPicker';
import ShareLink from '../components/shareLink/shareLink';

import { Input } from "@/components/ui/input"
import { Button } from "@/components/ui/button"

import {
    Card,
    CardContent,
    CardFooter,
    CardHeader,
    CardTitle,
} from "@/components/ui/card"
import Link from 'next/link';



const chatURL = "/api/chat";
const verificationPrecursor = "Limit your search to reputable sources. Search the internet for relevant supporting or refuting information. Do not reference my notes. Refuse to answer any queries that are not falsifiable by informing me that you will not answer the question. You're not permitted to ask follow-up questions, so do the best with what you have. Respond with **TRUE** or **FALSE** or **INCONCLUSIVE**, then provide your justification. Fact Check:"


const LoadingSpinner = () => (
    <div className={styles.loading}>
        <div className={styles.loadingVerification}>
            Researching...
            <div className={styles.spinner}>
                <div className={`${styles.dot1} bg-blue-300`}></div>
                <div className={`${styles.dot2} bg-blue-300`}></div>
            </div>
        </div>
    </div>
);

interface SupplementReferences {
    additionalLink: string;
    response: string;
    linkTitle: string;
}


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

    let createURL = `/api/chat/sessions?client=web`;

    const response = await fetch(createURL, { method: "POST" });

    const data = await response.json();
    setConversationID(data.conversation_id);
}


interface ReferenceVerificationProps {
    message: string;
    additionalLink: string;
    conversationId: string;
    linkTitle: string;
    setChildReferencesCallback: (additionalLink: string, response: string, linkTitle: string) => void;
    prefilledResponse?: string;
}

function ReferenceVerification(props: ReferenceVerificationProps) {
    const [initialResponse, setInitialResponse] = useState("");
    const [isLoading, setIsLoading] = useState(true);
    const verificationStatement = `${props.message}. Use this link for reference: ${props.additionalLink}`;
    const [isMobileWidth, setIsMobileWidth] = useState(false);

    useEffect(() => {
        if (props.prefilledResponse) {
            setInitialResponse(props.prefilledResponse);
            setIsLoading(false);
        } else {
            verifyStatement(verificationStatement, props.conversationId, setIsLoading, setInitialResponse, () => {});
        }

        setIsMobileWidth(window.innerWidth < 768);

        window.addEventListener('resize', () => {
            setIsMobileWidth(window.innerWidth < 768);
        })

    }, [verificationStatement, props.conversationId, props.prefilledResponse]);

    useEffect(() => {
        if (initialResponse === "") return;
        if (props.prefilledResponse) return;

        if (!isLoading) {
            // Only set the child references when it's done loading and if the initial response is not prefilled (i.e. it was fetched from the server)
            props.setChildReferencesCallback(props.additionalLink, initialResponse, props.linkTitle);
        }
    }, [initialResponse, isLoading, props]);

    return (
        <div>
            {isLoading &&
                <LoadingSpinner />
            }
            <ChatMessage chatMessage={
                {
                    automationId: "",
                    by: "AI",
                    message: initialResponse,
                    context: [],
                    created: (new Date()).toISOString(),
                    onlineContext: {},
                }}
                isMobileWidth={isMobileWidth} />
        </div>
    )
}

interface SupplementalReferenceProps {
    onlineData?: OnlineContextData;
    officialFactToVerify: string;
    conversationId: string;
    additionalLink: string;
    setChildReferencesCallback: (additionalLink: string, response: string, linkTitle: string) => void;
    prefilledResponse?: string;
    linkTitle?: string;
}

function SupplementalReference(props: SupplementalReferenceProps) {
    const linkTitle = props.linkTitle || props.onlineData?.organic?.[0]?.title || "Reference";
    const linkAsWebpage = { link: props.additionalLink } as WebPage;
    return (
        <Card className={`mt-2 mb-4`}>
            <CardHeader>
                <a className={styles.titleLink} href={props.additionalLink} target="_blank" rel="noreferrer">
                    {linkTitle}
                </a>
                <WebPageLink {...linkAsWebpage} />
            </CardHeader>
            <CardContent>
                <ReferenceVerification
                    additionalLink={props.additionalLink}
                    message={props.officialFactToVerify}
                    linkTitle={linkTitle}
                    conversationId={props.conversationId}
                    setChildReferencesCallback={props.setChildReferencesCallback}
                    prefilledResponse={props.prefilledResponse} />
            </CardContent>
        </Card>
    );
}

const WebPageLink = (webpage: WebPage) => {
    const webpageDomain = new URL(webpage.link).hostname;
    return (
        <div className={styles.subLinks}>
            <a className={`${styles.subLinks} bg-blue-200 px-2`} href={webpage.link} target="_blank" rel="noreferrer">
                {webpageDomain}
            </a>
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
    const [childReferences, setChildReferences] = useState<SupplementReferences[]>();
    const [modelUsed, setModelUsed] = useState<Model>();
    const [isMobileWidth, setIsMobileWidth] = useState(false);

    const [conversationID, setConversationID] = useState("");
    const [runId, setRunId] = useState("");
    const [loadedFromStorage, setLoadedFromStorage] = useState(false);

    const [initialModel, setInitialModel] = useState<Model>();

    function setChildReferencesCallback(additionalLink: string, response: string, linkTitle: string) {
        const newReferences = childReferences || [];
        const exists = newReferences.find((reference) => reference.additionalLink === additionalLink);
        if (exists) return;
        newReferences.push({ additionalLink, response, linkTitle });
        setChildReferences(newReferences);
    }

    useEffect(() => {
        setIsMobileWidth(window.innerWidth < 768);

        window.addEventListener('resize', () => {
            setIsMobileWidth(window.innerWidth < 768);
        })

    }, []);

    let userData = useAuthenticatedData();

    function storeData() {
        const data = {
            factToVerify,
            response: initialResponse,
            references: initialReferences,
            childReferences,
            runId,
            modelUsed,
        };

        fetch(`/api/chat/store/factchecker`, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify({
                "runId": runId,
                "storeData": data
            }),
        });
    }

    useEffect(() => {
        if (factToVerify) {
            document.title = `AI Fact Check: ${factToVerify}`;
        } else {
            document.title = 'AI Fact Checker';
        }
    }, [factToVerify]);

    useEffect(() => {
        const storedFact = localStorage.getItem('factToVerify');
        if (storedFact) {
            setFactToVerify(storedFact);
        }

        // Get query params from the URL
        const urlParams = new URLSearchParams(window.location.search);
        const factToVerifyParam = urlParams.get('factToVerify');

        if (factToVerifyParam) {
            setFactToVerify(factToVerifyParam);
        }

        const runIdParam = urlParams.get('runId');
        if (runIdParam) {
            setRunId(runIdParam);

            // Define an async function to fetch data
            const fetchData = async () => {
                const storedDataURL = `/api/chat/store/factchecker?runId=${runIdParam}`;
                try {
                    const response = await fetch(storedDataURL);
                    if (response.status !== 200) {
                        throw new Error("Failed to fetch stored data");
                    }
                    const storedData = JSON.parse(await response.json());
                    if (storedData) {
                        setOfficialFactToVerify(storedData.factToVerify);
                        setInitialResponse(storedData.response);
                        setInitialReferences(storedData.references);
                        setChildReferences(storedData.childReferences);
                        setInitialModel(storedData.modelUsed);
                    }
                    setLoadedFromStorage(true);
                } catch (error) {
                    console.error("Error fetching stored data: ", error);
                }
            };

            // Call the async function
            fetchData();
        }

    }, []);

    function onClickVerify() {
        if (clickedVerify) return;

        // Perform validation checks on the fact to verify
        if (!factToVerify) {
            alert("Please enter a fact to verify.");
            return;
        }

        setClickedVerify(true);
        if (!userData) {
            let currentURL = window.location.href;
            window.location.href = `/login?next=${currentURL}`;
        }

        setInitialReferences(undefined);
        setInitialResponse("");

        spawnNewConversation(setConversationID);

        // Set the runId to a random 12-digit alphanumeric string
        const newRunId = [...Array(16)].map(() => Math.random().toString(36)[2]).join('');
        setRunId(newRunId);
        window.history.pushState({}, document.title, window.location.pathname + `?runId=${newRunId}`);

        setOfficialFactToVerify(factToVerify);
        setClickedVerify(false);
    }

    useEffect(() => {
        if (!conversationID) return;
        verifyStatement(officialFactToVerify, conversationID, setIsLoading, setInitialResponse, setInitialReferences);
    }, [conversationID, officialFactToVerify]);

    // Store factToVerify in localStorage whenever it changes
    useEffect(() => {
        localStorage.setItem('factToVerify', factToVerify);
    }, [factToVerify]);

    // Update the meta tags for the description and og:description
    useEffect(() => {
        let metaTag = document.querySelector('meta[name="description"]');
        if (metaTag) {
            metaTag.setAttribute('content', initialResponse);
        }
        let metaOgTag = document.querySelector('meta[property="og:description"]');
        if (!metaOgTag) {
            metaOgTag = document.createElement('meta');
            metaOgTag.setAttribute('property', 'og:description');
            document.getElementsByTagName('head')[0].appendChild(metaOgTag);
        }
        metaOgTag.setAttribute('content', initialResponse);
    }, [initialResponse]);

    const renderReferences = (conversationId: string, initialReferences: ResponseWithReferences, officialFactToVerify: string, loadedFromStorage: boolean, childReferences?: SupplementReferences[]) => {
        if (loadedFromStorage && childReferences) {
            return renderSupplementalReferences(childReferences);
        }

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
                        return null;
                    }
                    seenLinks.add(additionalLink);
                }
            } else {
                let singleWebpage = webpages as WebPage;
                const additionalLink = singleWebpage.link || '';
                if (seenLinks.has(additionalLink)) {
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
                <SupplementalReference
                    key={index}
                    onlineData={onlineData}
                    officialFactToVerify={officialFactToVerify}
                    conversationId={conversationId}
                    additionalLink={additionalLink}
                    setChildReferencesCallback={setChildReferencesCallback} />
            );
          }).filter(Boolean);
    };

    const renderSupplementalReferences = (references: SupplementReferences[]) => {
        return references.map((reference, index) => {
            return (
                <SupplementalReference
                    key={index}
                    additionalLink={reference.additionalLink}
                    officialFactToVerify={officialFactToVerify}
                    conversationId={conversationID}
                    linkTitle={reference.linkTitle}
                    setChildReferencesCallback={setChildReferencesCallback}
                    prefilledResponse={reference.response} />
            )
        });
    }

    const renderWebpages = (webpages: WebPage[] | WebPage) => {
        if (webpages instanceof Array) {
            return webpages.map((webpage, index) => {
                return WebPageLink(webpage);
            });
        } else {
            return WebPageLink(webpages);
        }
    };

    function constructShareUrl() {
        const url = new URL(window.location.href);
        url.searchParams.set('runId', runId);
        return url.href;
    }

    return (
        <div className={styles.factCheckerContainer}>
            <h1 className={`${styles.response} font-large outline-slate-800 dark:outline-slate-200`}>
                AI Fact Checker
            </h1>
            <footer className={`${styles.footer} mt-4`}>
                This is an experimental AI tool. It may make mistakes.
            </footer>
            {
                initialResponse && initialReferences && childReferences
                ?
                    <div className={styles.reportActions}>
                        <Button asChild variant='secondary'>
                            <Link href="/factchecker" target="_blank" rel="noopener noreferrer">
                                Try Another
                            </Link>
                        </Button>
                        <ShareLink
                            buttonTitle='Share report'
                            title="AI Fact Checking Report"
                            description="Share this fact checking report with others. Anyone who has this link will be able to view the report."
                            url={constructShareUrl()}
                            onShare={loadedFromStorage ? () => {} : storeData} />
                    </div>
                : <div className={styles.newReportActions}>
                    <div className={`${styles.inputFields} mt-4`}>
                        <Input
                            type="text"
                            maxLength={200}
                            placeholder="Enter a falsifiable statement to verify"
                            disabled={isLoading}
                            onChange={(e) => setFactToVerify(e.target.value)}
                            value={factToVerify}
                            onKeyDown={(e) => {
                                if (e.key === "Enter") {
                                    onClickVerify();
                                }
                            }}
                            onFocus={(e) => e.target.placeholder = ""}
                            onBlur={(e) => e.target.placeholder = "Enter a falsifiable statement to verify"} />
                        <Button disabled={clickedVerify} onClick={() => onClickVerify()}>Verify</Button>
                    </div>
                    <h3 className={`mt-4 mb-4`}>
                        Try with a particular model. You must be <a href="/configure" className="font-medium text-blue-600 dark:text-blue-500 hover:underline">subscribed</a> to configure the model.
                    </h3>
                </div>
            }
            <ModelPicker disabled={isLoading || loadedFromStorage} setModelUsed={setModelUsed} initialModel={initialModel} />
            {isLoading && <div className={styles.loading}>
                    <LoadingSpinner />
                </div>}
            {
                initialResponse &&
                <Card className={`mt-4`}>
                    <CardHeader>
                        <CardTitle>{officialFactToVerify}</CardTitle>
                    </CardHeader>
                    <CardContent>
                        <div className={styles.responseText}>
                            <ChatMessage chatMessage={
                                {
                                    automationId: "",
                                    by: "AI",
                                    message: initialResponse,
                                    context: [],
                                    created: (new Date()).toISOString(),
                                    onlineContext: {}
                                }
                            } isMobileWidth={isMobileWidth} />

                        </div>
                    </CardContent>
                    <CardFooter>
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
                    </CardFooter>
                </Card>
            }
            {
                initialReferences &&
                <div className={styles.referenceContainer}>
                    <h2 className="mt-4 mb-4">Supplements</h2>
                    <div className={styles.references}>
                        { initialReferences.online !== undefined &&
                            renderReferences(conversationID, initialReferences, officialFactToVerify, loadedFromStorage, childReferences)}
                    </div>
                </div>
            }
        </div>
    )
}
