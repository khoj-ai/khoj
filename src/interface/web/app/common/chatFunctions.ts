import { Context, OnlineContextData } from "../components/chatMessage/chatMessage";

interface ResponseWithReferences {
    context?: Context[];
    online?: {
        [key: string]: OnlineContextData
    }
    response?: string;
}

export function handleCompiledReferences(chunk: string, currentResponse: string) {
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

async function sendChatStream(
    message: string,
    conversationId: string,
    setIsLoading: (loading: boolean) => void,
    setInitialResponse: (response: string) => void,
    setInitialReferences: (references: ResponseWithReferences) => void) {
        setIsLoading(true);
        // Send a message to the chat server to verify the fact
        const chatURL = "/api/chat";
        const apiURL = `${chatURL}?q=${encodeURIComponent(message)}&client=web&stream=true&conversation_id=${conversationId}`;
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

export const setupWebSocket = async (conversationId: string) => {
    const wsProtocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';

    const host = process.env.NODE_ENV === 'production' ? window.location.host : 'localhost:42110';

    let webSocketUrl = `${wsProtocol}//${host}/api/chat/ws`;

    if (conversationId === null) return null;

    if (conversationId) {
        webSocketUrl += `?conversation_id=${conversationId}`;
    }

    console.log("WebSocket URL: ", webSocketUrl);

    const chatWS = new WebSocket(webSocketUrl);

    chatWS.onopen = () => {
        console.log('WebSocket connection established');
    };

    chatWS.onmessage = (event) => {
        console.log(event.data);
    };

    chatWS.onerror = (error) => {
        console.error('WebSocket error: ', error);
    };

    chatWS.onclose = () => {
        console.log('WebSocket connection closed');
    };

    return chatWS;
};

export function handleImageResponse(imageJson: any) {

    let rawResponse = "";

    if (imageJson.image) {
        const inferredQuery = imageJson.inferredQueries?.[0] ?? "generated image";

        // If response has image field, response is a generated image.
        if (imageJson.intentType === "text-to-image") {
            rawResponse += `![generated_image](data:image/png;base64,${imageJson.image})`;
        } else if (imageJson.intentType === "text-to-image2") {
            rawResponse += `![generated_image](${imageJson.image})`;
        } else if (imageJson.intentType === "text-to-image-v3") {
            rawResponse = `![](data:image/webp;base64,${imageJson.image})`;
        }
        if (inferredQuery) {
            rawResponse += `\n\n**Inferred Query**:\n\n${inferredQuery}`;
        }
    }

    let reference: ResponseWithReferences = {};

    if (imageJson.context && imageJson.context.length > 0) {
        const rawReferenceAsJson = imageJson.context;
        if (rawReferenceAsJson instanceof Array) {
            reference.context = rawReferenceAsJson;
        } else if (typeof rawReferenceAsJson === "object" && rawReferenceAsJson !== null) {
            reference.online = rawReferenceAsJson;
        }
    }
    if (imageJson.detail) {
        // The detail field contains the improved image prompt
        rawResponse += imageJson.detail;
    }

    reference.response = rawResponse;
    return reference;
}
