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

export const setupWebSocket = async (conversationId: string, initialMessage?: string) => {
    const wsProtocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';

    const host = process.env.NODE_ENV === 'production' ? window.location.host : 'localhost:42110';

    let webSocketUrl = `${wsProtocol}//${host}/api/chat/ws`;

    if (conversationId === null) {
        return null;
    }

    if (conversationId) {
        webSocketUrl += `?conversation_id=${conversationId}`;
    }

    const chatWS = new WebSocket(webSocketUrl);

    chatWS.onopen = () => {
        console.log('WebSocket connection established');
        if (initialMessage) {
            chatWS.send(initialMessage);
        }
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


export function modifyFileFilterForConversation(
    conversationId: string | null,
    filenames: string[],
    setAddedFiles: (files: string[]) => void,
    mode: 'add' | 'remove') {

    if (!conversationId) {
        console.error("No conversation ID provided");
        return;
    }

    const method = mode === 'add' ? 'POST' : 'DELETE';

    const body = {
        conversation_id: conversationId,
        filenames: filenames,
    }
    const addUrl = `/api/chat/conversation/file-filters/bulk`;

    fetch(addUrl, {
        method: method,
        headers: {
            'Content-Type': 'application/json',
        },
        body: JSON.stringify(body),
    })
        .then(response => response.json())
        .then(data => {
            console.log("ADDEDFILES DATA: ", data);
            setAddedFiles(data);
        })
        .catch(err => {
            console.error(err);
            return;
        });
}

export function uploadDataForIndexing(
    files: FileList,
    setWarning: (warning: string) => void,
    setUploading: (uploading: boolean) => void,
    setError: (error: string) => void,
    setUploadedFiles?: (files: string[]) => void,
    conversationId?: string | null) {

    const allowedExtensions = ['text/org', 'text/markdown', 'text/plain', 'text/html', 'application/pdf', 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'];
    const allowedFileEndings = ['org', 'md', 'txt', 'html', 'pdf', 'docx'];
    const badFiles: string[] = [];
    const goodFiles: File[] = [];

    const uploadedFiles: string[] = [];

    for (let file of files) {
        const fileEnding = file.name.split('.').pop();
        if (!file || !file.name || !fileEnding) {
            if (file) {
                badFiles.push(file.name);
            }
        } else if ((!allowedExtensions.includes(file.type) && !allowedFileEndings.includes(fileEnding.toLowerCase()))) {
            badFiles.push(file.name);
        } else {
            goodFiles.push(file);
        }
    }

    if (goodFiles.length === 0) {
        setWarning("No supported files found");
        return;
    }

    if (badFiles.length > 0) {
        setWarning("The following files are not supported yet:\n" + badFiles.join('\n'));
    }


    const formData = new FormData();

    // Create an array of Promises for file reading
    const fileReadPromises = Array.from(goodFiles).map(file => {
        return new Promise<void>((resolve, reject) => {
            let reader = new FileReader();
            reader.onload = function (event) {

                if (event.target === null) {
                    reject();
                    return;
                }

                let fileContents = event.target.result;
                let fileType = file.type;
                let fileName = file.name;
                if (fileType === "") {
                    let fileExtension = fileName.split('.').pop();
                    if (fileExtension === "org") {
                        fileType = "text/org";
                    } else if (fileExtension === "md") {
                        fileType = "text/markdown";
                    } else if (fileExtension === "txt") {
                        fileType = "text/plain";
                    } else if (fileExtension === "html") {
                        fileType = "text/html";
                    } else if (fileExtension === "pdf") {
                        fileType = "application/pdf";
                    } else {
                        // Skip this file if its type is not supported
                        resolve();
                        return;
                    }
                }

                if (fileContents === null) {
                    reject();
                    return;
                }

                let fileObj = new Blob([fileContents], { type: fileType });
                formData.append("files", fileObj, file.name);
                resolve();
            };
            reader.onerror = reject;
            reader.readAsArrayBuffer(file);
        });
    });

    setUploading(true);

    // Wait for all files to be read before making the fetch request
    Promise.all(fileReadPromises)
        .then(() => {
            return fetch("/api/v1/index/update?force=false&client=web", {
                method: "POST",
                body: formData,
            });
        })
        .then((data) => {
            for (let file of goodFiles) {
                uploadedFiles.push(file.name);
                if (conversationId && setUploadedFiles) {
                    modifyFileFilterForConversation(conversationId, [file.name], setUploadedFiles, 'add');
                }
            }
            if (setUploadedFiles) setUploadedFiles(uploadedFiles);
        })
        .catch((error) => {
            console.log(error);
            setError(`Error uploading file: ${error}`);
        })
        .finally(() => {
            setUploading(false);
        });
}
