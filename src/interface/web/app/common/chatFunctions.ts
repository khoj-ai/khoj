import { Context, OnlineContext, StreamMessage } from "../components/chatMessage/chatMessage";

export interface RawReferenceData {
    context?: Context[];
    onlineContext?: OnlineContext;
}

export interface ResponseWithReferences {
    context?: Context[];
    online?: OnlineContext;
    response?: string;
}

interface MessageChunk {
    type: string;
    data: string | object;
}

export function convertMessageChunkToJson(chunk: string): MessageChunk {
    if (chunk.startsWith("{") && chunk.endsWith("}")) {
        try {
            const jsonChunk = JSON.parse(chunk);
            if (!jsonChunk.type) {
                return {
                    type: "message",
                    data: jsonChunk,
                };
            }
            return jsonChunk;
        } catch (error) {
            return {
                type: "message",
                data: chunk,
            };
        }
    } else if (chunk.length > 0) {
        return {
            type: "message",
            data: chunk,
        };
    } else {
        return {
            type: "message",
            data: "",
        };
    }
}

function handleJsonResponse(chunkData: any) {
    const jsonData = chunkData as any;
    if (jsonData.image || jsonData.detail) {
        let responseWithReference = handleImageResponse(chunkData, true);
        if (responseWithReference.response) return responseWithReference.response;
    } else if (jsonData.response) {
        return jsonData.response;
    } else {
        throw new Error("Invalid JSON response");
    }
}

export function processMessageChunk(
    rawChunk: string,
    currentMessage: StreamMessage,
    context: Context[] = [],
    onlineContext: OnlineContext = {},
): { context: Context[]; onlineContext: OnlineContext } {
    const chunk = convertMessageChunkToJson(rawChunk);

    if (!currentMessage || !chunk || !chunk.type) return { context, onlineContext };

    if (chunk.type === "status") {
        console.log(`status: ${chunk.data}`);
        const statusMessage = chunk.data as string;
        currentMessage.trainOfThought.push(statusMessage);
    } else if (chunk.type === "references") {
        const references = chunk.data as RawReferenceData;

        if (references.context) context = references.context;
        if (references.onlineContext) onlineContext = references.onlineContext;
        return { context, onlineContext };
    } else if (chunk.type === "message") {
        const chunkData = chunk.data;
        if (chunkData !== null && typeof chunkData === "object") {
            currentMessage.rawResponse += handleJsonResponse(chunkData);
        } else if (
            typeof chunkData === "string" &&
            chunkData.trim()?.startsWith("{") &&
            chunkData.trim()?.endsWith("}")
        ) {
            try {
                const jsonData = JSON.parse(chunkData.trim());
                currentMessage.rawResponse += handleJsonResponse(jsonData);
            } catch (e) {
                currentMessage.rawResponse += JSON.stringify(chunkData);
            }
        } else {
            currentMessage.rawResponse += chunkData;
        }
    } else if (chunk.type === "start_llm_response") {
        console.log(`Started streaming: ${new Date()}`);
    } else if (chunk.type === "end_llm_response") {
        console.log(`Completed streaming: ${new Date()}`);

        // Append any references after all the data has been streamed
        if (onlineContext) currentMessage.onlineContext = onlineContext;
        if (context) currentMessage.context = context;

        // Mark current message streaming as completed
        currentMessage.completed = true;
    }
    return { context, onlineContext };
}

export function handleImageResponse(imageJson: any, liveStream: boolean): ResponseWithReferences {
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
        if (inferredQuery && !liveStream) {
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
    mode: "add" | "remove",
) {
    if (!conversationId) {
        console.error("No conversation ID provided");
        return;
    }

    const method = mode === "add" ? "POST" : "DELETE";

    const body = {
        conversation_id: conversationId,
        filenames: filenames,
    };
    const addUrl = `/api/chat/conversation/file-filters/bulk`;

    fetch(addUrl, {
        method: method,
        headers: {
            "Content-Type": "application/json",
        },
        body: JSON.stringify(body),
    })
        .then((response) => response.json())
        .then((data) => {
            console.log("ADDEDFILES DATA: ", data);
            setAddedFiles(data);
        })
        .catch((err) => {
            console.error(err);
            return;
        });
}

interface NewConversationMetadata {
    conversationId: string;
    conversationUniqueId: string;
}

export async function createNewConversation(slug: string) {
    try {
        const response = await fetch(`/api/chat/sessions?client=web&agent_slug=${slug}`, {
            method: "POST",
        });
        if (!response.ok)
            throw new Error(`Failed to fetch chat sessions with status: ${response.status}`);
        const data = await response.json();
        const uniqueId = data.unique_id;
        const conversationId = data.conversation_id;
        if (!uniqueId) throw new Error("Unique ID not found in response");
        if (!conversationId) throw new Error("Conversation ID not found in response");
        return {
            conversationId: conversationId,
            conversationUniqueId: uniqueId,
        } as NewConversationMetadata;
    } catch (error) {
        console.error("Error creating new conversation:", error);
        throw error;
    }
}

export function uploadDataForIndexing(
    files: FileList,
    setWarning: (warning: string) => void,
    setUploading: (uploading: boolean) => void,
    setError: (error: string) => void,
    setUploadedFiles?: (files: string[]) => void,
    conversationId?: string | null,
) {
    const allowedExtensions = [
        "text/org",
        "text/markdown",
        "text/plain",
        "text/html",
        "application/pdf",
        "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    ];
    const allowedFileEndings = ["org", "md", "txt", "html", "pdf", "docx"];
    const badFiles: string[] = [];
    const goodFiles: File[] = [];

    const uploadedFiles: string[] = [];

    for (let file of files) {
        const fileEnding = file.name.split(".").pop();
        if (!file || !file.name || !fileEnding) {
            if (file) {
                badFiles.push(file.name);
            }
        } else if (
            !allowedExtensions.includes(file.type) &&
            !allowedFileEndings.includes(fileEnding.toLowerCase())
        ) {
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
        setWarning("The following files are not supported yet:\n" + badFiles.join("\n"));
    }

    const formData = new FormData();

    // Create an array of Promises for file reading
    const fileReadPromises = Array.from(goodFiles).map((file) => {
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
                    let fileExtension = fileName.split(".").pop();
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
            return fetch("/api/content?client=web", {
                method: "PATCH",
                body: formData,
            });
        })
        .then((data) => {
            for (let file of goodFiles) {
                uploadedFiles.push(file.name);
                if (conversationId && setUploadedFiles) {
                    modifyFileFilterForConversation(
                        conversationId,
                        [file.name],
                        setUploadedFiles,
                        "add",
                    );
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
