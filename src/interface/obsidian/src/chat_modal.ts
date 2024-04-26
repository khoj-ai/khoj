import { App, MarkdownRenderer, Modal, request, requestUrl, setIcon } from 'obsidian';
import { KhojSetting } from 'src/settings';

export interface ChatJsonResult {
    image?: string;
    detail?: string;
    intentType?: string;
    inferredQueries?: string[];
}


export class KhojChatModal extends Modal {
    result: string;
    setting: KhojSetting;
    region: string;
    city: string;
    countryName: string;
    timezone: string;

    constructor(app: App, setting: KhojSetting) {
        super(app);
        this.setting = setting;

        // Register Modal Keybindings to send user message
        this.scope.register([], 'Enter', async () => { await this.chat() });


        fetch("https://ipapi.co/json")
            .then(response => response.json())
            .then(data => {
                this.region = data.region;
                this.city = data.city;
                this.countryName = data.country_name;
                this.timezone = data.timezone;
            })
            .catch(err => {
                console.log(err);
                return;
            });
    }

    async chat() {
        // Get text in chat input element
        let input_el = <HTMLTextAreaElement>this.contentEl.getElementsByClassName("khoj-chat-input")[0];

        // Clear text after extracting message to send
        let user_message = input_el.value.trim();
        input_el.value = "";
        this.autoResize();

        // Get and render chat response to user message
        await this.getChatResponse(user_message);
    }

    async onOpen() {
        let { contentEl } = this;
        contentEl.addClass("khoj-chat");

        // Add title to the Khoj Chat modal
        contentEl.createEl("h1", ({ attr: { id: "khoj-chat-title" }, text: "Khoj Chat" }));

        // Create area for chat logs
        let chatBodyEl = contentEl.createDiv({ attr: { id: "khoj-chat-body", class: "khoj-chat-body" } });

        // Add chat input field
        let inputRow = contentEl.createDiv("khoj-input-row");
        let clearChat = inputRow.createEl("button", {
            text: "Clear History",
            attr: {
                class: "khoj-input-row-button clickable-icon",
            },
        })
        clearChat.addEventListener('click', async (_) => { await this.clearConversationHistory() });
        setIcon(clearChat, "trash");

        let chatInput = inputRow.createEl("textarea", {
            attr: {
                id: "khoj-chat-input",
                autofocus: "autofocus",
                class: "khoj-chat-input option",
            },
        })
        chatInput.addEventListener('input', (_) => { this.onChatInput() });
        chatInput.addEventListener('keydown', (event) => { this.incrementalChat(event) });

        let transcribe = inputRow.createEl("button", {
            text: "Transcribe",
            attr: {
                id: "khoj-transcribe",
                class: "khoj-transcribe khoj-input-row-button clickable-icon ",
            },
        })
        transcribe.addEventListener('mousedown', async (event) => { await this.speechToText(event) });
        transcribe.addEventListener('touchstart', async (event) => { await this.speechToText(event) });
        transcribe.addEventListener('touchend', async (event) => { await this.speechToText(event) });
        transcribe.addEventListener('touchcancel', async (event) => { await this.speechToText(event) });
        setIcon(transcribe, "mic");

        let send = inputRow.createEl("button", {
            text: "Send",
            attr: {
                id: "khoj-chat-send",
                class: "khoj-chat-send khoj-input-row-button clickable-icon",
            },
        })
        setIcon(send, "arrow-up-circle");
        let sendImg = <SVGElement>send.getElementsByClassName("lucide-arrow-up-circle")[0]
        sendImg.addEventListener('click', async (_) => { await this.chat() });

        // Get chat history from Khoj backend and set chat input state
        let getChatHistorySucessfully = await this.getChatHistory(chatBodyEl);
        let placeholderText = getChatHistorySucessfully ? "Message" : "Configure Khoj to enable chat";
        chatInput.placeholder = placeholderText;
        chatInput.disabled = !getChatHistorySucessfully;

        // Scroll to bottom of modal, till the send message input box
        this.scrollChatToBottom();
        chatInput.focus();
    }

    generateReference(messageEl: Element, reference: string, index: number) {
        // Escape reference for HTML rendering
        let escaped_ref = reference.replace(/"/g, "&quot;")

        // Generate HTML for Chat Reference
        let short_ref = escaped_ref.slice(0, 100);
        short_ref = short_ref.length < escaped_ref.length ? short_ref + "..." : short_ref;
        let referenceButton = messageEl.createEl('button');
        referenceButton.textContent = short_ref;
        referenceButton.id = `ref-${index}`;
        referenceButton.classList.add("reference-button");
        referenceButton.classList.add("collapsed");
        referenceButton.tabIndex = 0;

        // Add event listener to toggle full reference on click
        referenceButton.addEventListener('click', function() {
            console.log(`Toggling ref-${index}`)
            if (this.classList.contains("collapsed")) {
                this.classList.remove("collapsed");
                this.classList.add("expanded");
                this.textContent = escaped_ref;
            } else {
                this.classList.add("collapsed");
                this.classList.remove("expanded");
                this.textContent = short_ref;
            }
        });

        return referenceButton;
    }

    renderMessageWithReferences(chatEl: Element, message: string, sender: string, context?: string[], dt?: Date, intentType?: string, inferredQueries?: string) {
        if (!message) {
            return;
        } else if (intentType?.includes("text-to-image")) {
            let imageMarkdown = "";
            if (intentType === "text-to-image") {
                imageMarkdown = `![](data:image/png;base64,${message})`;
            } else if (intentType === "text-to-image2") {
                imageMarkdown = `![](${message})`;
            } else if (intentType === "text-to-image-v3") {
                imageMarkdown = `![](data:image/webp;base64,${message})`;
            }
            if (inferredQueries) {
                imageMarkdown += "\n\n**Inferred Query**:";
                for (let inferredQuery of inferredQueries) {
                    imageMarkdown += `\n\n${inferredQuery}`;
                }
            }
            this.renderMessage(chatEl, imageMarkdown, sender, dt);
            return;
        } else if (!context) {
            this.renderMessage(chatEl, message, sender, dt);
            return;
        } else if (!!context && context?.length === 0) {
            this.renderMessage(chatEl, message, sender, dt);
            return;
        }
        let chatMessageEl = this.renderMessage(chatEl, message, sender, dt);
        let chatMessageBodyEl = chatMessageEl.getElementsByClassName("khoj-chat-message-text")[0]
        let references = chatMessageBodyEl.createDiv();

        let referenceExpandButton = references.createEl('button');
        referenceExpandButton.classList.add("reference-expand-button");
        let numReferences = 0;

        if (context) {
            numReferences += context.length;
        }

        let referenceSection = references.createEl('div');
        referenceSection.classList.add("reference-section");
        referenceSection.classList.add("collapsed");

        referenceExpandButton.addEventListener('click', function() {
            if (referenceSection.classList.contains("collapsed")) {
                referenceSection.classList.remove("collapsed");
                referenceSection.classList.add("expanded");
            } else {
                referenceSection.classList.add("collapsed");
                referenceSection.classList.remove("expanded");
            }
        });

        references.classList.add("references");
        if (context) {
            context.map((reference, index) => {
                this.generateReference(referenceSection, reference, index + 1);
            });
        }

        let expandButtonText = numReferences == 1 ? "1 reference" : `${numReferences} references`;
        referenceExpandButton.innerHTML = expandButtonText;
    }

    renderMessage(chatEl: Element, message: string, sender: string, dt?: Date, raw: boolean=false): Element {
        let message_time = this.formatDate(dt ?? new Date());
        let emojified_sender = sender == "khoj" ? "🏮 Khoj" : "🤔 You";

        // Append message to conversation history HTML element.
        // The chat logs should display above the message input box to follow standard UI semantics
        let chatMessageEl = chatEl.createDiv({
            attr: {
                "data-meta": `${emojified_sender} at ${message_time}`,
                class: `khoj-chat-message ${sender}`
            },
        })
        let chat_message_body_el = chatMessageEl.createDiv();
        chat_message_body_el.addClasses(["khoj-chat-message-text", sender]);
        let chat_message_body_text_el = chat_message_body_el.createDiv();
        if (raw) {
            chat_message_body_text_el.innerHTML = message;
        } else {
            // @ts-ignore
            MarkdownRenderer.renderMarkdown(message, chat_message_body_text_el, '', null);
        }

        // Remove user-select: none property to make text selectable
        chatMessageEl.style.userSelect = "text";

        // Scroll to bottom after inserting chat messages
        this.scrollChatToBottom();

        return chatMessageEl
    }

    createKhojResponseDiv(dt?: Date): HTMLDivElement {
        let message_time = this.formatDate(dt ?? new Date());

        // Append message to conversation history HTML element.
        // The chat logs should display above the message input box to follow standard UI semantics
        let chat_body_el = this.contentEl.getElementsByClassName("khoj-chat-body")[0];
        let chat_message_el = chat_body_el.createDiv({
            attr: {
                "data-meta": `🏮 Khoj at ${message_time}`,
                class: `khoj-chat-message khoj`
            },
        }).createDiv({
            attr: {
                class: `khoj-chat-message-text khoj`
            },
        })

        // Scroll to bottom after inserting chat messages
        this.scrollChatToBottom();

        return chat_message_el
    }

    async renderIncrementalMessage(htmlElement: HTMLDivElement, additionalMessage: string) {
        this.result += additionalMessage;
        htmlElement.innerHTML = "";
        // @ts-ignore
        await MarkdownRenderer.renderMarkdown(this.result, htmlElement, '', null);
        // Scroll to bottom of modal, till the send message input box
        this.scrollChatToBottom();
    }

    formatDate(date: Date): string {
        // Format date in HH:MM, DD MMM YYYY format
        let time_string = date.toLocaleTimeString('en-IN', { hour: '2-digit', minute: '2-digit', hour12: false });
        let date_string = date.toLocaleString('en-IN', { year: 'numeric', month: 'short', day: '2-digit' }).replace(/-/g, ' ');
        return `${time_string}, ${date_string}`;
    }

    async getChatHistory(chatBodyEl: Element): Promise<boolean> {
        // Get chat history from Khoj backend
        let chatUrl = `${this.setting.khojUrl}/api/chat/history?client=obsidian`;

        try {
            let response = await fetch(chatUrl, {
                method: "GET",
                headers: { "Authorization": `Bearer ${this.setting.khojApiKey}` },
            });

            let responseJson: any = await response.json();

            if (responseJson.detail) {
                // If the server returns error details in response, render a setup hint.
                let setupMsg = "Hi 👋🏾, to start chatting add available chat models options via [the Django Admin panel](/server/admin) on the Server";
                this.renderMessage(chatBodyEl, setupMsg, "khoj", undefined);

                return false;
            } else if (responseJson.response) {
                let chatLogs = responseJson.response?.conversation_id ? responseJson.response.chat ?? [] : responseJson.response;
                chatLogs.forEach((chatLog: any) => {
                    this.renderMessageWithReferences(
                        chatBodyEl,
                        chatLog.message,
                        chatLog.by,
                        chatLog.context,
                        new Date(chatLog.created),
                        chatLog.intent?.type,
                        chatLog.intent?.["inferred-queries"],
                    );
                });
            }
        } catch (err) {
            let errorMsg = "Unable to get response from Khoj server ❤️‍🩹. Ensure server is running or contact developers for help at [team@khoj.dev](mailto:team@khoj.dev) or in [Discord](https://discord.gg/BDgyabRM6e)";
            this.renderMessage(chatBodyEl, errorMsg, "khoj", undefined);
            return false;
        }
        return true;
    }

    async readChatStream(response: Response, responseElement: HTMLDivElement): Promise<void> {
        // Exit if response body is empty
        if (response.body == null) return;

        const reader = response.body.getReader();
        const decoder = new TextDecoder();

        while (true) {
            const { value, done } = await reader.read();

            // Break if the stream is done
            if (done) break;

            let responseText = decoder.decode(value);
            if (responseText.includes("### compiled references:")) {
                // Render any references used to generate the response
                const [additionalResponse, rawReference] = responseText.split("### compiled references:", 2);
                await this.renderIncrementalMessage(responseElement, additionalResponse);

                const rawReferenceAsJson = JSON.parse(rawReference);
                let references = responseElement.createDiv();
                references.classList.add("references");

                let referenceExpandButton = references.createEl('button');
                referenceExpandButton.classList.add("reference-expand-button");

                let referenceSection = references.createDiv();
                referenceSection.classList.add("reference-section");
                referenceSection.classList.add("collapsed");

                let numReferences = 0;

                // If rawReferenceAsJson is a list, then count the length
                if (Array.isArray(rawReferenceAsJson)) {
                    numReferences = rawReferenceAsJson.length;

                    rawReferenceAsJson.forEach((reference, index) => {
                        this.generateReference(referenceSection, reference, index);
                    });
                }
                references.appendChild(referenceExpandButton);

                referenceExpandButton.addEventListener('click', function() {
                    if (referenceSection.classList.contains("collapsed")) {
                        referenceSection.classList.remove("collapsed");
                        referenceSection.classList.add("expanded");
                    } else {
                        referenceSection.classList.add("collapsed");
                        referenceSection.classList.remove("expanded");
                    }
                });

                let expandButtonText = numReferences == 1 ? "1 reference" : `${numReferences} references`;
                referenceExpandButton.innerHTML = expandButtonText;
                references.appendChild(referenceSection);
            } else {
                // Render incremental chat response
                await this.renderIncrementalMessage(responseElement, responseText);
            }
        }
    }

    async getChatResponse(query: string | undefined | null): Promise<void> {
        // Exit if query is empty
        if (!query || query === "") return;

        // Render user query as chat message
        let chatBodyEl = this.contentEl.getElementsByClassName("khoj-chat-body")[0];
        this.renderMessage(chatBodyEl, query, "you");

        // Get chat response from Khoj backend
        let encodedQuery = encodeURIComponent(query);
        let chatUrl = `${this.setting.khojUrl}/api/chat?q=${encodedQuery}&n=${this.setting.resultsCount}&client=obsidian&stream=true&region=${this.region}&city=${this.city}&country=${this.countryName}&timezone=${this.timezone}`;
        let responseElement = this.createKhojResponseDiv();

        // Temporary status message to indicate that Khoj is thinking
        this.result = "";
        await this.renderIncrementalMessage(responseElement, "🤔");

        let response = await fetch(chatUrl, {
            method: "GET",
            headers: {
                "Content-Type": "text/event-stream",
                "Authorization": `Bearer ${this.setting.khojApiKey}`,
            },
        })

        try {
            if (response.body === null) {
                throw new Error("Response body is null");
            }

            // Clear thinking status message
            if (responseElement.innerHTML === "🤔") {
                responseElement.innerHTML = "";
            }

            // Reset collated chat result to empty string
            this.result = "";
            responseElement.innerHTML = "";
            if (response.headers.get("content-type") === "application/json") {
                let responseText = ""
                try {
                    const responseAsJson = await response.json() as ChatJsonResult;
                    if (responseAsJson.image) {
                        // If response has image field, response is a generated image.
                        if (responseAsJson.intentType === "text-to-image") {
                            responseText += `![${query}](data:image/png;base64,${responseAsJson.image})`;
                        } else if (responseAsJson.intentType === "text-to-image2") {
                            responseText += `![${query}](${responseAsJson.image})`;
                        } else if (responseAsJson.intentType === "text-to-image-v3") {
                            responseText += `![${query}](data:image/webp;base64,${responseAsJson.image})`;
                        }
                        const inferredQuery = responseAsJson.inferredQueries?.[0];
                        if (inferredQuery) {
                            responseText += `\n\n**Inferred Query**:\n\n${inferredQuery}`;
                        }
                    } else if (responseAsJson.detail) {
                        responseText = responseAsJson.detail;
                    }
                } catch (error) {
                    // If the chunk is not a JSON object, just display it as is
                    responseText = await response.text();
                } finally {
                    await this.renderIncrementalMessage(responseElement, responseText);
                }
            } else {
                // Stream and render chat response
                await this.readChatStream(response, responseElement);
            }
        } catch (err) {
            console.log(`Khoj chat response failed with\n${err}`);
            let errorMsg = "Sorry, unable to get response from Khoj backend ❤️‍🩹. Retry or contact developers for help at <a href=mailto:'team@khoj.dev'>team@khoj.dev</a> or <a href='https://discord.gg/BDgyabRM6e'>on Discord</a>";
            responseElement.innerHTML = errorMsg
        }
    }

    flashStatusInChatInput(message: string) {
        // Get chat input element and original placeholder
        let chatInput = <HTMLTextAreaElement>this.contentEl.getElementsByClassName("khoj-chat-input")[0];
        let originalPlaceholder = chatInput.placeholder;
        // Set placeholder to message
        chatInput.placeholder = message;
        // Reset placeholder after 2 seconds
        setTimeout(() => {
            chatInput.placeholder = originalPlaceholder;
        }, 2000);
    }

    async clearConversationHistory() {
        let chatBody = this.contentEl.getElementsByClassName("khoj-chat-body")[0];

        let response = await request({
            url: `${this.setting.khojUrl}/api/chat/history?client=obsidian`,
            method: "DELETE",
            headers: { "Authorization": `Bearer ${this.setting.khojApiKey}` },
        })
        try {
            let result = JSON.parse(response);
            if (result.status !== "ok") {
                // Throw error if conversation history isn't cleared
                throw new Error("Failed to clear conversation history");
            } else {
                let getChatHistoryStatus = await this.getChatHistory(chatBody);
                // If conversation history is cleared successfully, clear chat logs from modal
                if (getChatHistoryStatus) chatBody.innerHTML = "";
                let statusMsg = getChatHistoryStatus ? result.message : "Failed to clear conversation history";
                this.flashStatusInChatInput(statusMsg);
            }
        } catch (err) {
            this.flashStatusInChatInput("Failed to clear conversation history");
        }
    }

    sendMessageTimeout: NodeJS.Timeout | undefined;
    mediaRecorder: MediaRecorder | undefined;
    async speechToText(event: MouseEvent | TouchEvent) {
        event.preventDefault();
        const transcribeButton = <HTMLButtonElement>this.contentEl.getElementsByClassName("khoj-transcribe")[0];
        const chatInput = <HTMLTextAreaElement>this.contentEl.getElementsByClassName("khoj-chat-input")[0];
        const sendButton = <HTMLButtonElement>this.modalEl.getElementsByClassName("khoj-chat-send")[0]

        const generateRequestBody = async (audioBlob: Blob, boundary_string: string) => {
            const boundary = `------${boundary_string}`;
            const chunks: ArrayBuffer[] = [];

            chunks.push(new TextEncoder().encode(`${boundary}\r\n`));
            chunks.push(new TextEncoder().encode(`Content-Disposition: form-data; name="file"; filename="blob"\r\nContent-Type: "application/octet-stream"\r\n\r\n`));
            chunks.push(await audioBlob.arrayBuffer());
            chunks.push(new TextEncoder().encode('\r\n'));

            await Promise.all(chunks);
            chunks.push(new TextEncoder().encode(`${boundary}--\r\n`));
            return await new Blob(chunks).arrayBuffer();
        };

        const sendToServer = async (audioBlob: Blob) => {
            const boundary_string = `Boundary${Math.random().toString(36).slice(2)}`;
            const requestBody = await generateRequestBody(audioBlob, boundary_string);

            const response = await requestUrl({
                url: `${this.setting.khojUrl}/api/transcribe?client=obsidian`,
                method: 'POST',
                headers: { "Authorization": `Bearer ${this.setting.khojApiKey}` },
                contentType: `multipart/form-data; boundary=----${boundary_string}`,
                body: requestBody,
            });

            // Parse response from Khoj backend
            if (response.status === 200) {
                console.log(response);
                chatInput.value += response.json.text.trimStart();
                this.autoResize();
            } else if (response.status === 501) {
                throw new Error("⛔️ Configure speech-to-text model on server.");
            } else if (response.status === 422) {
                throw new Error("⛔️ Audio file to large to process.");
            } else {
                throw new Error("⛔️ Failed to transcribe audio.");
            }

            // Don't auto-send empty messages
            if (chatInput.value.length === 0) return;

            // Show stop auto-send button. It stops auto-send when clicked
            setIcon(sendButton, "stop-circle");
            let stopSendButtonImg = <SVGElement>sendButton.getElementsByClassName("lucide-stop-circle")[0]
            stopSendButtonImg.addEventListener('click', (_) => { this.cancelSendMessage() });

            // Start the countdown timer UI
            stopSendButtonImg.getElementsByTagName("circle")[0].style.animation = "countdown 3s linear 1 forwards";

            // Auto send message after 3 seconds
            this.sendMessageTimeout = setTimeout(() => {
                // Stop the countdown timer UI
                setIcon(sendButton, "arrow-up-circle")
                let sendImg = <SVGElement>sendButton.getElementsByClassName("lucide-arrow-up-circle")[0]
                sendImg.addEventListener('click', async (_) => { await this.chat() });

                // Send message
                this.chat();
            }, 3000);
        };

        const handleRecording = (stream: MediaStream) => {
            const audioChunks: Blob[] = [];
            const recordingConfig = { mimeType: 'audio/webm' };
            this.mediaRecorder = new MediaRecorder(stream, recordingConfig);

            this.mediaRecorder.addEventListener("dataavailable", function(event) {
                if (event.data.size > 0) audioChunks.push(event.data);
            });

            this.mediaRecorder.addEventListener("stop", async function() {
                const audioBlob = new Blob(audioChunks, { type: 'audio/webm' });
                await sendToServer(audioBlob);
            });

            this.mediaRecorder.start();
            setIcon(transcribeButton, "mic-off");
        };

        // Toggle recording
        if (!this.mediaRecorder || this.mediaRecorder.state === 'inactive' || event.type === 'touchstart') {
            navigator.mediaDevices
                .getUserMedia({ audio: true })
                ?.then(handleRecording)
                .catch((e) => {
                    this.flashStatusInChatInput("⛔️ Failed to access microphone");
                });
        } else if (this.mediaRecorder.state === 'recording' || event.type === 'touchend' || event.type === 'touchcancel') {
            this.mediaRecorder.stop();
            this.mediaRecorder.stream.getTracks().forEach(track => track.stop());
            this.mediaRecorder = undefined;
            setIcon(transcribeButton, "mic");
        }
    }

    cancelSendMessage() {
        // Cancel the auto-send chat message timer if the stop-send-button is clicked
        clearTimeout(this.sendMessageTimeout);

        // Revert to showing send-button and hide the stop-send-button
        let sendButton = <HTMLButtonElement>this.modalEl.getElementsByClassName("khoj-chat-send")[0];
        setIcon(sendButton, "arrow-up-circle");
        let sendImg = <SVGElement>sendButton.getElementsByClassName("lucide-arrow-up-circle")[0]
        sendImg.addEventListener('click', async (_) => { await this.chat() });
    };

    incrementalChat(event: KeyboardEvent) {
        if (!event.shiftKey && event.key === 'Enter') {
            event.preventDefault();
            this.chat();
        }
    }

    onChatInput() {
        const chatInput = <HTMLTextAreaElement>this.contentEl.getElementsByClassName("khoj-chat-input")[0];
        chatInput.value = chatInput.value.trimStart();

        this.autoResize();
    }

    autoResize() {
        const chatInput = <HTMLTextAreaElement>this.contentEl.getElementsByClassName("khoj-chat-input")[0];
        const scrollTop = chatInput.scrollTop;
        chatInput.style.height = '0';
        const scrollHeight = chatInput.scrollHeight + 8;  // +8 accounts for padding
        chatInput.style.height = Math.min(scrollHeight, 200) + 'px';
        chatInput.scrollTop = scrollTop;
        this.scrollChatToBottom();
    }

    scrollChatToBottom() {
        let sendButton = <HTMLButtonElement>this.modalEl.getElementsByClassName("khoj-chat-send")[0];
        sendButton.scrollIntoView({ behavior: "auto", block: "center" });
    }
}
