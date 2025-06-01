import { ItemView, MarkdownRenderer, Scope, WorkspaceLeaf, request, requestUrl, setIcon, Platform, TFile } from 'obsidian';
import * as DOMPurify from 'isomorphic-dompurify';
import { KhojSetting } from 'src/settings';
import { KhojPaneView } from 'src/pane_view';
import { KhojView, createCopyParentText, getLinkToEntry, pasteTextAtCursor } from 'src/utils';
import { KhojSearchModal } from 'src/search_modal';
import { FileInteractions, EditBlock } from 'src/interact_with_files';

export interface ChatJsonResult {
    image?: string;
    detail?: string;
    intentType?: string;
    inferredQueries?: string[];
}

interface MessageChunk {
    type: string;
    data: any;
}

interface ChatMessageState {
    newResponseTextEl: HTMLDivElement | null;
    newResponseEl: HTMLDivElement | null;
    loadingEllipsis: HTMLDivElement | null;
    references: { [key: string]: any };
    rawResponse: string;
    rawQuery: string;
    isVoice: boolean;
    generatedAssets: string;
    turnId: string;
    editBlocks: EditBlock[];
    editRetryCount: number;
    parentRetryCount?: number;
}

interface Location {
    region?: string;
    city?: string;
    countryName?: string;
    countryCode?: string;
    timezone: string;
}

interface RenderMessageOptions {
    chatBodyEl: Element;
    message: string;
    sender: string;
    turnId?: string;
    dt?: Date;
    raw?: boolean;
    willReplace?: boolean;
    isSystemMessage?: boolean;
}

interface ChatMode {
    value: string;
    label: string;
    iconName: string;
    command: string;
}

interface Agent {
    name: string;
    slug: string;
    description: string;
}

export class KhojChatView extends KhojPaneView {
    result: string;
    setting: KhojSetting;
    waitingForLocation: boolean;
    location: Location = { timezone: Intl.DateTimeFormat().resolvedOptions().timeZone };
    keyPressTimeout: NodeJS.Timeout | null = null;
    userMessages: string[] = [];  // Store user sent messages for input history cycling
    currentMessageIndex: number = -1;  // Track current message index in userMessages array
    private currentUserInput: string = ""; // Stores the current user input that is being typed in chat
    private startingMessage: string = this.getLearningMoment();
    chatMessageState: ChatMessageState;
    private agents: Agent[] = [];
    private currentAgent: string | null = null;
    private fileAccessMode: 'none' | 'read' | 'write' = 'read'; // Track the current file access mode
    // TODO: Only show modes available on server and to current agent
    private chatModes: ChatMode[] = [
        { value: "default", label: "Default", iconName: "target", command: "/default" },
        { value: "general", label: "General", iconName: "message-circle", command: "/general" },
        { value: "notes", label: "Notes", iconName: "file-text", command: "/notes" },
        { value: "online", label: "Online", iconName: "globe", command: "/online" },
        { value: "code", label: "Code", iconName: "code", command: "/code" },
        { value: "image", label: "Image", iconName: "image", command: "/image" },
        { value: "research", label: "Research", iconName: "microscope", command: "/research" },
        { value: "operator", label: "Operator", iconName: "laptop", command: "/operator" }
    ];
    private editRetryCount: number = 0;  // Track number of retries for edit blocks
    private fileInteractions: FileInteractions;
    private modeDropdown: HTMLElement | null = null;
    private selectedOptionIndex: number = -1;
    private isStreaming: boolean = false; // Flag to track streaming state

    // Disabled retry logic for now. Can re-enable once:
    // 1. Handle chat history clutter
    // 2. Higher invalid edit blocks than tolerable
    private maxEditRetries: number = 1; // Maximum retries for edit blocks

    constructor(leaf: WorkspaceLeaf, setting: KhojSetting) {
        super(leaf, setting);
        this.fileInteractions = new FileInteractions(this.app);

        this.waitingForLocation = true;

        fetch("https://ipapi.co/json")
            .then(response => response.json())
            .then(data => {
                this.location = {
                    region: data.region,
                    city: data.city,
                    countryName: data.country_name,
                    countryCode: data.country_code,
                    timezone: data.timezone,
                };
            })
            .catch(err => {
                console.log(err);
            })
            .finally(() => {
                this.waitingForLocation = false;
            });

        // Register chat view keybindings
        this.scope = new Scope(this.app.scope);
        this.scope.register(["Ctrl", "Alt"], 'n', (_) => this.createNewConversation());
        this.scope.register(["Ctrl", "Alt"], 'o', async (_) => await this.toggleChatSessions());
        this.scope.register(["Ctrl", "Alt"], 'v', (_) => this.speechToText(new KeyboardEvent('keydown')));
        this.scope.register(["Ctrl"], 'f', (_) => new KhojSearchModal(this.app, this.setting).open());
        this.scope.register(["Ctrl"], 'r', (_) => { this.activateView(KhojView.SIMILAR); });
    }

    getViewType(): string {
        return KhojView.CHAT;
    }

    getDisplayText(): string {
        return "Khoj Chat";
    }

    getIcon(): string {
        return "message-circle";
    }

    async chat(isVoice: boolean = false) {
        // Cancel any pending edits before sending a new message
        await this.cancelPendingEdits();

        // Get text in chat input element
        let input_el = <HTMLTextAreaElement>this.contentEl.getElementsByClassName("khoj-chat-input")[0];

        // Clear text after extracting message to send
        let user_message = input_el.value.trim();

        // Store the message in the array if it's not empty
        if (user_message) {
            // Get the selected mode
            const selectedMode = this.chatModes.find(mode =>
                this.contentEl.querySelector(`#khoj-mode-${mode.value}:checked`)
            );

            // Check if message starts with a mode command
            const modeMatch = this.chatModes.find(mode => user_message.startsWith(mode.command));

            let displayMessage = user_message;
            let apiMessage = user_message;

            if (modeMatch) {
                // If message starts with a mode command, replace it with the icon in display
                // We'll use a generic marker since we can't display SVG icons in messages
                displayMessage = user_message.replace(modeMatch.command, `[${modeMatch.label}]`);
            } else if (selectedMode) {
                // If no mode in message but mode selected, add the mode command for API
                displayMessage = `[${selectedMode.label}] ${user_message}`;
                apiMessage = `${selectedMode.command} ${user_message}`;
            }

            this.userMessages.push(user_message);
            // Update starting message after sending a new message
            this.startingMessage = this.getLearningMoment();
            input_el.placeholder = this.startingMessage;

            // Clear input and resize
            input_el.value = "";
            this.autoResize();

            // Get and render chat response to user message
            await this.getChatResponse(apiMessage, displayMessage, isVoice);
        }
    }

    async onOpen() {
        let { contentEl } = this;

        // Construct Content Security Policy
        let defaultDomains = `'self' ${this.setting.khojUrl} https://*.obsidian.md https://app.khoj.dev https://assets.khoj.dev`;
        const defaultSrc = `default-src ${defaultDomains};`;
        const scriptSrc = `script-src ${defaultDomains} 'unsafe-inline';`;
        const connectSrc = `connect-src ${this.setting.khojUrl} wss://*.obsidian.md/ https://ipapi.co/json;`;
        const styleSrc = `style-src ${defaultDomains} 'unsafe-inline';`;
        const imgSrc = `img-src * app: data:;`;
        const childSrc = `child-src 'none';`;
        const objectSrc = `object-src 'none';`;
        const csp = `${defaultSrc} ${scriptSrc} ${connectSrc} ${styleSrc} ${imgSrc} ${childSrc} ${objectSrc}`;

        // WARNING: CSP DISABLED for now as it breaks other Obsidian plugins. Enable when can scope CSP to only Khoj plugin.
        // CSP meta tag for the Khoj Chat modal
        // document.head.createEl("meta", { attr: { "http-equiv": "Content-Security-Policy", "content": `${csp}` } });

        // The parent class handles creating the header and attaching the click on the "New Chat" button
        // We handle the rest of the interface here
        // Call the parent class's onOpen method first
        await super.onOpen();

        // Fetch available agents
        await this.fetchAgents();

        // Populate the agent selector in the header
        const headerAgentSelect = this.contentEl.querySelector('#khoj-header-agent-select') as HTMLSelectElement;
        if (headerAgentSelect && this.agents.length > 0) {
            // Clear existing options
            headerAgentSelect.innerHTML = '';

            // Add default option
            headerAgentSelect.createEl("option", {
                text: "Default Agent",
                value: "khoj"
            });

            // Add options for all other agents
            this.agents.forEach(agent => {
                if (agent.slug === 'khoj') return; // Skip the default agent
                const option = headerAgentSelect.createEl("option", {
                    text: agent.name,
                    value: agent.slug
                });
                if (agent.description) {
                    option.title = agent.description;
                }
            });

            // Add change event listener
            headerAgentSelect.addEventListener('change', (event) => {
                const select = event.target as HTMLSelectElement;
                this.currentAgent = select.value || null;
            });
        }

        contentEl.addClass("khoj-chat");

        // Create the chat body
        let chatBodyEl = contentEl.createDiv({ attr: { id: "khoj-chat-body", class: "khoj-chat-body" } });
        // Add chat input field
        let inputRow = contentEl.createDiv("khoj-input-row");

        let chatSessions = inputRow.createEl("button", {
            text: "Chat Sessions",
            attr: {
                class: "khoj-input-row-button clickable-icon",
                title: "Show Conversations (Ctrl+Alt+O)",
            },
        })
        chatSessions.addEventListener('click', async (_) => { await this.toggleChatSessions() });
        setIcon(chatSessions, "history");

        // Add file access mode button
        let fileAccessButton = inputRow.createEl("button", {
            text: "File Access",
            attr: {
                class: "khoj-input-row-button clickable-icon",
                title: "Toggle file access mode (Read Only)",
            },
        });
        setIcon(fileAccessButton, "file-search");
        fileAccessButton.addEventListener('click', () => {
            // Cycle through modes: none -> read -> write -> none
            switch (this.fileAccessMode) {
                case 'none':
                    this.fileAccessMode = 'read';
                    setIcon(fileAccessButton, "file-search");
                    fileAccessButton.title = "Toggle file access mode (Read Only)";
                    break;
                case 'read':
                    this.fileAccessMode = 'write';
                    setIcon(fileAccessButton, "file-edit");
                    fileAccessButton.title = "Toggle file access mode (Read & Write)";
                    break;
                case 'write':
                    this.fileAccessMode = 'none';
                    setIcon(fileAccessButton, "file-x");
                    fileAccessButton.title = "Toggle file access mode (No Access)";
                    break;
            }
        });

        let chatInput = inputRow.createEl("textarea", {
            attr: {
                id: "khoj-chat-input",
                autofocus: "autofocus",
                class: "khoj-chat-input option",
            },
        })
        chatInput.addEventListener('input', (_) => { this.onChatInput() });
        chatInput.addEventListener('keydown', (event) => {
            this.incrementalChat(event);
            this.handleArrowKeys(event);
        });

        // Add event listeners for long press keybinding
        this.contentEl.addEventListener('keydown', this.handleKeyDown.bind(this));
        this.contentEl.addEventListener('keyup', this.handleKeyUp.bind(this));

        let transcribe = inputRow.createEl("button", {
            text: "Transcribe",
            attr: {
                id: "khoj-transcribe",
                class: "khoj-transcribe khoj-input-row-button clickable-icon ",
                title: "Start Voice Chat (Ctrl+Alt+V)",
            },
        })
        transcribe.addEventListener('mousedown', (event) => { this.startSpeechToText(event) });
        transcribe.addEventListener('mouseup', async (event) => { await this.stopSpeechToText(event) });
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

        let placeholderText: string = getChatHistorySucessfully ? this.startingMessage : "Configure Khoj to enable chat";
        chatInput.placeholder = placeholderText;
        chatInput.disabled = !getChatHistorySucessfully;
        this.autoResize();

        // Scroll to bottom of chat messages and focus on chat input field, once messages rendered
        requestAnimationFrame(() => {
            // Ensure layout and paint have occurred
            requestAnimationFrame(() => {
                this.scrollChatToBottom();
                const chatInput = <HTMLTextAreaElement>this.contentEl.getElementsByClassName("khoj-chat-input")[0];
                chatInput?.focus();
            });
        });
    }

    startSpeechToText(event: KeyboardEvent | MouseEvent | TouchEvent, timeout = 200) {
        if (!this.keyPressTimeout) {
            this.keyPressTimeout = setTimeout(async () => {
                // Reset auto send voice message timer, UI if running
                if (this.sendMessageTimeout) {
                    // Stop the auto send voice message countdown timer UI
                    clearTimeout(this.sendMessageTimeout);
                    const sendButton = <HTMLButtonElement>this.contentEl.getElementsByClassName("khoj-chat-send")[0]
                    setIcon(sendButton, "arrow-up-circle")
                    let sendImg = <SVGElement>sendButton.getElementsByClassName("lucide-arrow-up-circle")[0]
                    sendImg.addEventListener('click', async (_) => { await this.chat() });
                    // Reset chat input value
                    const chatInput = <HTMLTextAreaElement>this.contentEl.getElementsByClassName("khoj-chat-input")[0];
                    chatInput.value = "";
                }
                // Start new voice message
                await this.speechToText(event);
            }, timeout);
        }
    }
    async stopSpeechToText(event: KeyboardEvent | MouseEvent | TouchEvent) {
        if (this.mediaRecorder) {
            await this.speechToText(event);
        }
        if (this.keyPressTimeout) {
            clearTimeout(this.keyPressTimeout);
            this.keyPressTimeout = null;
        }
    }

    handleKeyDown(event: KeyboardEvent) {
        // Start speech to text if keyboard shortcut is pressed
        if (event.key === 's' && event.getModifierState('Control')) this.startSpeechToText(event);
    }

    async handleKeyUp(event: KeyboardEvent) {
        // Stop speech to text if keyboard shortcut is released
        if (event.key === 's' && event.getModifierState('Control')) await this.stopSpeechToText(event);
    }

    processOnlineReferences(referenceSection: HTMLElement, onlineContext: any) {
        let numOnlineReferences = 0;
        for (let subquery in onlineContext) {
            let onlineReference = onlineContext[subquery];
            if (onlineReference.organic && onlineReference.organic.length > 0) {
                numOnlineReferences += onlineReference.organic.length;
                for (let key in onlineReference.organic) {
                    let reference = onlineReference.organic[key];
                    let polishedReference = this.generateOnlineReference(referenceSection, reference, key);
                    referenceSection.appendChild(polishedReference);
                }
            }

            if (onlineReference.knowledgeGraph && onlineReference.knowledgeGraph.length > 0) {
                numOnlineReferences += onlineReference.knowledgeGraph.length;
                for (let key in onlineReference.knowledgeGraph) {
                    let reference = onlineReference.knowledgeGraph[key];
                    let polishedReference = this.generateOnlineReference(referenceSection, reference, key);
                    referenceSection.appendChild(polishedReference);
                }
            }

            if (onlineReference.peopleAlsoAsk && onlineReference.peopleAlsoAsk.length > 0) {
                numOnlineReferences += onlineReference.peopleAlsoAsk.length;
                for (let key in onlineReference.peopleAlsoAsk) {
                    let reference = onlineReference.peopleAlsoAsk[key];
                    let polishedReference = this.generateOnlineReference(referenceSection, reference, key);
                    referenceSection.appendChild(polishedReference);
                }
            }

            if (onlineReference.webpages && onlineReference.webpages.length > 0) {
                numOnlineReferences += onlineReference.webpages.length;
                for (let key in onlineReference.webpages) {
                    let reference = onlineReference.webpages[key];
                    let polishedReference = this.generateOnlineReference(referenceSection, reference, key);
                    referenceSection.appendChild(polishedReference);
                }
            }
        }

        return numOnlineReferences;
    }

    generateOnlineReference(messageEl: Element, reference: any, index: string) {
        // Generate HTML for Chat Reference
        let title = reference.title || reference.link;
        let link = reference.link;
        let snippet = reference.snippet;
        let question = reference.question ? `<b>Question:</b> ${reference.question}<br><br>` : "";

        let referenceButton = messageEl.createEl('button');
        let linkElement = referenceButton.createEl('a');
        linkElement.setAttribute('href', link);
        linkElement.setAttribute('target', '_blank');
        linkElement.setAttribute('rel', 'noopener noreferrer');
        linkElement.classList.add("reference-link");
        linkElement.setAttribute('title', title);
        linkElement.textContent = title;

        referenceButton.id = `ref-${index}`;
        referenceButton.classList.add("reference-button");
        referenceButton.classList.add("collapsed");
        referenceButton.tabIndex = 0;

        // Add event listener to toggle full reference on click
        referenceButton.addEventListener('click', function() {
            if (this.classList.contains("collapsed")) {
                this.classList.remove("collapsed");
                this.classList.add("expanded");
                this.innerHTML = linkElement.outerHTML + `<br><br>${question + snippet}`;
            } else {
                this.classList.add("collapsed");
                this.classList.remove("expanded");
                this.innerHTML = linkElement.outerHTML;
            }
        });

        return referenceButton;
    }

    generateReference(messageEl: Element, referenceJson: any, index: number) {
        let reference: string = referenceJson.hasOwnProperty("compiled") ? referenceJson.compiled : referenceJson;
        let referenceFile = referenceJson.hasOwnProperty("file") ? referenceJson.file : null;

        // Get all markdown and PDF files in vault
        const mdFiles = this.app.vault.getMarkdownFiles();
        const pdfFiles = this.app.vault.getFiles().filter(file => file.extension === 'pdf');

        // Escape reference for HTML rendering
        reference = reference.split('\n').slice(1).join('\n');
        let escaped_ref = reference.replace(/"/g, "&quot;")

        // Generate HTML for Chat Reference
        let referenceButton = messageEl.createEl('button');

        if (referenceFile) {
            // Find vault file associated with current reference
            const linkToEntry = getLinkToEntry(mdFiles.concat(pdfFiles), referenceFile, reference);

            const linkElement: Element = referenceButton.createEl('span');
            linkElement.setAttribute('title', escaped_ref);
            linkElement.textContent = referenceFile;
            if (linkElement && linkToEntry) {
                linkElement.classList.add("reference-link");
                linkElement.addEventListener('click', (event) => {
                    event.stopPropagation();
                    this.app.workspace.openLinkText(linkToEntry, '');
                });
            }
        }

        let referenceText = referenceButton.createDiv();
        referenceText.textContent = escaped_ref;

        referenceButton.id = `ref-${index}`;
        referenceButton.classList.add("reference-button");
        referenceButton.classList.add("collapsed");
        referenceButton.tabIndex = 0;

        // Add event listener to toggle full reference on click
        referenceButton.addEventListener('click', function() {
            if (this.classList.contains("collapsed")) {
                this.classList.remove("collapsed");
                this.classList.add("expanded");
            } else {
                this.classList.add("collapsed");
                this.classList.remove("expanded");
            }
        });

        return referenceButton;
    }

    textToSpeech(message: string, event: MouseEvent | null = null): void {
        // Replace the speaker with a loading icon.
        let loader = document.createElement("span");
        loader.classList.add("loader");

        let speechButton: HTMLButtonElement;
        let speechIcon: Element;

        if (event === null) {
            // Pick the last speech button if none is provided
            let speechButtons = document.getElementsByClassName("speech-button");
            speechButton = speechButtons[speechButtons.length - 1] as HTMLButtonElement;

            let speechIcons = document.getElementsByClassName("speech-icon");
            speechIcon = speechIcons[speechIcons.length - 1];
        } else {
            speechButton = event.currentTarget as HTMLButtonElement;
            speechIcon = event.target as Element;
        }

        speechButton.appendChild(loader);
        speechButton.disabled = true;

        const context = new AudioContext();
        let textToSpeechApi = `${this.setting.khojUrl}/api/chat/speech?text=${encodeURIComponent(message)}`;
        fetch(textToSpeechApi, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
                "Authorization": `Bearer ${this.setting.khojApiKey}`,
            },
        })
            .then(response => response.arrayBuffer())
            .then(arrayBuffer => context.decodeAudioData(arrayBuffer))
            .then(audioBuffer => {
                const source = context.createBufferSource();
                source.buffer = audioBuffer;
                source.connect(context.destination);
                source.start(0);
                source.onended = function() {
                    speechButton.removeChild(loader);
                    speechButton.disabled = false;
                };
            })
            .catch(err => {
                console.error("Error playing speech:", err);
                speechButton.removeChild(loader);
                speechButton.disabled = false; // Consider enabling the button again to allow retrying
            });
    }

    formatHTMLMessage(message: string, raw = false, willReplace = true) {
        // Remove any text between <s>[INST] and </s> tags. These are spurious instructions for some AI chat model.
        message = message.replace(/<s>\[INST\].+(<\/s>)?/g, '');

        // Render train of thought messages
        const { content, header } = this.processTrainOfThought(message);
        message = content;

        if (!raw) {
            // Render text edit blocks
            message = this.transformEditBlocks(message);
        }

        // Sanitize the markdown message
        message = DOMPurify.sanitize(message);

        // Convert the message to html, sanitize the message html and render it to the real DOM
        let chatMessageBodyTextEl = this.contentEl.createDiv();
        chatMessageBodyTextEl.innerHTML = this.markdownTextToSanitizedHtml(message, this);

        // Add action buttons to each chat message, if they don't already exist
        if (willReplace === true) {
            this.renderActionButtons(message, chatMessageBodyTextEl);
        }

        return chatMessageBodyTextEl;
    }

    markdownTextToSanitizedHtml(markdownText: string, component: ItemView): string {
        // Render markdown to an unlinked DOM element
        let virtualChatMessageBodyTextEl = document.createElement("div");

        // Convert the message to html
        MarkdownRenderer.render(this.app, markdownText, virtualChatMessageBodyTextEl, '', component);

        // Remove image HTML elements with any non whitelisted src prefix
        virtualChatMessageBodyTextEl.innerHTML = virtualChatMessageBodyTextEl.innerHTML.replace(
            /<img(?:(?!src=["'](app:|data:|https:\/\/generated\.khoj\.dev)).)*?>/gis,
            ''
        );

        // Sanitize the markdown text rendered as HTML
        return DOMPurify.sanitize(virtualChatMessageBodyTextEl.innerHTML);
    }

    private processTrainOfThought(message: string): { content: string, header: string } {
        // The train of thought comes in as a markdown-formatted string. It starts with a heading delimited by two asterisks at the start and end and a colon, followed by the message. Example: **header**: status. This function will parse the message and render it as a div.
        let extractedHeader = message.match(/\*\*(.*)\*\*/);
        let header = extractedHeader ? extractedHeader[1] : "";
        let content = message;

        // Render screenshot image in screenshot action message
        let jsonMessage = null;
        try {
            const jsonMatch = message.match(
                /\{.*("action": "screenshot"|"type": "screenshot"|"image": "data:image\/.*").*\}/,
            );
            if (jsonMatch) {
                jsonMessage = JSON.parse(jsonMatch[0]);
                const screenshotHtmlString = `<img src="${jsonMessage.image}" alt="State of environment" class="max-w-full" />`;
                content = content.replace(
                    `:\n**Action**: ${jsonMatch[0]}`,
                    `\n\n- ${jsonMessage.text}\n${screenshotHtmlString}`,
                );
            }
        } catch (e) {
            console.error("Failed to parse screenshot data", e);
        }

        return { content, header };
    }


    renderMessageWithReferences(
        chatEl: Element,
        message: string,
        sender: string,
        turnId: string,
        context?: string[],
        onlineContext?: object,
        dt?: Date,
        intentType?: string,
        inferredQueries?: string[],
        conversationId?: string,
        images?: string[],
        excalidrawDiagram?: string,
        mermaidjsDiagram?: string
    ) {
        if (!message) return;

        let chatMessageEl;
        if (
            intentType?.includes("text-to-image") ||
            intentType === "excalidraw" ||
            (images && images.length > 0) ||
            mermaidjsDiagram ||
            excalidrawDiagram) {
            let imageMarkdown = this.generateImageMarkdown(message, intentType ?? "", inferredQueries, conversationId, images, excalidrawDiagram, mermaidjsDiagram);
            chatMessageEl = this.renderMessage({
                chatBodyEl: chatEl,
                message: imageMarkdown,
                sender,
                dt,
                turnId
            });
        } else {
            chatMessageEl = this.renderMessage({
                chatBodyEl: chatEl,
                message,
                sender,
                dt,
                turnId
            });
        }

        // If no document or online context is provided, skip rendering the reference section
        if ((context == null || context.length == 0)
            && (onlineContext == null || (onlineContext && Object.keys(onlineContext).length == 0))) {
            return;
        }

        // If document or online context is provided, render the message with its references
        let references: any = {};
        if (!!context) references["notes"] = context;
        if (!!onlineContext) references["online"] = onlineContext;
        let chatMessageBodyEl = chatMessageEl.getElementsByClassName("khoj-chat-message-text")[0];
        chatMessageBodyEl.appendChild(this.createReferenceSection(references));
    }

    generateImageMarkdown(message: string, intentType: string, inferredQueries?: string[], conversationId?: string, images?: string[], excalidrawDiagram?: string, mermaidjsDiagram?: string): string {
        let imageMarkdown = "";
        if (intentType === "text-to-image") {
            imageMarkdown = `![](data:image/png;base64,${message})`;
        } else if (intentType === "text-to-image2") {
            imageMarkdown = `![](${message})`;
        } else if (intentType === "text-to-image-v3") {
            imageMarkdown = `![](${message})`;
        } else if (intentType === "excalidraw" || excalidrawDiagram) {
            const domain = this.setting.khojUrl.endsWith("/") ? this.setting.khojUrl : `${this.setting.khojUrl}/`;
            const redirectMessage = `Hey, I'm not ready to show you diagrams yet here. But you can view it in ${domain}chat?conversationId=${conversationId}`;
            imageMarkdown = redirectMessage;
        } else if (mermaidjsDiagram) {
            imageMarkdown = "```mermaid\n" + mermaidjsDiagram + "\n```";
        } else if (images && images.length > 0) {
            imageMarkdown += images.map(image => `![](${image})`).join('\n\n');
            imageMarkdown += message;
        }

        if (images?.length === 0 && inferredQueries) {
            imageMarkdown += "\n\n**Inferred Query**:";
            for (let inferredQuery of inferredQueries) {
                imageMarkdown += `\n\n${inferredQuery}`;
            }
        }
        return imageMarkdown;
    }

    renderMessage({ chatBodyEl, message, sender, dt, turnId, raw = false, willReplace = true, isSystemMessage = false }: RenderMessageOptions): Element {
        let message_time = this.formatDate(dt ?? new Date());

        // Append message to conversation history HTML element.
        // The chat logs should display above the message input box to follow standard UI semantics
        let chatMessageEl = chatBodyEl.createDiv({
            attr: {
                "data-meta": message_time,
                class: `khoj-chat-message ${sender}`,
                ...(turnId && { "data-turnid": turnId })
            },
        })
        let chatMessageBodyEl = chatMessageEl.createDiv();
        chatMessageBodyEl.addClasses(["khoj-chat-message-text", sender]);
        let chatMessageBodyTextEl = chatMessageBodyEl.createDiv();

        // Remove Obsidian specific instructions sent alongside user query in between <SYSTEM></SYSTEM> tags
        if (sender === "you") {
            message = message.replace(/<SYSTEM>.*?<\/SYSTEM>/s, '');
        }

        // Sanitize the markdown to render
        message = DOMPurify.sanitize(message);

        if (raw) {
            chatMessageBodyTextEl.innerHTML = message;
        } else {
            // @ts-ignore
            chatMessageBodyTextEl.innerHTML = this.markdownTextToSanitizedHtml(message, this);
        }

        // Add action buttons to each chat message element
        if (willReplace === true) {
            this.renderActionButtons(message, chatMessageBodyTextEl, isSystemMessage);
        }

        // Remove user-select: none property to make text selectable
        chatMessageEl.style.userSelect = "text";

        // Scroll to bottom after inserting chat messages
        this.scrollChatToBottom();

        return chatMessageEl;
    }

    createKhojResponseDiv(dt?: Date): HTMLDivElement {
        let messageTime = this.formatDate(dt ?? new Date());

        // Append message to conversation history HTML element.
        // The chat logs should display above the message input box to follow standard UI semantics
        let chatBodyEl = this.contentEl.getElementsByClassName("khoj-chat-body")[0];
        let chatMessageEl = chatBodyEl.createDiv({
            attr: {
                "data-meta": messageTime,
                class: `khoj-chat-message khoj`
            },
        })

        // Scroll to bottom after inserting chat messages
        this.scrollChatToBottom();

        return chatMessageEl;
    }

    async renderIncrementalMessage(htmlElement: HTMLDivElement, additionalMessage: string) {
        this.chatMessageState.rawResponse += additionalMessage;

        // Sanitize the markdown to render
        let sanitizedResponse = DOMPurify.sanitize(this.chatMessageState.rawResponse);

        // Apply transformations including partial edit block detection
        const transformedResponse = this.transformEditBlocks(sanitizedResponse);

        // Create a temporary element to get the rendered HTML
        const tempElement = document.createElement('div');
        tempElement.innerHTML = this.markdownTextToSanitizedHtml(transformedResponse, this);

        // Update the content in separate step for a smoother transition
        htmlElement.innerHTML = tempElement.innerHTML;

        // Render action buttons for the message
        this.renderActionButtons(this.chatMessageState.rawResponse, htmlElement);

        // Scroll to bottom of modal, till the send message input box
        this.scrollChatToBottom();
    }

    renderActionButtons(message: string, chatMessageBodyTextEl: HTMLElement, isSystemMessage: boolean = false) {
        let copyButton = this.contentEl.createEl('button');
        copyButton.classList.add("chat-action-button");
        copyButton.title = "Copy Message to Clipboard";
        setIcon(copyButton, "copy-plus");
        copyButton.addEventListener('click', createCopyParentText(message));

        // Add button to paste into current buffer
        let pasteToFile = this.contentEl.createEl('button');
        pasteToFile.classList.add("chat-action-button");
        pasteToFile.title = "Paste Message to File";
        setIcon(pasteToFile, "clipboard-paste");
        pasteToFile.addEventListener('click', (event) => { pasteTextAtCursor(createCopyParentText(message, 'clipboard-paste')(event)); });

        // Add edit button only for user messages
        let editButton = null;
        if (!isSystemMessage && chatMessageBodyTextEl.closest('.khoj-chat-message.you')) {
            editButton = this.contentEl.createEl('button');
            editButton.classList.add("chat-action-button");
            editButton.title = "Edit Message";
            setIcon(editButton, "edit-3");
            editButton.addEventListener('click', async () => {
                const messageEl = chatMessageBodyTextEl.closest('.khoj-chat-message');
                if (messageEl) {
                    // Get all messages up to this one
                    const allMessages = Array.from(this.contentEl.getElementsByClassName('khoj-chat-message'));
                    const currentIndex = allMessages.indexOf(messageEl as HTMLElement);

                    // Store reference to messages that need to be deleted from backend
                    const messagesToDelete = allMessages.slice(currentIndex);

                    // Get the message content without the emoji if it exists
                    let messageContent = message;
                    const emojiRegex = /^[^\p{L}\p{N}]+\s*/u;
                    messageContent = messageContent.replace(emojiRegex, '');

                    // Set the message in the input field
                    const chatInput = this.contentEl.querySelector('.khoj-chat-input') as HTMLTextAreaElement;
                    if (chatInput) {
                        chatInput.value = messageContent;
                        chatInput.focus();
                    }

                    // Remove all messages from UI immediately for better UX
                    for (let i = messagesToDelete.length - 1; i >= 0; i--) {
                        messagesToDelete[i].remove();
                    }

                    // Then delete messages from backend in background
                    (async () => {
                        for (const msgToDelete of messagesToDelete) {
                            await this.deleteMessage(msgToDelete as HTMLElement, true, false);
                        }
                    })();
                }
            });
        }

        // Add delete button
        let deleteButton = null;
        if (!isSystemMessage) {
            deleteButton = this.contentEl.createEl('button');
            deleteButton.classList.add("chat-action-button");
            deleteButton.title = "Delete Message";
            setIcon(deleteButton, "trash-2");
            deleteButton.addEventListener('click', () => {
                const messageEl = chatMessageBodyTextEl.closest('.khoj-chat-message');
                if (messageEl) {
                    // Ask for confirmation before deleting
                    if (confirm('Are you sure you want to delete this message?')) {
                        this.deleteMessage(messageEl as HTMLElement);
                    }
                }
            });
        }

        // Append buttons to parent element
        chatMessageBodyTextEl.append(copyButton, pasteToFile);
        if (editButton) {
            chatMessageBodyTextEl.append(editButton);
        }
        if (deleteButton) {
            chatMessageBodyTextEl.append(deleteButton);
        }
        if (this.setting.userInfo?.is_active) {
            // Create a speech button icon to play the message out loud
            let speechButton = this.contentEl.createEl('button');
            speechButton.classList.add("chat-action-button", "speech-button");
            speechButton.title = "Listen to Message";
            setIcon(speechButton, "speech")
            speechButton.addEventListener('click', (event) => this.textToSpeech(message, event));
            chatMessageBodyTextEl.append(speechButton);
        }
    }

    formatDate(date: Date): string {
        // Format date in HH:MM, DD MMM YYYY format
        let time_string = date.toLocaleTimeString('en-IN', { hour: '2-digit', minute: '2-digit', hour12: false });
        let date_string = date.toLocaleString('en-IN', { year: 'numeric', month: 'short', day: '2-digit' }).replace(/-/g, ' ');
        return `${time_string}, ${date_string}`;
    }

    getLearningMoment(): string {
        const modifierKey = Platform.isMacOS ? '⌘' : '^';
        const learningMoments = [
            "Type '/' to select response mode.",
        ];
        if (this.userMessages.length > 0) {
            learningMoments.push(`Load previous messages with ${modifierKey}+↑/↓`);
        }

        // Return a random learning moment
        return learningMoments[Math.floor(Math.random() * learningMoments.length)];
    }

    async createNewConversation(agentSlug?: string) {
        let chatBodyEl = this.contentEl.getElementsByClassName("khoj-chat-body")[0] as HTMLElement;
        chatBodyEl.innerHTML = "";
        chatBodyEl.dataset.conversationId = "";
        chatBodyEl.dataset.conversationTitle = "";
        this.userMessages = [];
        this.startingMessage = this.getLearningMoment();

        // Update the placeholder of the chat input
        const chatInput = this.contentEl.querySelector('.khoj-chat-input') as HTMLTextAreaElement;
        if (chatInput) {
            chatInput.placeholder = this.startingMessage;
        }

        try {
            // Create a new conversation with or without an agent
            let endpoint = `${this.setting.khojUrl}/api/chat/sessions`;
            if (agentSlug) {
                endpoint += `?agent_slug=${encodeURIComponent(agentSlug)}`;
            }

            const response = await fetch(endpoint, {
                method: "POST",
                headers: {
                    "Authorization": `Bearer ${this.setting.khojApiKey}`,
                    "Content-Type": "application/json"
                },
                body: JSON.stringify({}) // Empty body as agent_slug is in the URL
            });

            if (response.ok) {
                const sessionInfo = await response.json();
                chatBodyEl.dataset.conversationId = sessionInfo.conversation_id;
                this.currentAgent = agentSlug || null;

                // Update agent selector to reflect current agent
                const agentSelect = this.contentEl.querySelector('.khoj-agent-select') as HTMLSelectElement;
                if (agentSelect) {
                    agentSelect.value = this.currentAgent || '';
                }
            } else {
                console.error("Failed to create session:", response.statusText);
            }
        } catch (error) {
            console.error("Error creating session:", error);
        }

        this.renderMessage({ chatBodyEl, message: "Hey, what's up?", sender: "khoj", isSystemMessage: true });
    }

    async toggleChatSessions(forceShow: boolean = false): Promise<boolean> {
        this.userMessages = [];  // clear user previous message history
        let chatBodyEl = this.contentEl.getElementsByClassName("khoj-chat-body")[0] as HTMLElement;
        if (!forceShow && this.contentEl.getElementsByClassName("side-panel")?.length > 0) {
            chatBodyEl.innerHTML = "";
            return this.getChatHistory(chatBodyEl);
        }
        chatBodyEl.innerHTML = "";
        const sidePanelEl = chatBodyEl.createDiv("side-panel");
        const newConversationEl = sidePanelEl.createDiv("new-conversation");
        const conversationHeaderTitleEl = newConversationEl.createDiv("conversation-header-title");
        conversationHeaderTitleEl.textContent = "Conversations";

        const newConversationButtonEl = newConversationEl.createEl("button");
        newConversationButtonEl.classList.add("new-conversation-button");
        newConversationButtonEl.classList.add("side-panel-button");
        newConversationButtonEl.addEventListener('click', (_) => this.createNewConversation());
        setIcon(newConversationButtonEl, "plus");
        newConversationButtonEl.innerHTML += "New";
        newConversationButtonEl.title = "New Conversation (Ctrl+Alt+N)";

        const existingConversationsEl = sidePanelEl.createDiv("existing-conversations");
        const conversationListEl = existingConversationsEl.createDiv("conversation-list");
        const conversationListBodyHeaderEl = conversationListEl.createDiv("conversation-list-header");
        const conversationListBodyEl = conversationListEl.createDiv("conversation-list-body");

        const chatSessionsUrl = `${this.setting.khojUrl}/api/chat/sessions?client=obsidian`;
        const headers = { 'Authorization': `Bearer ${this.setting.khojApiKey}` };
        try {
            let response = await fetch(chatSessionsUrl, { method: "GET", headers: headers });
            let responseJson: any = await response.json();
            let conversationId = chatBodyEl.dataset.conversationId;

            if (responseJson.length > 0) {
                conversationListBodyHeaderEl.style.display = "block";
                for (let key in responseJson) {
                    let conversation = responseJson[key];
                    let conversationSessionEl = this.contentEl.createEl('div');
                    let incomingConversationId = conversation["conversation_id"];
                    conversationSessionEl.classList.add("conversation-session");
                    if (incomingConversationId == conversationId) {
                        conversationSessionEl.classList.add("selected-conversation");
                    }
                    const conversationTitle = conversation["slug"] || `New conversation 🌱`;
                    const conversationSessionTitleEl = conversationSessionEl.createDiv("conversation-session-title");
                    conversationSessionTitleEl.textContent = conversationTitle;
                    conversationSessionTitleEl.addEventListener('click', () => {
                        chatBodyEl.innerHTML = "";
                        chatBodyEl.dataset.conversationId = incomingConversationId;
                        chatBodyEl.dataset.conversationTitle = conversationTitle;
                        this.getChatHistory(chatBodyEl);
                    });

                    let conversationMenuEl = this.contentEl.createEl('div');
                    conversationMenuEl = this.addConversationMenu(
                        conversationMenuEl,
                        conversationSessionEl,
                        conversationTitle,
                        conversationSessionTitleEl,
                        chatBodyEl,
                        incomingConversationId,
                        incomingConversationId == conversationId,
                    );

                    conversationSessionEl.appendChild(conversationMenuEl);
                    conversationListBodyEl.appendChild(conversationSessionEl);
                }
            }
        } catch (err) {
            return false;
        }
        return true;
    }

    addConversationMenu(
        conversationMenuEl: HTMLDivElement,
        conversationSessionEl: HTMLElement,
        conversationTitle: string,
        conversationSessionTitleEl: HTMLElement,
        chatBodyEl: HTMLElement,
        incomingConversationId: string,
        selectedConversation: boolean,
    ) {
        conversationMenuEl.classList.add("conversation-menu");

        const headers = { 'Authorization': `Bearer ${this.setting.khojApiKey}` };

        let editConversationTitleButtonEl = this.contentEl.createEl('button');
        setIcon(editConversationTitleButtonEl, "edit");
        editConversationTitleButtonEl.title = "Rename";
        editConversationTitleButtonEl.classList.add("edit-title-button", "three-dot-menu-button-item", "clickable-icon");
        if (selectedConversation) editConversationTitleButtonEl.classList.add("selected-conversation");
        editConversationTitleButtonEl.addEventListener('click', (event) => {
            event.stopPropagation();

            let conversationMenuChildren = conversationMenuEl.children;
            let totalItems = conversationMenuChildren.length;

            for (let i = totalItems - 1; i >= 0; i--) {
                conversationMenuChildren[i].remove();
            }

            // Create a dialog box to get new title for conversation
            let editConversationTitleInputEl = this.contentEl.createEl('input');
            editConversationTitleInputEl.classList.add("conversation-title-input");
            editConversationTitleInputEl.value = conversationTitle;
            editConversationTitleInputEl.addEventListener('click', function(event) {
                event.stopPropagation();
            });
            editConversationTitleInputEl.addEventListener('keydown', function(event) {
                if (event.key === "Enter") {
                    event.preventDefault();
                    editConversationTitleSaveButtonEl.click();
                }
            });
            let editConversationTitleSaveButtonEl = this.contentEl.createEl('button');
            conversationSessionTitleEl.replaceWith(editConversationTitleInputEl);
            editConversationTitleSaveButtonEl.innerHTML = "Save";
            editConversationTitleSaveButtonEl.classList.add("three-dot-menu-button-item", "clickable-icon");
            if (selectedConversation) editConversationTitleSaveButtonEl.classList.add("selected-conversation");
            editConversationTitleSaveButtonEl.addEventListener('click', (event) => {
                event.stopPropagation();
                let newTitle = editConversationTitleInputEl.value;
                if (newTitle != null) {
                    let editURL = `/api/chat/title?client=web&conversation_id=${incomingConversationId}&title=${newTitle}`;
                    fetch(`${this.setting.khojUrl}${editURL}`, { method: "PATCH", headers })
                        .then(response => response.ok ? response.json() : Promise.reject(response))
                        .then(data => {
                            conversationSessionTitleEl.textContent = newTitle;
                        })
                        .catch(err => {
                            return;
                        });
                    const conversationSessionTitleEl = conversationSessionEl.createDiv("conversation-session-title");
                    conversationSessionTitleEl.textContent = newTitle;
                    conversationSessionTitleEl.addEventListener('click', () => {
                        chatBodyEl.innerHTML = "";
                        chatBodyEl.dataset.conversationId = incomingConversationId;
                        chatBodyEl.dataset.conversationTitle = conversationTitle;
                        this.getChatHistory(chatBodyEl);
                    });

                    let newConversationMenuEl = this.contentEl.createEl('div');
                    newConversationMenuEl = this.addConversationMenu(
                        newConversationMenuEl,
                        conversationSessionEl,
                        newTitle,
                        conversationSessionTitleEl,
                        chatBodyEl,
                        incomingConversationId,
                        selectedConversation,
                    );

                    conversationMenuEl.replaceWith(newConversationMenuEl);
                    editConversationTitleInputEl.replaceWith(conversationSessionTitleEl);
                }
            });
            conversationMenuEl.appendChild(editConversationTitleSaveButtonEl);
        });

        conversationMenuEl.appendChild(editConversationTitleButtonEl);

        let deleteConversationButtonEl = this.contentEl.createEl('button');
        setIcon(deleteConversationButtonEl, "trash");
        deleteConversationButtonEl.title = "Delete";
        deleteConversationButtonEl.classList.add("delete-conversation-button", "three-dot-menu-button-item", "clickable-icon");
        if (selectedConversation) deleteConversationButtonEl.classList.add("selected-conversation");
        deleteConversationButtonEl.addEventListener('click', () => {
            // Ask for confirmation before deleting chat session
            let confirmation = confirm('Are you sure you want to delete this chat session?');
            if (!confirmation) return;
            let deleteURL = `/api/chat/history?client=obsidian&conversation_id=${incomingConversationId}`;
            fetch(`${this.setting.khojUrl}${deleteURL}`, { method: "DELETE", headers })
                .then(response => response.ok ? response.json() : Promise.reject(response))
                .then(data => {
                    chatBodyEl.innerHTML = "";
                    chatBodyEl.dataset.conversationId = "";
                    chatBodyEl.dataset.conversationTitle = "";
                    this.toggleChatSessions(true);
                })
                .catch(err => {
                    return;
                });
        });

        conversationMenuEl.appendChild(deleteConversationButtonEl);
        return conversationMenuEl;
    }

    async getChatHistory(chatBodyEl: HTMLElement): Promise<boolean> {
        // Get chat history from Khoj backend
        let chatUrl = `${this.setting.khojUrl}/api/chat/history?client=obsidian`;
        if (chatBodyEl.dataset.conversationId) {
            chatUrl += `&conversation_id=${chatBodyEl.dataset.conversationId}`;
        }

        console.log("Fetching chat history from:", chatUrl);

        try {
            let response = await fetch(chatUrl, {
                method: "GET",
                headers: { "Authorization": `Bearer ${this.setting.khojApiKey}` },
            });

            let responseJson: any = await response.json();
            console.log("Chat history response:", responseJson);

            chatBodyEl.dataset.conversationId = responseJson.conversation_id;

            if (responseJson.detail) {
                // If the server returns error details in response, render a setup hint.
                let setupMsg = "Hi 👋🏾, to start chatting add available chat models options via [the Django Admin panel](/server/admin) on the Server";
                this.renderMessage({
                    chatBodyEl,
                    message: setupMsg,
                    sender: "khoj",
                    isSystemMessage: true
                });

                return false;
            } else if (responseJson.response) {
                // Render conversation history, if any
                chatBodyEl.dataset.conversationId = responseJson.response.conversation_id;
                chatBodyEl.dataset.conversationTitle = responseJson.response.slug || `New conversation 🌱`;

                // Update current agent from conversation history
                if (responseJson.response.agent?.slug) {
                    console.log("Found agent in conversation history:", responseJson.response.agent);
                    this.currentAgent = responseJson.response.agent.slug;
                    // Update the agent selector if it exists
                    const agentSelect = this.contentEl.querySelector('.khoj-agent-select') as HTMLSelectElement;
                    if (agentSelect && this.currentAgent) {
                        agentSelect.value = this.currentAgent;
                        console.log("Updated agent selector to:", this.currentAgent);
                    }
                }

                let chatLogs = responseJson.response?.conversation_id ? responseJson.response.chat ?? [] : responseJson.response;
                chatLogs.forEach((chatLog: any) => {
                    // Convert commands to emojis for user messages
                    if (chatLog.by === "you") {
                        chatLog.message = this.convertCommandsToEmojis(chatLog.message);
                    }

                    // Transform khoj-edit blocks into accordions
                    if (chatLog.by === "khoj") {
                        chatLog.message = this.transformEditBlocks(chatLog.message);
                    }

                    this.renderMessageWithReferences(
                        chatBodyEl,
                        chatLog.message,
                        chatLog.by,
                        chatLog.turnId,
                        chatLog.context,
                        chatLog.onlineContext,
                        new Date(chatLog.created),
                        chatLog.intent?.type,
                        chatLog.intent?.["inferred-queries"],
                        chatBodyEl.dataset.conversationId ?? "",
                        chatLog.images,
                        chatLog.excalidrawDiagram,
                        chatLog.mermaidjsDiagram,
                    );
                    // push the user messages to the chat history
                    if (chatLog.by === "you") {
                        this.userMessages.push(chatLog.message);
                    }
                });

                // Update starting message after loading history
                this.startingMessage = this.getLearningMoment();

                // Update the placeholder of the chat input
                const chatInput = this.contentEl.querySelector('.khoj-chat-input') as HTMLTextAreaElement;
                if (chatInput) {
                    chatInput.placeholder = this.startingMessage;
                }
            }
        } catch (err) {
            let errorMsg = "Unable to get response from Khoj server ❤️‍🩹. Ensure server is running or contact developers for help at [team@khoj.dev](mailto:team@khoj.dev) or in [Discord](https://discord.gg/BDgyabRM6e)";
            this.renderMessage({
                chatBodyEl,
                message: errorMsg,
                sender: "khoj",
                isSystemMessage: true
            });
            return false;
        }
        return true;
    }

    convertMessageChunkToJson(rawChunk: string): MessageChunk {
        if (rawChunk?.startsWith("{") && rawChunk?.endsWith("}")) {
            try {
                let jsonChunk = JSON.parse(rawChunk);
                if (!jsonChunk.type)
                    jsonChunk = { type: 'message', data: jsonChunk };
                return jsonChunk;
            } catch (e) {
                return { type: 'message', data: rawChunk };
            }
        } else if (rawChunk.length > 0) {
            return { type: 'message', data: rawChunk };
        }
        return { type: '', data: '' };
    }

    async processMessageChunk(rawChunk: string): Promise<void> {
        const chunk = this.convertMessageChunkToJson(rawChunk);
        if (!chunk || !chunk.type) return;

        if (chunk.type === 'start_llm_response') {
            // Start of streaming - set flag and ensure UI is stable
            this.isStreaming = true;

            // Disable input resizing during streaming
            const chatInput = <HTMLTextAreaElement>this.contentEl.getElementsByClassName("khoj-chat-input")[0];
            if (chatInput) {
                chatInput.style.overflowY = 'hidden';
            }
        }
        else if (chunk.type === 'status') {
            const statusMessage = chunk.data;
            this.handleStreamResponse(this.chatMessageState.newResponseTextEl, statusMessage, this.chatMessageState.loadingEllipsis, false);
        }
        else if (chunk.type === 'generated_assets') {
            const generatedAssets = chunk.data;
            const imageData = this.handleImageResponse(generatedAssets, this.chatMessageState.rawResponse);
            this.chatMessageState.generatedAssets = imageData;
            this.handleStreamResponse(this.chatMessageState.newResponseTextEl, imageData, this.chatMessageState.loadingEllipsis, false);
        }
        else if (chunk.type === 'end_llm_response') {
            // End of streaming - reset flag and restore normal UI behavior
            this.isStreaming = false;

            // Re-enable input resizing after streaming
            const chatInput = <HTMLTextAreaElement>this.contentEl.getElementsByClassName("khoj-chat-input")[0];
            if (chatInput) {
                // Call autoResize to restore proper sizing
                this.autoResize();
            }
        }
        else if (chunk.type === 'end_response') {
            // Ensure streaming flag is reset at the end of the response
            this.isStreaming = false;

            // Re-enable input resizing after streaming
            const chatInput = <HTMLTextAreaElement>this.contentEl.getElementsByClassName("khoj-chat-input")[0];
            if (chatInput) {
                // Call autoResize to restore proper sizing
                this.autoResize();
            }

            // Check for edit blocks if in write mode
            if (this.fileAccessMode === 'write') {
                const editBlocks = this.parseEditBlocks(this.chatMessageState.rawResponse);

                // Check for errors and retry if needed
                if (editBlocks.length > 0 && editBlocks[0].hasError && this.editRetryCount < this.maxEditRetries) {
                    await this.handleEditRetry(editBlocks[0]);
                    return;
                }

                // Reset retry count on success
                this.editRetryCount = 0;

                // Apply edits if there are any
                if (editBlocks.length > 0) {
                    await this.applyEditBlocks(editBlocks);
                }
            }

            // Automatically respond with voice if the subscribed user has sent voice message
            if (this.chatMessageState.isVoice && this.setting.userInfo?.is_active && this.setting.autoVoiceResponse)
                this.textToSpeech(this.chatMessageState.rawResponse);

            // Append any references after all the data has been streamed
            this.finalizeChatBodyResponse(this.chatMessageState.references, this.chatMessageState.newResponseTextEl, this.chatMessageState.turnId);

            // Reset state including retry counts
            const liveQuery = this.chatMessageState.rawQuery;
            this.chatMessageState = {
                newResponseTextEl: null,
                newResponseEl: null,
                loadingEllipsis: null,
                references: {},
                rawResponse: "",
                rawQuery: liveQuery,
                isVoice: false,
                generatedAssets: "",
                turnId: "",
                editBlocks: [],
                editRetryCount: 0,
                parentRetryCount: 0 // Reset parent retry count
            };
        }
        else if (chunk.type === "references") {
            this.chatMessageState.references = { "notes": chunk.data.context, "online": chunk.data.onlineContext };
        }
        else if (chunk.type === 'message') {
            const chunkData = chunk.data;
            if (typeof chunkData === 'object' && chunkData !== null) {
                this.handleJsonResponse(chunkData);
            } else if (typeof chunkData === 'string' && chunkData.trim()?.startsWith("{") && chunkData.trim()?.endsWith("}")) {
                try {
                    const jsonData = JSON.parse(chunkData.trim());
                    this.handleJsonResponse(jsonData);
                } catch (e) {
                    this.chatMessageState.rawResponse += chunkData;
                    this.handleStreamResponse(this.chatMessageState.newResponseTextEl, this.chatMessageState.rawResponse + this.chatMessageState.generatedAssets, this.chatMessageState.loadingEllipsis);
                }
            } else {
                this.chatMessageState.rawResponse += chunkData;
                this.handleStreamResponse(this.chatMessageState.newResponseTextEl, this.chatMessageState.rawResponse + this.chatMessageState.generatedAssets, this.chatMessageState.loadingEllipsis);
            }
        }
        else if (chunk.type === "metadata") {
            const { turnId } = chunk.data;
            if (turnId) {
                this.chatMessageState.turnId = turnId;
            }
        }
    }

    handleJsonResponse(jsonData: any): void {
        if (jsonData.image || jsonData.detail || jsonData.images || jsonData.mermaidjsDiagram) {
            this.chatMessageState.rawResponse = this.handleImageResponse(jsonData, this.chatMessageState.rawResponse);
        } else if (jsonData.response) {
            this.chatMessageState.rawResponse = jsonData.response;
        }

        if (this.chatMessageState.newResponseTextEl) {
            this.chatMessageState.newResponseTextEl.innerHTML = "";
            this.chatMessageState.newResponseTextEl.appendChild(this.formatHTMLMessage(this.chatMessageState.rawResponse));
        }
    }

    async readChatStream(response: Response): Promise<void> {
        // Exit if response body is empty
        if (response.body == null) return;

        const reader = response.body.getReader();
        const decoder = new TextDecoder();
        const eventDelimiter = '␃🔚␗';
        let buffer = '';

        while (true) {
            const { value, done } = await reader.read();

            if (done) {
                this.processMessageChunk(buffer);
                buffer = '';
                // Break if the stream is done
                break;
            }

            const chunk = decoder.decode(value, { stream: true });
            console.debug("Raw Chunk:", chunk)
            // Start buffering chunks until complete event is received
            buffer += chunk;

            // Once the buffer contains a complete event
            let newEventIndex;
            while ((newEventIndex = buffer.indexOf(eventDelimiter)) !== -1) {
                // Extract the event from the buffer
                const event = buffer.slice(0, newEventIndex);
                buffer = buffer.slice(newEventIndex + eventDelimiter.length);

                // Process the event
                if (event) this.processMessageChunk(event);
            }
        }
    }

    async getChatResponse(query: string | undefined | null, displayQuery: string | undefined | null, isVoice: boolean = false, displayUserMessage: boolean = true): Promise<void> {
        // Exit if query is empty
        if (!query || query === "") return;

        // Get chat body element
        let chatBodyEl = this.contentEl.getElementsByClassName("khoj-chat-body")[0] as HTMLElement;

        // Render user query as chat message with display version only if displayUserMessage is true
        if (displayUserMessage) {
            this.renderMessage({ chatBodyEl, message: displayQuery || query, sender: "you" });
        }

        let conversationId = chatBodyEl.dataset.conversationId;

        if (!conversationId) {
            try {
                const requestBody = {
                    ...(this.currentAgent && { agent_slug: this.currentAgent })
                };

                const response = await fetch(`${this.setting.khojUrl}/api/chat/sessions`, {
                    method: "POST",
                    headers: {
                        "Authorization": `Bearer ${this.setting.khojApiKey}`,
                        "Content-Type": "application/json"
                    },
                    body: JSON.stringify(requestBody)
                });
                if (response.ok) {
                    const data = await response.json();
                    conversationId = data.conversation_id;
                    chatBodyEl.dataset.conversationId = conversationId;
                } else {
                    console.error("Failed to create session:", response.statusText);
                    return;
                }
            } catch (error) {
                console.error("Error creating session:", error);
                return;
            }
        }

        // Get open files content if we have access
        const openFilesContent = await this.getOpenFilesContent();

        // Extract mode command if present
        const modeMatch = this.chatModes.find(mode => query.startsWith(mode.command));
        const modeCommand = modeMatch ? query.substring(0, modeMatch.command.length) : '';
        const queryWithoutMode = modeMatch ? query.substring(modeMatch.command.length).trim() : query;

        // Combine mode, query and files content
        const finalQuery = modeCommand + (modeCommand ? ' ' : '') + queryWithoutMode + openFilesContent;

        // Get chat response from Khoj backend
        const chatUrl = `${this.setting.khojUrl}/api/chat?client=obsidian`;
        const body = {
            q: finalQuery,
            n: this.setting.resultsCount,
            stream: true,
            conversation_id: conversationId,
            ...(this.currentAgent && { agent_slug: this.currentAgent }),
            ...(!!this.location && this.location.city && { city: this.location.city }),
            ...(!!this.location && this.location.region && { region: this.location.region }),
            ...(!!this.location && this.location.countryName && { country: this.location.countryName }),
            ...(!!this.location && this.location.countryCode && { country_code: this.location.countryCode }),
            ...(!!this.location && this.location.timezone && { timezone: this.location.timezone }),
        };

        let newResponseEl = this.createKhojResponseDiv();
        let newResponseTextEl = newResponseEl.createDiv();
        newResponseTextEl.classList.add("khoj-chat-message-text", "khoj");

        // Temporary status message to indicate that Khoj is thinking
        let loadingEllipsis = this.createLoadingEllipse();
        newResponseTextEl.appendChild(loadingEllipsis);

        // Set chat message state
        this.chatMessageState = {
            newResponseEl: newResponseEl,
            newResponseTextEl: newResponseTextEl,
            loadingEllipsis: loadingEllipsis,
            references: {},
            rawQuery: query,
            rawResponse: "",
            isVoice: isVoice,
            generatedAssets: "",
            turnId: "",
            editBlocks: [],
            editRetryCount: 0
        };

        let response = await fetch(chatUrl, {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
                "Authorization": `Bearer ${this.setting.khojApiKey}`,
            },
            body: JSON.stringify(body),
        })

        try {
            if (response.body === null) throw new Error("Response body is null");

            // Stream and render chat response
            await this.readChatStream(response);
        } catch (err) {
            console.error(`Khoj chat response failed with\n${err}`);
            let errorMsg = "Sorry, unable to get response from Khoj backend ❤️‍🩹. Retry or contact developers for help at <a href=mailto:'team@khoj.dev'>team@khoj.dev</a> or <a href='https://discord.gg/BDgyabRM6e'>on Discord</a>";
            newResponseTextEl.textContent = errorMsg;
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
        let chatBody = this.contentEl.getElementsByClassName("khoj-chat-body")[0] as HTMLElement;

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
    async speechToText(event: MouseEvent | TouchEvent | KeyboardEvent) {
        event.preventDefault();
        const transcribeButton = <HTMLButtonElement>this.contentEl.getElementsByClassName("khoj-transcribe")[0];
        const chatInput = <HTMLTextAreaElement>this.contentEl.getElementsByClassName("khoj-chat-input")[0];
        const sendButton = <HTMLButtonElement>this.contentEl.getElementsByClassName("khoj-chat-send")[0]

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
            let noSpeechText: string[] = [
                "Thanks for watching!",
                "Thanks for watching.",
                "Thank you for watching!",
                "Thank you for watching.",
                "You",
                "Bye."
            ];
            let noSpeech: boolean = false;
            if (response.status === 200) {
                console.log(response);
                noSpeech = noSpeechText.includes(response.json.text.trimStart());
                if (!noSpeech) chatInput.value += response.json.text.trimStart();
                this.autoResize();
            } else if (response.status === 501) {
                throw new Error("⛔️ Configure speech-to-text model on server.");
            } else if (response.status === 422) {
                throw new Error("⛔️ Audio file to large to process.");
            } else {
                throw new Error("⛔️ Failed to transcribe audio.");
            }

            // Don't auto-send empty messages or when no speech is detected
            if (chatInput.value.length === 0 || noSpeech) return;

            // Show stop auto-send button. It stops auto-send when clicked
            setIcon(sendButton, "stop-circle");
            let stopSendButtonImg = <SVGElement>sendButton.getElementsByClassName("lucide-stop-circle")[0]
            stopSendButtonImg.addEventListener('click', (_) => { this.cancelSendMessage() });

            // Start the countdown timer UI
            stopSendButtonImg.getElementsByTagName("circle")[0].style.animation = "countdown 3s linear 1 forwards";
            stopSendButtonImg.getElementsByTagName("circle")[0].style.color = "var(--icon-color-active)";

            // Auto send message after 3 seconds
            this.sendMessageTimeout = setTimeout(() => {
                // Stop the countdown timer UI
                setIcon(sendButton, "arrow-up-circle")
                let sendImg = <SVGElement>sendButton.getElementsByClassName("lucide-arrow-up-circle")[0]
                sendImg.addEventListener('click', async (_) => { await this.chat() });

                // Send message
                this.chat(true);
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
            // setIcon(transcribeButton, "mic-off");
            transcribeButton.classList.add("loading-encircle")
        };

        // Toggle recording
        if (!this.mediaRecorder || this.mediaRecorder.state === 'inactive' || event.type === 'touchstart' || event.type === 'mousedown' || event.type === 'keydown') {
            navigator.mediaDevices
                .getUserMedia({ audio: true })
                ?.then(handleRecording)
                .catch((e) => {
                    this.flashStatusInChatInput("⛔️ Failed to access microphone");
                });
        } else if (this.mediaRecorder?.state === 'recording' || event.type === 'touchend' || event.type === 'touchcancel' || event.type === 'mouseup' || event.type === 'keyup') {
            this.mediaRecorder.stop();
            this.mediaRecorder.stream.getTracks().forEach(track => track.stop());
            this.mediaRecorder = undefined;
            transcribeButton.classList.remove("loading-encircle");
            setIcon(transcribeButton, "mic");
        }
    }

    cancelSendMessage() {
        // Cancel the auto-send chat message timer if the stop-send-button is clicked
        clearTimeout(this.sendMessageTimeout);

        // Revert to showing send-button and hide the stop-send-button
        let sendButton = <HTMLButtonElement>this.contentEl.getElementsByClassName("khoj-chat-send")[0];
        setIcon(sendButton, "arrow-up-circle");
        let sendImg = <SVGElement>sendButton.getElementsByClassName("lucide-arrow-up-circle")[0]
        sendImg.addEventListener('click', async (_) => { await this.chat() });
    };

    incrementalChat(event: KeyboardEvent) {
        // If dropdown is visible and Enter is pressed, select the current option
        if (this.modeDropdown && this.modeDropdown.style.display !== "none" && event.key === "Enter") {
            event.preventDefault();

            const options = this.modeDropdown.querySelectorAll<HTMLElement>(".khoj-mode-dropdown-option");
            const visibleOptions = Array.from(options).filter(option =>
                option.style.display !== "none"
            );

            // If any option is selected, use that one
            if (this.selectedOptionIndex >= 0 && this.selectedOptionIndex < visibleOptions.length) {
                const selectedOption = visibleOptions[this.selectedOptionIndex];
                const index = parseInt(selectedOption.getAttribute("data-index") || "0");
                const chatInput = <HTMLTextAreaElement>this.contentEl.getElementsByClassName("khoj-chat-input")[0];

                chatInput.value = this.chatModes[index].command + " ";
                chatInput.focus();
                this.currentUserInput = chatInput.value;
                this.hideModeDropdown();
            }
            // If no option is selected but there's exactly one visible option, use that
            else if (visibleOptions.length === 1) {
                const onlyOption = visibleOptions[0];
                const index = parseInt(onlyOption.getAttribute("data-index") || "0");
                const chatInput = <HTMLTextAreaElement>this.contentEl.getElementsByClassName("khoj-chat-input")[0];

                chatInput.value = this.chatModes[index].command + " ";
                chatInput.focus();
                this.currentUserInput = chatInput.value;
                this.hideModeDropdown();
            }
            return;
        }

        const chatInput = <HTMLTextAreaElement>this.contentEl.getElementsByClassName("khoj-chat-input")[0];
        const trimmedValue = chatInput.value.trim();

        // Check if value is empty or just a mode command
        const isOnlyModeCommand = this.chatModes.some(mode =>
            trimmedValue === mode.command || trimmedValue === mode.command + " "
        );

        if (event.key === 'Enter' && !event.shiftKey) {
            // If message is empty or just a mode command, don't send
            if (!trimmedValue || isOnlyModeCommand) {
                event.preventDefault();
                return;
            }

            // Otherwise, send message as normal
            event.preventDefault();
            this.chat();
        }
    }

    onChatInput() {
        const chatInput = <HTMLTextAreaElement>this.contentEl.getElementsByClassName("khoj-chat-input")[0];
        chatInput.value = chatInput.value.trimStart();
        this.currentMessageIndex = -1;

        // Store the current input
        this.currentUserInput = chatInput.value;

        // Check if input starts with "/" and show dropdown
        if (chatInput.value.startsWith("/")) {
            this.showModeDropdown(chatInput);
            this.selectedOptionIndex = -1; // Reset selected index
        } else if (this.modeDropdown) {
            // Hide dropdown if input doesn't start with "/"
            this.hideModeDropdown();
        }

        this.autoResize();
    }

    autoResize() {
        const chatInput = <HTMLTextAreaElement>this.contentEl.getElementsByClassName("khoj-chat-input")[0];

        // Skip resizing completely during active streaming to avoid UI jumps
        if (this.isStreaming) {
            return;
        }

        // Reset height to auto to get the correct scrollHeight
        chatInput.style.height = 'auto';

        // Calculate new height based on content with a larger maximum height
        const maxHeight = 400;
        const newHeight = Math.min(chatInput.scrollHeight, maxHeight);
        chatInput.style.height = newHeight + 'px';
        // Add overflow-y: auto only if content exceeds max height
        if (chatInput.scrollHeight > maxHeight) {
            chatInput.style.overflowY = 'auto';
        } else {
            chatInput.style.overflowY = 'hidden';
        }

        // Update dropdown position if it exists and is visible
        if (this.modeDropdown && this.modeDropdown.style.display !== "none") {
            const inputRect = chatInput.getBoundingClientRect();
            const containerRect = this.contentEl.getBoundingClientRect();

            this.modeDropdown.style.left = `${inputRect.left - containerRect.left}px`;
            this.modeDropdown.style.top = `${inputRect.top - containerRect.top - 4}px`; // Position above with small gap
            this.modeDropdown.style.width = `${inputRect.width}px`;
        }
    }

    scrollChatToBottom() {
        const chat_body_el = this.contentEl.getElementsByClassName("khoj-chat-body")[0];
        if (!!chat_body_el) chat_body_el.scrollTop = chat_body_el.scrollHeight;
    }

    createLoadingEllipse() {
        // Temporary status message to indicate that Khoj is thinking
        let loadingEllipsis = this.contentEl.createEl("div");
        loadingEllipsis.classList.add("lds-ellipsis");

        let firstEllipsis = this.contentEl.createEl("div");
        firstEllipsis.classList.add("lds-ellipsis-item");

        let secondEllipsis = this.contentEl.createEl("div");
        secondEllipsis.classList.add("lds-ellipsis-item");

        let thirdEllipsis = this.contentEl.createEl("div");
        thirdEllipsis.classList.add("lds-ellipsis-item");

        let fourthEllipsis = this.contentEl.createEl("div");
        fourthEllipsis.classList.add("lds-ellipsis-item");

        loadingEllipsis.appendChild(firstEllipsis);
        loadingEllipsis.appendChild(secondEllipsis);
        loadingEllipsis.appendChild(thirdEllipsis);
        loadingEllipsis.appendChild(fourthEllipsis);

        return loadingEllipsis;
    }

    handleStreamResponse(newResponseElement: HTMLElement | null, rawResponse: string, loadingEllipsis: HTMLElement | null, replace = true) {
        if (!newResponseElement) return;

        // Remove loading ellipsis if it exists
        if (newResponseElement.getElementsByClassName("lds-ellipsis").length > 0 && loadingEllipsis)
            newResponseElement.removeChild(loadingEllipsis);

        // Always replace the content completely
        newResponseElement.innerHTML = "";
        const messageEl = this.formatHTMLMessage(rawResponse, false, true);
        messageEl.classList.add('khoj-message-new-content');
        newResponseElement.appendChild(messageEl);

        // Remove the animation class after the animation completes
        setTimeout(() => {
            newResponseElement.classList.remove('khoj-message-new-content');
        }, 300);
    }

    handleImageResponse(imageJson: any, rawResponse: string) {
        if (imageJson.image) {
            const inferredQuery = imageJson.inferredQueries?.[0] ?? "generated image";

            // If response has image field, response is a generated image.
            if (imageJson.intentType === "text-to-image") {
                rawResponse += `![generated_image](data:image/png;base64,${imageJson.image})`;
            } else if (imageJson.intentType === "text-to-image2") {
                rawResponse += `![generated_image](${imageJson.image})`;
            } else if (imageJson.intentType === "text-to-image-v3") {
                rawResponse = `![generated_image](${imageJson.image})`;
            } else if (imageJson.intentType === "excalidraw") {
                const domain = this.setting.khojUrl.endsWith("/") ? this.setting.khojUrl : `${this.setting.khojUrl}/`;
                const redirectMessage = `Hey, I'm not ready to show you diagrams yet here. But you can view it in ${domain}`;
                rawResponse += redirectMessage;
            }
            if (inferredQuery) {
                rawResponse += `\n\n**Inferred Query**:\n\n${inferredQuery}`;
            }
        } else if (imageJson.images) {
            // If response has images field, response is a list of generated images.
            imageJson.images.forEach((image: any) => {
                rawResponse += `![generated_image](${image})\n\n`;
            });
        } else if (imageJson.excalidrawDiagram) {
            const domain = this.setting.khojUrl.endsWith("/") ? this.setting.khojUrl : `${this.setting.khojUrl}/`;
            const redirectMessage = `Hey, I'm not ready to show you diagrams yet here. But you can view it in ${domain}`;
            rawResponse += redirectMessage;
        } else if (imageJson.mermaidjsDiagram) {
            rawResponse += imageJson.mermaidjsDiagram;
        }

        // If response has detail field, response is an error message.
        if (imageJson.detail) rawResponse += imageJson.detail;

        return rawResponse;
    }

    finalizeChatBodyResponse(references: object, newResponseElement: HTMLElement | null, turnId: string) {
        if (!!newResponseElement && references != null && Object.keys(references).length > 0) {
            newResponseElement.appendChild(this.createReferenceSection(references));
        }
        if (!!newResponseElement && turnId) {
            // Set the turnId for the new response and the previous user message
            newResponseElement.parentElement?.setAttribute("data-turnid", turnId);
            newResponseElement.parentElement?.previousElementSibling?.setAttribute("data-turnid", turnId);
        }
        this.scrollChatToBottom();
        let chatInput = this.contentEl.getElementsByClassName("khoj-chat-input")[0];
        if (chatInput) chatInput.removeAttribute("disabled");
    }

    createReferenceSection(references: any) {
        let referenceSection = this.contentEl.createEl('div');
        referenceSection.classList.add("reference-section");
        referenceSection.classList.add("collapsed");

        let numReferences = 0;

        if (references.hasOwnProperty("notes")) {
            numReferences += references["notes"].length;

            references["notes"].forEach((reference: any, index: number) => {
                let polishedReference = this.generateReference(referenceSection, reference, index);
                referenceSection.appendChild(polishedReference);
            });
        }
        if (references.hasOwnProperty("online")) {
            numReferences += this.processOnlineReferences(referenceSection, references["online"]);
        }

        let referenceExpandButton = this.contentEl.createEl('button');
        referenceExpandButton.classList.add("reference-expand-button");
        referenceExpandButton.innerHTML = numReferences == 1 ? "1 reference" : `${numReferences} references`;

        referenceExpandButton.addEventListener('click', function() {
            if (referenceSection.classList.contains("collapsed")) {
                referenceSection.classList.remove("collapsed");
                referenceSection.classList.add("expanded");
            } else {
                referenceSection.classList.add("collapsed");
                referenceSection.classList.remove("expanded");
            }
        });

        let referencesDiv = this.contentEl.createEl('div');
        referencesDiv.classList.add("references");
        referencesDiv.appendChild(referenceExpandButton);
        referencesDiv.appendChild(referenceSection);

        return referencesDiv;
    }

    // function to loop through the user's past messages
    handleArrowKeys(event: KeyboardEvent) {
        const chatInput = <HTMLTextAreaElement>this.contentEl.getElementsByClassName("khoj-chat-input")[0];

        // Handle dropdown navigation with arrow keys
        if (this.modeDropdown && this.modeDropdown.style.display !== "none") {
            const options = this.modeDropdown.querySelectorAll<HTMLElement>(".khoj-mode-dropdown-option");
            // Only consider visible options
            const visibleOptions = Array.from(options).filter(option =>
                option.style.display !== "none"
            );

            if (visibleOptions.length === 0) {
                this.hideModeDropdown();
                return;
            }

            switch (event.key) {
                case "ArrowDown":
                    event.preventDefault();
                    if (this.selectedOptionIndex < 0) {
                        this.selectedOptionIndex = 0;
                    } else {
                        this.selectedOptionIndex = Math.min(this.selectedOptionIndex + 1, visibleOptions.length - 1);
                    }
                    this.highlightVisibleOption(visibleOptions);
                    break;

                case "ArrowUp":
                    event.preventDefault();
                    if (this.selectedOptionIndex < 0) {
                        this.selectedOptionIndex = visibleOptions.length - 1;
                    } else {
                        this.selectedOptionIndex = Math.max(this.selectedOptionIndex - 1, 0);
                    }
                    this.highlightVisibleOption(visibleOptions);
                    break;

                case "Enter":
                    // We handle Enter in incrementalChat now
                    break;

                case "Escape":
                    event.preventDefault();
                    this.hideModeDropdown();
                    break;
            }

            // Don't process arrow keys for history navigation if dropdown is open
            if (event.key === "ArrowUp" || event.key === "ArrowDown") {
                return;
            }
        }

        // Original arrow key handling for message history
        // (Le code existant pour gérer les touches fléchées)
    }

    /**
     * Highlights the selected option among visible options
     * @param {HTMLElement[]} visibleOptions - Array of visible dropdown options
     */
    private highlightVisibleOption(visibleOptions: HTMLElement[]) {
        const allOptions = this.modeDropdown?.querySelectorAll<HTMLElement>(".khoj-mode-dropdown-option");
        if (!allOptions) return;

        // Clear highlighting on all options first
        allOptions.forEach(option => {
            option.classList.remove("khoj-mode-dropdown-option-selected");
        });

        // Add highlighting to the selected visible option
        if (this.selectedOptionIndex >= 0 && this.selectedOptionIndex < visibleOptions.length) {
            const selectedOption = visibleOptions[this.selectedOptionIndex];
            selectedOption.classList.add("khoj-mode-dropdown-option-selected");

            // Scroll to selected option if needed
            if (this.modeDropdown) {
                const container = this.modeDropdown;
                if (selectedOption.offsetTop < container.scrollTop) {
                    container.scrollTop = selectedOption.offsetTop;
                } else if (selectedOption.offsetTop + selectedOption.offsetHeight > container.scrollTop + container.offsetHeight) {
                    container.scrollTop = selectedOption.offsetTop + selectedOption.offsetHeight - container.offsetHeight;
                }
            }
        }
    }

    // Add this new method to handle message deletion
    async deleteMessage(messageEl: HTMLElement, skipPaired: boolean = false, skipBackend: boolean = false) {
        // Find parent message container
        const messageContainer = messageEl.closest('.khoj-chat-message');
        if (!messageContainer) return;

        // Get paired message to delete if needed
        let pairedMessageContainer: Element | null = null;
        if (!skipPaired) {
            const messages = Array.from(document.getElementsByClassName('khoj-chat-message'));
            const currentIndex = messages.indexOf(messageContainer as HTMLElement);

            // If we're deleting a user message, also delete the subsequent khoj message (if any)
            if (messageContainer.classList.contains('you') && currentIndex < messages.length - 1) {
                pairedMessageContainer = messages[currentIndex + 1];
            }
            // If we're deleting a khoj message, also delete the preceding user message (if any)
            else if (messageContainer.classList.contains('khoj') && currentIndex > 0) {
                pairedMessageContainer = messages[currentIndex - 1];
            }
        }

        // Add animation class
        messageContainer.classList.add('deleting');
        if (pairedMessageContainer) {
            pairedMessageContainer.classList.add('deleting');
        }

        // Wait for animation to complete
        setTimeout(async () => {
            // Get turn ID for message
            const turnId = messageContainer.getAttribute('data-turnid');

            // Remove message(s) from DOM
            messageContainer.remove();
            if (pairedMessageContainer) {
                pairedMessageContainer.remove();
            }

            // Only delete in backend if not skipped
            if (!skipBackend && turnId) {
                const chatBodyEl = this.contentEl.getElementsByClassName("khoj-chat-body")[0] as HTMLElement;
                const conversationId = chatBodyEl.dataset.conversationId;

                if (!conversationId) return;

                try {
                    // Delete from backend
                    const response = await fetch(`${this.setting.khojUrl}/api/chat/conversation/message`, {
                        method: 'DELETE',
                        headers: {
                            'Content-Type': 'application/json',
                            'Authorization': `Bearer ${this.setting.khojApiKey}`
                        },
                        body: JSON.stringify({
                            conversation_id: conversationId,
                            turn_id: turnId
                        }),
                    });

                    if (!response.ok) {
                        console.error('Failed to delete message from backend:', await response.text());
                        this.flashStatusInChatInput("Failed to delete message");
                    }
                } catch (error) {
                    console.error('Error deleting message:', error);
                    this.flashStatusInChatInput("Error deleting message");
                }
            }
        }, 300); // Matches the animation duration
    }

    async fetchAgents() {
        try {
            const response = await fetch(`${this.setting.khojUrl}/api/agents`, {
                headers: {
                    "Authorization": `Bearer ${this.setting.khojApiKey}`
                }
            });

            if (response.ok) {
                this.agents = await response.json();
            } else {
                console.error("Failed to fetch agents:", response.statusText);
            }
        } catch (error) {
            console.error("Error fetching agents:", error);
        }
    }

    // Add this new method after the class declaration
    private async getOpenFilesContent(): Promise<string> {
        return this.fileInteractions.getOpenFilesContent(this.fileAccessMode);
    }

    private parseEditBlocks(message: string): EditBlock[] {
        return this.fileInteractions.parseEditBlocks(message);
    }

    private async applyEditBlocks(editBlocks: EditBlock[]) {
        // Check for parsing errors first
        if (editBlocks.length === 0) return;

        // Apply edits using the FileInteractions class
        const { editResults, fileBackups } = await this.fileInteractions.applyEditBlocks(
            editBlocks,
            (blockToRetry) => {
                if (this.editRetryCount < this.maxEditRetries) {
                    this.handleEditRetry(blockToRetry);
                }
            }
        );

        // Add confirmation buttons to the last message
        const chatBodyEl = this.contentEl.getElementsByClassName("khoj-chat-body")[0];
        const lastMessage = chatBodyEl.lastElementChild;

        if (lastMessage) {
            const buttonsContainer = lastMessage.createDiv({ cls: "edit-confirmation-buttons" });

            // Create a dedicated container for status summary to ensure proper separation
            const statusContainer = buttonsContainer.createDiv({ cls: "edit-status-container" });

            // Add status summary as a separate element with its own styling
            const statusSummary = statusContainer.createDiv({ cls: "edit-status-summary" });
            const successCount = editResults.filter(r => r.success).length;

            // Add appropriate status class based on success/failure
            if (successCount === editResults.length) {
                statusSummary.innerHTML = `All edits applied successfully`;
                statusSummary.addClass("success");
            } else if (successCount === 0) {
                statusSummary.innerHTML = `No edits were applied`;
                statusSummary.addClass("error");
            } else {
                // This should not happen with atomic approach, but keeping for safety
                statusSummary.innerHTML = `${successCount}/${editResults.length} edits applied successfully`;
                statusSummary.addClass(successCount > 0 ? "success" : "error");
            }

            if (editResults.some(r => !r.success)) {
                const errorDetails = editResults
                    .filter(r => !r.success)
                    .map(r => {
                        // Check if the error is due to atomic validation failure
                        if (r.error && r.error.includes('Other edits in the group failed')) {
                            return `• ${r.block.note}: Not applied due to atomic validation failure`;
                        }
                        return `• ${r.block.note}: ${r.error}`;
                    })
                    .join('\n');
                statusSummary.title = `Failed edits:\n${errorDetails}`;
            }

            // Create Apply button
            const applyButton = buttonsContainer.createEl("button", {
                text: "Apply",
                cls: ["edit-confirm-button", "edit-apply-button"],
            });

            // Create Cancel button
            const cancelButton = buttonsContainer.createEl("button", {
                text: "Cancel",
                cls: ["edit-confirm-button", "edit-cancel-button"],
            });

            // Scroll to the buttons
            buttonsContainer.scrollIntoView({ behavior: "smooth", block: "center" });

            // Handle Apply/Cancel clicks
            this.setupConfirmationButtons(applyButton, cancelButton, fileBackups, lastMessage, buttonsContainer);
        }
    }

    // Helper method to setup confirmation buttons
    private setupConfirmationButtons(
        applyButton: HTMLButtonElement,
        cancelButton: HTMLButtonElement,
        fileBackups: Map<string, string>,
        lastMessage: Element,
        buttonsContainer: HTMLDivElement
    ) {
        applyButton.addEventListener("click", async () => {
            try {
                for (const [filePath, originalContent] of fileBackups) {
                    const file = this.app.vault.getAbstractFileByPath(filePath);
                    if (file && file instanceof TFile) {
                        const currentContent = await this.app.vault.read(file);
                        let finalContent = currentContent;

                        // Remove diff markers
                        finalContent = finalContent.replace(/~~[^~]*~~\n?(?=~~)/g, '');
                        finalContent = finalContent.replace(/~~[^~]*~~/g, '');
                        finalContent = finalContent.replace(/==/g, '');

                        await this.app.vault.modify(file, finalContent);
                    }
                }

                const successMessage = lastMessage.createDiv({ cls: "edit-status-message success" });
                successMessage.textContent = "Changes applied successfully";
                setTimeout(() => successMessage.remove(), 3000);
            } catch (error) {
                console.error("Error applying changes:", error);
                const errorMessage = lastMessage.createDiv({ cls: "edit-status-message error" });
                errorMessage.textContent = "Error applying changes";
                setTimeout(() => errorMessage.remove(), 3000);
            } finally {
                buttonsContainer.remove();
            }
        });

        cancelButton.addEventListener("click", async () => {
            try {
                for (const [filePath, originalContent] of fileBackups) {
                    const file = this.app.vault.getAbstractFileByPath(filePath);
                    if (file && file instanceof TFile) {
                        await this.app.vault.modify(file, originalContent);
                    }
                }
                const successMessage = lastMessage.createDiv({ cls: "edit-status-message success" });
                successMessage.textContent = "Changes cancelled successfully";
                setTimeout(() => successMessage.remove(), 3000);
            } catch (error) {
                console.error("Error cancelling changes:", error);
                const errorMessage = lastMessage.createDiv({ cls: "edit-status-message error" });
                errorMessage.textContent = "Error cancelling changes";
                setTimeout(() => errorMessage.remove(), 3000);
            } finally {
                buttonsContainer.remove();
            }
        });
    }

    private convertCommandsToEmojis(message: string): string {
        const modeMatch = this.chatModes.find(mode => message.startsWith(mode.command));
        if (modeMatch) {
            return message.replace(modeMatch.command, `[${modeMatch.label}]`);
        }
        return message;
    }

    // Make the method public and async
    public async applyPendingEdits() {
        const chatBodyEl = this.contentEl.getElementsByClassName("khoj-chat-body")[0];
        const lastMessage = chatBodyEl.lastElementChild;
        if (!lastMessage) return;

        // Check for edit confirmation buttons
        const buttonsContainer = lastMessage.querySelector(".edit-confirmation-buttons");
        if (!buttonsContainer) return;

        // Find and click the apply button if it exists
        const applyButton = buttonsContainer.querySelector(".edit-apply-button");
        if (applyButton instanceof HTMLElement) {
            applyButton.click();
        }
    }

    // Make the method public
    public async cancelPendingEdits() {
        const chatBodyEl = this.contentEl.getElementsByClassName("khoj-chat-body")[0];
        const lastMessage = chatBodyEl.lastElementChild;
        if (!lastMessage) return;

        // Check for edit confirmation buttons
        const buttonsContainer = lastMessage.querySelector(".edit-confirmation-buttons");
        if (!buttonsContainer) return;

        // Find and click the cancel button if it exists
        const cancelButton = buttonsContainer.querySelector(".edit-cancel-button");
        if (cancelButton instanceof HTMLElement) {
            cancelButton.click();
        }
    }

    // Add this new method to handle khoj-edit block transformation
    private transformEditBlocks(message: string): string {
        return this.fileInteractions.transformEditBlocks(message);
    }

    private async handleEditRetry(errorBlock: EditBlock) {
        this.editRetryCount++;

        // Format error message based on the error type
        let errorDetails = '';
        if (errorBlock.error?.type === 'missing_field') {
            errorDetails = `Missing required fields: ${errorBlock.error.message}\n`;
            errorDetails += `Please include all required fields:\n${errorBlock.error.details}\n`;
        } else if (errorBlock.error?.type === 'invalid_format') {
            errorDetails = `The JSON format is invalid: ${errorBlock.error.message}\n`;
            errorDetails += "Please check the syntax and provide a valid JSON edit block.\n";
        } else {
            errorDetails = `Error: ${errorBlock.error?.message || 'Unknown error'}\n`;
            if (errorBlock.error?.details) {
                errorDetails += `Details: ${errorBlock.error.details}\n`;
            }
        }

        // Create retry badge - keep it simple and focused only on retry functionality
        const chatBodyEl = this.contentEl.getElementsByClassName("khoj-chat-body")[0];

        // Create a container for the retry badge to ensure proper separation
        const retryContainer = chatBodyEl.createDiv({ cls: "khoj-retry-container" });

        // Create the retry badge inside the container
        const retryBadge = retryContainer.createDiv({ cls: "khoj-retry-badge" });

        // Add retry icon
        const retryIcon = retryBadge.createSpan({ cls: "retry-icon" });
        setIcon(retryIcon, "refresh-cw");

        // Add main text - keep it focused only on retry action
        retryBadge.createSpan({ text: "Try again to apply changes" });

        // Add retry count
        retryBadge.createSpan({
            cls: "retry-count",
            text: `Attempt ${this.editRetryCount}/3`
        });

        // Add error details as a tooltip
        retryBadge.setAttribute('aria-label', errorDetails);
        // @ts-ignore - Obsidian's custom tooltip API
        const hoverEditor = this.app.plugins.plugins["obsidian-hover-editor"];
        if (hoverEditor) {
            new hoverEditor.HoverPopover(this.app, retryBadge, errorDetails);
        }

        // Scroll to the badge
        retryBadge.scrollIntoView({ behavior: "smooth", block: "center" });

        // Create a retry prompt for the LLM
        const retryPrompt = `/general I noticed some issues with the edit block. Please fix the following and provide a corrected version (retry ${this.editRetryCount}/3):\n\n${errorDetails}\n\nPlease provide a new edit block that fixes these issues. Make sure to follow the exact format required.`;

        // Send retry request without displaying the user message
        await this.getChatResponse(retryPrompt, "", false, false);
    }

    private showModeDropdown(inputEl: HTMLTextAreaElement) {
        // Create dropdown if it doesn't exist
        if (!this.modeDropdown) {
            this.modeDropdown = this.contentEl.createDiv({
                cls: "khoj-mode-dropdown"
            });

            // Position the dropdown ABOVE the input (instead of below)
            const inputRect = inputEl.getBoundingClientRect();
            const containerRect = this.contentEl.getBoundingClientRect();

            this.modeDropdown.style.position = "absolute";
            this.modeDropdown.style.left = `${inputRect.left - containerRect.left}px`;
            this.modeDropdown.style.top = `${inputRect.top - containerRect.top - 4}px`; // Position above with small gap
            this.modeDropdown.style.width = `${inputRect.width}px`;
            this.modeDropdown.style.zIndex = "1000";
            this.modeDropdown.style.transform = "translateY(-100%)"; // Move up by 100% of its height

            // Add mode options to dropdown - we'll create all initially then show/hide based on filter
            this.chatModes.forEach((mode, index) => {
                const option = this.modeDropdown!.createDiv({
                    cls: "khoj-mode-dropdown-option",
                    attr: {
                        "data-index": index.toString(),
                        "data-command": mode.command,
                    }
                });

                // Create emoji span and label span for better styling control
                const emojiSpan = option.createSpan({
                    cls: "khoj-mode-dropdown-emoji"
                });
                setIcon(emojiSpan, mode.iconName);

                option.createSpan({
                    cls: "khoj-mode-dropdown-label",
                    text: ` ${mode.label} `
                });

                option.createSpan({
                    cls: "khoj-mode-dropdown-command",
                    text: `(${mode.command})`
                });

                // Select mode on click
                option.addEventListener("click", () => {
                    inputEl.value = mode.command + " ";
                    inputEl.focus();
                    this.currentUserInput = inputEl.value;
                    this.hideModeDropdown();
                });
            });

            // Close dropdown when clicking outside
            document.addEventListener("click", (e) => {
                if (this.modeDropdown && !this.modeDropdown.contains(e.target as Node) &&
                    e.target !== inputEl) {
                    this.hideModeDropdown();
                }
            });
        } else {
            // Show the dropdown if it already exists
            this.modeDropdown.style.display = "block";
        }

        // Filter options based on current input
        this.filterDropdownOptions(inputEl.value);
    }

    /**
     * Filters dropdown options based on user input
     * @param {string} inputValue - Current input value from textarea
     */
    private filterDropdownOptions(inputValue: string) {
        if (!this.modeDropdown) return;

        // Get all options
        const options = this.modeDropdown.querySelectorAll<HTMLElement>(".khoj-mode-dropdown-option");
        let visibleOptionsCount = 0;

        options.forEach((option) => {
            const command = option.getAttribute("data-command") || "";

            // If input starts with "/" and has additional characters, filter based on that
            if (inputValue.startsWith("/") && inputValue.length > 1) {
                // Check if command starts with the input value
                if (command.toLowerCase().startsWith(inputValue.toLowerCase())) {
                    option.style.display = "flex";
                    visibleOptionsCount++;
                } else {
                    option.style.display = "none";
                }
            } else {
                // Show all options if just "/" is typed
                option.style.display = "flex";
                visibleOptionsCount++;
            }
        });

        // Hide dropdown if no matches
        if (visibleOptionsCount === 0) {
            this.hideModeDropdown();
        }

        // Reset selection since we filtered the options
        this.selectedOptionIndex = -1;
    }

    private hideModeDropdown() {
        if (this.modeDropdown) {
            this.modeDropdown.style.display = "none";
            this.selectedOptionIndex = -1;
        }
    }
}
