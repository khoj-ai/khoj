import { App, Modal, request, Setting } from 'obsidian';
import { KhojSetting } from 'src/settings';


export class KhojChatModal extends Modal {
    result: string;
    setting: KhojSetting;

    constructor(app: App, setting: KhojSetting) {
        super(app);
        this.setting = setting;
    }

    async onOpen() {
        let { contentEl } = this;

        // Add title to the Khoj Chat modal
        contentEl.createEl("h1", { text: "Khoj Chat" });

        // Create area for chat logs
        contentEl.createDiv({ attr: { class: "chat-body" } });

        // Get conversation history from Khoj backend
        let chatUrl = `${this.setting.khojUrl}/api/chat?`;
        let response = await request(chatUrl);
        let chatLogs = JSON.parse(response).response;
        chatLogs.forEach((chatLog: any) => {
            this.renderMessageWithReferences(chatLog.message, chatLog.by, chatLog.context, new Date(chatLog.created));
        });

        // Add chat input field
        new Setting(contentEl)
            .addText((text) => {
                text.onChange((value) => { this.result = value });
                text.setPlaceholder("What is the meaning of life?");
            })
            .addButton((btn) => btn
                .setButtonText("Send")
                .setCta()
                .onClick(async () => { await this.getChatResponse(this.result) }));
    }

    generateReference(messageEl: any, reference: string, index: number) {
        // Generate HTML for Chat Reference
        // `<sup><abbr title="${escaped_ref}" tabindex="0">${index}</abbr></sup>`;
        let escaped_ref = reference.replace(/"/g, "\\\"")
        return messageEl.createEl("sup").createEl("abbr", {
            attr: {
                title: escaped_ref,
                tabindex: "0",
            },
            text: `[${index}] `,
        });
    }

    renderMessageWithReferences(message: string, sender: string, context?: [string], dt?: Date) {
        let messageEl = this.renderMessage(message, sender, dt);
        if (context && !!messageEl) {
            context.map((reference, index) => this.generateReference(messageEl, reference, index));
        }
    }
    renderMessage(message: string, sender: string, dt?: Date): Element | null {
        let { contentEl } = this;

        // Append message to conversation history HTML element.
        // The chat logs should display above the message input box to follow standard UI semantics
        let chat_body_el = contentEl.getElementsByClassName("chat-body").item(0);
        if (!!chat_body_el) {
            let emojified_sender = sender == "khoj" ? "🦅 Khoj" : "🤔 You";
            chat_body_el.createDiv({ text: `${emojified_sender}: ${message}` })
        }

        return chat_body_el
    }

    async getChatResponse(query: string): Promise<void> {
        // Exit if query is empty
        if (!query || query === "") return;

        // Render user query as chat message
        this.renderMessage(query, "you");

        // Get chat response from Khoj backend
        let encodedQuery = encodeURIComponent(query);
        let chatUrl = `${this.setting.khojUrl}/api/chat?q=${encodedQuery}`;
        let response = await request(chatUrl);
        let data = JSON.parse(response);

        // Render Khoj response as chat message
        this.renderMessage(data.response, "khoj");
    }
}
