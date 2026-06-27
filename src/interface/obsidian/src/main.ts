import { Plugin, WorkspaceLeaf } from 'obsidian';
import { AlphaMindSetting, AlphaMindSettingTab, DEFAULT_SETTINGS } from 'src/settings'
import { AlphaMindSearchModal } from 'src/search_modal'
import { AlphaMindChatView } from 'src/chat_view'
import { AlphaMindSimilarView } from 'src/similar_view'
import { updateContentIndex, canConnectToBackend, AlphaMindView } from 'src/utils';
import { AlphaMindPaneView } from 'src/pane_view';


export default class AlphaMind extends Plugin {
    settings: AlphaMindSetting;
    indexingTimer: NodeJS.Timeout;

    async onload() {
        await this.loadSettings();

        // Add search command. It can be triggered from anywhere
        this.addCommand({
            id: 'search',
            name: 'Search',
            hotkeys: [{ modifiers: ["Ctrl", "Alt"], key: "S" }],
            callback: () => { new AlphaMindSearchModal(this.app, this.settings).open(); }
        });

        // Add similar notes command. It can only be triggered from the editor
        this.addCommand({
            id: 'similar',
            name: 'Find similar notes',
            hotkeys: [{ modifiers: ["Ctrl", "Alt"], key: "F" }],
            editorCallback: () => { this.activateView(AlphaMindView.SIMILAR); }
        });

        // Add chat command. It can be triggered from anywhere
        this.addCommand({
            id: 'chat',
            name: 'Chat',
            callback: () => { this.activateView(AlphaMindView.CHAT); }
        });

        // Add similar documents view command
        this.addCommand({
            id: 'similar-view',
            name: 'Open Similar Documents View',
            callback: () => { this.activateView(AlphaMindView.SIMILAR); }
        });

        // Add new chat command with hotkey
        this.addCommand({
            id: 'new-chat',
            name: 'New Chat',
            hotkeys: [{ modifiers: ["Ctrl", "Alt"], key: "N" }],
            callback: async () => {
                // First, activate the chat view
                await this.activateView(AlphaMindView.CHAT);

                // Wait a short moment for the view to activate
                setTimeout(() => {
                    // Try to get the active chat view
                    const chatView = this.app.workspace.getActiveViewOfType(AlphaMindChatView);
                    if (chatView) {
                        chatView.createNewConversation();
                    }
                }, 100);
            }
        });

        // Add conversation history command with hotkey
        this.addCommand({
            id: 'conversation-history',
            name: 'Show Conversation History',
            hotkeys: [{ modifiers: ["Ctrl", "Alt"], key: "O" }],
            callback: () => {
                this.activateView(AlphaMindView.CHAT).then(() => {
                    const chatView = this.app.workspace.getActiveViewOfType(AlphaMindChatView);
                    if (chatView) {
                        chatView.toggleChatSessions();
                    }
                });
            }
        });

        // Add voice capture command with hotkey
        this.addCommand({
            id: 'voice-capture',
            name: 'Start Voice Capture',
            hotkeys: [{ modifiers: ["Ctrl", "Alt"], key: "V" }],
            callback: () => {
                this.activateView(AlphaMindView.CHAT).then(() => {
                    const chatView = this.app.workspace.getActiveViewOfType(AlphaMindChatView);
                    if (chatView) {
                        // Toggle speech to text functionality
                        const toggleEvent = chatView.voiceChatActive ? 'keyup' : 'keydown';
                        chatView.speechToText(new KeyboardEvent(toggleEvent));
                    }
                });
            }
        });

        // Add sync command to manually sync new changes
        this.addCommand({
            id: 'sync',
            name: 'Sync new changes',
            callback: async () => {
                this.settings.lastSync = await updateContentIndex(
                    this.app.vault,
                    this.settings,
                    this.settings.lastSync,
                    false,
                    true
                );
            }
        });

        // Add edit confirmation commands
        this.addCommand({
            id: 'apply-edits',
            name: 'Apply pending edits',
            hotkeys: [{ modifiers: ["Ctrl", "Shift"], key: "Enter" }],
            callback: () => {
                const chatView = this.app.workspace.getActiveViewOfType(AlphaMindChatView);
                if (chatView) {
                    chatView.applyPendingEdits();
                }
            }
        });

        this.addCommand({
            id: 'cancel-edits',
            name: 'Cancel pending edits',
            hotkeys: [{ modifiers: ["Ctrl", "Shift"], key: "Backspace" }],
            callback: () => {
                const chatView = this.app.workspace.getActiveViewOfType(AlphaMindChatView);
                if (chatView) {
                    chatView.cancelPendingEdits();
                }
            }
        });

        // Register views
        this.registerView(AlphaMindView.CHAT, (leaf) => new AlphaMindChatView(leaf, this));
        this.registerView(AlphaMindView.SIMILAR, (leaf) => new AlphaMindSimilarView(leaf, this));

        // Create an icon in the left ribbon.
        this.addRibbonIcon('message-circle', 'AlphaMind', (_: MouseEvent) => {
            this.activateView(AlphaMindView.CHAT);
        });

        // Add a settings tab so the user can configure alphamind
        this.addSettingTab(new AlphaMindSettingTab(this.app, this));

        // Start the sync timer
        this.startSyncTimer();
    }

    // Method to start the sync timer
    private startSyncTimer() {
        // Clean up the old timer if it exists
        if (this.indexingTimer) {
            clearInterval(this.indexingTimer);
        }

        // Start a new timer with the configured interval
        this.indexingTimer = setInterval(async () => {
            if (this.settings.autoConfigure) {
                this.settings.lastSync = await updateContentIndex(
                    this.app.vault,
                    this.settings,
                    this.settings.lastSync
                );
            }
        }, this.settings.syncInterval * 60 * 1000); // Convert minutes to milliseconds
    }

    // Public method to restart the timer (called from settings)
    public restartSyncTimer() {
        this.startSyncTimer();
    }

    async loadSettings() {
        // Load alphamind obsidian plugin settings
        this.settings = Object.assign({}, DEFAULT_SETTINGS, await this.loadData());

        // Check if can connect to alphamind server
        ({ connectedToBackend: this.settings.connectedToBackend } =
            await canConnectToBackend(this.settings.alphamindUrl, this.settings.alphamindApiKey, true));
    }

    async saveSettings() {
        await this.saveData(this.settings);
    }

    async onunload() {
        // Remove scheduled job to update index at regular cadence
        if (this.indexingTimer)
            clearInterval(this.indexingTimer);

        this.unload();
    }

    async activateView(viewType: AlphaMindView, existingLeaf?: WorkspaceLeaf) {
        const { workspace } = this.app;
        let leafToUse: WorkspaceLeaf | null = null;

        // Check if an existingLeaf is provided and is suitable for a view type switch
        if (existingLeaf && existingLeaf.view &&
            (existingLeaf.view.getViewType() === AlphaMindView.CHAT || existingLeaf.view.getViewType() === AlphaMindView.SIMILAR) &&
            existingLeaf.view.getViewType() !== viewType) {
            // The existing leaf is a AlphaMind pane and we want to switch its type
            leafToUse = existingLeaf;
            await leafToUse.setViewState({ type: viewType, active: true });
        } else {
            // Standard logic: find an existing leaf of the target type, or create a new one
            const leaves = workspace.getLeavesOfType(viewType);
            if (leaves.length > 0) {
                leafToUse = leaves[0];
            } else {
                // If we are not switching an existing AlphaMind leaf,
                // and no leaf of the target type exists, create a new one.
                // Use the provided existingLeaf if it's not a AlphaMind pane we're trying to switch,
                // otherwise, get a new right leaf.
                leafToUse = (existingLeaf && !(existingLeaf.view instanceof AlphaMindPaneView)) ? existingLeaf : workspace.getRightLeaf(false);
                if (leafToUse) {
                    await leafToUse.setViewState({ type: viewType, active: true });
                } else {
                    console.error("AlphaMind: Could not get a leaf to activate view.");
                    return;
                }
            }
        }

        if (leafToUse) {
            workspace.revealLeaf(leafToUse); // Ensure the leaf is visible

            // Specific actions after revealing/switching
            if (viewType === AlphaMindView.CHAT) {
                // Ensure the view instance is correct after potential setViewState
                const chatView = leafToUse.view as AlphaMindChatView;
                if (chatView instanceof AlphaMindChatView) { // Double check instance type
                    // Use a more robust way to get the input, or ensure it's always present after onOpen
                    const chatInput = chatView.containerEl.querySelector<HTMLTextAreaElement>(".alphamind-chat-input");
                    chatInput?.focus();
                }
            }
        }
    }
}
