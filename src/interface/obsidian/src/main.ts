import { Plugin, WorkspaceLeaf } from 'obsidian';
import { KhojSetting, KhojSettingTab, DEFAULT_SETTINGS } from 'src/settings'
import { KhojSearchModal } from 'src/search_modal'
import { KhojChatView } from 'src/chat_view'
import { updateContentIndex, canConnectToBackend, KhojView, jumpToPreviousView } from './utils';
import { KhojPaneView } from './pane_view';


export default class Khoj extends Plugin {
    settings: KhojSetting;
    indexingTimer: NodeJS.Timeout;

    async onload() {
        await this.loadSettings();

        // Add search command. It can be triggered from anywhere
        this.addCommand({
            id: 'search',
            name: 'Search',
            callback: () => { new KhojSearchModal(this.app, this.settings).open(); }
        });

        // Add similar notes command. It can only be triggered from the editor
        this.addCommand({
            id: 'similar',
            name: 'Find similar notes',
            editorCallback: () => { new KhojSearchModal(this.app, this.settings, true).open(); }
        });

        // Add chat command. It can be triggered from anywhere
        this.addCommand({
            id: 'chat',
            name: 'Chat',
            callback: () => { this.activateView(KhojView.CHAT); }
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

        this.registerView(KhojView.CHAT, (leaf) => new KhojChatView(leaf, this.settings));

        // Create an icon in the left ribbon.
        this.addRibbonIcon('message-circle', 'Khoj', (_: MouseEvent) => {
            this.activateView(KhojView.CHAT);
        });

        // Add a settings tab so the user can configure khoj
        this.addSettingTab(new KhojSettingTab(this.app, this));

        // Démarrer le timer de synchronisation
        this.startSyncTimer();
    }

    // Méthode pour démarrer le timer de synchronisation
    private startSyncTimer() {
        // Nettoyer l'ancien timer s'il existe
        if (this.indexingTimer) {
            clearInterval(this.indexingTimer);
        }

        // Démarrer un nouveau timer avec l'intervalle configuré
        this.indexingTimer = setInterval(async () => {
            if (this.settings.autoConfigure) {
                this.settings.lastSync = await updateContentIndex(
                    this.app.vault,
                    this.settings,
                    this.settings.lastSync
                );
            }
        }, this.settings.syncInterval * 60 * 1000); // Convertir les minutes en millisecondes
    }

    // Méthode publique pour redémarrer le timer (appelée depuis les paramètres)
    public restartSyncTimer() {
        this.startSyncTimer();
    }

    async loadSettings() {
        // Load khoj obsidian plugin settings
        this.settings = Object.assign({}, DEFAULT_SETTINGS, await this.loadData());

        // Check if can connect to khoj server
        ({ connectedToBackend: this.settings.connectedToBackend } =
            await canConnectToBackend(this.settings.khojUrl, this.settings.khojApiKey, true));
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

    async activateView(viewType: KhojView) {
        const { workspace } = this.app;

        let leaf: WorkspaceLeaf | null = null;
        const leaves = workspace.getLeavesOfType(viewType);

        if (leaves.length > 0) {
            // A leaf with our view already exists, use that
            leaf = leaves[0];
        } else {
            // Our view could not be found in the workspace, create a new leaf
            // in the right sidebar for it
            leaf = workspace.getRightLeaf(false);
            await leaf?.setViewState({ type: viewType, active: true });
        }

        if (leaf) {
            const activeKhojLeaf = workspace.getActiveViewOfType(KhojPaneView)?.leaf;
            // Jump to the previous view if the current view is Khoj Side Pane
            if (activeKhojLeaf === leaf) jumpToPreviousView();
            // Else Reveal the leaf in case it is in a collapsed sidebar
            else {
                workspace.revealLeaf(leaf);

                if (viewType === KhojView.CHAT) {
                    // focus on the chat input when the chat view is opened
                    let chatView = leaf.view as KhojChatView;
                    let chatInput = <HTMLTextAreaElement>chatView.contentEl.getElementsByClassName("khoj-chat-input")[0];
                    if (chatInput) chatInput.focus();
                }
            }
        }
    }
}
