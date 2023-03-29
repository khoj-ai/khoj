import { Notice, Plugin } from 'obsidian';
import { KhojSetting, KhojSettingTab, DEFAULT_SETTINGS } from 'src/settings'
import { KhojSearchModal } from 'src/search_modal'
import { KhojChatModal } from 'src/chat_modal'
import { configureKhojBackend } from './utils';


export default class Khoj extends Plugin {
    settings: KhojSetting;

    async onload() {
        await this.loadSettings();

        // Add search command. It can be triggered from anywhere
        this.addCommand({
            id: 'search',
            name: 'Search',
            checkCallback: (checking) => {
                if (!checking && this.settings.connectedToBackend)
                    new KhojSearchModal(this.app, this.settings).open();
                return this.settings.connectedToBackend;
            }
        });

        // Add similar notes command. It can only be triggered from the editor
        this.addCommand({
            id: 'similar',
            name: 'Find similar notes',
            editorCheckCallback: (checking) => {
                if (!checking && this.settings.connectedToBackend)
                    new KhojSearchModal(this.app, this.settings, true).open();
                return this.settings.connectedToBackend;
            }
        });

        // Add chat command. It can be triggered from anywhere
        this.addCommand({
            id: 'chat',
            name: 'Chat',
            checkCallback: (checking) => {
                if (!checking && this.settings.connectedToBackend && !!this.settings.openaiApiKey)
                    new KhojChatModal(this.app, this.settings).open();
                return !!this.settings.openaiApiKey;
            }
        });

        // Create an icon in the left ribbon.
        this.addRibbonIcon('search', 'Khoj', (_: MouseEvent) => {
            // Called when the user clicks the icon.
            this.settings.connectedToBackend
                ? new KhojSearchModal(this.app, this.settings).open()
                : new Notice(`❗️Ensure Khoj backend is running and Khoj URL is pointing to it in the plugin settings`);
        });

        // Add a settings tab so the user can configure khoj
        this.addSettingTab(new KhojSettingTab(this.app, this));
    }

    async loadSettings() {
        // Load khoj obsidian plugin settings
        this.settings = Object.assign({}, DEFAULT_SETTINGS, await this.loadData());

        if (this.settings.autoConfigure) {
            // Load, configure khoj server settings
            await configureKhojBackend(this.app.vault, this.settings);
        }
    }

    async saveSettings() {
        if (this.settings.autoConfigure) {
            await configureKhojBackend(this.app.vault, this.settings, false);
        }
        this.saveData(this.settings);
    }
}
