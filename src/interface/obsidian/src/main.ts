import { Plugin } from 'obsidian';
import { KhojSetting, KhojSettingTab, DEFAULT_SETTINGS } from 'src/settings'
import { KhojModal } from 'src/modal'
import { configureKhojBackend } from './utils';


export default class Khoj extends Plugin {
    settings: KhojSetting;

    async onload() {
        await this.loadSettings();

        // Add a search command. It can be triggered from anywhere
        this.addCommand({
            id: 'search',
            name: 'Search',
            callback: () => {
                new KhojModal(this.app, this.settings).open();
            }
        });

        // Create an icon in the left ribbon.
        this.addRibbonIcon('search', 'Khoj', (_: MouseEvent) => {
            // Called when the user clicks the icon.
            new KhojModal(this.app, this.settings).open();
        });

        // Add a settings tab so the user can configure various aspects of the plugin
        this.addSettingTab(new KhojSettingTab(this.app, this));
    }

    onunload() {
    }

    async loadSettings() {
        // Load khoj obsidian plugin settings
        this.settings = Object.assign({}, DEFAULT_SETTINGS, await this.loadData());

        // Load, configure khoj server settings
        await configureKhojBackend(this.settings);
    }

    async saveSettings() {
        await this.saveData(this.settings)
            .then(() => configureKhojBackend(this.settings));
    }
}
