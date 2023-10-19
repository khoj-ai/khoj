import { App, Notice, PluginSettingTab, Setting, TFile } from 'obsidian';
import Khoj from 'src/main';
import { updateContentIndex } from './utils';

export interface KhojSetting {
    enableOfflineChat: boolean;
    openaiApiKey: string;
    resultsCount: number;
    khojUrl: string;
    connectedToBackend: boolean;
    autoConfigure: boolean;
    lastSyncedFiles: TFile[];
}

export const DEFAULT_SETTINGS: KhojSetting = {
    enableOfflineChat: false,
    resultsCount: 6,
    khojUrl: 'http://127.0.0.1:42110',
    connectedToBackend: false,
    autoConfigure: true,
    openaiApiKey: '',
    lastSyncedFiles: []
}

export class KhojSettingTab extends PluginSettingTab {
    plugin: Khoj;

    constructor(app: App, plugin: Khoj) {
        super(app, plugin);
        this.plugin = plugin;
    }

    display(): void {
        const { containerEl } = this;
        containerEl.empty();

        // Add notice whether able to connect to khoj backend or not
        containerEl.createEl('small', { text: this.getBackendStatusMessage() });

        // Add khoj settings configurable from the plugin settings tab
        new Setting(containerEl)
            .setName('Khoj URL')
            .setDesc('The URL of the Khoj backend.')
            .addText(text => text
                .setValue(`${this.plugin.settings.khojUrl}`)
                .onChange(async (value) => {
                    this.plugin.settings.khojUrl = value.trim();
                    await this.plugin.saveSettings();
                    containerEl.firstElementChild?.setText(this.getBackendStatusMessage());
                }));
        new Setting(containerEl)
            .setName('OpenAI API Key')
            .setDesc('Use OpenAI for Khoj Chat with your API key.')
            .addText(text => text
                .setValue(`${this.plugin.settings.openaiApiKey}`)
                .onChange(async (value) => {
                    this.plugin.settings.openaiApiKey = value.trim();
                    await this.plugin.saveSettings();
                }));
        new Setting(containerEl)
            .setName('Enable Offline Chat')
            .setDesc('Chat privately without an internet connection. Enabling this will use offline chat even if OpenAI is configured.')
            .addToggle(toggle => toggle
                .setValue(this.plugin.settings.enableOfflineChat)
                .onChange(async (value) => {
                    this.plugin.settings.enableOfflineChat = value;
                    await this.plugin.saveSettings();
                }));
        new Setting(containerEl)
            .setName('Results Count')
            .setDesc('The number of results to show in search and use for chat.')
            .addSlider(slider => slider
                .setLimits(1, 10, 1)
                .setValue(this.plugin.settings.resultsCount)
                .setDynamicTooltip()
                .onChange(async (value) => {
                    this.plugin.settings.resultsCount = value;
                    await this.plugin.saveSettings();
                }));
        new Setting(containerEl)
            .setName('Auto Configure')
            .setDesc('Automatically configure the Khoj backend.')
            .addToggle(toggle => toggle
                .setValue(this.plugin.settings.autoConfigure)
                .onChange(async (value) => {
                    this.plugin.settings.autoConfigure = value;
                    await this.plugin.saveSettings();
                }));
        let indexVaultSetting = new Setting(containerEl);
        indexVaultSetting
            .setName('Index Vault')
            .setDesc('Manually force Khoj to re-index your Obsidian Vault.')
            .addButton(button => button
                .setButtonText('Update')
                .setCta()
                .onClick(async () => {
                    // Disable button while updating index
                    button.setButtonText('Updating 🌑');
                    button.removeCta();
                    indexVaultSetting = indexVaultSetting.setDisabled(true);

                    // Show indicator for indexing in progress
                    const progress_indicator = window.setInterval(() => {
                        if (button.buttonEl.innerText === 'Updating 🌑') {
                            button.setButtonText('Updating 🌘');
                        } else if (button.buttonEl.innerText === 'Updating 🌘') {
                            button.setButtonText('Updating 🌗');
                        } else if (button.buttonEl.innerText === 'Updating 🌗') {
                            button.setButtonText('Updating 🌖');
                        } else if (button.buttonEl.innerText === 'Updating 🌖') {
                            button.setButtonText('Updating 🌕');
                        } else if (button.buttonEl.innerText === 'Updating 🌕') {
                            button.setButtonText('Updating 🌔');
                        } else if (button.buttonEl.innerText === 'Updating 🌔') {
                            button.setButtonText('Updating 🌓');
                        } else if (button.buttonEl.innerText === 'Updating 🌓') {
                            button.setButtonText('Updating 🌒');
                        } else if (button.buttonEl.innerText === 'Updating 🌒') {
                            button.setButtonText('Updating 🌑');
                        }
                    }, 300);
                    this.plugin.registerInterval(progress_indicator);

                    this.plugin.settings.lastSyncedFiles = await updateContentIndex(
                        this.app.vault, this.plugin.settings, this.plugin.settings.lastSyncedFiles, true
                    );
                    new Notice('✅ Updated Khoj index.');

                    // Reset button once index is updated
                    window.clearInterval(progress_indicator);
                    button.setButtonText('Update');
                    button.setCta();
                    indexVaultSetting = indexVaultSetting.setDisabled(false);
                })
            );
    }

    getBackendStatusMessage() {
        return !this.plugin.settings.connectedToBackend
            ? '❗Disconnected from Khoj backend. Ensure Khoj backend is running and Khoj URL is correctly set below.'
            : '✅ Connected to Khoj backend.';
    }
}
