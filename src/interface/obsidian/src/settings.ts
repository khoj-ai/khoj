import { App, Notice, PluginSettingTab, Setting, TFile } from 'obsidian';
import Khoj from 'src/main';
import { canConnectToBackend, getBackendStatusMessage, updateContentIndex } from './utils';

export interface KhojSetting {
    resultsCount: number;
    khojUrl: string;
    khojApiKey: string;
    connectedToBackend: boolean;
    autoConfigure: boolean;
    lastSync: Map<TFile, number>;
    userEmail: string;
}

export const DEFAULT_SETTINGS: KhojSetting = {
    resultsCount: 6,
    khojUrl: 'https://app.khoj.dev',
    khojApiKey: '',
    connectedToBackend: false,
    autoConfigure: true,
    lastSync: new Map(),
    userEmail: '',
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
        let backendStatusEl = containerEl.createEl('small', {
            text: getBackendStatusMessage(
                this.plugin.settings.connectedToBackend,
                this.plugin.settings.userEmail,
                this.plugin.settings.khojUrl,
                this.plugin.settings.khojApiKey
            )}
        );
        let backendStatusMessage: string = '';

        // Add khoj settings configurable from the plugin settings tab
        new Setting(containerEl)
            .setName('Khoj URL')
            .setDesc('The URL of the Khoj backend.')
            .addText(text => text
                .setValue(`${this.plugin.settings.khojUrl}`)
                .onChange(async (value) => {
                    this.plugin.settings.khojUrl = value.trim().replace(/\/$/, '');
                    ({
                        connectedToBackend: this.plugin.settings.connectedToBackend,
                        userEmail: this.plugin.settings.userEmail,
                        statusMessage: backendStatusMessage,
                    } = await canConnectToBackend(this.plugin.settings.khojUrl, this.plugin.settings.khojApiKey));

                    await this.plugin.saveSettings();
                    backendStatusEl.setText(backendStatusMessage);
                }));
        new Setting(containerEl)
            .setName('Khoj API Key')
            .setDesc('Use Khoj Cloud with your Khoj API Key')
            .addText(text => text
                .setValue(`${this.plugin.settings.khojApiKey}`)
                .onChange(async (value) => {
                    this.plugin.settings.khojApiKey = value.trim();
                    ({
                        connectedToBackend: this.plugin.settings.connectedToBackend,
                        userEmail: this.plugin.settings.userEmail,
                        statusMessage: backendStatusMessage,
                    } = await canConnectToBackend(this.plugin.settings.khojUrl, this.plugin.settings.khojApiKey));
                    await this.plugin.saveSettings();
                    backendStatusEl.setText(backendStatusMessage);
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
            .setName('Auto Sync')
            .setDesc('Automatically index your vault with Khoj.')
            .addToggle(toggle => toggle
                .setValue(this.plugin.settings.autoConfigure)
                .onChange(async (value) => {
                    this.plugin.settings.autoConfigure = value;
                    await this.plugin.saveSettings();
                }));
        let indexVaultSetting = new Setting(containerEl);
        indexVaultSetting
            .setName('Force Sync')
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

                    this.plugin.settings.lastSync = await updateContentIndex(
                        this.app.vault, this.plugin.settings, this.plugin.settings.lastSync, true
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
}
