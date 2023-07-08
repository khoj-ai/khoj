import { App, Notice, PluginSettingTab, request, Setting } from 'obsidian';
import Khoj from 'src/main';

export interface KhojSetting {
    openaiApiKey: string;
    resultsCount: number;
    khojUrl: string;
    connectedToBackend: boolean;
    autoConfigure: boolean;
}

export const DEFAULT_SETTINGS: KhojSetting = {
    resultsCount: 6,
    khojUrl: 'http://127.0.0.1:8000',
    connectedToBackend: false,
    autoConfigure: true,
    openaiApiKey: '',
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
            .setDesc('The URL of the Khoj backend')
            .addText(text => text
                .setValue(`${this.plugin.settings.khojUrl}`)
                .onChange(async (value) => {
                    this.plugin.settings.khojUrl = value.trim();
                    await this.plugin.saveSettings();
                    containerEl.firstElementChild?.setText(this.getBackendStatusMessage());
                }));
        new Setting(containerEl)
            .setName('OpenAI API Key')
            .setDesc('Your OpenAI API Key for Khoj Chat')
            .addText(text => text
                .setValue(`${this.plugin.settings.openaiApiKey}`)
                .onChange(async (value) => {
                    this.plugin.settings.openaiApiKey = value.trim();
                    await this.plugin.saveSettings();
                }));
        new Setting(containerEl)
            .setName('Results Count')
            .setDesc('The number of results to show in search and use for chat')
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
            .setDesc('Automatically configure the Khoj backend')
            .addToggle(toggle => toggle
                .setValue(this.plugin.settings.autoConfigure)
                .onChange(async (value) => {
                    this.plugin.settings.autoConfigure = value;
                    await this.plugin.saveSettings();
                }));
        let indexVaultSetting = new Setting(containerEl);
        indexVaultSetting
            .setName('Index Vault')
            .setDesc('Manually force Khoj to re-index your Obsidian Vault')
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

                    await request(`${this.plugin.settings.khojUrl}/api/update?t=markdown&force=true&client=obsidian`);
                    await request(`${this.plugin.settings.khojUrl}/api/update?t=pdf&force=true&client=obsidian`);
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
