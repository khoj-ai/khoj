import { App, Notice, PluginSettingTab, request, Setting } from 'obsidian';
import Khoj from 'src/main';

export interface KhojSetting {
    resultsCount: number;
    khojUrl: string;
    connectedToBackend: boolean;
}

export const DEFAULT_SETTINGS: KhojSetting = {
    resultsCount: 6,
    khojUrl: 'http://localhost:8000',
    connectedToBackend: false,
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
            .setName('Results Count')
            .setDesc('The number of search results to show')
            .addSlider(slider => slider
                .setLimits(1, 10, 1)
                .setValue(this.plugin.settings.resultsCount)
                .setDynamicTooltip()
                .onChange(async (value) => {
                    this.plugin.settings.resultsCount = value;
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
                    button.setButtonText('Updating...');
                    button.removeCta();
                    indexVaultSetting = indexVaultSetting.setDisabled(true);

                    await request(`${this.plugin.settings.khojUrl}/api/update?t=markdown&force=true`);
                    new Notice('✅ Updated Khoj index.');

                    // Re-enable button once index is updated
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
