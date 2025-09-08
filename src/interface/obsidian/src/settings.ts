import { App, Notice, PluginSettingTab, Setting, TFile, SuggestModal } from 'obsidian';
import Khoj from 'src/main';
import { canConnectToBackend, fetchChatModels, fetchUserServerSettings, getBackendStatusMessage, updateContentIndex, updateServerChatModel } from './utils';

export interface UserInfo {
    username?: string;
    photo?: string;
    is_active?: boolean;
    has_documents?: boolean;
    email?: string;
}

interface SyncFileTypes {
    markdown: boolean;
    images: boolean;
    pdf: boolean;
}

export interface ModelOption {
    id: string;
    name: string;
}

export interface ServerUserConfig {
    selected_chat_model_config?: number; // This is the ID from the server
    // Add other fields from UserConfig if needed by the plugin elsewhere
}

export interface KhojSetting {
    resultsCount: number;
    khojUrl: string;
    khojApiKey: string;
    connectedToBackend: boolean;
    autoConfigure: boolean;
    lastSync: Map<TFile, number>;
    syncFileType: SyncFileTypes;
    userInfo: UserInfo | null;
    syncFolders: string[];
    syncInterval: number;
    autoVoiceResponse: boolean;
    fileAccessMode: 'none' | 'read' | 'write';
    selectedChatModelId: string | null; // Mirrors server's selected_chat_model_config
    availableChatModels: ModelOption[];
}

export const DEFAULT_SETTINGS: KhojSetting = {
    resultsCount: 15,
    khojUrl: 'https://app.khoj.dev',
    khojApiKey: '',
    connectedToBackend: false,
    autoConfigure: true,
    lastSync: new Map(),
    syncFileType: {
        markdown: true,
        images: true,
        pdf: true,
    },
    userInfo: null,
    syncFolders: [],
    syncInterval: 60,
    autoVoiceResponse: true,
    fileAccessMode: 'read',
    selectedChatModelId: null, // Will be populated from server
    availableChatModels: [],
}

export class KhojSettingTab extends PluginSettingTab {
    plugin: Khoj;
    private chatModelSetting: Setting | null = null;

    constructor(app: App, plugin: Khoj) {
        super(app, plugin);
        this.plugin = plugin;
    }

    display(): void {
        const { containerEl } = this;
        containerEl.empty();
        this.chatModelSetting = null; // Reset when display is called

        // Add notice whether able to connect to khoj backend or not
        let backendStatusMessage = getBackendStatusMessage(
            this.plugin.settings.connectedToBackend,
            this.plugin.settings.userInfo?.email,
            this.plugin.settings.khojUrl,
            this.plugin.settings.khojApiKey
        );

        const connectHeaderEl = containerEl.createEl('h3', { title: backendStatusMessage });
        const connectHeaderContentEl = connectHeaderEl.createSpan({ cls: 'khoj-connect-settings-header' });
        const connectTitleEl = connectHeaderContentEl.createSpan({ text: 'Connect' });
        const backendStatusEl = connectTitleEl.createSpan({ text: this.connectStatusIcon(), cls: 'khoj-connect-settings-header-status' });
        if (this.plugin.settings.userInfo && this.plugin.settings.connectedToBackend) {
            if (this.plugin.settings.userInfo.photo) {
                const profilePicEl = connectHeaderContentEl.createEl('img', {
                    attr: { src: this.plugin.settings.userInfo.photo },
                    cls: 'khoj-profile'
                });
                profilePicEl.addEventListener('click', () => { new Notice(backendStatusMessage); });
            } else if (this.plugin.settings.userInfo.email) {
                const initial = this.plugin.settings.userInfo.email[0].toUpperCase();
                const profilePicEl = connectHeaderContentEl.createDiv({
                    text: initial,
                    cls: 'khoj-profile khoj-profile-initial'
                });
                profilePicEl.addEventListener('click', () => { new Notice(backendStatusMessage); });
            }
        }
        if (this.plugin.settings.userInfo && this.plugin.settings.userInfo.email) {
            connectHeaderEl.title = this.plugin.settings.userInfo?.email === 'default@example.com'
                ? "Signed in"
                : `Signed in as ${this.plugin.settings.userInfo.email}`;
        }

        // Add khoj settings configurable from the plugin settings tab
        const apiKeySetting = new Setting(containerEl)
            .setName('Khoj API Key')
            .addText(text => text
                .setValue(`${this.plugin.settings.khojApiKey}`)
                .onChange(async (value) => {
                    this.plugin.settings.khojApiKey = value.trim();
                    ({
                        connectedToBackend: this.plugin.settings.connectedToBackend,
                        userInfo: this.plugin.settings.userInfo,
                        statusMessage: backendStatusMessage,
                    } = await canConnectToBackend(this.plugin.settings.khojUrl, this.plugin.settings.khojApiKey));

                    if (!this.plugin.settings.connectedToBackend) {
                        this.plugin.settings.availableChatModels = [];
                        this.plugin.settings.selectedChatModelId = null;
                    }
                    await this.plugin.saveSettings();
                    backendStatusEl.setText(this.connectStatusIcon())
                    connectHeaderEl.title = backendStatusMessage;
                    await this.refreshModelsAndServerPreference();
                }));

        // Add API key setting description with link to get API key
        apiKeySetting.descEl.createEl('span', {
            text: 'Connect your Khoj Cloud account. ',
        });
        apiKeySetting.descEl.createEl('a', {
            text: 'Get your API Key',
            href: `${this.plugin.settings.khojUrl}/settings#clients`,
            attr: { target: '_blank' }
        });

        new Setting(containerEl)
            .setName('Khoj URL')
            .setDesc('The URL of the Khoj backend.')
            .addText(text => text
                .setValue(`${this.plugin.settings.khojUrl}`)
                .onChange(async (value) => {
                    this.plugin.settings.khojUrl = value.trim().replace(/\/$/, '');
                    ({
                        connectedToBackend: this.plugin.settings.connectedToBackend,
                        userInfo: this.plugin.settings.userInfo,
                        statusMessage: backendStatusMessage,
                    } = await canConnectToBackend(this.plugin.settings.khojUrl, this.plugin.settings.khojApiKey));

                    if (!this.plugin.settings.connectedToBackend) {
                        this.plugin.settings.availableChatModels = [];
                        this.plugin.settings.selectedChatModelId = null;
                    }
                    await this.plugin.saveSettings();
                    backendStatusEl.setText(this.connectStatusIcon())
                    connectHeaderEl.title = backendStatusMessage;
                    await this.refreshModelsAndServerPreference();
                }));

        // Interact section
        containerEl.createEl('h3', { text: 'Interact' });

        // Chat Model Dropdown
        this.renderChatModelDropdown();

        // Initial fetch of models and server preference if connected
        if (this.plugin.settings.connectedToBackend) {
            // Defer slightly to ensure UI is ready and avoid race conditions
            setTimeout(async () => {
                await this.refreshModelsAndServerPreference();
            }, 1000);
        }

        // Add new setting for auto voice response after voice input
        new Setting(containerEl)
            .setName('Auto Voice Response')
            .setDesc('Automatically read responses after voice messages')
            .addToggle(toggle => toggle
                .setValue(this.plugin.settings.autoVoiceResponse)
                .onChange(async (value) => {
                    this.plugin.settings.autoVoiceResponse = value;
                    await this.plugin.saveSettings();
                }));

        new Setting(containerEl)
            .setName('Results Count')
            .setDesc('The number of results to show in search and use for chat.')
            .addSlider(slider => slider
                .setLimits(1, 30, 1)
                .setValue(this.plugin.settings.resultsCount)
                .setDynamicTooltip()
                .onChange(async (value) => {
                    this.plugin.settings.resultsCount = value;
                    await this.plugin.saveSettings();
                }));

        // Add new "Sync" heading
        containerEl.createEl('h3', { text: 'Sync' });

        new Setting(containerEl)
            .setName('Auto Sync')
            .setDesc('Automatically index your vault with Khoj.')
            .addToggle(toggle => toggle
                .setValue(this.plugin.settings.autoConfigure)
                .onChange(async (value) => {
                    this.plugin.settings.autoConfigure = value;
                    await this.plugin.saveSettings();
                }));

        // Add setting to sync markdown notes
        new Setting(containerEl)
            .setName('Sync Notes')
            .setDesc('Index Markdown files in your vault with Khoj.')
            .addToggle(toggle => toggle
                .setValue(this.plugin.settings.syncFileType.markdown)
                .onChange(async (value) => {
                    this.plugin.settings.syncFileType.markdown = value;
                    await this.plugin.saveSettings();
                }));

        // Add setting to sync images
        new Setting(containerEl)
            .setName('Sync Images')
            .setDesc('Index images in your vault with Khoj.')
            .addToggle(toggle => toggle
                .setValue(this.plugin.settings.syncFileType.images)
                .onChange(async (value) => {
                    this.plugin.settings.syncFileType.images = value;
                    await this.plugin.saveSettings();
                }));

        // Add setting to sync PDFs
        new Setting(containerEl)
            .setName('Sync PDFs')
            .setDesc('Index PDF files in your vault with Khoj.')
            .addToggle(toggle => toggle
                .setValue(this.plugin.settings.syncFileType.pdf)
                .onChange(async (value) => {
                    this.plugin.settings.syncFileType.pdf = value;
                    await this.plugin.saveSettings();
                }));

        // Add setting for sync interval
        const syncIntervalValues = [1, 5, 10, 20, 30, 45, 60, 120, 1440];
        new Setting(containerEl)
            .setName('Sync Interval')
            .setDesc('Minutes between automatic synchronizations')
            .addDropdown(dropdown => dropdown
                .addOptions(Object.fromEntries(
                    syncIntervalValues.map(value => [
                        value.toString(),
                        value === 1 ? '1 minute' :
                            value === 1440 ? '24 hours' :
                                `${value} minutes`
                    ])
                ))
                .setValue(this.plugin.settings.syncInterval.toString())
                .onChange(async (value) => {
                    this.plugin.settings.syncInterval = parseInt(value);
                    await this.plugin.saveSettings();
                    // Restart the timer with the new interval
                    this.plugin.restartSyncTimer();
                }));

        // Add setting to manage sync folders
        const syncFoldersContainer = containerEl.createDiv('sync-folders-container');
        new Setting(syncFoldersContainer)
            .setName('Sync Folders')
            .setDesc('Specify folders to sync (leave empty to sync entire vault)')
            .addButton(button => button
                .setButtonText('Add Folder')
                .onClick(() => {
                    const modal = new FolderSuggestModal(this.app, (folder: string) => {
                        if (!this.plugin.settings.syncFolders.includes(folder)) {
                            this.plugin.settings.syncFolders.push(folder);
                            this.plugin.saveSettings();
                            this.updateFolderList(folderListEl);
                        }
                    });
                    modal.open();
                }));

        // Create a list to display selected folders
        const folderListEl = syncFoldersContainer.createDiv('folder-list');
        this.updateFolderList(folderListEl);

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

                    // Show indicator for indexing in progress (animated text)
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

                    // Obtain sync progress elements by id (created below)
                    const syncProgressEl = document.getElementById('khoj-sync-progress') as HTMLProgressElement | null;
                    const syncProgressText = document.getElementById('khoj-sync-progress-text') as HTMLElement | null;

                    if (syncProgressEl && syncProgressText) {
                        syncProgressEl.style.display = '';
                        syncProgressText.style.display = '';
                        syncProgressText.textContent = 'Syncing... 0 / ? files';
                        syncProgressEl.value = 0;
                        syncProgressEl.max = 1;
                    }

                    // Define progress callback
                    const onProgress = (progress: { processed: number, total: number }) => {
                        try {
                            const { processed, total } = progress;
                            const el = document.getElementById('khoj-sync-progress') as HTMLProgressElement | null;
                            const txt = document.getElementById('khoj-sync-progress-text') as HTMLElement | null;
                            if (!el || !txt) return;
                            el.max = Math.max(total, 1);
                            el.value = Math.min(processed, el.max);
                            txt.textContent = `Syncing... ${processed} / ${total} files`;
                        } catch (err) {
                            console.warn('Khoj: Error updating sync progress UI', err);
                        }
                    };

                    try {
                        this.plugin.settings.lastSync = await updateContentIndex(
                            this.app.vault, this.plugin.settings, this.plugin.settings.lastSync, true, true, onProgress
                        );
                    } finally {
                        // Cleanup: hide sync progress UI and refresh storage estimation
                        const el = document.getElementById('khoj-sync-progress') as HTMLProgressElement | null;
                        const txt = document.getElementById('khoj-sync-progress-text') as HTMLElement | null;
                        if (el) el.style.display = 'none';
                        if (txt) txt.style.display = 'none';
                        try {
                            (this as any).updateStorageDisplay();
                        } catch (err) {
                            console.warn('Khoj: Failed to refresh storage display after sync', err);
                        }

                        // Reset animated text and button state
                        window.clearInterval(progress_indicator);
                        button.setButtonText('Update');
                        button.setCta();
                        indexVaultSetting = indexVaultSetting.setDisabled(false);
                    }
                })
            );
        // Estimated Cloud Storage (client-side estimation)
        const storageSetting = new Setting(containerEl)
            .setName('Estimated Cloud Storage (estimation)')
            .setDesc('Estimated storage usage based on files configured for sync. This is a client-side estimation.')
            .then(() => { });

        // Create custom elements: progress and text for storage estimation
        const progressEl = document.createElement('progress');
        progressEl.value = 0;
        progressEl.max = 1;
        progressEl.style.width = '100%';
        const progressText = document.createElement('span');
        progressText.textContent = 'Calculating...';
        storageSetting.descEl.appendChild(progressEl);
        storageSetting.descEl.appendChild(progressText);

        // Create second progress bar for Force Sync operation (hidden by default)
        const syncProgressEl = document.createElement('progress');
        syncProgressEl.id = 'khoj-sync-progress';
        syncProgressEl.value = 0;
        syncProgressEl.max = 1;
        syncProgressEl.style.width = '100%';
        syncProgressEl.style.display = 'none';
        const syncProgressText = document.createElement('span');
        syncProgressText.id = 'khoj-sync-progress-text';
        syncProgressText.textContent = '';
        syncProgressText.style.display = 'none';
        storageSetting.descEl.appendChild(syncProgressEl);
        storageSetting.descEl.appendChild(syncProgressText);

        // Bind update method
        (this as any).updateStorageDisplay = async () => {
            // Show calculating state
            progressEl.removeAttribute('value');
            progressText.textContent = 'Calculating...';
            try {
                const { calculateVaultSyncMetrics } = await import('./utils');
                const metrics = await calculateVaultSyncMetrics(this.app.vault, this.plugin.settings);
                const usedMB = (metrics.usedBytes / (1024 * 1024));
                const totalMB = (metrics.totalBytes / (1024 * 1024));
                const usedStr = `${usedMB.toFixed(1)} Mo`;
                const totalStr = `${totalMB.toFixed(0)} Mo`;
                progressEl.value = metrics.usedBytes;
                progressEl.max = metrics.totalBytes;
                progressText.textContent = `${usedStr} / ${totalStr}`;
            } catch (err) {
                console.error('Khoj: Failed to update storage display', err);
                progressText.textContent = 'Estimation unavailable';
            }
        };

        // Call initial update
        (this as any).updateStorageDisplay();
    }

    private connectStatusIcon() {
        if (this.plugin.settings.connectedToBackend && this.plugin.settings.userInfo?.email)
            return '🟢';
        else if (this.plugin.settings.connectedToBackend)
            return '🟡'
        else
            return '🔴';
    }

    private async refreshModelsAndServerPreference() {
        let serverSelectedModelId: string | null = null;
        if (this.plugin.settings.connectedToBackend) {
            const [availableModels, serverConfig] = await Promise.all([
                fetchChatModels(this.plugin.settings),
                fetchUserServerSettings(this.plugin.settings)
            ]);

            this.plugin.settings.availableChatModels = availableModels;

            if (serverConfig && serverConfig.selected_chat_model_config !== undefined && serverConfig.selected_chat_model_config !== null) {
                const serverModelIdStr = serverConfig.selected_chat_model_config.toString();
                // Ensure the server's selected model is actually in the available list
                if (this.plugin.settings.availableChatModels.some(m => m.id === serverModelIdStr)) {
                    serverSelectedModelId = serverModelIdStr;
                } else {
                    // Server has a selection, but it's not in the options list (e.g. model removed, or different set of models)
                    // In this case, we might fall back to null (Khoj Default)
                    console.warn(`Khoj: Server's selected model ID ${serverModelIdStr} not in available models. Falling back to default.`);
                    serverSelectedModelId = null;
                }
            } else {
                // No specific model configured on the server, or it's explicitly null
                serverSelectedModelId = null;
            }
            this.plugin.settings.selectedChatModelId = serverSelectedModelId;

        } else {
            this.plugin.settings.availableChatModels = [];
            this.plugin.settings.selectedChatModelId = null; // Clear selection if disconnected
        }
        await this.plugin.saveSettings(); // Save the potentially updated selectedChatModelId
        this.renderChatModelDropdown(); // Re-render the dropdown with new data
    }

    private renderChatModelDropdown() {
        if (!this.chatModelSetting) {
            this.chatModelSetting = new Setting(this.containerEl)
                .setName('Chat Model');
        } else {
            // Clear previous description and controls to prepare for re-rendering
            this.chatModelSetting.descEl.empty();
            this.chatModelSetting.controlEl.empty();
        }
        // Use this.chatModelSetting directly for modifications
        const modelSetting = this.chatModelSetting;

        if (!this.plugin.settings.connectedToBackend) {
            modelSetting.setDesc('Connect to Khoj to load and set chat model options.');
            modelSetting.addText(text => text.setValue("Not connected").setDisabled(true));
            return;
        }

        if (this.plugin.settings.availableChatModels.length === 0 && this.plugin.settings.connectedToBackend) {
            modelSetting.setDesc('Fetching models or no models available. Check Khoj connection or try refreshing.');
            modelSetting.addButton(button => button
                .setButtonText('Refresh Models')
                .onClick(async () => {
                    button.setButtonText('Refreshing...').setDisabled(true);
                    await this.refreshModelsAndServerPreference();
                    // Re-rendering happens inside refreshModelsAndServerPreference
                }));
            return;
        }

        modelSetting.setDesc('The default AI model used for chat.');
        modelSetting.addDropdown(dropdown => {
            dropdown.addOption('', 'Default'); // Placeholder when cannot retrieve chat model options from server.
            this.plugin.settings.availableChatModels.forEach(model => {
                dropdown.addOption(model.id, model.name);
            });
            dropdown
                .setValue(this.plugin.settings.selectedChatModelId || '')
                .onChange(async (value) => {
                    // Attempt to update the server
                    const success = await updateServerChatModel(value, this.plugin.settings);
                    if (success) {
                        await this.plugin.saveSettings();
                    } else {
                        // Server update failed, revert dropdown to the current setting value
                        // to avoid UI mismatch.
                        dropdown.setValue(this.plugin.settings.selectedChatModelId || '');
                    }
                    // Potentially re-render or refresh if needed, though setValue should update UI.
                    // this.refreshModelsAndServerPreference(); // Could be called to ensure full sync, but might be too much
                });
        });
    }

    // Helper method to update the folder list display
    private updateFolderList(containerEl: HTMLElement) {
        containerEl.empty();
        if (this.plugin.settings.syncFolders.length === 0) {
            containerEl.createEl('div', {
                text: 'Syncing entire vault',
                cls: 'folder-list-empty'
            });
            return;
        }

        const list = containerEl.createEl('ul', { cls: 'folder-list' });
        this.plugin.settings.syncFolders.forEach(folder => {
            const item = list.createEl('li', { cls: 'folder-list-item' });
            item.createSpan({ text: folder });

            const removeButton = item.createEl('button', {
                cls: 'folder-list-remove',
                text: '×'
            });
            removeButton.addEventListener('click', async () => {
                this.plugin.settings.syncFolders = this.plugin.settings.syncFolders.filter(f => f !== folder);
                await this.plugin.saveSettings();
                this.updateFolderList(containerEl);
            });
        });
    }
}

// Modal with folder suggestions
class FolderSuggestModal extends SuggestModal<string> {
    constructor(app: App, private onChoose: (folder: string) => void) {
        super(app);
    }

    getSuggestions(query: string): string[] {
        const folders = this.getAllFolders();
        if (!query) return folders;

        return folders.filter(folder =>
            folder.toLowerCase().includes(query.toLowerCase())
        );
    }

    renderSuggestion(folder: string, el: HTMLElement) {
        el.createSpan({
            text: folder || '/',
            cls: 'folder-suggest-item'
        });
    }

    onChooseSuggestion(folder: string, _: MouseEvent | KeyboardEvent) {
        this.onChoose(folder);
    }

    private getAllFolders(): string[] {
        const folders = new Set<string>();
        folders.add(''); // Root folder

        // Get all files and extract folder paths
        this.app.vault.getAllLoadedFiles().forEach(file => {
            const folderPath = file.parent?.path;
            if (folderPath) {
                folders.add(folderPath);

                // Also add all parent folders
                let parent = folderPath;
                while (parent.includes('/')) {
                    parent = parent.substring(0, parent.lastIndexOf('/'));
                    folders.add(parent);
                }
            }
        });

        return Array.from(folders).sort();
    }
}
