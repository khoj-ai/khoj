import { FileSystemAdapter, Notice, RequestUrlParam, request, Vault, Modal, TFile } from 'obsidian';
import { KhojSetting } from 'src/settings'

export function getVaultAbsolutePath(vault: Vault): string {
    let adaptor = vault.adapter;
    if (adaptor instanceof FileSystemAdapter) {
        return adaptor.getBasePath();
    }
    return '';
}

type OpenAIType = null | {
    "chat-model": string;
    "api-key": string;
};

interface ProcessorData {
    conversation: {
      "conversation-logfile": string;
      openai: OpenAIType;
      "enable-offline-chat": boolean;
    };
}

function fileExtensionToMimeType (extension: string): string {
    switch (extension) {
        case 'pdf':
            return 'application/pdf';
        case 'png':
            return 'image/png';
        case 'jpg':
        case 'jpeg':
            return 'image/jpeg';
        case 'md':
        case 'markdown':
            return 'text/markdown';
        case 'org':
            return 'text/org';
        default:
            return 'text/plain';
    }
}

export async function updateContentIndex(vault: Vault, setting: KhojSetting, lastSyncedFiles: TFile[], regenerate: boolean = false): Promise<TFile[]> {
    // Get all markdown, pdf files in the vault
    console.log(`Khoj: Updating Khoj content index...`)
    const files = vault.getFiles().filter(file => file.extension === 'md' || file.extension === 'pdf');
    const binaryFileTypes = ['pdf', 'png', 'jpg', 'jpeg']
    let countOfFilesToIndex = 0;
    let countOfFilesToDelete = 0;

    // Add all files to index as multipart form data
    const formData = new FormData();
    for (const file of files) {
        countOfFilesToIndex++;
        const encoding = binaryFileTypes.includes(file.extension) ? "binary" : "utf8";
        const mimeType = fileExtensionToMimeType(file.extension) + (encoding === "utf8" ? "; charset=UTF-8" : "");
        const fileContent = await vault.read(file);
        formData.append('files', new Blob([fileContent], { type: mimeType }), file.path);
    }

    // Add any previously synced files to be deleted to multipart form data
    for (const lastSyncedFile of lastSyncedFiles) {
        if (!files.includes(lastSyncedFile)) {
            countOfFilesToDelete++;
            formData.append('files', new Blob([]), lastSyncedFile.path);
        }
    }

    // Call Khoj backend to update index with all markdown, pdf files
    const response = await fetch(`${setting.khojUrl}/api/v1/index/update?regenerate=${regenerate}`, {
        method: 'POST',
        headers: {
            'x-api-key': 'secret',
        },
        body: formData,
    });

    if (!response.ok) {
        new Notice(`❗️Failed to update Khoj content index. Ensure Khoj server connected or raise issue on Khoj Discord/Github\nError: ${response.statusText}`);
    } else {
        console.log(`✅ Refreshed Khoj content index. Updated: ${countOfFilesToIndex} files, Deleted: ${countOfFilesToDelete} files.`);
    }

    return files;
}

export async function configureKhojBackend(vault: Vault, setting: KhojSetting, notify: boolean = true) {
    let khojConfigUrl = `${setting.khojUrl}/api/config/data`;

    // Check if khoj backend is configured, note if cannot connect to backend
    let khoj_already_configured = await request(khojConfigUrl)
        .then(response => {
            setting.connectedToBackend = true;
            return response !== "null"
        })
        .catch(error => {
            setting.connectedToBackend = false;
            if (notify)
                new Notice(`❗️Ensure Khoj backend is running and Khoj URL is pointing to it in the plugin settings.\n\n${error}`);
        })
    // Short-circuit configuring khoj if unable to connect to khoj backend
    if (!setting.connectedToBackend) return;

    // Set index name from the path of the current vault
    // Get default config fields from khoj backend
    let defaultConfig = await request(`${khojConfigUrl}/default`).then(response => JSON.parse(response));
    let khojDefaultChatDirectory = getIndexDirectoryFromBackendConfig(defaultConfig["processor"]["conversation"]["conversation-logfile"]);
    let khojDefaultChatModelName = defaultConfig["processor"]["conversation"]["openai"]["chat-model"];

    // Get current config if khoj backend configured, else get default config from khoj backend
    await request(khoj_already_configured ? khojConfigUrl : `${khojConfigUrl}/default`)
        .then(response => JSON.parse(response))
        .then(data => {
            let conversationLogFile = data?.["processor"]?.["conversation"]?.["conversation-logfile"] ?? `${khojDefaultChatDirectory}/conversation.json`;
            let processorData: ProcessorData = {
                "conversation": {
                    "conversation-logfile": conversationLogFile,
                    "openai": null,
                    "enable-offline-chat": setting.enableOfflineChat,
                }
            }

            // If the Open AI API Key was configured in the plugin settings
            if (!!setting.openaiApiKey) {
                let openAIChatModel = data?.["processor"]?.["conversation"]?.["openai"]?.["chat-model"] ?? khojDefaultChatModelName;
                processorData = {
                    "conversation": {
                        "conversation-logfile": conversationLogFile,
                        "openai": {
                            "chat-model": openAIChatModel,
                            "api-key": setting.openaiApiKey,
                        },
                        "enable-offline-chat": setting.enableOfflineChat,
                    },
                }
            }

            // Set khoj processor config to conversation processor config
            data["processor"] = processorData;

            // Save updated config and refresh index on khoj backend
            updateKhojBackend(setting.khojUrl, data);
            if (!khoj_already_configured)
                console.log(`Khoj: Created khoj backend config:\n${JSON.stringify(data)}`)
            else
                console.log(`Khoj: Updated khoj backend config:\n${JSON.stringify(data)}`)
        })
        .catch(error => {
            if (notify)
                new Notice(`❗️Failed to configure Khoj backend. Contact developer on Github.\n\nError: ${error}`);
        })
}

export async function updateKhojBackend(khojUrl: string, khojConfig: Object) {
    // POST khojConfig to khojConfigUrl
    let requestContent: RequestUrlParam = {
        url: `${khojUrl}/api/config/data`,
        body: JSON.stringify(khojConfig),
        method: 'POST',
        contentType: 'application/json',
    };

    // Save khojConfig on khoj backend at khojConfigUrl
    await request(requestContent)
        // Refresh khoj search index after updating config
        .then(_ => request(`${khojUrl}/api/update?t=markdown`))
        .then(_ => request(`${khojUrl}/api/update?t=pdf`));
}

function getIndexDirectoryFromBackendConfig(filepath: string) {
    return filepath.split("/").slice(0, -1).join("/");
}

export async function createNote(name: string, newLeaf = false): Promise<void> {
    try {
      let pathPrefix: string
      // @ts-ignore
      switch (app.vault.getConfig('newFileLocation')) {
        case 'current':
          pathPrefix = (app.workspace.getActiveFile()?.parent.path ?? '') + '/'
          break
        case 'folder':
          pathPrefix = this.app.vault.getConfig('newFileFolderPath') + '/'
          break
        default: // 'root'
          pathPrefix = ''
          break
      }
      await app.workspace.openLinkText(`${pathPrefix}${name}.md`, '', newLeaf)
    } catch (e) {
      console.error('Khoj: Could not create note.\n' + (e as any).message);
      throw e
    }
  }

export async function createNoteAndCloseModal(query: string, modal: Modal, opt?: { newLeaf: boolean }): Promise<void> {
    try {
        await createNote(query, opt?.newLeaf);
    }
    catch (e) {
        new Notice((e as Error).message)
        return
    }
    modal.close();
}
