import { FileSystemAdapter, Notice, RequestUrlParam, request, Vault, Modal } from 'obsidian';
import { KhojSetting } from 'src/settings'
import * as fs from 'fs';

export function getVaultAbsolutePath(vault: Vault): string {
    let adaptor = vault.adapter;
    if (adaptor instanceof FileSystemAdapter) {
        return adaptor.getBasePath();
    }
    return '';
}

export async function configureKhojBackend(vault: Vault, setting: KhojSetting, notify: boolean = true) {
    let vaultPath = getVaultAbsolutePath(vault);
    let mdInVault = `${vaultPath}/**/*.md`;
    let pdfInVault = `${vaultPath}/**/*.pdf`;
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
    let indexName = vaultPath.replace(/\//g, '_').replace(/\\/g, '_').replace(/ /g, '_').replace(/:/g, '_');
    // Get default config fields from khoj backend
    let defaultConfig = await request(`${khojConfigUrl}/default`).then(response => JSON.parse(response));
    let khojDefaultMdIndexDirectory = getIndexDirectoryFromBackendConfig(defaultConfig["content-type"]["markdown"]["embeddings-file"]);
    let khojDefaultPdfIndexDirectory = getIndexDirectoryFromBackendConfig(defaultConfig["content-type"]["pdf"]["embeddings-file"]);
    let khojDefaultChatDirectory = getIndexDirectoryFromBackendConfig(defaultConfig["processor"]["conversation"]["conversation-logfile"]);
    let khojDefaultChatModelName = defaultConfig["processor"]["conversation"]["model"];

    // Get current config if khoj backend configured, else get default config from khoj backend
    await request(khoj_already_configured ? khojConfigUrl : `${khojConfigUrl}/default`)
        .then(response => JSON.parse(response))
        .then(data => {
            // If khoj backend not configured yet
            if (!khoj_already_configured) {
                // Create khoj content-type config with only markdown configured
                data["content-type"] = {
                    "markdown": {
                        "input-filter": [mdInVault],
                        "input-files": null,
                        "embeddings-file": `${khojDefaultMdIndexDirectory}/${indexName}.pt`,
                        "compressed-jsonl": `${khojDefaultMdIndexDirectory}/${indexName}.jsonl.gz`,
                    },
                    "pdf": {
                        "input-filter": [pdfInVault],
                        "input-files": null,
                        "embeddings-file": `${khojDefaultPdfIndexDirectory}/${indexName}.pt`,
                        "compressed-jsonl": `${khojDefaultPdfIndexDirectory}/${indexName}.jsonl.gz`,
                    }
                }
            }
            // Else if khoj config has no markdown content config
            else if (!data["content-type"]["markdown"]) {
                // Add markdown config to khoj content-type config
                // Set markdown config to index markdown files in configured obsidian vault
                data["content-type"]["markdown"] = {
                    "input-filter": [mdInVault],
                    "input-files": null,
                    "embeddings-file": `${khojDefaultMdIndexDirectory}/${indexName}.pt`,
                    "compressed-jsonl": `${khojDefaultMdIndexDirectory}/${indexName}.jsonl.gz`,
                }
            }
            // Else if khoj is not configured to index markdown files in configured obsidian vault
            else if (data["content-type"]["markdown"]["input-filter"] == null ||
                data["content-type"]["markdown"]["input-files"] != null ||
                data["content-type"]["markdown"]["input-filter"].length != 1 ||
                data["content-type"]["markdown"]["input-filter"][0] !== mdInVault) {
                // Update markdown config in khoj content-type config
                // Set markdown config to only index markdown files in configured obsidian vault
                let khojMdIndexDirectory = getIndexDirectoryFromBackendConfig(data["content-type"]["markdown"]["embeddings-file"]);
                data["content-type"]["markdown"] = {
                    "input-filter": [mdInVault],
                    "input-files": null,
                    "embeddings-file": `${khojMdIndexDirectory}/${indexName}.pt`,
                    "compressed-jsonl": `${khojMdIndexDirectory}/${indexName}.jsonl.gz`,
                }
            }

            if (khoj_already_configured && !data["content-type"]["pdf"]) {
                // Add pdf config to khoj content-type config
                // Set pdf config to index pdf files in configured obsidian vault

                let pdfs = fs.readdirSync(vaultPath).filter(file => file.endsWith(".pdf"));

                if (pdfs.length > 0) {
                    data["content-type"]["pdf"] = {
                        "input-filter": [pdfInVault],
                        "input-files": null,
                        "embeddings-file": `${khojDefaultPdfIndexDirectory}/${indexName}.pt`,
                        "compressed-jsonl": `${khojDefaultPdfIndexDirectory}/${indexName}.jsonl.gz`,
                    }
                } else {
                    data["content-type"]["pdf"] = null;
                }
            }
            // Else if khoj is not configured to index pdf files in configured obsidian vault
            else if (khoj_already_configured &&
                (data["content-type"]["pdf"]["input-filter"] == null ||
                data["content-type"]["pdf"]["input-filter"].length != 1 ||
                data["content-type"]["pdf"]["input-filter"][0] !== pdfInVault)) {

                let pdfs = fs.readdirSync(vaultPath).filter(file => file.endsWith(".pdf"));

                if (pdfs.length > 0) {
                    // Update pdf config in khoj content-type config
                    // Set pdf config to only index pdf files in configured obsidian vault
                    let khojPdfIndexDirectory = getIndexDirectoryFromBackendConfig(data["content-type"]["pdf"]["embeddings-file"]);
                    data["content-type"]["pdf"] = {
                        "input-filter": [pdfInVault],
                        "input-files": null,
                        "embeddings-file": `${khojPdfIndexDirectory}/${indexName}.pt`,
                        "compressed-jsonl": `${khojPdfIndexDirectory}/${indexName}.jsonl.gz`,
                    }
                } else {
                    data["content-type"]["pdf"] = null;
                }
            }

            // If OpenAI API key not set in Khoj plugin settings
            if (!setting.openaiApiKey) {
                // Disable khoj processors, as not required
                delete data["processor"];
            }
            // Else if khoj backend not configured yet
            else if (!khoj_already_configured || !data["processor"]) {
                data["processor"] = {
                    "conversation": {
                        "conversation-logfile": `${khojDefaultChatDirectory}/conversation.json`,
                        "model": khojDefaultChatModelName,
                        "openai-api-key": setting.openaiApiKey,
                    }
                }
            }
            // Else if khoj config has no conversation processor config
            else if (!data["processor"]["conversation"]) {
                data["processor"]["conversation"] = {
                    "conversation-logfile": `${khojDefaultChatDirectory}/conversation.json`,
                    "model": khojDefaultChatModelName,
                    "openai-api-key": setting.openaiApiKey,
                }
            }
            // Else if khoj is not configured with OpenAI API key from khoj plugin settings
            else if (data["processor"]["conversation"]["openai-api-key"] !== setting.openaiApiKey) {
                data["processor"]["conversation"] = {
                    "conversation-logfile": data["processor"]["conversation"]["conversation-logfile"],
                    "model": data["processor"]["conversation"]["model"],
                    "openai-api-key": setting.openaiApiKey,
                }
            }

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
