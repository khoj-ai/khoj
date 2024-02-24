import { FileSystemAdapter, Notice, Vault, Modal, TFile, request } from 'obsidian';
import { KhojSetting } from 'src/settings'

export function getVaultAbsolutePath(vault: Vault): string {
    let adaptor = vault.adapter;
    if (adaptor instanceof FileSystemAdapter) {
        return adaptor.getBasePath();
    }
    return '';
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

function filenameToMimeType (filename: TFile): string {
    switch (filename.extension) {
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
            console.warn(`Unknown file type: ${filename.extension}. Defaulting to text/plain.`);
            return 'text/plain';
    }
}

export async function updateContentIndex(vault: Vault, setting: KhojSetting, lastSync: Map<TFile, number>, regenerate: boolean = false): Promise<Map<TFile, number>> {
    // Get all markdown, pdf files in the vault
    console.log(`Khoj: Updating Khoj content index...`)
    const files = vault.getFiles().filter(file => file.extension === 'md' || file.extension === 'markdown' || file.extension === 'pdf');
    const binaryFileTypes = ['pdf']
    let countOfFilesToIndex = 0;
    let countOfFilesToDelete = 0;
    lastSync = lastSync.size > 0 ? lastSync : new Map<TFile, number>();

    // Add all files to index as multipart form data
    const fileData = [];
    for (const file of files) {
        // Only push files that have been modified since last sync if not regenerating
        if (!regenerate && file.stat.mtime < (lastSync.get(file) ?? 0)){
            continue;
        }

        countOfFilesToIndex++;
        const encoding = binaryFileTypes.includes(file.extension) ? "binary" : "utf8";
        const mimeType = fileExtensionToMimeType(file.extension) + (encoding === "utf8" ? "; charset=UTF-8" : "");
        const fileContent = encoding == 'binary' ? await vault.readBinary(file) : await vault.read(file);
        fileData.push({blob: new Blob([fileContent], { type: mimeType }), path: file.path});
    }

    // Add any previously synced files to be deleted to multipart form data
    let filesToDelete: TFile[] = [];
    for (const lastSyncedFile of lastSync.keys()) {
        if (!files.includes(lastSyncedFile)) {
            countOfFilesToDelete++;
            let fileObj = new Blob([""], { type: filenameToMimeType(lastSyncedFile) });
            fileData.push({blob: fileObj, path: lastSyncedFile.path});
            filesToDelete.push(lastSyncedFile);
        }
    }

    // Iterate through all indexable files in vault, 1000 at a time
    let responses: string[]  = [];
    let error_message = null;
    for (let i = 0; i < fileData.length; i += 1000) {
        const filesGroup = fileData.slice(i, i + 1000);
        const formData = new FormData();
        filesGroup.forEach(fileItem => { formData.append('files', fileItem.blob, fileItem.path) });
        // Call Khoj backend to update index with all markdown, pdf files
        const response = await fetch(`${setting.khojUrl}/api/v1/index/update?force=${regenerate}&client=obsidian`, {
            method: 'POST',
            headers: {
                'Authorization': `Bearer ${setting.khojApiKey}`,
            },
            body: formData,
        });

        if (!response.ok) {
            if (response.status === 429) {
                error_message = `❗️Failed to sync your content with Khoj server. Requests were throttled. Upgrade your subscription or try again later.`;
                break;
            } else if (response.status === 404) {
                error_message = `❗️Could not connect to Khoj server. Ensure you can connect to it.`;
                break;
            } else {
                error_message = `❗️Failed to sync your content with Khoj server. Raise issue on Khoj Discord or Github\nError: ${response.statusText}`;
            }
        } else {
            responses.push(await response.text());
        }
    }

    // Update last sync time for each successfully indexed file
   files
    .filter(file => responses.find(response => response.includes(file.path)))
    .reduce((newSync, file) => {
        newSync.set(file, new Date().getTime());
        return newSync;
    }, lastSync);

    // Remove files that were deleted from last sync
    filesToDelete
    .filter(file => responses.find(response => response.includes(file.path)))
    .forEach(file => lastSync.delete(file));

    if (error_message) {
        new Notice(error_message);
    } else {
        console.log(`✅ Refreshed Khoj content index. Updated: ${countOfFilesToIndex} files, Deleted: ${countOfFilesToDelete} files.`);
    }

    return lastSync;
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

export async function canConnectToBackend(
    khojUrl: string,
    khojApiKey: string,
    showNotice: boolean = false
): Promise<{ connectedToBackend: boolean; statusMessage: string, userEmail: string }> {
    let connectedToBackend = false;
    let userEmail: string = '';

    if (!!khojUrl) {
        let headers  = !!khojApiKey ? { "Authorization": `Bearer ${khojApiKey}` } : undefined;
        await request({ url: `${khojUrl}/api/health`, method: "GET", headers: headers })
        .then(response => {
            connectedToBackend = true;
            userEmail = JSON.parse(response)?.email;
        })
        .catch(error => {
            connectedToBackend = false;
            console.log(`Khoj connection error:\n\n${error}`);
        });
    }

    let statusMessage: string = getBackendStatusMessage(connectedToBackend, userEmail, khojUrl, khojApiKey);
    if (showNotice) new Notice(statusMessage);
    return { connectedToBackend, statusMessage, userEmail };
}

export function getBackendStatusMessage(
    connectedToServer: boolean,
    userEmail: string,
    khojUrl: string,
    khojApiKey: string
): string {
    // Welcome message with default settings. Khoj cloud always expects an API key.
    if (!khojApiKey && khojUrl === 'https://app.khoj.dev')
        return `🌈 Welcome to Khoj! Get your API key from ${khojUrl}/config#clients and set it in the Khoj plugin settings on Obsidian`;

    if (!connectedToServer)
        return `❗️Could not connect to Khoj at ${khojUrl}. Ensure your can access it`;
    else if (!userEmail)
        return `✅ Connected to Khoj. ❗️Get a valid API key from ${khojUrl}/config#clients to log in`;
    else if (userEmail === 'default@example.com')
        // Logged in as default user in anonymous mode
        return `✅ Signed in to Khoj`;
    else
        return `✅ Signed in to Khoj as ${userEmail}`;
}
