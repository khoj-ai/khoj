import { FileSystemAdapter, Notice, Vault, Modal, TFile } from 'obsidian';
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

export async function updateContentIndex(vault: Vault, setting: KhojSetting, lastSyncedFiles: TFile[], regenerate: boolean = false): Promise<TFile[]> {
    // Get all markdown, pdf files in the vault
    console.log(`Khoj: Updating Khoj content index...`)
    const files = vault.getFiles().filter(file => file.extension === 'md' || file.extension === 'markdown' || file.extension === 'pdf');
    const binaryFileTypes = ['pdf']
    let countOfFilesToIndex = 0;
    let countOfFilesToDelete = 0;

    // Add all files to index as multipart form data
    const fileData = [];
    for (const file of files) {
        countOfFilesToIndex++;
        const encoding = binaryFileTypes.includes(file.extension) ? "binary" : "utf8";
        const mimeType = fileExtensionToMimeType(file.extension) + (encoding === "utf8" ? "; charset=UTF-8" : "");
        const fileContent = encoding == 'binary' ? await vault.readBinary(file) : await vault.read(file);
        fileData.push({blob: new Blob([fileContent], { type: mimeType }), path: file.path});
    }

    // Add any previously synced files to be deleted to multipart form data
    for (const lastSyncedFile of lastSyncedFiles) {
        if (!files.includes(lastSyncedFile)) {
            countOfFilesToDelete++;
            fileData.push({blob: new Blob([]), path: lastSyncedFile.path});
        }
    }

    // Iterate through all indexable files in vault, 1000 at a time
    let batchResponseSuccess = true;
    let batchResponseThrottled = false;
    for (let i = 0; i < fileData.length && !batchResponseThrottled; i += 1000) {
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
            batchResponseSuccess = false;
            batchResponseThrottled = response.status === 429;
        }
    }

    if (batchResponseThrottled) {
        new Notice(`❗️Failed to update Khoj content index. Requests were throttled. Upgrade your subscription or try again later.`);
    } else if (!batchResponseSuccess) {
        new Notice(`❗️Failed to update Khoj content index. Ensure Khoj server connected or raise issue on Khoj Discord/Github\nError: ${response.statusText}`);
    } else {
        console.log(`✅ Refreshed Khoj content index. Updated: ${countOfFilesToIndex} files, Deleted: ${countOfFilesToDelete} files.`);
    }

    return files;
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
