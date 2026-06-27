export async function deleteContentByType(alphamindUrl: string, alphamindApiKey: string, contentType: string): Promise<void> {
    // Deletes all content of a given type on AlphaMind server for Obsidian client
    const response = await fetch(`${alphamindUrl}/api/content/type/${contentType}?client=obsidian`, {
        method: 'DELETE',
        headers: alphamindApiKey ? { 'Authorization': `Bearer ${alphamindApiKey}` } : {},
    });
    if (!response.ok) {
        const text = await response.text().catch(() => '');
        throw new Error(`Failed to delete content type ${contentType}: ${response.status} ${text}`);
    }
}

export async function uploadContentBatch(alphamindUrl: string, alphamindApiKey: string, files: { blob: Blob, path: string }[]): Promise<string> {
    // Uploads a batch of files to AlphaMind content endpoint
    const formData = new FormData();
    files.forEach(fileItem => { formData.append('files', fileItem.blob, fileItem.path); });

    const response = await fetch(`${alphamindUrl}/api/content?client=obsidian`, {
        method: 'PATCH',
        headers: alphamindApiKey ? { 'Authorization': `Bearer ${alphamindApiKey}` } : {},
        body: formData,
    });

    if (!response.ok) {
        const text = await response.text().catch(() => '');
        throw new Error(`Failed to upload batch: ${response.status} ${text}`);
    }

    return await response.text();
}
