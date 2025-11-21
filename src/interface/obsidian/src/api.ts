export async function deleteContentByType(khojUrl: string, khojApiKey: string, contentType: string): Promise<void> {
    // Deletes all content of a given type on Khoj server for Obsidian client
    const response = await fetch(`${khojUrl}/api/content/type/${contentType}?client=obsidian`, {
        method: 'DELETE',
        headers: khojApiKey ? { 'Authorization': `Bearer ${khojApiKey}` } : {},
    });
    if (!response.ok) {
        const text = await response.text().catch(() => '');
        throw new Error(`Failed to delete content type ${contentType}: ${response.status} ${text}`);
    }
}

export async function uploadContentBatch(khojUrl: string, khojApiKey: string, method: 'PUT' | 'PATCH', files: { blob: Blob, path: string }[]): Promise<string> {
    // Uploads a batch of files to Khoj content endpoint
    const formData = new FormData();
    files.forEach(fileItem => { formData.append('files', fileItem.blob, fileItem.path); });

    const response = await fetch(`${khojUrl}/api/content?client=obsidian`, {
        method: method,
        headers: khojApiKey ? { 'Authorization': `Bearer ${khojApiKey}` } : {},
        body: formData,
    });

    if (!response.ok) {
        const text = await response.text().catch(() => '');
        throw new Error(`Failed to upload batch: ${response.status} ${text}`);
    }

    return await response.text();
}
