import { App, TFile } from 'obsidian';
import * as JSON5 from 'json5';

/**
 * Interface representing a block of edit instructions for modifying files
 */
export interface EditBlock {
    note: string;      // Required: Brief one-line explanation
    before: string;    // location.start
    after: string;     // location.end
    replacement: string; // content
    file: string;      // Required: Target file name (without extension)
    hasError?: boolean; // Optional: Flag to indicate parsing error
    error?: {
        type: 'missing_field' | 'invalid_json' | 'preprocessing' | 'unknown';
        message: string;
        details?: string;
    };
}

/**
 * Interface representing the result of parsing a Khoj edit block
 */
export interface ParseKhojEditResult {
    editData: any;
    cleanContent: string;
    error?: {
        type: 'missing_field' | 'invalid_json' | 'preprocessing' | 'unknown';
        message: string;
        details?: string;
    };
}

/**
 * Class that handles file operations for the Khoj plugin
 */
export class FileInteractions {
    private app: App;

    /**
     * Constructor for FileInteractions
     *
     * @param app - The Obsidian App instance
     */
    constructor(app: App) {
        this.app = app;
    }

    /**
     * Gets the content of all open files
     *
     * @param fileAccessMode - The access mode ('none', 'read', or 'write')
     * @returns A string containing the content of all open files
     */
    public async getOpenFilesContent(fileAccessMode: 'none' | 'read' | 'write'): Promise<string> {
        // Only proceed if we have read or write access
        if (fileAccessMode === 'none') return '';

        // Get all open markdown leaves
        const leaves = this.app.workspace.getLeavesOfType('markdown');
        if (leaves.length === 0) return '';

        let openFilesContent = "\n\n[SYSTEM]The user is currently working on the following files (content provided for context):\n\n";

        // Simplification des instructions d'édition
        if (fileAccessMode === 'write') {
            openFilesContent += `[EDIT INSTRUCTIONS] I can help you edit your notes using targeted modifications. I'll use multiple edit blocks to make precise changes rather than rewriting entire sections.

\`\`\`khoj-edit
{
    "note": "Brief one-line explanation of what this edit does",
    "location": {
        "start": "<start words>",
        "end": "<end words>"
    },
    "content": "<complete new content, including end marker if you want to keep it>",
    "file": "target-filename"  // Required: specify which open file to edit (without .md extension)
}
\`\`\`

⚠️ Important:
- The end marker text is included in the edited section and will be deleted. If you want to keep it, make sure to include it in your "content"
- The "file" parameter is required and must match an open file name (without .md extension)
- Use \" for quotes and \`\`\` for backticks in your content to ensure proper parsing

📝 Example note:

\`\`\`
---
date: 2024-01-20
tags: meeting, planning
status: active
---
# Meeting Notes

Action items from today:
- Review Q4 metrics
- Schedule follow-up with marketing team about new campaign launch
- Update project timeline and milestones for Q1 2024

Next steps:
- Send summary to team
- Book conference room for next week
\`\`\`

Examples of targeted edits:

1. Using just a few words to identify long text (notice how "campaign launch" is kept in content):
\`\`\`khoj-edit
{
    "note": "Add deadline and specificity to the marketing team follow-up",
    "location": {
        "start": "- Schedule follow-up",
        "end": "campaign launch"
    },
    "content": "- Schedule follow-up with marketing team by Wednesday to discuss Q1 campaign launch",
    "file": "Meeting Notes"
}
\`\`\`

2. Multiple targeted changes with escaped characters:
\`\`\`khoj-edit
{
    "note": "Add HIGH priority flag with code reference to Q4 metrics review",
    "location": {
        "start": "- Review Q4",
        "end": "metrics"
    },
    "content": "- [HIGH] Review Q4 metrics (see \"metrics.ts\" and \`calculateQ4Metrics()\`)",
    "file": "Meeting Notes"
}
\`\`\`
\`\`\`khoj-edit
{
    "note": "Add resource allocation to project timeline task",
    "location": {
        "start": "- Update project",
        "end": "Q1 2024"
    },
    "content": "- Update project timeline and add resource allocation for Q1 2024",
    "file": "Meeting Notes"
}
\`\`\`

3. Adding new content between sections:
\`\`\`khoj-edit
{
    "note": "Insert Discussion Points section between Action Items and Next Steps",
    "location": {
        "start": "Action items from today:",
        "end": "Next steps:"
    },
    "content": "Action items from today:\\n- Review Q4 metrics\\n- Schedule follow-up\\n- Update timeline\\n\\nDiscussion Points:\\n- Budget review\\n- Team feedback\\n\\nNext steps:",
    "file": "Meeting Notes"
}
\`\`\`

4. Completely replacing a file content (preserving frontmatter):
\`\`\`khoj-edit
{
    "note": "Replace entire file content while keeping frontmatter metadata",
    "location": {
        "start": "<file-start>",
        "end": "<file-end>"
    },
    "content": "# Project Overview\\n\\n## Goals\\n- Increase user engagement by 25%\\n- Launch mobile app by Q3\\n- Expand to 3 new markets\\n\\n## Timeline\\n1. Q1: Research & Planning\\n2. Q2: Development\\n3. Q3: Testing & Launch\\n4. Q4: Market Expansion",
    "file": "Meeting Notes"
}
\`\`\`

💡 Key points:
- note: Brief one-line explanation of what the edit does
- location.start: few words from start of target text (no ambiguity). Use <file-start> to target beginning of content after frontmatter
- location.end: few words from end of target text (no ambiguity). Use <file-end> to target file end
- content: complete new content, including end marker text if you want to keep it
- file: (required) name of the file to edit (without .md extension)
- Words must uniquely identify the location (no ambiguity)
- Changes apply to first matching location in the specified file
- Use <file-start> and <file-end> markers to replace entire file content while preserving frontmatter
- Frontmatter metadata (between --- markers at top of file) cannot be modified
- Remember to escape special characters: use \" for quotes and \`\`\` for backticks in content

[END OF EDIT INSTRUCTIONS]\n\n`;
        }

        openFilesContent += `[OPEN FILES CONTEXT]\n\n`;

        for (const leaf of leaves) {
            const view = leaf.view as any;
            const file = view?.file;
            if (!file || file.extension !== 'md') continue;

            // Add file title without brackets
            openFilesContent += `# ${file.basename}\n\`\`\`markdown\n`;

            // Read file content
            try {
                const content = await this.app.vault.read(file);
                openFilesContent += content;
            } catch (error) {
                console.error(`Error reading file ${file.path}:`, error);
            }

            openFilesContent += "\n```\n\n";
        }

        openFilesContent += "[END OF CURRENT FILES CONTEXT]\n\n";
        openFilesContent += "[END OF SYSTEM INSTRUCTIONS]\n\n";

        return openFilesContent;
    }

    /**
     * Calculates the Levenshtein distance between two strings
     *
     * @param a - First string
     * @param b - Second string
     * @returns The Levenshtein distance
     */
    public levenshteinDistance(a: string, b: string): number {
        if (a.length === 0) return b.length;
        if (b.length === 0) return a.length;

        const matrix = Array(b.length + 1).fill(null).map(() =>
            Array(a.length + 1).fill(null)
        );

        for (let i = 0; i <= a.length; i++) matrix[0][i] = i;
        for (let j = 0; j <= b.length; j++) matrix[j][0] = j;

        for (let j = 1; j <= b.length; j++) {
            for (let i = 1; i <= a.length; i++) {
                const cost = a[i - 1] === b[j - 1] ? 0 : 1;
                matrix[j][i] = Math.min(
                    matrix[j][i - 1] + 1, // deletion
                    matrix[j - 1][i] + 1, // insertion
                    matrix[j - 1][i - 1] + cost // substitution
                );
            }
        }

        return matrix[b.length][a.length];
    }

    /**
     * Finds the best matching file from a list of files based on the target name
     *
     * @param targetName - The name to match against
     * @param files - Array of TFile objects to search
     * @returns The best matching TFile or null if no matches found
     */
    public findBestMatchingFile(targetName: string, files: TFile[]): TFile | null {
        const MAX_DISTANCE = 10;
        let bestMatch: { file: TFile, distance: number } | null = null;

        for (const file of files) {
            // Try both with and without extension
            const distanceWithExt = this.levenshteinDistance(targetName.toLowerCase(), file.name.toLowerCase());
            const distanceWithoutExt = this.levenshteinDistance(targetName.toLowerCase(), file.basename.toLowerCase());
            const distance = Math.min(distanceWithExt, distanceWithoutExt);

            if (distance <= MAX_DISTANCE && (!bestMatch || distance < bestMatch.distance)) {
                bestMatch = { file, distance };
            }
        }

        return bestMatch?.file || null;
    }

    /**
     * Parses a khoj-edit block from the content string
     *
     * @param content - The content to parse containing the edit block
     * @returns Object with the parsed edit data and cleaned content
     */
    public parseKhojEditBlock(content: string): ParseKhojEditResult {
        let cleanContent = '';
        try {
            // Normalize line breaks and clean control characters, but preserve empty lines
            cleanContent = content
                .replace(/\r\n/g, '\n')
                .replace(/\r/g, '\n')
                .replace(/[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]/g, '')
                .trim();

            // Parse the JSON first to identify the content field
            let jsonContent = cleanContent;
            try {
                // Use a regex to find the content field and temporarily replace newlines
                jsonContent = cleanContent.replace(
                    /("content"\s*:\s*")((?:\\.|[^"\\])*?)(")/g,
                    (match, prefix, contentValue, suffix) => {
                        // Preserve actual newlines in content by escaping them differently
                        const preservedContent = contentValue.replace(/\n/g, '§§NEWLINE§§');
                        return prefix + preservedContent + suffix;
                    }
                );

                // Escape newlines in other fields
                jsonContent = jsonContent.replace(/"(?:\\.|[^"\\])*"/g, (match) => {
                    if (!match.includes('"content":')) {
                        return match.replace(/\n\s*\n/g, '\\n').replace(/\n/g, '\\n');
                    }
                    return match;
                });

                // Restore actual newlines in content field
                jsonContent = jsonContent.replace(/§§NEWLINE§§/g, '\\n');
            } catch (err) {
                console.error("Error preprocessing JSON:", err);
                jsonContent = cleanContent;
            }

            // Use JSON5 for tolerant parsing
            const editData = JSON5.parse(jsonContent);

            // Validate required fields
            if (!editData.note) {
                return {
                    editData,
                    cleanContent,
                    error: {
                        type: 'missing_field',
                        message: 'Missing "note" field in edit block',
                        details: 'The "note" field is required and must contain a brief explanation of the edit'
                    }
                };
            }

            if (!editData.file) {
                return {
                    editData,
                    cleanContent,
                    error: {
                        type: 'missing_field',
                        message: 'Missing "file" field in edit block',
                        details: 'The "file" field is required and should contain the target file name'
                    }
                };
            }

            if (!editData.location || (!editData.location.start && !editData.location.end)) {
                return {
                    editData,
                    cleanContent,
                    error: {
                        type: 'missing_field',
                        message: 'Missing location field or both start/end markers',
                        details: 'The "location" field must contain at least one of "start" or "end" to mark the section to edit'
                    }
                };
            }

            if (editData.content === undefined) {
                return {
                    editData,
                    cleanContent,
                    error: {
                        type: 'missing_field',
                        message: 'Missing "content" field in edit block',
                        details: 'The "content" field is required and contains the replacement text'
                    }
                };
            }

            return { editData, cleanContent };
        } catch (error) {
            console.error("Error parsing edit block:", error);
            return {
                editData: null,
                cleanContent,
                error: {
                    type: 'invalid_json',
                    message: 'Invalid JSON format in edit block',
                    details: error.message
                }
            };
        }
    }

    /**
     * Parses all edit blocks from a message
     *
     * @param message - The message containing khoj-edit blocks
     * @returns Array of EditBlock objects
     */
    public parseEditBlocks(message: string): EditBlock[] {
        const editBlocks: EditBlock[] = [];
        const editBlockRegex = /```khoj-edit\s*([\s\S]*?)```/g;
        let hasError = false;

        let match;
        while ((match = editBlockRegex.exec(message)) !== null) {
            const { editData, cleanContent, error } = this.parseKhojEditBlock(match[1]);

            if (error) {
                console.error("Failed to parse edit block:", error);
                console.debug("Content causing error:", match[1]);
                hasError = true;
                editBlocks.push({
                    note: "Error parsing edit block",
                    before: "",
                    after: "",
                    replacement: `Error: ${error.message}\nOriginal content:\n${match[1]}`,
                    file: "unknown", // Fallback value quand editData est null
                    hasError: true,
                    error: error // On passe aussi l'erreur pour le retry
                });
                continue;
            }

            if (!editData) {
                console.error("No edit data parsed");
                continue;
            }

            editBlocks.push({
                note: editData.note,
                before: editData.location.start,
                after: editData.location.end,
                replacement: editData.content,
                file: editData.file
            });
        }

        if (editBlocks.length > 0) {
            editBlocks[0] = { ...editBlocks[0], hasError };
        }

        return editBlocks;
    }

    /**
     * Creates a preview of changes with differences highlighted
     *
     * @param originalText - The original text
     * @param newText - The modified text
     * @returns A string with differences highlighted
     */
    public createPreviewWithDiff(originalText: string, newText: string): string {
        // Find common prefix and suffix
        let prefixLength = 0;
        const minLength = Math.min(originalText.length, newText.length);
        while (prefixLength < minLength && originalText[prefixLength] === newText[prefixLength]) {
            prefixLength++;
        }

        let suffixLength = 0;
        while (
            suffixLength < minLength - prefixLength &&
            originalText[originalText.length - 1 - suffixLength] === newText[newText.length - 1 - suffixLength]
        ) {
            suffixLength++;
        }

        // Extract the parts
        const commonPrefix = originalText.slice(0, prefixLength);
        const commonSuffix = originalText.slice(originalText.length - suffixLength);
        const originalDiff = originalText.slice(prefixLength, originalText.length - suffixLength);
        const newDiff = newText.slice(prefixLength, newText.length - suffixLength);

        // Format the differences
        const formatLines = (text: string, marker: string): string => {
            if (!text) return '';
            return text.split('\n')
                .map(line => {
                    line = line.trim();
                    if (!line) {
                        return marker === '==' ? '' : '~~';
                    }
                    return `${marker}${line}${marker}`;
                })
                .filter(line => line !== '~~')
                .join('\n');
        };

        return commonPrefix +
            (originalDiff ? formatLines(originalDiff, '~~') : '') +
            (newDiff ? formatLines(newDiff, '==') : '') +
            commonSuffix;
    }

    /**
     * Applies edit blocks to modify files
     *
     * @param editBlocks - Array of EditBlock objects to apply
     * @param addConfirmationButtons - Optional callback to add confirmation UI elements
     * @returns Object containing edit results and file backups
     */
    public async applyEditBlocks(
        editBlocks: EditBlock[],
        onRetryNeeded?: (blockToRetry: EditBlock) => void
    ): Promise<{
        editResults: { block: EditBlock, success: boolean, error?: string }[],
        fileBackups: Map<string, string>,
        blocksNeedingRetry: EditBlock[]
    }> {
        // Check for parsing errors first
        if (editBlocks.length === 0) {
            return { editResults: [], fileBackups: new Map(), blocksNeedingRetry: [] };
        }

        // Store original content for each file in case we need to cancel
        const fileBackups = new Map<string, string>();

        // Get all open markdown files
        const files = this.app.workspace.getLeavesOfType('markdown')
            .map(leaf => (leaf.view as any)?.file)
            .filter(file => file && file.extension === 'md');

        // Track success/failure for each edit
        const editResults: { block: EditBlock, success: boolean, error?: string }[] = [];
        const blocksNeedingRetry: EditBlock[] = [];

        // Process each edit block independently
        for (const block of editBlocks) {
            try {
                // Skip blocks with parsing errors but mark them for retry
                if (block.hasError) {
                    blocksNeedingRetry.push(block);
                    editResults.push({ block, success: false, error: block.error?.message || 'Parsing error' });
                    continue;
                }

                const targetFile = this.findBestMatchingFile(block.file, files);
                if (!targetFile) {
                    blocksNeedingRetry.push(block);
                    editResults.push({ block, success: false, error: `No matching file found for "${block.file}"` });
                    continue;
                }

                // Read the file content if not already backed up
                if (!fileBackups.has(targetFile.path)) {
                    const content = await this.app.vault.read(targetFile);
                    fileBackups.set(targetFile.path, content);
                }
                const content = fileBackups.get(targetFile.path)!;

                // Find frontmatter boundaries
                const frontmatterMatch = content.match(/^---\n[\s\S]*?\n---\n/);
                const frontmatterEndIndex = frontmatterMatch ? frontmatterMatch[0].length : 0;

                // Clean up the replacement content
                let replacement = block.replacement
                    .replace(/^[\s\n]*<file-start>[\s\n]*/, '')
                    .replace(/[\s\n]*<file-end>[\s\n]*$/, '')
                    .trim();

                // Remove frontmatter if block starts at beginning and has frontmatter
                if ((block.before === '' || block.before.includes('<file-start>')) &&
                    replacement.startsWith('---\n')) {
                    const frontmatterEnd = replacement.indexOf('\n---\n');
                    if (frontmatterEnd !== -1) {
                        replacement = replacement.substring(frontmatterEnd + 5).trim();
                    }
                }

                // Handle special markers for file start and end
                const before = block.before.replace('<file-start>', '');
                const after = block.after.replace('<file-end>', '');

                // Find the text to replace in original content
                let startIndex = -1;
                let endIndex = -1;

                if (block.before.includes('<file-start>')) {
                    startIndex = frontmatterEndIndex;
                } else {
                    startIndex = content.indexOf(before, frontmatterEndIndex);
                }

                if (block.after.includes('<file-end>')) {
                    endIndex = content.length;
                } else {
                    if (startIndex !== -1) {
                        endIndex = content.indexOf(after, startIndex);
                        if (endIndex !== -1) {
                            endIndex = endIndex + after.length;
                        }
                    }
                }

                if (startIndex === -1 || endIndex === -1) {
                    blocksNeedingRetry.push({
                        ...block,
                        hasError: true,
                        error: {
                            type: 'invalid_json',
                            message: 'Could not locate the text to edit',
                            details: `Could not find the specified text in file "${targetFile.basename}"`
                        }
                    });
                    editResults.push({
                        block,
                        success: false,
                        error: `Could not find the specified text in file "${targetFile.basename}"`
                    });
                    continue;
                }

                // Get the text to replace from original content
                const textToReplace = content.substring(startIndex, endIndex);
                const originalText = textToReplace;
                const newText = replacement;

                // Create preview with diff markers
                const preview = this.createPreviewWithDiff(originalText, newText);

                // Apply the edit
                const newContent =
                    content.substring(0, startIndex) +
                    preview +
                    content.substring(endIndex);

                // Save the changes
                await this.app.vault.modify(targetFile, newContent);
                editResults.push({ block, success: true });

            } catch (error) {
                console.error(`Error applying edit:`, error);
                blocksNeedingRetry.push(block);
                editResults.push({ block, success: false, error: error.message });
            }
        }

        // Trigger retry for failed blocks if requested via callback
        if (blocksNeedingRetry.length > 0 && onRetryNeeded) {
            // Take the first block that needs retry
            onRetryNeeded(blocksNeedingRetry[0]);
        }

        return { editResults, fileBackups, blocksNeedingRetry };
    }

    /**
     * Transforms khoj-edit blocks in a message to HTML for display
     *
     * @param message - The message containing khoj-edit blocks
     * @returns The transformed message with HTML for edit blocks
     */
    public transformKhojEditBlocks(message: string): string {
        // Get all open markdown files
        const files = this.app.workspace.getLeavesOfType('markdown')
            .map(leaf => (leaf.view as any)?.file)
            .filter(file => file && file.extension === 'md');

        return message.replace(/```khoj-edit\s*([\s\S]*?)```/g, (match, content) => {
            const { editData, cleanContent, error } = this.parseKhojEditBlock(content);

            // Escape content for HTML display
            const escapedContent = cleanContent
                .replace(/&/g, '&amp;')
                .replace(/</g, '&lt;')
                .replace(/>/g, '&gt;')
                .replace(/"/g, '&quot;')
                .replace(/'/g, '&#039;');

            if (error) {
                console.error("Error parsing khoj-edit block:", error);
                console.debug("Content causing error:", content);

                const errorTitle = `Error: ${error.message}`;
                const errorDetails = `Failed to parse edit block. Please check the JSON format and ensure all required fields are present.`;

                return `<details class="khoj-edit-accordion error">
                    <summary>${errorTitle}</summary>
                    <div class="khoj-edit-content">
                        <p class="khoj-edit-error-message">${errorDetails}</p>
                        <pre><code class="language-khoj-edit error">${escapedContent}</code></pre>
                    </div>
                </details>`;
            }

            // Find the actual file that will be modified
            const targetFile = this.findBestMatchingFile(editData.file, files);
            const displayFileName = targetFile ? targetFile.basename : editData.file;

            return `<details class="khoj-edit-accordion success">
                <summary>${editData.note} <span class="khoj-edit-file">(📄 ${displayFileName})</span></summary>
                <div class="khoj-edit-content">
                    <pre><code class="language-khoj-edit">${escapedContent}</code></pre>
                </div>
            </details>`;
        });
    }
}
