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
    inProgress?: boolean;
    error?: {
        type: 'missing_field' | 'invalid_json' | 'preprocessing' | 'unknown';
        message: string;
        details?: string;
    };
}

/**
 * Interface representing the result of detecting a partial edit block
 */
interface PartialEditBlockResult {
    content: string;
    isComplete: boolean;
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

<khoj-edit>
{
    "note": "Brief one-line explanation of what this edit does",
    "location": {
        "start": "<start words>",
        "end": "<end words>"
    },
    "content": "<complete new content, including end marker if you want to keep it>",
    "file": "target-filename"  // Required: specify which open file to edit (without .md extension)
}
</khoj-edit>

⚠️ Important:
- The end marker text is included in the edited section and will be deleted. If you want to keep it, make sure to include it in your "content"
- The "file" parameter is required and must match an open file name (without .md extension)
- Use \" for quotes in your content to ensure proper parsing
- The XML format <khoj-edit>...</khoj-edit> ensures more reliable parsing compared to code blocks

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
<khoj-edit>
{
    "note": "Add deadline and specificity to the marketing team follow-up",
    "location": {
        "start": "- Schedule follow-up",
        "end": "campaign launch"
    },
    "content": "- Schedule follow-up with marketing team by Wednesday to discuss Q1 campaign launch",
    "file": "Meeting Notes"
}
</khoj-edit>

2. Multiple targeted changes with escaped characters:
<khoj-edit>
{
    "note": "Add HIGH priority flag with code reference to Q4 metrics review",
    "location": {
        "start": "- Review Q4",
        "end": "metrics"
    },
    "content": "- [HIGH] Review Q4 metrics (see \"metrics.ts\" and \`calculateQ4Metrics()\`)",
    "file": "Meeting Notes"
}
</khoj-edit>
<khoj-edit>
{
    "note": "Add resource allocation to project timeline task",
    "location": {
        "start": "- Update project",
        "end": "Q1 2024"
    },
    "content": "- Update project timeline and add resource allocation for Q1 2024",
    "file": "Meeting Notes"
}
</khoj-edit>

3. Adding new content between sections:
<khoj-edit>
{
    "note": "Insert Discussion Points section between Action Items and Next Steps",
    "location": {
        "start": "Action items from today:",
        "end": "Next steps:"
    },
    "content": "Action items from today:\\n- Review Q4 metrics\\n- Schedule follow-up\\n- Update timeline\\n\\nDiscussion Points:\\n- Budget review\\n- Team feedback\\n\\nNext steps:",
    "file": "Meeting Notes"
}
</khoj-edit>

4. Completely replacing a file content (preserving frontmatter):
<khoj-edit>
{
    "note": "Replace entire file content while keeping frontmatter metadata",
    "location": {
        "start": "<file-start>",
        "end": "<file-end>"
    },
    "content": "# Project Overview\\n\\n## Goals\\n- Increase user engagement by 25%\\n- Launch mobile app by Q3\\n- Expand to 3 new markets\\n\\n## Timeline\\n1. Q1: Research & Planning\\n2. Q2: Development\\n3. Q3: Testing & Launch\\n4. Q4: Market Expansion",
    "file": "Meeting Notes"
}
</khoj-edit>

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
- Remember to escape special characters: use \" for quotes in content
- Always use the XML format <khoj-edit>...</khoj-edit> instead of code blocks

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
     * Enhanced to handle incomplete blocks and extract partial information
     *
     * @param content - The content to parse containing the edit block
     * @param isComplete - Whether the block is complete (has closing tag)
     * @returns Object with the parsed edit data and cleaned content
     */
    public parseKhojEditBlock(content: string, isComplete: boolean = true): ParseKhojEditResult {
        let cleanContent = '';
        try {
            // Normalize line breaks and clean control characters, but preserve empty lines
            cleanContent = content
                .replace(/\r\n/g, '\n')
                .replace(/\r/g, '\n')
                .replace(/[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]/g, '')
                .trim();

            // For incomplete blocks, try to extract partial information
            if (!isComplete) {
                // Initialize with basic structure
                const partialData: any = {
                    inProgress: true
                };

                // Try to extract note field
                const noteMatch = cleanContent.match(/"note"\s*:\s*"([^"]+)"/);
                if (noteMatch) {
                    partialData.note = noteMatch[1];
                }

                // Try to extract file field
                const fileMatch = cleanContent.match(/"file"\s*:\s*"([^"]+)"/);
                if (fileMatch) {
                    partialData.file = fileMatch[1];
                }

                return {
                    editData: partialData,
                    cleanContent,
                    inProgress: true
                };
            }

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
     * @param message - The message containing khoj-edit blocks in XML format
     * @returns Array of EditBlock objects
     */
    public parseEditBlocks(message: string): EditBlock[] {
        const editBlocks: EditBlock[] = [];
        // Updated regex to match XML format <khoj-edit>...</khoj-edit>
        const editBlockRegex = /<khoj-edit>([\s\S]*?)<\/khoj-edit>/g;
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
     * Creates a preview with differences highlighted
     *
     * @param originalText - The original text
     * @param newText - The modified text
     * @returns A string with differences highlighted
     */
    public createPreviewWithDiff(originalText: string, newText: string): string {
        // Define unique tokens to temporarily replace existing formatting markers
        const HIGHLIGHT_TOKEN = "___KHOJ_HIGHLIGHT_MARKER___";
        const STRIKETHROUGH_TOKEN = "___KHOJ_STRIKETHROUGH_MARKER___";
        const NESTED_HIGHLIGHT_TOKEN = "___KHOJ_NESTED_HIGHLIGHT_MARKER___";
        const NESTED_STRIKETHROUGH_TOKEN = "___KHOJ_NESTED_STRIKETHROUGH_MARKER___";

        // Function to preserve existing formatting markers by replacing them with tokens
        const preserveFormatting = (text: string): string => {
            // First, handle potential nested formatting by temporarily marking them
            let processed = text;

            // Find and mark nested highlight within strikethrough
            processed = processed.replace(/~~(.*?==.*?==.*?)~~/g, (match, content) => {
                // Replace inner highlight markers
                const innerProcessed = content.replace(/==(.*?)==/g, `${NESTED_HIGHLIGHT_TOKEN}$1${NESTED_HIGHLIGHT_TOKEN}`);
                return `${STRIKETHROUGH_TOKEN}${innerProcessed}${STRIKETHROUGH_TOKEN}`;
            });

            // Find and mark nested strikethrough within highlight
            processed = processed.replace(/==(.*?~~.*?~~.*?)==/g, (match, content) => {
                // Replace inner strikethrough markers
                const innerProcessed = content.replace(/~~(.*?)~~/g, `${NESTED_STRIKETHROUGH_TOKEN}$1${NESTED_STRIKETHROUGH_TOKEN}`);
                return `${HIGHLIGHT_TOKEN}${innerProcessed}${HIGHLIGHT_TOKEN}`;
            });

            // Now handle non-nested formatting
            // Replace remaining highlight markers with non-greedy pattern
            processed = processed.replace(/==(.*?)==/g, `${HIGHLIGHT_TOKEN}$1${HIGHLIGHT_TOKEN}`);
            // Replace remaining strikethrough markers with non-greedy pattern
            processed = processed.replace(/~~(.*?)~~/g, `${STRIKETHROUGH_TOKEN}$1${STRIKETHROUGH_TOKEN}`);

            return processed;
        };

        // Function to restore original formatting markers
        const restoreFormatting = (text: string): string => {
            let processed = text;

            // Restore regular formatting first
            processed = processed.replace(new RegExp(HIGHLIGHT_TOKEN + "(.*?)" + HIGHLIGHT_TOKEN, "g"), "==$1==");
            processed = processed.replace(new RegExp(STRIKETHROUGH_TOKEN + "(.*?)" + STRIKETHROUGH_TOKEN, "g"), "~~$1~~");

            // Then restore nested formatting
            processed = processed.replace(new RegExp(NESTED_HIGHLIGHT_TOKEN + "(.*?)" + NESTED_HIGHLIGHT_TOKEN, "g"), "==$1==");
            processed = processed.replace(new RegExp(NESTED_STRIKETHROUGH_TOKEN + "(.*?)" + NESTED_STRIKETHROUGH_TOKEN, "g"), "~~$1~~");

            return processed;
        };

        // Preserve existing formatting in both texts
        const preservedOriginal = preserveFormatting(originalText);
        const preservedNew = preserveFormatting(newText);

        // Find common prefix and suffix
        let prefixLength = 0;
        const minLength = Math.min(preservedOriginal.length, preservedNew.length);
        while (prefixLength < minLength && preservedOriginal[prefixLength] === preservedNew[prefixLength]) {
            prefixLength++;
        }

        let suffixLength = 0;
        while (
            suffixLength < minLength - prefixLength &&
            preservedOriginal[preservedOriginal.length - 1 - suffixLength] === preservedNew[preservedNew.length - 1 - suffixLength]
        ) {
            suffixLength++;
        }

        // Extract the parts
        const commonPrefix = preservedOriginal.slice(0, prefixLength);
        const commonSuffix = preservedOriginal.slice(preservedOriginal.length - suffixLength);
        const originalDiff = preservedOriginal.slice(prefixLength, preservedOriginal.length - suffixLength);
        const newDiff = preservedNew.slice(prefixLength, preservedNew.length - suffixLength);

        // Format the differences with special handling for empty lines
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

        // Create the diff preview with preserved formatting tokens
        const diffPreview = commonPrefix +
            (originalDiff ? formatLines(originalDiff, '~~') : '') +
            (newDiff ? formatLines(newDiff, '==') : '') +
            commonSuffix;

        // Restore original formatting markers in the final result
        return restoreFormatting(diffPreview);
    }

    /**
     * Applies the final changes to a text by replacing diff markers with the final content
     * This is used to preserve existing formatting in the document
     *
     * @param text - The text with diff markers
     * @returns The text with diff markers replaced by the final content
     */
    public applyFinalChanges(text: string): string {
        // First, handle nested formatting patterns
        // This regex looks for patterns of ~~deleted text~~==added text== and replaces them with just the added text
        // It uses a non-greedy match to avoid capturing too much text
        let result = text.replace(/~~(.*?)~~==([^=]*?)==/g, (match, deleted, added) => {
            return added;
        });

        // Handle cases where there might be formatting markers within the diff markers
        // For example: ==This is **highlighted and bold**== or ~~This is *strikethrough and italic*~~

        // First, extract content between == markers (additions)
        result = result.replace(/==(.*?)==/g, (match, content) => {
            // If the content contains formatting markers, preserve them
            return content;
        });

        // Then remove any remaining strikethrough markers (deletions)
        result = result.replace(/~~(.*?)~~/g, '');

        return result;
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

                // Apply the final changes to preserve formatting
                const finalPreview = this.applyFinalChanges(preview);

                // Apply the edit
                const newContent =
                    content.substring(0, startIndex) +
                    finalPreview +
                    content.substring(endIndex);

                // Save the changes
                await this.app.vault.modify(targetFile, newContent);
                editResults.push({ block, success: true });

                // Update fileBackups with the new content to ensure subsequent edits use the updated file
                fileBackups.set(targetFile.path, newContent);

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
     * Enhanced to handle partial blocks and display different HTML based on the block state
     *
     * @param message - The message containing khoj-edit blocks in XML format
     * @returns The transformed message with HTML for edit blocks
     */
    public transformKhojEditBlocks(message: string): string {
        // Get all open markdown files
        const files = this.app.workspace.getLeavesOfType('markdown')
            .map(leaf => (leaf.view as any)?.file)
            .filter(file => file && file.extension === 'md');

        // Detect all edit blocks, including partial ones
        const partialBlocks = this.detectPartialEditBlocks(message);

        // Process each detected block
        let transformedMessage = message;
        for (const block of partialBlocks) {
            const isComplete = block.isComplete;
            const content = block.content;

            // Parse the block content
            const { editData, cleanContent, error, inProgress } = this.parseKhojEditBlock(content, isComplete);

            // Escape content for HTML display
            const escapedContent = cleanContent
                .replace(/&/g, '&amp;')
                .replace(/</g, '&lt;')
                .replace(/>/g, '&gt;')
                .replace(/"/g, '&quot;')
                .replace(/'/g, '&#039;');

            let htmlReplacement = '';

            if (inProgress) {
                // In-progress block
                const noteText = editData.note ? editData.note : 'Edit in progress...';
                const fileText = editData.file ? `(📄 ${editData.file})` : '';

                htmlReplacement = `<details class="khoj-edit-accordion in-progress">
                    <summary>${noteText} <span class="khoj-edit-file">${fileText}</span> <span class="khoj-edit-status">In Progress</span></summary>
                    <div class="khoj-edit-content">
                        <pre><code class="language-khoj-edit">${escapedContent}</code></pre>
                    </div>
                </details>`;
            } else if (error) {
                // Error block
                console.error("Error parsing khoj-edit block:", error);
                console.debug("Content causing error:", content);

                const errorTitle = `Error: ${error.message}`;
                const errorDetails = `Failed to parse edit block. Please check the JSON format and ensure all required fields are present.`;

                htmlReplacement = `<details class="khoj-edit-accordion error">
                    <summary>${errorTitle}</summary>
                    <div class="khoj-edit-content">
                        <p class="khoj-edit-error-message">${errorDetails}</p>
                        <pre><code class="language-khoj-edit error">${escapedContent}</code></pre>
                    </div>
                </details>`;
            } else {
                // Success block
                // Find the actual file that will be modified
                const targetFile = this.findBestMatchingFile(editData.file, files);
                const displayFileName = targetFile ? targetFile.basename : editData.file;

                htmlReplacement = `<details class="khoj-edit-accordion success">
                    <summary>${editData.note} <span class="khoj-edit-file">(📄 ${displayFileName})</span></summary>
                    <div class="khoj-edit-content">
                        <pre><code class="language-khoj-edit">${escapedContent}</code></pre>
                    </div>
                </details>`;
            }

            // Replace the block in the message
            if (isComplete) {
                transformedMessage = transformedMessage.replace(`<khoj-edit>${content}</khoj-edit>`, htmlReplacement);
            } else {
                transformedMessage = transformedMessage.replace(`<khoj-edit>${content}`, htmlReplacement);
            }
        }

        return transformedMessage;
    }

    /**
     * Detects partial edit blocks in a message
     * This allows for early detection of edit blocks before they are complete
     *
     * @param message - The message to search for partial edit blocks
     * @returns An array of detected blocks with their content and completion status
     */
    public detectPartialEditBlocks(message: string): PartialEditBlockResult[] {
        const results: PartialEditBlockResult[] = [];

        // This regex captures both complete and incomplete khoj-edit blocks
        // It looks for <khoj-edit> followed by any content, and then either </khoj-edit> or the end of the string
        const regex = /<khoj-edit>([\s\S]*?)(?:<\/khoj-edit>|$)/g;

        let match;
        while ((match = regex.exec(message)) !== null) {
            const content = match[1];
            const isComplete = match[0].endsWith('</khoj-edit>');

            results.push({
                content,
                isComplete
            });
        }

        return results;
    }
}
