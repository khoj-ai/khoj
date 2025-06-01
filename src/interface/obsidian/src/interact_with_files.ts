import { App, TFile } from 'obsidian';
import { diffWords } from 'diff';

/**
 * Interface representing a block of edit instructions for modifying files
 */
export interface EditBlock {
    file: string;       // Target file name [Required]
    find: string;       // Content to find in file [Required]
    replace: string;    // Content to replace with in file [Required]
    note?: string;      // Brief explanation of edit [Optional]
    hasError?: boolean; // Flag to indicate parsing error [Optional]
    error?: {
        type: 'missing_field' | 'invalid_format' | 'preprocessing' | 'unknown';
        message: string;
        details?: string;
    };
}

/**
 * Interface representing the result of parsing a Khoj edit block
 */
export interface ParsedEditBlock {
    editData: EditBlock | null;
    cleanContent: string;
    inProgress?: boolean;
    error?: {
        type: 'missing_field' | 'invalid_format' | 'preprocessing' | 'unknown';
        message: string;
        details?: string;
    };
}

/**
 * Interface representing the result of processing an edit block
 */
interface ProcessedEditResult {
    preview: string; // The content with diff markers to be inserted
    newContent: string; // The new content after replacement
    error?: string;  // Error message if processing failed (e.g., 'find' text not found)
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
    private readonly EDIT_BLOCK_START = '<khoj_edit>';
    private readonly EDIT_BLOCK_END = '</khoj_edit>';

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

        // Instructions in write access mode
        let editInstructions: string = '';
        if (fileAccessMode === 'write') {
            editInstructions = `
If the user requests, you can suggest edits to files provided in the WORKING_FILE_SET provided below.
Once you understand the user request you MUST:

1. Decide if you need to propose *SEARCH/REPLACE* edits to any files that haven't been added to the chat.

If you need to propose edits to existing files not already added to the chat, you *MUST* tell the user their full path names and ask them to *add the files to the chat*.
End your reply and wait for their approval.
You can keep asking if you then decide you need to edit more files.

2. Think step-by-step and explain the needed changes in a few short sentences before each EDIT block.

3. Describe each change with a *SEARCH/REPLACE block* like the examples below.

All changes to files must use this *SEARCH/REPLACE block* format.
ONLY EVER RETURN EDIT TEXT IN A *SEARCH/REPLACE BLOCK*!

# *SEARCH/REPLACE block* Rules:

Every *SEARCH/REPLACE block* must use this format:
1. The opening fence: \`${this.EDIT_BLOCK_START}\`
2. The *FULL* file path alone on a line, verbatim. No bold asterisks, no quotes around it, no escaping of characters, etc.
3. The start of search block: <<<<<<< SEARCH
4. A contiguous chunk of lines to search for in the source file
5. The dividing line: =======
6. The lines to replace into the source file
7. The end of the replace block: >>>>>>> REPLACE
8. The closing fence: \`${this.EDIT_BLOCK_END}\`

Use the *FULL* file path, as shown to you by the user.

Every *SEARCH* section must *EXACTLY MATCH* the existing file content, character for character, including all comments, docstrings, etc.
If the file contains code or other data wrapped/escaped in json/xml/quotes or other containers, you need to propose edits to the literal contents of the file, including the container markup.

*SEARCH/REPLACE* blocks will *only* replace the first match occurrence.
Including multiple unique *SEARCH/REPLACE* blocks if needed.
Include enough lines in each SEARCH section to uniquely match each set of lines that need to change.

Keep *SEARCH/REPLACE* blocks concise.
Break large *SEARCH/REPLACE* blocks into a series of smaller blocks that each change a small portion of the file.
Include just the changing lines, and a few surrounding lines if needed for uniqueness.
Do not include long runs of unchanging lines in *SEARCH/REPLACE* blocks.

Only create *SEARCH/REPLACE* blocks for files that the user has added to the chat!

To move text within a file, use 2 *SEARCH/REPLACE* blocks: 1 to delete it from its current location, 1 to insert it in the new location.

Pay attention to which filenames the user wants you to edit, especially if they are asking you to create a new file.

If you want to put text in a new file, use a *SEARCH/REPLACE block* with:
- A new file path, including dir name if needed
- An empty \`SEARCH\` section
- The new file's contents in the \`REPLACE\` section

ONLY EVER RETURN EDIT TEXT IN A *SEARCH/REPLACE BLOCK*!

<EDIT_INSTRUCTIONS>
Suggest edits using targeted modifications. Use multiple edit blocks to make precise changes rather than rewriting entire sections.

Here's how to use the *SEARCH/REPLACE block* format:

${this.EDIT_BLOCK_START}
target-filename
<<<<<<< SEARCH
from flask import Flask
=======
import math
from flask import Flask
>>>>>>> REPLACE
${this.EDIT_BLOCK_END}

⚠️ Important:
- The target-filename parameter is required and must match an open file name.
- The XML format ${this.EDIT_BLOCK_START}...${this.EDIT_BLOCK_END} ensures reliable parsing.
- The SEARCH block content must completely and uniquely identify the section to edit.
- The REPLACE block content will replace the first SEARCH block match in the specified \`target-filename\`.

📝 Example note:

\`\`\`
---
date: 2024-01-20
tags: meeting, planning
status: active
---
# file: Meeting Notes.md

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

Add deadline and specificity to the marketing team follow-up.
${this.EDIT_BLOCK_START}
Meeting Notes.md
<<<<<<< SEARCH
- Schedule follow-up with marketing team about new campaign launch
=======
- Schedule follow-up with marketing team by Wednesday to discuss Q1 campaign launch
>>>>>>> REPLACE
${this.EDIT_BLOCK_END}

2. Multiple targeted changes with escaped characters:

Add HIGH priority flag with code reference to Q4 metrics review"
${this.EDIT_BLOCK_START}
Meeting Notes.md
<<<<<<< SEARCH
- Review Q4 metrics
=======
- [HIGH] Review Q4 metrics (see "metrics.ts" and \`calculateQ4Metrics()\`)
>>>>>>> REPLACE
</${this.EDIT_BLOCK_END}>

Add resource allocation to project timeline task
<${this.EDIT_BLOCK_START}>
Meeting Notes.md
<<<<<<< SEARCH
- Update project timeline and milestones for Q1 2024
=======
- Update project timeline and add resource allocation for Q1 2024
>>>>>>> REPLACE
</${this.EDIT_BLOCK_END}>

3. Adding new content between sections:
Insert a new section for discussion points after the action items section:
${this.EDIT_BLOCK_START}
Meeting Notes.md
<<<<<<< SEARCH
Action items from today:
- Review Q4 metrics
- Schedule follow-up with marketing team about new campaign launch
- Update project timeline and milestones for Q1 2024
=======
Action items from today:
- Review Q4 metrics
- Schedule follow-up
- Update timeline

Discussion Points:
- Budget review
- Team feedback
>>>>>>> REPLACE
</${this.EDIT_BLOCK_END}>

4. Completely replacing a file content (preserving frontmatter):
Replace entire file content while keeping frontmatter metadata
${this.EDIT_BLOCK_START}
Meeting Notes.md
<<<<<<< SEARCH
=======
# Project Overview

## Goals
- Increase user engagement by 25%
- Launch mobile app by Q3
- Expand to 3 new markets

## Timeline
1. Q1: Research & Planning
2. Q2: Development
3. Q3: Testing & Launch
4. Q4: Market Expansion
>>>>>>> REPLACE
${this.EDIT_BLOCK_END}

- The SEARCH block must uniquely identify the section to edit
- The REPLACE block content replaces the first SEARCH block match in the specified file
- Frontmatter metadata (between --- markers at top of file) cannot be modified
- Use an empty SEARCH block to replace entire file content with content in REPLACE block (while preserving frontmatter).
- Remember to escape special characters: use \" for quotes in content
- Each edit block must be fenced in ${this.EDIT_BLOCK_START}...${this.EDIT_BLOCK_END} XML tags

</EDIT_INSTRUCTIONS>
`;
        }

        let openFilesContent = `
For context, the user is currently working on the following files:
<WORKING_FILE_SET>

`;

        for (const leaf of leaves) {
            const view = leaf.view as any;
            const file = view?.file;
            if (!file || file.extension !== 'md') continue;

            // Read file content
            let fileContent: string;
            try {
                fileContent = await this.app.vault.read(file);
            } catch (error) {
                console.error(`Error reading file ${file.path}:`, error);
                continue;
            }

            openFilesContent += `<OPEN_FILE>\n# file: ${file.basename}.md\n\n${fileContent}\n</OPEN_FILE>\n\n`;
        }

        openFilesContent += "</WORKING_FILE_SET>\n";

        // Collate open files content with instructions
        let context: string;
        if (fileAccessMode === 'write') {
             context = `\n\n<SYSTEM>${editInstructions + openFilesContent}</SYSTEM>`;
        } else {
             context = `\n\n<SYSTEM>${openFilesContent}</SYSTEM>`;
        }

        return context;
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
     * Parses a text edit block from the content string
     * Enhanced to handle incomplete blocks and extract partial information
     *
     * @param content - The content from which to parse edit blocks
     * @param isComplete - Whether the edit block is complete (has closing tag)
     * @returns Object with the parsed edit data and cleaned content
     */
    public parseEditBlock(content: string, isComplete: boolean = true): ParsedEditBlock {
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
                const partialData: EditBlock = {
                    file: "",
                    find: "",
                    replace: ""
                };

                // Try to extract file name from the first line
                const firstLineMatch = cleanContent.match(/^([^\n]+)/);
                if (firstLineMatch) {
                    partialData.file = firstLineMatch[1].trim();
                }

                // Try to extract search content field
                const searchStartMatch = cleanContent.match(/<<<<<<< SEARCH\n([\s\S]*)/);
                if (searchStartMatch) {
                    partialData.find = searchStartMatch[1];
                    if (!partialData.file) { // If file not on first line, try line before SEARCH
                        const lines = cleanContent.split('\n');
                        const searchIndex = lines.findIndex(line => line.startsWith("<<<<<<< SEARCH"));
                        if (searchIndex > 0) {
                            partialData.file = lines[searchIndex - 1].trim();
                        }
                    }
                }

                return {
                    editData: partialData,
                    cleanContent,
                    inProgress: true
                };
            }

            // Try parse SEARCH/REPLACE format for complete edit blocks
            // Regex: file_path\n<<<<<<< SEARCH\nsearch_content\n=======\nreplacement_content\n>>>>>>> REPLACE
            const newFormatRegex = /^([^\n]+)\n<<<<<<< SEARCH\n([\s\S]*?)\n=======\n([\s\S]*?)\n>>>>>>> REPLACE\s*$/;
            const newFormatMatch = newFormatRegex.exec(cleanContent);

            let editData: EditBlock | null = null;
            if (newFormatMatch) {
                editData = {
                    file: newFormatMatch[1].trim(),
                    find: newFormatMatch[2],
                    replace: newFormatMatch[3],
                };
            }

            // Validate required fields
            let error: { type: 'missing_field' | 'invalid_format' | 'preprocessing' | 'unknown', message: string, details?: string } | null = null;
            if (!editData) {
                error = {
                    type: 'invalid_format',
                    message: 'Invalid edit block format',
                    details: 'The edit block does not match the expected format'
                };
            }
            else if (!editData.file) {
                error = {
                    type: 'missing_field',
                    message: 'Missing "file" field in edit block',
                    details: 'The "file" field is required and should contain the target file name'
                };
            }
            else if (editData.find === undefined || editData.find === null) {
                error = {
                    type: 'missing_field',
                    message: 'Missing "find" field markers',
                    details: 'The "find" field is required and should contain the content to find in the file'
                };
            }
            else if (!editData.replace) {
                error = {
                    type: 'missing_field',
                    message: 'Missing "replace" field in edit block',
                    details: 'The "replace" field is required and should contain the replacement text'
                };
            }

            return error
            ? { editData, cleanContent, error }
            : { editData, cleanContent };
        } catch (error) {
            console.error("Error parsing edit block:", error);
            console.error("Content causing error:", content);
            return {
                editData: null,
                cleanContent,
                error: {
                    type: 'invalid_format',
                    message: 'Invalid JSON format in edit block',
                    details: error.message
                }
            };
        }
    }

    /**
     * Parses all edit blocks from a message
     *
     * @param message - The message containing text edit blocks in XML format
     * @returns Array of EditBlock objects
     */
    public parseEditBlocks(message: string): EditBlock[] {
        const editBlocks: EditBlock[] = [];
        // Set regex to match edit blocks based on Edit Start, End XML tags in the message
        const editBlockRegex = new RegExp(`${this.EDIT_BLOCK_START}([\\s\\S]*?)${this.EDIT_BLOCK_END}`, 'g');

        let match;
        while ((match = editBlockRegex.exec(message)) !== null) {
            const { editData, cleanContent, error } = this.parseEditBlock(match[1]);

            if (error) {
                console.error("Failed to parse edit block:", error);
                console.debug("Content causing error:", match[1]);
                editBlocks.push({
                    file: "unknown", // Fallback value when editData is null
                    find: "",
                    replace: `Error: ${error.message}\nOriginal content:\n${match[1]}`,
                    note: "Error parsing edit block",
                    hasError: true,
                    error: error
                });
                continue;
            }

            if (!editData) {
                console.error("No edit data parsed");
                continue;
            }

            editBlocks.push({
                note: "Suggested edit",
                file: editData.file,
                find: editData.find,
                replace: editData.replace,
                hasError: !!error,
                error: error || undefined
            });
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

        // Function to preserve existing formatting markers by replacing them with tokens
        const preserveFormatting = (text: string): string => {
            // Replace existing highlight markers with non-greedy pattern
            let processed = text.replace(/==(.*?)==/g, `${HIGHLIGHT_TOKEN}$1${HIGHLIGHT_TOKEN}`);
            // Replace existing strikethrough markers with non-greedy pattern
            processed = processed.replace(/~~(.*?)~~/g, `${STRIKETHROUGH_TOKEN}$1${STRIKETHROUGH_TOKEN}`);
            return processed;
        };

        // Function to restore original formatting markers
        const restoreFormatting = (text: string): string => {
            // Restore highlight markers
            let processed = text.replace(new RegExp(HIGHLIGHT_TOKEN + "(.*?)" + HIGHLIGHT_TOKEN, "g"), "==$1==");
            // Restore strikethrough markers
            processed = processed.replace(new RegExp(STRIKETHROUGH_TOKEN + "(.*?)" + STRIKETHROUGH_TOKEN, "g"), "~~$1~~");
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

        // Create the diff preview with preserved formatting tokens
        const diffPreview = commonPrefix +
            (originalDiff ? formatLines(originalDiff, '~~') : '') +
            (newDiff ? formatLines(newDiff, '==') : '') +
            commonSuffix;

        // Restore original formatting markers in the final result
        return restoreFormatting(diffPreview);
    }

    private textNormalize(text: string): string {
        // Normalize whitespace and special characters
        return text
            .replace(/\u00A0/g, " ")         // Replace non-breaking spaces with regular spaces
            .replace(/[\u2002\u2003\u2007\u2008\u2009\u200A\u205F\u3000]/g, " ") // Replace various other Unicode spaces with regular spaces
            .replace(/[\u2013\u2014]/g, '-') // Replace en-dash and em-dash with hyphen
            .replace(/[\u2018\u2019]/g, "'") // Replace smart quotes with regular quotes
            .replace(/[\u201C\u201D]/g, '"') // Replace smart double quotes with regular quotes
            .replace(/\u2026/g, '...')       // Replace ellipsis with three dots
            .normalize('NFC')                // Normalize to NFC form
    }

    private processSingleEdit(
        rawFindText: string,
        replaceText: string,
        rawCurrentFileContent: string,
        frontmatterEndIndex: number
    ): ProcessedEditResult {
        let startIndex = -1;
        let endIndex = -1;
        // Normalize special characters before searching
        const findText = this.textNormalize(rawFindText);
        const currentFileContent = this.textNormalize(rawCurrentFileContent);

        if (findText === "") {
            // Empty search means replace entire content after frontmatter
            startIndex = frontmatterEndIndex;
            endIndex = currentFileContent.length;
        } else {
            startIndex = currentFileContent.indexOf(findText, frontmatterEndIndex);
            if (startIndex !== -1) {
                endIndex = startIndex + findText.length;
            }
        }

        if (startIndex === -1 || endIndex === -1 || startIndex > endIndex) {
            return {
                preview: "",
                newContent: currentFileContent,
                error: `No matching text found in file.`

            };
        }

        const textToReplace = currentFileContent.substring(startIndex, endIndex);
        const newText = replaceText.trim();
        const preview = this.createPreviewWithDiff(textToReplace, newText);
        const newContent =
            currentFileContent.substring(0, startIndex) +
            preview +
            currentFileContent.substring(endIndex);

        return { preview, newContent };
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
    }> {
        // Check for parsing errors first
        if (editBlocks.length === 0) {
            return { editResults: [], fileBackups: new Map() };
        }

        // Store original content for each file in case we need to cancel
        const fileBackups = new Map<string, string>();

        // Track current content for each file as we apply edits
        const currentFileContents = new Map<string, string>();

        // Get all open markdown files
        const files = this.app.workspace.getLeavesOfType('markdown')
            .map(leaf => (leaf.view as any)?.file)
            .filter(file => file && file.extension === 'md');

        // Track success/failure for each edit
        const editResults: { block: EditBlock, success: boolean, error?: string }[] = [];
        const blocksNeedingRetry: EditBlock[] = [];

        // PHASE 1: Validation - Check all blocks before applying any changes
        const validationResults: { block: EditBlock, valid: boolean, error?: string, targetFile?: TFile }[] = [];

        for (const block of editBlocks) {
            try {
                // Skip blocks with parsing errors
                if (block.hasError) {
                    validationResults.push({
                        block,
                        valid: false,
                        error: block.error?.message || 'Parsing error'
                    });
                    continue;
                }

                const targetFile = this.findBestMatchingFile(block.file, files);
                if (!targetFile) {
                    validationResults.push({
                        block,
                        valid: false,
                        error: `No matching file found for "${block.file}"`
                    });
                    continue;
                }

                // Read the file content if not already backed up
                if (!fileBackups.has(targetFile.path)) {
                    const content = await this.app.vault.read(targetFile);
                    fileBackups.set(targetFile.path, content);
                    currentFileContents.set(targetFile.path, content);
                }

                // Use current content (which may have been modified by previous validations)
                const currentContent = currentFileContents.get(targetFile.path)!;

                // Find frontmatter boundaries
                const frontmatterMatch = currentContent.match(/^---\n[\s\S]*?\n---\n/);
                const frontmatterEndIndex = frontmatterMatch ? frontmatterMatch[0].length : 0;

                const processedEdit = this.processSingleEdit(block.find, block.replace, currentContent, frontmatterEndIndex);

                if (processedEdit.error) {
                    validationResults.push({ block, valid: false, error: processedEdit.error });
                    continue;
                }

                // Validation passed
                validationResults.push({ block, valid: true, targetFile });

                // Update the current content for this file for subsequent validations
                currentFileContents.set(targetFile.path, processedEdit.newContent);

            } catch (error) {
                validationResults.push({ block, valid: false, error: error.message });
            }
        }

        // Check if all blocks are valid
        const allValid = validationResults.every(result => result.valid);

        // If any block is invalid, don't apply any changes
        if (!allValid) {
            // Reset current file contents
            currentFileContents.clear();

            // Add all invalid blocks to retry list
            for (const result of validationResults) {
                if (!result.valid) {
                    blocksNeedingRetry.push({
                        ...result.block,
                        hasError: true,
                        error: {
                            type: 'invalid_format',
                            message: result.error || 'Validation failed',
                            details: result.error || 'Could not validate edit'
                        }
                    });

                    editResults.push({
                        block: result.block,
                        success: false,
                        error: result.error || 'Validation failed'
                    });
                } else {
                    // Even valid blocks are considered failed in atomic mode if any block fails
                    editResults.push({
                        block: result.block,
                        success: false,
                        error: 'Other edits in the group failed validation'
                    });
                }
            }

            // Trigger retry for the first failed block
            if (blocksNeedingRetry.length > 0 && onRetryNeeded) {
                onRetryNeeded(blocksNeedingRetry[0]);
            }

            return { editResults, fileBackups };
        }

        // PHASE 2: Application - Apply all changes since all blocks are valid
        try {
            // Reset current file contents to original state
            currentFileContents.clear();
            for (const [path, content] of fileBackups.entries()) {
                currentFileContents.set(path, content);
            }

            // Apply all edits
            for (const result of validationResults) {
                const block = result.block;
                const targetFile = result.targetFile!;

                // Use current content (which may have been modified by previous edits)
                const content = currentFileContents.get(targetFile.path)!;

                // Find frontmatter boundaries
                const frontmatterMatch = content.match(/^---\n[\s\S]*?\n---\n/);
                const frontmatterEndIndex = frontmatterMatch ? frontmatterMatch[0].length : 0;

                // Find the text to replace in original content
                // Recalculate based on the current state of the file content for this phase
                const processedEdit = this.processSingleEdit(block.find, block.replace, content, frontmatterEndIndex);

                if (processedEdit.error) {
                     throw new Error(`Failed to re-locate edit markers for file "${targetFile.basename}" during application. Content may have shifted.`);
                }

                // Apply the changes to the file
                await this.app.vault.modify(targetFile, processedEdit.newContent);

                // Update the current content for this file for subsequent edits
                currentFileContents.set(targetFile.path, processedEdit.newContent);

                editResults.push({ block: {...block, replace: processedEdit.preview}, success: true });
            }
        } catch (error) {
            console.error(`Error applying edits:`, error);

            // Restore all files to their original state
            for (const [path, content] of fileBackups.entries()) {
                const file = this.app.vault.getAbstractFileByPath(path);
                if (file && file instanceof TFile) {
                    await this.app.vault.modify(file, content);
                }
            }

            // Mark all blocks as failed
            for (const block of editBlocks) {
                blocksNeedingRetry.push(block);
                editResults.push({
                    block,
                    success: false,
                    error: `Failed to apply edits: ${error.message}`
                });
            }

            // Trigger retry for the first block
            if (blocksNeedingRetry.length > 0 && onRetryNeeded) {
                onRetryNeeded(blocksNeedingRetry[0]);
            }
        }

        return { editResults, fileBackups };
    }

    /**
     * Transforms content edit blocks in a message to HTML for display
     *
     * @param message - The message containing content edit blocks in XML format
     * @returns The transformed message with HTML for edit blocks
     */
    public transformEditBlocks(message: string): string {
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
            const { editData, cleanContent, error, inProgress } = this.parseEditBlock(content, isComplete);

            // Escape content for HTML display
            const diff = diffWords(editData?.find || '', editData?.replace || '');
            let diffContent = diff.map(part => {
                if (part.added) {
                    return `<span class="cm-positive">${part.value}</span>`;
                } else if (part.removed) {
                    return `<span class="cm-negative"><s>${part.value}</s></span>`;
                } else {
                    return `<span>${part.value}</span>`;
                }
            }
            ).join('').trim();

            let htmlRender = '';
            if (error || !editData) {
                // Error block
                console.error("Error parsing khoj-edit block:", error);
                console.error("Content causing error:", content);

                const errorTitle = `Error: ${error?.message || 'Parse error'}`;
                const errorDetails = `Failed to parse edit block. Please check the JSON format and ensure all required fields are present.`;

                htmlRender = `<details class="khoj-edit-accordion error">
                    <summary>${errorTitle}</summary>
                    <div class="khoj-edit-content">
                        <p class="khoj-edit-error-message">${errorDetails}</p>
                        <pre><code class="language-md error">${diffContent}</code></pre>
                    </div>
                </details>`;
            } else if (inProgress) {
                // In-progress block
                htmlRender = `<details class="khoj-edit-accordion in-progress">
                    <summary>📄 ${editData.file} <span class="khoj-edit-status">In Progress</span></summary>
                    <div class="khoj-edit-content">
                        <pre><code class="language-md">${diffContent}</code></pre>
                    </div>
                </details>`;
            } else {
                // Success block
                // Find the actual file that will be modified
                const targetFile = this.findBestMatchingFile(editData.file, files);
                const displayFileName = targetFile ? `${targetFile.basename}.${targetFile.extension}` : editData.file;

                htmlRender = `<details class="khoj-edit-accordion success">
                    <summary>📄 ${displayFileName}</summary>
                    <div class="khoj-edit-content">
                        <div>${diffContent}</div>
                    </div>
                </details>`;
            }

            // Replace the block in the message
            if (isComplete) {
                transformedMessage = transformedMessage.replace(`${this.EDIT_BLOCK_START}${content}${this.EDIT_BLOCK_END}`, htmlRender);
            } else {
                transformedMessage = transformedMessage.replace(`${this.EDIT_BLOCK_START}${content}`, htmlRender);
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

        // This regex captures both complete and incomplete edit blocks
        // It looks for EDIT_BLOCK_START tag followed by any content, and then either EDIT_BLOCK_END or the end of the string
        const regex = new RegExp(`${this.EDIT_BLOCK_START}([\\s\\S]*?)(?:${this.EDIT_BLOCK_END}|$)`, 'g');

        let match;
        while ((match = regex.exec(message)) !== null) {
            const content = match[1];
            const isComplete = match[0].endsWith(this.EDIT_BLOCK_END);

            results.push({
                content,
                isComplete
            });
        }

        return results;
    }
}
