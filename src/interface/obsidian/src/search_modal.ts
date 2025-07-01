  // @ts-nocheck
  /* eslint-disable */

import { App, SuggestModal, request, MarkdownRenderer, Instruction, Platform, Notice } from 'obsidian';
import { KhojSetting } from 'src/settings';
import { supportedBinaryFileTypes, createNoteAndCloseModal, getFileFromPath, getLinkToEntry, supportedImageFilesTypes } from 'src/utils';

export interface SearchResult {
    entry: string;
    file: string;
}

export class KhojSearchModal extends SuggestModal<SearchResult> {
    setting: KhojSetting;
    rerank: boolean = false;
    find_similar_notes: boolean;
    query: string = "";
    app: App;
    currentController: AbortController | null = null;  // To cancel requests
    isLoading: boolean = false;
    loadingEl: HTMLElement;
    fileFilterDropdown: HTMLSelectElement;
    fileTypeCheckboxes: { [key: string]: HTMLInputElement } = {};
    fileTypeOptions = [
        { label: '.md', value: '.md' },
        { label: '.pdf', value: '.pdf' },
        { label: 'images', value: '.png,.jpg,.jpeg,.gif' },
    ];
    fileFilterOptions = [
        { label: 'include all', mode: 'include', prefix: '' },
        { label: 'exclude _khoj', mode: 'exclude', prefix: '_khoj' },
        { label: 'exclude all underscored', mode: 'exclude', prefix: '_' },
        { label: 'only _khoj', mode: 'only', prefix: '_khoj' },
    ];
    selectedFileFilter = this.fileFilterOptions[0];

    constructor(app: App, setting: KhojSetting, find_similar_notes: boolean = false) {
        super(app);
        this.app = app;
        this.setting = setting;
        this.find_similar_notes = find_similar_notes;

        // Hide input element in Similar Notes mode
        this.inputEl.hidden = this.find_similar_notes;

        // Create loading element
        this.loadingEl = document.createElement('div');
        this.loadingEl.className = "search-loading";
        const spinnerEl = document.createElement('div');
        spinnerEl.className = "search-loading-spinner";
        this.loadingEl.appendChild(spinnerEl);

        this.loadingEl.style.position = "absolute";
        this.loadingEl.style.top = "50%";
        this.loadingEl.style.left = "50%";
        this.loadingEl.style.transform = "translate(-50%, -50%)";
        this.loadingEl.style.zIndex = "1000";
        this.loadingEl.style.display = "none";

        // Add the element to the modal
        this.modalEl.appendChild(this.loadingEl);

        // Customize empty state message
        this.emptyStateText = "";

        // Register Modal Keybindings to Rerank Results
        this.scope.register(['Mod'], 'Enter', async () => {
            // Re-rank when explicitly triggered by user
            this.rerank = true
            // Trigger input event to get and render (reranked) results from khoj backend
            this.inputEl.dispatchEvent(new Event('input'));
            // Rerank disabled by default to satisfy latency requirements for incremental search
            this.rerank = false
        });

        // Register Modal Keybindings to Create New Note with Query as Title
        this.scope.register(['Shift'], 'Enter', async () => {
            if (this.query != "") createNoteAndCloseModal(this.query, this);
        });
        this.scope.register(['Ctrl', 'Shift'], 'Enter', async () => {
            if (this.query != "") createNoteAndCloseModal(this.query, this, { newLeaf: true });
        });

        // Add Hints to Modal for available Keybindings
        const modalInstructions: Instruction[] = [
            {
                command: '↑↓',
                purpose: 'to navigate',
            },
            {
                command: '↵',
                purpose: 'to open',
            },
            {
                command: Platform.isMacOS ? 'cmd ↵' : 'ctrl ↵',
                purpose: 'to rerank',
            },
            {
                command: 'esc',
                purpose: 'to dismiss',
            },
        ]
        this.setInstructions(modalInstructions);

        // Set Placeholder Text for Modal
        this.setPlaceholder('Search with Khoj...');

        // Add file filter dropdown and file type checkboxes above the input
        const controlsContainer = document.createElement('div');
        controlsContainer.className = "search-controls-container";
        // File type checkboxes (group images)
        const fileTypeRow = document.createElement('div');
        fileTypeRow.className = 'khoj-file-type-row';
        fileTypeRow.style.marginBottom = '8px'; // Add space below checkboxes
        this.fileTypeOptions.forEach(opt => {
            const label = document.createElement('label');
            label.style.marginLeft = '8px';
            const checkbox = document.createElement('input');
            checkbox.type = 'checkbox';
            checkbox.value = opt.value;
            checkbox.checked = true;
            this.fileTypeCheckboxes[opt.value] = checkbox;
            label.appendChild(checkbox);
            label.appendChild(document.createTextNode(opt.label));
            fileTypeRow.appendChild(label);
        });
        controlsContainer.appendChild(fileTypeRow);
        // File filter dropdown
        this.fileFilterDropdown = document.createElement('select');
        this.fileFilterDropdown.className = "search-dropdown";
        this.fileFilterDropdown.style.width = '140px'; // Make dropdown skinnier
        this.fileFilterDropdown.style.marginTop = '8px'; // Add space above dropdown
        this.fileFilterDropdown.style.background = 'var(--background-primary)';
        this.fileFilterDropdown.style.color = 'var(--text-normal)';
        this.fileFilterOptions.forEach((opt, i) => {
            const option = document.createElement('option');
            option.value = `${opt.mode}:${opt.prefix}`;
            option.text = opt.label;
            this.fileFilterDropdown.appendChild(option);
        });
        this.fileFilterDropdown.value = 'include:';
        this.fileFilterDropdown.addEventListener('change', (e) => {
            const [mode, prefix] = (e.target as HTMLSelectElement).value.split(':');
            this.selectedFileFilter = this.fileFilterOptions.find(opt => opt.mode === mode && opt.prefix === prefix) || this.fileFilterOptions[0];
            // Trigger search update
            this.inputEl.dispatchEvent(new Event('input'));
        });
        controlsContainer.appendChild(this.fileFilterDropdown);
        // Insert controls at the very top of the modal
        this.modalEl.insertBefore(controlsContainer, this.modalEl.firstChild);
    }

    // Check if the file exists in the vault
    private isFileInVault(filePath: string): boolean {
        // Normalize the path to handle different separators
        const normalizedPath = filePath.replace(/\\/g, '/');

        // Check if the file exists in the vault
        return this.app.vault.getFiles().some(file =>
            file.path === normalizedPath
        );
    }

    async getSuggestions(query: string): Promise<SearchResult[]> {
        // Do not show loading if the query is empty
        if (!query.trim()) {
            this.isLoading = false;
            this.updateLoadingState();
            return [];
        }

        // Show loading state
        this.isLoading = true;
        this.updateLoadingState();

        // Cancel previous request if it exists
        if (this.currentController) {
            this.currentController.abort();
        }

        try {
            // Create a new controller for this request
            this.currentController = new AbortController();

            // Gather file filter and type options
            let [filename_prefix_mode, filename_prefix] = this.fileFilterDropdown.value.split(':');
            let file_extensions: string[] = [];
            Object.entries(this.fileTypeCheckboxes).forEach(([ext, checkbox]) => {
                if (checkbox.checked) {
                    if (ext.startsWith('.')) {
                        file_extensions.push(ext);
                    } else {
                        // For images, add all extensions
                        file_extensions.push(...ext.split(','));
                    }
                }
            });

            // Setup Query Khoj backend for search results
            let encodedQuery = encodeURIComponent(query);
            let searchUrl = `${this.setting.khojUrl}/api/search?q=${encodedQuery}` +
                `&n=${this.setting.resultsCount}` +
                `&r=${this.rerank}` +
                `&client=obsidian` +
                `&filename_prefix_mode=${filename_prefix_mode}`;
            // Always send filename_prefix if mode is not 'include'
            if (filename_prefix_mode !== 'include') {
                searchUrl += `&filename_prefix=${encodeURIComponent(filename_prefix)}`;
            }
            if (file_extensions.length > 0) {
                searchUrl += `&file_extension=${encodeURIComponent(file_extensions.join(','))}`;
            }
            let headers = {
                'Authorization': `Bearer ${this.setting.khojApiKey}`,
            }

            // Get search results from Khoj backend
            const response = await fetch(searchUrl, {
                headers: headers,
                signal: this.currentController.signal
            });

            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }

            const data = await response.json();

            // Parse search results
            let results = data
                .filter((result: any) =>
                    !this.find_similar_notes || !result.additional.file.endsWith(this.app.workspace.getActiveFile()?.path)
                )
                .map((result: any) => {
                    return {
                        entry: result.entry,
                        file: result.additional.file,
                        inVault: this.isFileInVault(result.additional.file)
                    } as SearchResult & { inVault: boolean };
                })
                .sort((a: SearchResult & { inVault: boolean }, b: SearchResult & { inVault: boolean }) => {
                    if (a.inVault === b.inVault) return 0;
                    return a.inVault ? -1 : 1;
                });

            this.query = query;

            // Hide loading state only on successful completion
            this.isLoading = false;
            this.updateLoadingState();

            return results;
        } catch (error) {
            // Ignore cancellation errors and keep loading state
            if (error.name === 'AbortError') {
                // When cancelling, we don't want to render anything
                return undefined as any;
            }

            // For other errors, hide loading state
            console.error('Search error:', error);
            this.isLoading = false;
            this.updateLoadingState();
            return [];
        }
    }

    private updateLoadingState() {
        // Show or hide loading element
        this.loadingEl.style.display = this.isLoading ? "block" : "none";
    }

    async onOpen() {
        if (this.find_similar_notes) {
            // If markdown file is currently active
            let file = this.app.workspace.getActiveFile();
            if (file && file.extension === 'md') {
                // Enable rerank of search results
                this.rerank = true
                // Set input element to contents of active markdown file
                // truncate to first 8,000 characters to avoid hitting query size limits
                this.inputEl.value = await this.app.vault.read(file).then((file_str: any) => file_str.slice(0, 42110));
                // Trigger search to get and render similar notes from khoj backend
                this.inputEl.dispatchEvent(new Event('input'));
                this.rerank = false
            }
            else {
                this.resultContainerEl.setText('Cannot find similar notes for non-markdown files');
            }
        }
    }

    async renderSuggestion(result: SearchResult & { inVault: boolean }, el: HTMLElement) {
        // Max number of lines to render
        let lines_to_render = 8;

        // Extract filename of result
        let os_path_separator = result.file.includes('\\') ? '\\' : '/';
        let filename = result.file.split(os_path_separator).pop();

        // Show filename of each search result for context with appropriate color
        const fileEl = el.createEl("div", {
            cls: `khoj-result-file ${result.inVault ? 'in-vault' : 'not-in-vault'}`
        });
        fileEl.setText(filename ?? "");

        // Add a visual indication for files not in vault
        if (!result.inVault) {
            fileEl.createSpan({
                text: " (not in vault)",
                cls: "khoj-result-file-status"
            });
        }

        let result_el = el.createEl("div", { cls: 'khoj-result-entry' })

        let resultToRender = "";
        let fileExtension = filename?.split(".").pop() ?? "";
        if (supportedImageFilesTypes.includes(fileExtension) && filename && result.inVault) {
            let linkToEntry: string = filename;
            let imageFiles = this.app.vault.getFiles().filter(file => supportedImageFilesTypes.includes(fileExtension));
            // Find vault file of chosen search result
            let fileInVault = getFileFromPath(imageFiles, result.file);
            if (fileInVault)
                linkToEntry = this.app.vault.getResourcePath(fileInVault);

            resultToRender = `![](${linkToEntry})`;
        } else {
            // Remove YAML frontmatter when rendering string
            result.entry = result.entry.replace(/---[\n\r][\s\S]*---[\n\r]/, '');

            // Truncate search results to lines_to_render
            let entry_snipped_indicator = result.entry.split('\n').length > lines_to_render ? ' **...**' : '';
            let snipped_entry = result.entry.split('\n').slice(0, lines_to_render).join('\n');
            resultToRender = `${snipped_entry}${entry_snipped_indicator}`;
        }
        // @ts-ignore
        MarkdownRenderer.renderMarkdown(resultToRender, result_el, result.file, null);
    }

    async onChooseSuggestion(result: SearchResult & { inVault: boolean }, _: MouseEvent | KeyboardEvent) {
        // Only open files that are in the vault
        if (!result.inVault) {
            new Notice("This file is not in your vault");
            return;
        }

        // Get all markdown, pdf and image files in vault
        const mdFiles = this.app.vault.getMarkdownFiles();
        const binaryFiles = this.app.vault.getFiles().filter(file => supportedBinaryFileTypes.includes(file.extension));

        // Find, Open vault file at heading of chosen search result
        let linkToEntry = getLinkToEntry(mdFiles.concat(binaryFiles), result.file, result.entry);
        if (linkToEntry) this.app.workspace.openLinkText(linkToEntry, '');
    }
}
