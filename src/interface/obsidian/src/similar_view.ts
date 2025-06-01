import { WorkspaceLeaf, TFile, MarkdownRenderer, Notice, setIcon } from 'obsidian';
import { KhojSetting } from 'src/settings';
import { KhojPaneView } from 'src/pane_view';
import { KhojView, getLinkToEntry, supportedBinaryFileTypes } from 'src/utils';

export interface SimilarResult {
    entry: string;
    file: string;
    inVault: boolean;
}

export class KhojSimilarView extends KhojPaneView {
    static iconName: string = "search";
    setting: KhojSetting;
    currentController: AbortController | null = null;
    isLoading: boolean = false;
    loadingEl: HTMLElement;
    resultsContainerEl: HTMLElement;
    searchInputEl: HTMLInputElement;
    currentFile: TFile | null = null;
    fileWatcher: any;
    component: any;

    constructor(leaf: WorkspaceLeaf, setting: KhojSetting) {
        super(leaf, setting);
        this.setting = setting;
        this.component = this;
    }

    getViewType(): string {
        return KhojView.SIMILAR;
    }

    getDisplayText(): string {
        return "Khoj Similar Documents";
    }

    getIcon(): string {
        return "search";
    }

    async onOpen() {
        await super.onOpen();
        const { contentEl } = this;

        // Create main container
        const mainContainerEl = contentEl.createDiv({ cls: "khoj-similar-container" });

        // Create search input container
        const searchContainerEl = mainContainerEl.createDiv({ cls: "khoj-similar-search-container" });

        // Create search input
        this.searchInputEl = searchContainerEl.createEl("input", {
            cls: "khoj-similar-search-input",
            attr: {
                type: "text",
                placeholder: "Search or use current file"
            }
        });

        // Create refresh button
        const refreshButtonEl = searchContainerEl.createEl("button", {
            cls: "khoj-similar-refresh-button"
        });
        setIcon(refreshButtonEl, "refresh-cw");
        refreshButtonEl.createSpan({ text: "Refresh" });
        refreshButtonEl.addEventListener("click", () => {
            this.updateSimilarDocuments();
        });

        // Create results container
        this.resultsContainerEl = mainContainerEl.createDiv({ cls: "khoj-similar-results" });

        // Create loading element
        this.loadingEl = mainContainerEl.createDiv({ cls: "search-loading" });
        const spinnerEl = this.loadingEl.createDiv({ cls: "search-loading-spinner" });

        this.loadingEl.style.position = "absolute";
        this.loadingEl.style.top = "50%";
        this.loadingEl.style.left = "50%";
        this.loadingEl.style.transform = "translate(-50%, -50%)";
        this.loadingEl.style.zIndex = "1000";
        this.loadingEl.style.display = "none";

        // Register event handlers
        this.registerFileActiveHandler();

        // Add event listener for search input
        this.searchInputEl.addEventListener("keydown", (e) => {
            if (e.key === "Enter") {
                this.getSimilarDocuments(this.searchInputEl.value);
            }
        });

        // Update similar documents for current file
        this.updateSimilarDocuments();
    }

    /**
     * Register handler for file active events
     */
    registerFileActiveHandler() {
        // Clean up existing watcher if any
        if (this.fileWatcher) {
            this.app.workspace.off("file-open", this.fileWatcher);
        }

        // Set up new watcher
        this.fileWatcher = this.app.workspace.on("file-open", (file) => {
            if (file) {
                this.currentFile = file;
                this.updateSimilarDocuments();
            }
        });

        // Register for cleanup when view is closed
        this.register(() => {
            this.app.workspace.off("file-open", this.fileWatcher);
        });
    }

    /**
     * Update similar documents based on current file
     */
    async updateSimilarDocuments() {
        const file = this.app.workspace.getActiveFile();

        if (!file) {
            this.updateUI("no-file");
            return;
        }

        if (file.extension !== 'md') {
            this.updateUI("unsupported-file");
            return;
        }

        this.currentFile = file;

        // Read file content
        const content = await this.app.vault.read(file);

        // Get similar documents
        await this.getSimilarDocuments(content);
    }

    /**
     * Get similar documents from Khoj API
     *
     * @param query - The query text to find similar documents
     */
    async getSimilarDocuments(query: string): Promise<SimilarResult[]> {
        // Do not show loading if the query is empty
        if (!query.trim()) {
            this.isLoading = false;
            this.updateLoadingState();
            this.updateUI("empty-query");
            return [];
        }

        // Show loading state
        this.isLoading = true;
        this.updateLoadingState();
        this.updateUI("loading");

        // Cancel previous request if it exists
        if (this.currentController) {
            this.currentController.abort();
        }

        try {
            // Create a new controller for this request
            this.currentController = new AbortController();

            // Setup Query Khoj backend for search results
            let encodedQuery = encodeURIComponent(query);
            let searchUrl = `${this.setting.khojUrl}/api/search?q=${encodedQuery}&n=${this.setting.resultsCount}&r=true&client=obsidian`;
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
                .filter((result: any) => {
                    // Filter out the current file if it's in the results
                    if (this.currentFile && result.additional.file.endsWith(this.currentFile.path)) {
                        return false;
                    }
                    return true;
                })
                .map((result: any) => {
                    return {
                        entry: result.entry,
                        file: result.additional.file,
                        inVault: this.isFileInVault(result.additional.file)
                    } as SimilarResult;
                })
                .sort((a: SimilarResult, b: SimilarResult) => {
                    if (a.inVault === b.inVault) return 0;
                    return a.inVault ? -1 : 1;
                });

            // Hide loading state
            this.isLoading = false;
            this.updateLoadingState();

            // Render results
            this.renderResults(results);

            return results;
        } catch (error) {
            // Ignore cancellation errors
            if (error.name === 'AbortError') {
                return [];
            }

            // For other errors, show error state
            console.error('Search error:', error);
            this.isLoading = false;
            this.updateLoadingState();
            this.updateUI("error", error.message);
            return [];
        }
    }

    /**
     * Check if a file is in the vault
     *
     * @param filePath - The file path to check
     * @returns True if the file is in the vault
     */
    isFileInVault(filePath: string): boolean {
        const files = this.app.vault.getFiles();
        return files.some(file => filePath.endsWith(file.path));
    }

    /**
     * Render search results
     *
     * @param results - The search results to render
     */
    renderResults(results: SimilarResult[]) {
        // Clear previous results
        this.resultsContainerEl.empty();

        if (results.length === 0) {
            this.updateUI("no-results");
            return;
        }

        // Show results count
        this.resultsContainerEl.createEl("div", {
            cls: "khoj-results-count",
            text: `Found ${results.length} similar document${results.length > 1 ? 's' : ''}`
        });

        // Create results list
        const resultsListEl = this.resultsContainerEl.createEl("div", { cls: "khoj-similar-results-list" });

        // Render each result
        results.forEach(async (result) => {
            const resultEl = resultsListEl.createEl("div", { cls: "khoj-similar-result-item" });

            // Extract filename
            let os_path_separator = result.file.includes('\\') ? '\\' : '/';
            let filename = result.file.split(os_path_separator).pop();

            // Create header container for filename and more context button
            const headerEl = resultEl.createEl("div", { cls: "khoj-similar-result-header" });

            // Show filename with appropriate color
            const fileEl = headerEl.createEl("div", {
                cls: `khoj-result-file ${result.inVault ? 'in-vault' : 'not-in-vault'}`
            });
            fileEl.setText(filename ?? "");

            // Add indicator for files not in vault
            if (!result.inVault) {
                fileEl.createSpan({
                    text: " (not in vault)",
                    cls: "khoj-result-file-status"
                });
            }

            // Add "More context" button
            const moreContextButton = headerEl.createEl("button", {
                cls: "khoj-more-context-button"
            });
            moreContextButton.createSpan({ text: "More context" });
            setIcon(moreContextButton.createSpan(), "chevron-down");

            // Create content element (hidden by default)
            const contentEl = resultEl.createEl("div", {
                cls: "khoj-result-entry khoj-similar-content-hidden"
            });

            // Prepare content for rendering
            let contentToRender = "";

            // Remove YAML frontmatter
            result.entry = result.entry.replace(/---[\n\r][\s\S]*---[\n\r]/, '');

            // Truncate to 8 lines
            const lines_to_render = 8;
            let entry_snipped_indicator = result.entry.split('\n').length > lines_to_render ? ' **...**' : '';
            let snipped_entry = result.entry.split('\n').slice(0, lines_to_render).join('\n');
            contentToRender = `${snipped_entry}${entry_snipped_indicator}`;

            // Render markdown
            await MarkdownRenderer.renderMarkdown(
                contentToRender,
                contentEl,
                result.file,
                this.component
            );

            // Add click handler to the more context button
            moreContextButton.addEventListener("click", (e) => {
                e.stopPropagation(); // Prevent opening the file

                // Toggle content visibility
                if (contentEl.classList.contains("khoj-similar-content-hidden")) {
                    contentEl.classList.remove("khoj-similar-content-hidden");
                    contentEl.classList.add("khoj-similar-content-visible");
                    moreContextButton.empty();
                    moreContextButton.createSpan({ text: "Less context" });
                    setIcon(moreContextButton.createSpan(), "chevron-up");
                } else {
                    contentEl.classList.remove("khoj-similar-content-visible");
                    contentEl.classList.add("khoj-similar-content-hidden");
                    moreContextButton.empty();
                    moreContextButton.createSpan({ text: "More context" });
                    setIcon(moreContextButton.createSpan(), "chevron-down");
                }
            });

            // Add click handler to open the file
            resultEl.addEventListener("click", (e) => {
                // Don't open if clicking on the more context button
                if (e.target === moreContextButton || moreContextButton.contains(e.target as Node)) {
                    return;
                }
                this.openResult(result);
            });
        });
    }

    /**
     * Open a search result
     *
     * @param result - The result to open
     */
    async openResult(result: SimilarResult) {
        // Only open files that are in the vault
        if (!result.inVault) {
            new Notice("This file is not in your vault");
            return;
        }

        // Get all markdown and binary files in vault
        const mdFiles = this.app.vault.getMarkdownFiles();
        const binaryFiles = this.app.vault.getFiles().filter(file =>
            supportedBinaryFileTypes.includes(file.extension)
        );

        // Find and open the file
        let linkToEntry = getLinkToEntry(
            mdFiles.concat(binaryFiles),
            result.file,
            result.entry
        );

        if (linkToEntry) {
            this.app.workspace.openLinkText(linkToEntry, '');
        }
    }

    /**
     * Update the loading state
     */
    private updateLoadingState() {
        this.loadingEl.style.display = this.isLoading ? "block" : "none";
    }

    /**
     * Update the UI based on the current state
     *
     * @param state - The current state
     * @param message - Optional message for error states
     */
    updateUI(state: "loading" | "no-file" | "unsupported-file" | "no-results" | "error" | "empty-query", message?: string) {
        // Clear results container if not loading
        if (state !== "loading") {
            this.resultsContainerEl.empty();
        }

        // Create message element
        const messageEl = this.resultsContainerEl.createEl("div", { cls: "khoj-similar-message" });

        // Set message based on state
        switch (state) {
            case "loading":
                // Loading is handled by the loading spinner
                break;
            case "no-file":
                messageEl.setText("No file is currently open. Open a markdown file to see similar documents.");
                break;
            case "unsupported-file":
                messageEl.setText("This file type is not supported. Only markdown files are supported.");
                break;
            case "no-results":
                messageEl.setText("No similar documents found.");
                break;
            case "error":
                messageEl.setText(`Error: ${message || "Failed to fetch similar documents"}`);
                break;
            case "empty-query":
                messageEl.setText("Please enter a search query or open a markdown file.");
                break;
        }
    }
}
