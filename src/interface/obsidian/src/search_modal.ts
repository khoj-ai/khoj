import { App, SuggestModal, request, MarkdownRenderer, Instruction, Platform } from 'obsidian';
import { KhojSetting } from 'src/settings';
import { createNoteAndCloseModal } from 'src/utils';

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

    constructor(app: App, setting: KhojSetting, find_similar_notes: boolean = false) {
        super(app);
        this.app = app;
        this.setting = setting;
        this.find_similar_notes = find_similar_notes;

        // Hide input element in Similar Notes mode
        this.inputEl.hidden = this.find_similar_notes;

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
                this.inputEl.value = await this.app.vault.read(file).then(file_str => file_str.slice(0, 42110));
                // Trigger search to get and render similar notes from khoj backend
                this.inputEl.dispatchEvent(new Event('input'));
                this.rerank = false
            }
            else {
                this.resultContainerEl.setText('Cannot find similar notes for non-markdown files');
            }
        }
    }

    async getSuggestions(query: string): Promise<SearchResult[]> {
        // Setup Query Khoj backend for search results
        let encodedQuery = encodeURIComponent(query);
        let searchUrl = `${this.setting.khojUrl}/api/search?q=${encodedQuery}&n=${this.setting.resultsCount}&r=${this.rerank}&client=obsidian`;
        let headers = { 'Authorization': `Bearer ${this.setting.khojApiKey}` }

        // Get search results from Khoj backend
        let response = await request({ url: `${searchUrl}`, headers: headers });

        // Parse search results
        let results = JSON.parse(response)
            .filter((result: any) => !this.find_similar_notes || !result.additional.file.endsWith(this.app.workspace.getActiveFile()?.path))
            .map((result: any) => { return { entry: result.entry, file: result.additional.file } as SearchResult; });

        this.query = query;
        return results;
    }

    async renderSuggestion(result: SearchResult, el: HTMLElement) {
        // Max number of lines to render
        let lines_to_render = 8;

        // Extract filename of result
        let os_path_separator = result.file.includes('\\') ? '\\' : '/';
        let filename = result.file.split(os_path_separator).pop();

        // Remove YAML frontmatter when rendering string
        result.entry = result.entry.replace(/---[\n\r][\s\S]*---[\n\r]/, '');

        // Truncate search results to lines_to_render
        let entry_snipped_indicator = result.entry.split('\n').length > lines_to_render ? ' **...**' : '';
        let snipped_entry = result.entry.split('\n').slice(0, lines_to_render).join('\n');

        // Show filename of each search result for context
        el.createEl("div",{ cls: 'khoj-result-file' }).setText(filename ?? "");
        let result_el = el.createEl("div", { cls: 'khoj-result-entry' })

        // @ts-ignore
        MarkdownRenderer.renderMarkdown(snipped_entry + entry_snipped_indicator, result_el, result.file, null);
    }

    async onChooseSuggestion(result: SearchResult, _: MouseEvent | KeyboardEvent) {
        // Get all markdown and PDF files in vault
        const mdFiles = this.app.vault.getMarkdownFiles();
        const pdfFiles = this.app.vault.getFiles().filter(file => file.extension === 'pdf');

        // Find the vault file matching file of chosen search result
        let file_match = mdFiles.concat(pdfFiles)
            // Sort by descending length of path
            // This finds longest path match when multiple files have same name
            .sort((a, b) => b.path.length - a.path.length)
            // The first match is the best file match across OS
            // e.g Khoj server on Linux, Obsidian vault on Android
            .find(file => result.file.replace(/\\/g, "/").endsWith(file.path))

        // Open vault file at heading of chosen search result
        if (file_match) {
            let resultHeading = file_match.extension !== 'pdf' ? result.entry.split('\n', 1)[0] : '';
            let linkToEntry = resultHeading.startsWith('#') ? `${file_match.path}${resultHeading}` : file_match.path;
            this.app.workspace.openLinkText(linkToEntry, '');
            console.log(`Link: ${linkToEntry}, File: ${file_match.path}, Heading: ${resultHeading}`);
        }
    }
}
