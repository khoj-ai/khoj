import { App, SuggestModal, request, MarkdownRenderer, Instruction, Platform } from 'obsidian';
import { KhojSetting } from 'src/settings';

export interface SearchResult {
    entry: string;
    file: string;
}

export class KhojModal extends SuggestModal<SearchResult> {
    setting: KhojSetting;
    rerank: boolean = false;
    find_similar_notes: boolean;
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
        this.setPlaceholder('Search with Khoj 🦅...');
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
                this.inputEl.value = await this.app.vault.read(file).then(file_str => file_str.slice(0, 8000));
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
        // Query Khoj backend for search results
        let encodedQuery = encodeURIComponent(query);
        let searchUrl = `${this.setting.khojUrl}/api/search?q=${encodedQuery}&n=${this.setting.resultsCount}&r=${this.rerank}&t=markdown`;
        let response = await request(searchUrl);
        let data = JSON.parse(response);
        let results = data
            .filter((result: any) => !this.find_similar_notes || !result.additional.file.endsWith(this.app.workspace.getActiveFile()?.path))
            .map((result: any) => { return { entry: result.entry, file: result.additional.file } as SearchResult; });

        return results;
    }

    async renderSuggestion(result: SearchResult, el: HTMLElement) {
        let words_to_render = 30;
        let entry_words = result.entry.split(' ')
        let entry_snipped_indicator = entry_words.length > words_to_render ? ' **...**' : '';
        let snipped_entry = entry_words.slice(0, words_to_render).join(' ');
        MarkdownRenderer.renderMarkdown(snipped_entry + entry_snipped_indicator, el, null, null);
    }

    async onChooseSuggestion(result: SearchResult, _: MouseEvent | KeyboardEvent) {
        // Get all markdown files in vault
        const mdFiles = this.app.vault.getMarkdownFiles();

        // Find the vault file matching file of chosen search result
        let file_match = mdFiles
            // Sort by descending length of path
            // This finds longest path match when multiple files have same name
            .sort((a, b) => b.path.length - a.path.length)
            // The first match is the best file match across OS
            // e.g Khoj server on Linux, Obsidian vault on Android
            .find(file => result.file.endsWith(file.path))

        // Open vault file at heading of chosen search result
        if (file_match){
            let resultHeading = result.entry.split('\n', 1)[0];
            let linkToEntry = `${file_match.path}${resultHeading}`
            this.app.workspace.openLinkText(linkToEntry, '');
            console.log(`Link: ${linkToEntry}, File: ${file_match.path}, Heading: ${resultHeading}`);
        }
    }
}
