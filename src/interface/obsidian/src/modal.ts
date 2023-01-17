import { App, SuggestModal, request, MarkdownRenderer, Instruction, Platform } from 'obsidian';
import { KhojSetting } from 'src/settings';

export interface SearchResult {
    entry: string;
    file: string;
}

export class KhojModal extends SuggestModal<SearchResult> {
    setting: KhojSetting;
    rerank: boolean = false;

    constructor(app: App, setting: KhojSetting) {
        super(app);
        this.setting = setting;

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

    async getSuggestions(query: string): Promise<SearchResult[]> {
        // Query Khoj backend for search results
        let searchUrl = `${this.setting.khojUrl}/api/search?q=${query}&n=${this.setting.resultsCount}&r=${this.rerank}&t=markdown`
        let results = await request(searchUrl)
            .then(response => JSON.parse(response))
            .then(data => {
                return data.map((result: any) => {
                    let processedResult: SearchResult = {
                        entry: result.entry,
                        file: result.additional.file
                    };
                    return processedResult;
                })
            });

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

        // Find the vault file matching file of result. Open file at result heading
        mdFiles
            // Sort by descending length of path
            // This finds longest path match when multiple files have same name
            .sort((a, b) => b.path.length - a.path.length)
            .forEach((file) => {
                // Find best file match across operating systems
                // E.g Khoj Server on Linux, Obsidian Vault on Android
                if (result.file.endsWith(file.path)) {
                    let resultHeading = result.entry.split('\n', 1)[0];
                    let linkToEntry = `${file.path}${resultHeading}`
                    this.app.workspace.openLinkText(linkToEntry, '');
                    console.log(`Link: ${linkToEntry}, File: ${file.path}, Heading: ${resultHeading}`);
                    return
                }
            });
    }
}
