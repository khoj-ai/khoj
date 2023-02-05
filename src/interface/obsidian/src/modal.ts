import { App, SuggestModal, request, MarkdownRenderer, Instruction, Platform} from 'obsidian';
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
        // Max Size (words or number of lines)
        let lines_to_render = 8;
        let words_to_render = 30;
        var path = "";
        var basename = "";

        // set path and basename of file depending on OS
        if (result.file.indexOf('\\') > -1) {
            path = result.file.split('\\').slice(0, -1).join('\\');
            basename = result.file.split('\\').slice(-1)[0];
        }
        if (result.file.indexOf('/') > -1) {
            path = result.file.split('/').slice(0, -1).join('/');
            basename = result.file.split('/').slice(-1)[0];
        }
        basename = basename.replace('.md', '');
        let entry_lines_split = result.entry.split('\n')
        // remove frontmatter if present
        if (entry_lines_split[0] == '# ---'){
            //loop through lines until we find the end of the frontmatter
            for (let i = 1; i < entry_lines_split.length; i++){
                if (entry_lines_split[i] == '---'){
                    //remove the frontmatter
                    entry_lines_split = entry_lines_split.slice(i+1);
                    break;
                }
            }
        }
        entry_lines_split = entry_lines_split.join('\n').trim().split('\n');
        let entry_lines = entry_lines_split.slice(0, lines_to_render).join('\n');
        let entry_words_split = entry_lines.split(' ');
        let entry_snipped_indicator = entry_words_split.length > words_to_render ? ' **...**' : '';
        let entry_words = entry_words_split.slice(0, words_to_render).join(' ').trim();
        el.createEl("div",{ cls: 'khoj-result-file' }).setText(basename);
        var result_div = el.createEl("div", { cls: 'khoj-result-entry' })
        MarkdownRenderer.renderMarkdown(entry_words + entry_snipped_indicator, result_div, path, null);
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
