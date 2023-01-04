import { App, SuggestModal, Notice, request, MarkdownRenderer } from 'obsidian';
import { KhojSetting } from 'src/settings';
import { getVaultAbsolutePath } from 'src/utils';

export interface SearchResult {
    entry: string;
    file: string;
}

export class KhojModal extends SuggestModal<SearchResult> {
    setting: KhojSetting;

    constructor(app: App, setting: KhojSetting) {
        super(app);
        this.setting = setting;
    }

    async getSuggestions(query: string): Promise<SearchResult[]> {
        // Query Khoj backend for search results
        var searchUrl = `${this.setting.khojUrl}/api/search?q=${query}&n=${this.setting.resultsCount}&t=markdown`
        var results = await request(searchUrl)
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
        MarkdownRenderer.renderMarkdown(result.entry, el, null, null);
    }

    async onChooseSuggestion(result: SearchResult, _: MouseEvent | KeyboardEvent) {
        const mdFiles = this.app.vault.getMarkdownFiles();
        const vaultPath = getVaultAbsolutePath();

        if (!vaultPath) {
            new Notice('Khoj: Cannot open file. Make sure we are in a file system vault');
            return;
        }

        // Find the vault file matching file of result. Open file at result heading
        mdFiles.forEach((file) => {
            if (`${vaultPath}/${file.path}` === result.file) {
                let resultHeading = result.entry.split('\n', 1)[0];
                let linkToEntry = `${file.path}${resultHeading}`
                this.app.workspace.openLinkText(linkToEntry, '');
                console.log(`Link: ${linkToEntry}, File: ${file.path}, Heading: ${resultHeading}`);
                return
            }
        });
    }
}
