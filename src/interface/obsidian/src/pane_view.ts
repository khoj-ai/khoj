import { ItemView, WorkspaceLeaf } from 'obsidian';
import { KhojSetting } from 'src/settings';
import { KhojView, populateHeaderPane } from './utils';

export abstract class KhojPaneView extends ItemView {
    setting: KhojSetting;

    constructor(leaf: WorkspaceLeaf, setting: KhojSetting) {
        super(leaf);

        this.setting = setting;

        // Register Modal Keybindings to send user message
        // this.scope.register([], 'Enter', async () => { await this.chat() });
    }

    async onOpen() {
        let { contentEl } = this;

        // Add title to the Khoj Chat modal
        let headerEl = contentEl.createDiv(({ attr: { id: "khoj-header", class: "khoj-header" } }));

        // Setup the header pane
        const viewType = this.getViewType();
        await populateHeaderPane(headerEl, this.setting, viewType);

        // Set the active nav pane based on the current view's type
        if (viewType === KhojView.CHAT) {
            headerEl.querySelector(".chat-nav")?.classList.add("khoj-nav-selected");
        } else if (viewType === KhojView.SIMILAR) {
            headerEl.querySelector(".similar-nav")?.classList.add("khoj-nav-selected");
        }
        // The similar-nav event listener is already set in utils.ts
        let similarNavSvgEl = headerEl.getElementsByClassName("khoj-nav-icon-similar")[0]?.firstElementChild;
        if (!!similarNavSvgEl) similarNavSvgEl.id = "similar-nav-icon-svg";
    }

    async activateView(viewType: string) {
        const { workspace } = this.app;

        let leaf: WorkspaceLeaf | null = null;
        const leaves = workspace.getLeavesOfType(viewType);

        if (leaves.length > 0) {
            // A leaf with our view already exists, use that
            leaf = leaves[0];
        } else {
            // Our view could not be found in the workspace, create a new leaf
            // in the right sidebar for it
            leaf = workspace.getRightLeaf(false);
            await leaf?.setViewState({ type: viewType, active: true });
        }

        if (leaf) {
            if (viewType === KhojView.CHAT) {
                // focus on the chat input when the chat view is opened
                let chatInput = <HTMLTextAreaElement>this.contentEl.getElementsByClassName("khoj-chat-input")[0];
                if (chatInput) chatInput.focus();
            }

            // "Reveal" the leaf in case it is in a collapsed sidebar
            workspace.revealLeaf(leaf);
        }
    }
}
