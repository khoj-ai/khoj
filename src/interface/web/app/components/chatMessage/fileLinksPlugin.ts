import MarkdownIt from "markdown-it";

// File link renderer plugin for markdown-it
// Handles links of the form [text](file:///path/to/file) or [text](file:///path/to/file#line=123)
export function fileLinksPlugin(md: MarkdownIt) {
    // Store the original link_open renderer
    const defaultLinkOpenRenderer =
        md.renderer.rules.link_open ||
        function (tokens, idx, options, env, self) {
            return self.renderToken(tokens, idx, options);
        };

    // Override the link_open renderer
    md.renderer.rules.link_open = function (tokens, idx, options, env, self) {
        const token = tokens[idx];
        const hrefIndex = token.attrIndex("href");

        if (hrefIndex >= 0) {
            const href = token.attrs![hrefIndex][1];

            // Check if this is a filelink:// link (our preprocessed file:// links)
            if (href.startsWith("filelink://")) {
                // Extract file path and line number from filelink://path format
                const filePath = href.replace("filelink://", "");
                const fileMatch = filePath.match(/^(.+?)(?:#line=(\d+))?$/);

                if (fileMatch) {
                    const actualFilePath = fileMatch[1];
                    const lineNumber = fileMatch[2];

                    // Add custom attributes for file links
                    token.attrSet("data-file-path", actualFilePath);
                    if (lineNumber) {
                        token.attrSet("data-line-number", lineNumber);
                    }
                    // Append class if it exists; otherwise set it
                    const classIdx = token.attrIndex("class");
                    if (classIdx >= 0 && token.attrs) {
                        token.attrs[classIdx][1] = `${token.attrs[classIdx][1]} file-link`;
                    } else {
                        token.attrSet("class", "file-link");
                    }
                    token.attrSet("href", "#"); // Prevent default navigation
                    token.attrSet("role", "button");
                    token.attrSet("tabindex", "0");
                }
            }
        }

        return defaultLinkOpenRenderer(tokens, idx, options, env, self);
    };
}
