import MarkdownIt from "markdown-it";

/**
 * Checks if a URL is a valid, loadable image URL.
 * Returns true for URLs that browsers can actually fetch:
 * - data: URLs (base64 encoded)
 * - blob: URLs (object URLs)
 * - http:// and https:// URLs
 */
function isValidImageUrl(url: string): boolean {
    if (!url || typeof url !== "string") {
        return false;
    }

    const trimmedUrl = url.trim();

    // Allow data URLs (base64 encoded images)
    if (trimmedUrl.startsWith("data:")) {
        return true;
    }

    // Allow blob URLs
    if (trimmedUrl.startsWith("blob:")) {
        return true;
    }

    // Allow HTTP/HTTPS URLs
    if (trimmedUrl.startsWith("http://") || trimmedUrl.startsWith("https://")) {
        return true;
    }

    // Reject everything else (file://, relative paths, absolute paths, etc.)
    return false;
}

/**
 * Image validation plugin for markdown-it
 * Filters out images with invalid/non-loadable URLs at render time.
 * This prevents broken images from ever being added to the DOM.
 */
export function imageValidationPlugin(md: MarkdownIt) {
    // Store the original image renderer
    const defaultImageRenderer =
        md.renderer.rules.image ||
        function (tokens, idx, options, env, self) {
            return self.renderToken(tokens, idx, options);
        };

    // Override the image renderer
    md.renderer.rules.image = function (tokens, idx, options, env, self) {
        const token = tokens[idx];
        const srcIndex = token.attrIndex("src");

        if (srcIndex >= 0 && token.attrs) {
            const src = token.attrs[srcIndex][1];

            // If the URL is not valid, don't render the image at all
            if (!isValidImageUrl(src)) {
                // Return alt text as fallback, or empty string
                const altText = token.content || "";
                if (altText) {
                    return `<em style="color: #888; font-size: 0.9em;">[Image: ${md.utils.escapeHtml(altText)}]</em>`;
                }
                return "";
            }
        }

        return defaultImageRenderer(tokens, idx, options, env, self);
    };
}
