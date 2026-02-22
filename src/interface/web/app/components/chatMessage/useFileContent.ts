import { useEffect, useState } from "react";

export interface UseFileContentResult {
    content: string;
    loading: boolean;
    error: string | null;
}

// Fetch file content for a given path when `enabled` is true.
export function useFileContent(path: string | undefined, enabled: boolean): UseFileContentResult {
    const [content, setContent] = useState<string>("");
    const [loading, setLoading] = useState<boolean>(false);
    const [error, setError] = useState<string | null>(null);

    useEffect(() => {
        let cancelled = false;
        async function run() {
            if (!enabled || !path) return;
            setLoading(true);
            setError(null);
            setContent("");
            try {
                const resp = await fetch(`/api/content/file?file_name=${encodeURIComponent(path)}`);
                if (!resp.ok) {
                    throw new Error(`Failed to fetch file content (${resp.status})`);
                }
                const data = await resp.json();
                if (!cancelled) setContent(data.raw_text || "");
            } catch (err) {
                if (!cancelled)
                    setError(err instanceof Error ? err.message : "Failed to load file content");
            } finally {
                if (!cancelled) setLoading(false);
            }
        }
        run();
        return () => {
            cancelled = true;
        };
    }, [path, enabled]);

    return { content, loading, error };
}
