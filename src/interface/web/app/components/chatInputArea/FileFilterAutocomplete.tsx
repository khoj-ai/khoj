import React, { forwardRef, useEffect, useImperativeHandle, useRef, useState } from "react";
import { CircleNotch } from "@phosphor-icons/react";

import { getIconFromFilename } from "@/app/common/iconUtils";

const MAX_VISIBLE_SUGGESTIONS = 8;

interface FileListItem {
    file_name: string;
}

interface FileListResponse {
    files: FileListItem[];
    num_pages: number;
}

interface FileFilterAutocompleteProps {
    query: string;
    onSelect: (file: string) => void;
    onClose: () => void;
    isVisible: boolean;
}

type FileFilterAutocompleteKeyboardEvent = Pick<
    React.KeyboardEvent<HTMLTextAreaElement>,
    "altKey" | "ctrlKey" | "key" | "metaKey" | "preventDefault" | "shiftKey"
>;

export interface FileFilterAutocompleteHandle {
    handleKeyDown: (event: FileFilterAutocompleteKeyboardEvent) => boolean;
}

let cachedFilePaths: string[] | null = null;
let pendingFilePathsRequest: Promise<string[]> | null = null;

async function fetchAllFiles() {
    if (cachedFilePaths) {
        return cachedFilePaths;
    }

    if (pendingFilePathsRequest) {
        return pendingFilePathsRequest;
    }

    pendingFilePathsRequest = (async () => {
        const collectedFiles: string[] = [];
        let currentPage = 0;
        let totalPages = 1;

        while (currentPage < totalPages) {
            const response = await fetch(`/api/content/files?page=${currentPage}`);

            if (!response.ok) {
                throw new Error(`Failed to fetch files: ${response.status}`);
            }

            const data = (await response.json()) as FileListResponse;
            collectedFiles.push(...data.files.map((file) => file.file_name));
            totalPages = Math.max(data.num_pages || 0, currentPage + 1);
            currentPage += 1;
        }

        cachedFilePaths = Array.from(new Set(collectedFiles)).sort((firstFile, secondFile) =>
            firstFile.localeCompare(secondFile),
        );

        return cachedFilePaths;
    })();

    try {
        return await pendingFilePathsRequest;
    } finally {
        pendingFilePathsRequest = null;
    }
}

function getMatchingFiles(allFiles: string[], query: string) {
    const normalizedQuery = query.trim().toLowerCase();
    const matchingFiles = normalizedQuery
        ? allFiles.filter((file) => file.toLowerCase().includes(normalizedQuery))
        : allFiles;

    return matchingFiles.slice(0, MAX_VISIBLE_SUGGESTIONS);
}

function getDisplayFileName(filePath: string) {
    const pathParts = filePath.split(/[\\/]/);
    return pathParts[pathParts.length - 1] || filePath;
}

const FileFilterAutocomplete = forwardRef<
    FileFilterAutocompleteHandle,
    FileFilterAutocompleteProps
>((props, ref) => {
    const containerRef = useRef<HTMLDivElement>(null);
    const [files, setFiles] = useState<string[]>(cachedFilePaths ?? []);
    const [loading, setLoading] = useState(cachedFilePaths === null);
    const [error, setError] = useState<string | null>(null);
    const [activeIndex, setActiveIndex] = useState(0);

    const matchingFiles = getMatchingFiles(files, props.query);

    useEffect(() => {
        if (!props.isVisible) {
            return;
        }

        let isCancelled = false;

        if (cachedFilePaths) {
            setFiles(cachedFilePaths);
            setLoading(false);
            setError(null);
            return;
        }

        setLoading(true);
        setError(null);

        fetchAllFiles()
            .then((loadedFiles) => {
                if (isCancelled) {
                    return;
                }

                setFiles(loadedFiles);
            })
            .catch((error) => {
                if (isCancelled) {
                    return;
                }

                console.error("Error loading synced files:", error);
                setError("Failed to load synced files.");
            })
            .finally(() => {
                if (isCancelled) {
                    return;
                }

                setLoading(false);
            });

        return () => {
            isCancelled = true;
        };
    }, [props.isVisible]);

    useEffect(() => {
        if (props.isVisible) {
            setActiveIndex(0);
        }
    }, [files.length, props.isVisible, props.query]);

    useEffect(() => {
        if (!props.isVisible) {
            return;
        }

        function handlePointerDown(event: MouseEvent | TouchEvent) {
            if (containerRef.current?.contains(event.target as Node)) {
                return;
            }

            props.onClose();
        }

        document.addEventListener("mousedown", handlePointerDown);
        document.addEventListener("touchstart", handlePointerDown);

        return () => {
            document.removeEventListener("mousedown", handlePointerDown);
            document.removeEventListener("touchstart", handlePointerDown);
        };
    }, [props.isVisible, props.onClose]);

    useImperativeHandle(
        ref,
        () => ({
            handleKeyDown(event) {
                if (!props.isVisible || event.altKey || event.ctrlKey || event.metaKey) {
                    return false;
                }

                if (event.key === "Escape") {
                    event.preventDefault();
                    props.onClose();
                    return true;
                }

                if (loading || error || matchingFiles.length === 0) {
                    return false;
                }

                if (event.key === "ArrowDown") {
                    event.preventDefault();
                    setActiveIndex((previousIndex) => (previousIndex + 1) % matchingFiles.length);
                    return true;
                }

                if (event.key === "ArrowUp") {
                    event.preventDefault();
                    setActiveIndex(
                        (previousIndex) =>
                            (previousIndex - 1 + matchingFiles.length) % matchingFiles.length,
                    );
                    return true;
                }

                if (event.key === "Enter" && !event.shiftKey) {
                    event.preventDefault();

                    const selectedFile = matchingFiles[activeIndex] || matchingFiles[0];
                    if (!selectedFile) {
                        return false;
                    }

                    props.onSelect(selectedFile);
                    return true;
                }

                return false;
            },
        }),
        [
            activeIndex,
            error,
            loading,
            matchingFiles,
            props.isVisible,
            props.onClose,
            props.onSelect,
        ],
    );

    if (!props.isVisible) {
        return null;
    }

    return (
        <div
            ref={containerRef}
            className="absolute left-0 right-0 top-full z-50 mt-2 overflow-hidden rounded-md border bg-popover text-popover-foreground shadow-md"
        >
            <div className="border-b px-3 py-2 text-xs font-medium text-muted-foreground">
                Synced files
            </div>
            {loading ? (
                <div className="flex items-center gap-2 px-3 py-4 text-sm text-muted-foreground">
                    <CircleNotch className="h-4 w-4 animate-spin" />
                    Loading synced files...
                </div>
            ) : error ? (
                <div className="px-3 py-4 text-sm text-destructive">{error}</div>
            ) : matchingFiles.length === 0 ? (
                <div className="px-3 py-4 text-sm text-muted-foreground">
                    {props.query ? `No files match "${props.query}".` : "No synced files found."}
                </div>
            ) : (
                <div className="max-h-72 overflow-y-auto py-1">
                    {matchingFiles.map((file, index) => {
                        const displayName = getDisplayFileName(file);
                        const isActive = index === activeIndex;

                        return (
                            <button
                                key={file}
                                type="button"
                                aria-selected={isActive}
                                className={`flex w-full items-start gap-3 px-3 py-2 text-left transition-colors ${
                                    isActive
                                        ? "bg-accent text-accent-foreground"
                                        : "hover:bg-accent/60"
                                }`}
                                onMouseEnter={() => setActiveIndex(index)}
                                onClick={() => props.onSelect(file)}
                            >
                                <span className="mt-0.5 shrink-0">
                                    {getIconFromFilename(file, "h-4 w-4 text-muted-foreground")}
                                </span>
                                <span className="min-w-0">
                                    <span className="block truncate text-sm font-medium">
                                        {displayName}
                                    </span>
                                    {displayName !== file && (
                                        <span className="block truncate text-xs text-muted-foreground">
                                            {file}
                                        </span>
                                    )}
                                </span>
                            </button>
                        );
                    })}
                </div>
            )}
        </div>
    );
});

FileFilterAutocomplete.displayName = "FileFilterAutocomplete";

export default FileFilterAutocomplete;
