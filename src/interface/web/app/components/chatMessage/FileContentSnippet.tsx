"use client";

import React from "react";

interface FileContentSnippetProps {
    content: string;
    targetLine?: number;
    maxLines?: number; // Used when no target line; defaults to 20
}

export default function FileContentSnippet({
    content,
    targetLine,
    maxLines = 20,
}: FileContentSnippetProps) {
    const lines = (content || "").split("\n");

    if (targetLine && targetLine > 0 && targetLine <= lines.length) {
        const startLine = Math.max(1, targetLine - 2);
        const endLine = Math.min(lines.length, targetLine + 5);
        const contextLines = lines.slice(startLine - 1, endLine);
        return (
            <pre className="whitespace-pre-wrap text-xs">
                {contextLines.map((line, idx) => {
                    const lineNum = startLine + idx;
                    const isTarget = lineNum === targetLine;
                    return (
                        <div
                            key={lineNum}
                            className={isTarget ? "bg-green-100 dark:bg-green-900" : ""}
                        >
                            <span className="text-gray-400 select-none mr-2">
                                {lineNum.toString().padStart(3, " ")}:
                            </span>
                            {line}
                        </div>
                    );
                })}
            </pre>
        );
    }

    const previewLines = lines.slice(0, maxLines);
    return (
        <pre className="whitespace-pre-wrap text-xs">
            {previewLines.map((line, index) => (
                <div key={index}>
                    <span className="text-gray-400 select-none mr-2">
                        {(index + 1).toString().padStart(3, " ")}:
                    </span>
                    {line}
                </div>
            ))}
            {lines.length > maxLines && (
                <div className="text-gray-500 italic">
                    ... and {lines.length - maxLines} more lines
                </div>
            )}
        </pre>
    );
}
