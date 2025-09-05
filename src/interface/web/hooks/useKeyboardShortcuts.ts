"use client";

import { useEffect } from "react";

type ShortcutMap = Record<string, (event: KeyboardEvent) => void>;

function normalizeKeyCombo(combo: string) {
    // Normalize combos like "Alt+N" -> "alt+n"
    return combo
        .split("+")
        .map((p) => p.trim().toLowerCase())
        .join("+");
}

export default function useKeyboardShortcuts(shortcuts: ShortcutMap | null) {
    useEffect(() => {
        if (!shortcuts) return;

        const normalizedMap: Record<string, (event: KeyboardEvent) => void> = {};
        for (const key in shortcuts) {
            normalizedMap[normalizeKeyCombo(key)] = shortcuts[key];
        }

        const handler = (event: KeyboardEvent) => {

            const parts: string[] = [];
            if (event.altKey) parts.push("alt");
            if (event.ctrlKey) parts.push("ctrl");
            if (event.metaKey) parts.push("meta");
            if (event.shiftKey) parts.push("shift");

            // Normalize the key (comma stays comma)
            const key = event.key.length === 1 ? event.key.toLowerCase() : event.key.toLowerCase();
            parts.push(key);

            const combo = parts.join("+");

            const cb = normalizedMap[combo];
            if (cb) {
                event.preventDefault();
                cb(event);
            }
        };

        window.addEventListener("keydown", handler);
        return () => window.removeEventListener("keydown", handler);
    }, [shortcuts]);
}
