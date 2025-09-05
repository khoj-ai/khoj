"use client";

import { useIsDarkMode } from "@/app/common/utils";
import useKeyboardShortcuts from "@/hooks/useKeyboardShortcuts";
import { useRouter } from "next/navigation";

export function ThemeProvider({ children }: { children: React.ReactNode }) {
    const [darkMode, setDarkMode] = useIsDarkMode();
    const router = useRouter();

    useKeyboardShortcuts({
        "Alt+N": () => {
            router.push("/chat");
        },
        "Alt+H": () => router.push("/"),
        "Alt+A": () => router.push("/agents"),
        "Alt+U": () => router.push("/automations"),
        "Alt+K": () => router.push("/search"),
        "Alt+,": () => router.push("/settings"),
        "Alt+F": () => {
            const el = document.querySelector('input[placeholder="Find file"], input[placeholder="Search conversations"], input[placeholder="Search conversations"]');
            if (el && el instanceof HTMLElement) el.focus();
        },
    });

    return <>{children}</>;
}
