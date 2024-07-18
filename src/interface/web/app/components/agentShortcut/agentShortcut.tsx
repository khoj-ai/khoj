import { Button } from "@/components/ui/button"

import styles from "./agentShortcut.module.css";
import { useRef, useState } from "react";
import Image from 'next/image';

interface AgentShortcutProps {
    title: string;
    color: string;
    isSelected?: boolean;
    onSelect?: () => void;
}

export default function AgentShortcut(data: AgentShortcutProps) {
    const handleClick = (e: React.MouseEvent) => {
        e.preventDefault(); // Prevent default anchor behavior
        if (data.onSelect) {
            data.onSelect(); // Call the onSelect callback
        }
    };

    const baseClassName = "bg-white text-black hover:bg-stone-100 transition-all duration-200";
    const selectedClassName = data.isSelected ? "border border-stone-400" : "border border-stone-100";
    return (
        <Button className={`${baseClassName} ${selectedClassName}`}>
            {data.title}
        </Button>
    );
}
