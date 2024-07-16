import { Button } from "@/components/ui/button"

import styles from "./agentShortcut.module.css";
import { useRef, useState } from "react";

interface AgentShortcutProps {
    title: string;
    link: string;
    image: string;
    color: string;
    isSelected?: boolean;
    onSelect?: () => void;
}

export default function AgentShortcut(data: AgentShortcutProps) {
    const handleClick = (e: React.MouseEvent) => {
        if (data.image !== "") {
            e.preventDefault(); // Prevent default anchor behavior
            if (data.onSelect) {
                data.onSelect(); // Call the onSelect callback
            }
        }
    };

    const baseClassName = "bg-white text-black hover:bg-stone-100 transition-all duration-200";
    const selectedClassName = data.isSelected ? "border border-stone-400" : "border border-stone-100";


    if (data.image !== "") {
        return (
            <a className="no-underline" href={data.link} onClick={handleClick}>
                <Button className={`${baseClassName} ${selectedClassName}`}>
                    <img className="w-7 h-7 mr-1" src={data.image} alt=""/>
                    {data.title}
                </Button>
            </a>
        );
    }
    else {
        return (
            <a className="no-underline" href={data.link} onClick={handleClick}>
                <Button className={`${baseClassName} ${selectedClassName}`}>
                    {data.title}
                </Button>
            </a>
        );
    }
}
