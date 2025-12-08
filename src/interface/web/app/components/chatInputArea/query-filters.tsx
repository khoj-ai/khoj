import React from "react";

export function getTriggerAtCaret(value: string, caretPos: number) {
    const before = value.slice(0, caretPos);
    const atMatch = before.match(/(?:^|\s)@([^\s@]*)$/);
    if (atMatch) return { kind: "at" as const, query: atMatch[1], triggerLen: atMatch[0].length };
    const fileMatch = before.match(/(?:^|\s)file:("?)([^"\s]*)$/i);
    if (fileMatch) return { kind: "file" as const, query: fileMatch[2], triggerLen: fileMatch[0].length };

    const dateModeMatch = before.match(/(?:^|\s)(?:date|dt)((?:>=|<=):?|:)(\"?)([^\"\s]*)$/i);
    if (dateModeMatch) return { kind: "dateMode" as const, query: dateModeMatch[3], triggerLen: dateModeMatch[0].length, operator: dateModeMatch[1] };

    const dateMatch = before.match(/(?:^|\s)(?:date|dt):?([^\s\"]*)$/i);
    if (dateMatch) return { kind: "date" as const, query: dateMatch[1] || "", triggerLen: dateMatch[0].length };

    const wordModeMatch = before.match(/(?:^|\s)([+-])\"([^\"]*)$/);
    if (wordModeMatch) return { kind: "wordMode" as const, query: wordModeMatch[2], triggerLen: wordModeMatch[0].length, sign: wordModeMatch[1] as "+" | "-" };

    return null;
}

export type MenuLevel =
    | "file"
    | "date"
    | "dateMode"
    | "word"
    | "wordMode"
    | "main";

interface Props {
    menuLevel: MenuLevel | null;
    suggestions: string[];
    highlightIndex: number;
    setHighlightIndex: (i: number) => void;
    chatInputRef: React.RefObject<HTMLTextAreaElement>;
    insertSuggestionAtCaret: (ta: HTMLTextAreaElement, s: string) => void;
    insertTriggerToken: (ta: HTMLTextAreaElement, token: string, newMenuLevel: any, levelOverride?: any) => void;
    prepareDateOperatorInfo: (choice: string) => void;
    prepareWordOperatorInfo: (sign: "+" | "-") => void;
    setDateMode: (m: any) => void;
    setWordMode: (m: any) => void;
    setMenuLevel: (m: any) => void;
    insertDateFilterFromCaret: (ta: HTMLTextAreaElement) => void;
    insertWordFilterFromCaret: (ta: HTMLTextAreaElement) => void;
}

export default function QueryFiltersPopover(props: Props) {
    const {
        menuLevel,
        suggestions,
        highlightIndex,
        setHighlightIndex,
        chatInputRef,
        insertSuggestionAtCaret,
        insertTriggerToken,
        prepareDateOperatorInfo,
        prepareWordOperatorInfo,
        setDateMode,
        setWordMode,
        setMenuLevel,
        insertDateFilterFromCaret,
        insertWordFilterFromCaret,
    } = props;

    const displayList =
        menuLevel === "file"
            ? suggestions
            : menuLevel === "date"
                ? ["exact", "after", "before"]
                : menuLevel === "dateMode"
                    ? suggestions
                    : menuLevel === "word"
                        ? ["include", "exclude"]
                        : menuLevel === "wordMode"
                            ? suggestions
                            : menuLevel === "main"
                                ? ["file", "date", "word"]
                                : [];

    if (displayList.length === 0) return null;

    return (
        <div className="absolute z-50 bg-background border rounded-md mt-2 w-80 max-h-64 overflow-auto shadow-lg">
            {displayList.map((s, idx) => (
                <div
                    key={s + idx}
                    className={`p-2 cursor-pointer ${idx === highlightIndex ? "bg-sky-100" : ""}`}
                    onMouseDown={(ev) => {
                        ev.preventDefault();
                        if (menuLevel === "file") insertSuggestionAtCaret(chatInputRef.current!, s);
                        else if (menuLevel === "main") {
                            const choice = String(s).toLowerCase();
                            if (choice === "file") insertTriggerToken(chatInputRef.current!, "file:", "file", "file");
                            else if (choice === "date") insertTriggerToken(chatInputRef.current!, "dt", "date", "date");
                            else if (choice === "word") setMenuLevel("word");
                        } else if (menuLevel === "date") {
                            const choice = String(s).toLowerCase();
                            if (choice === "exact") setDateMode("exact");
                            else if (choice === "after") setDateMode(">=");
                            else setDateMode("<=");
                            prepareDateOperatorInfo(choice);
                            setMenuLevel("dateMode");
                        } else if (menuLevel === "dateMode") {
                            insertDateFilterFromCaret(chatInputRef.current!);
                        } else if (menuLevel === "word") {
                            const choice = String(s).toLowerCase();
                            const sign = choice === "include" ? "+" : "-";
                            const token = sign + '"';
                            insertTriggerToken(chatInputRef.current!, token, "wordMode", "wordMode");
                            setWordMode(choice === "include" ? "include" : "exclude");
                            setTimeout(() => prepareWordOperatorInfo(sign as "+" | "-"), 0);
                        } else if (menuLevel === "wordMode") {
                            insertWordFilterFromCaret(chatInputRef.current!);
                        }
                    }}
                    onMouseEnter={() => setHighlightIndex(idx)}
                >
                    <div className="text-sm truncate">{menuLevel === "file" ? s.split("/").pop() : s}</div>
                    {menuLevel === "file" && <div className="text-xs text-muted-foreground truncate">{s}</div>}
                </div>
            ))}
        </div>
    );
}
