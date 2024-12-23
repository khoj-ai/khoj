"use client";
import { Card, CardContent, CardDescription } from "@/components/ui/card";

import styles from "./suggestions.module.css";
import { convertSuggestionTitleToIconClass } from "./suggestionsData";
import { ArrowLeft, ArrowRight, MagicWand, XCircle } from "@phosphor-icons/react";

interface StepOneSuggestionCardProps {
    title: string;
    body: string;
    color: string;
}

interface StepOneSuggestionRevertCardProps extends StepOneSuggestionCardProps {
    onClick: () => void;
}

interface StepTwoSuggestionCardProps {
    prompt: string;
}

export function StepOneSuggestionCard(data: StepOneSuggestionCardProps) {
    const cardClassName = `${styles.card} md:w-full md:h-fit sm:w-full h-fit md:w-[200px] cursor-pointer md:p-2 animate-fade-in-up`;
    const descriptionClassName = `${styles.text} dark:text-white`;

    const cardContent = (
        <Card className={cardClassName}>
            <div className="flex w-full">
                <CardContent className="m-0 p-2 w-full flex flex-row">
                    {convertSuggestionTitleToIconClass(data.title, data.color.toLowerCase())}
                    <CardDescription
                        className={`${descriptionClassName} sm:line-clamp-2 md:line-clamp-4 pt-1 break-words whitespace-pre-wrap max-w-full`}
                    >
                        {data.body}
                    </CardDescription>
                </CardContent>
            </div>
        </Card>
    );

    return cardContent;
}

export function StepTwoSuggestionCard(data: StepTwoSuggestionCardProps) {
    const cardClassName = `${styles.card} md:h-fit sm:w-full h-fit cursor-pointer md:p-2`;

    return (
        <Card className={cardClassName}>
            <div className="flex w-full items-center">
                <CardContent className="m-0 p-1 w-full flex flex-row items-center">
                    <MagicWand
                        weight="thin"
                        className="w-4 h-4 text-muted-foreground inline-flex mr-1 text-opacity-40"
                    />
                    <CardDescription
                        className={`sm:line-clamp-2 md:line-clamp-4 break-words whitespace-pre-wrap max-w-full text-sm text-wrap text-black dark:text-white`}
                    >
                        {data.prompt}
                    </CardDescription>
                </CardContent>
            </div>
        </Card>
    );
}

export function StepOneSuggestionRevertCard(data: StepOneSuggestionRevertCardProps) {
    const cardClassName = `${styles.card} md:w-full md:h-fit sm:w-full h-fit md:w-fit cursor-pointer m-2 md:p-1 animate-fade-in-up border-opacity-50 shadow-none`;
    const descriptionClassName = `${styles.text} dark:text-white`;

    return (
        <Card className={cardClassName} onClick={data.onClick}>
            <div className="flex w-full h-full items-center justify-center">
                <CardContent className="m-0 p-2 w-full flex flex-row items-center justify-center">
                    {convertSuggestionTitleToIconClass(data.title, data.color.toLowerCase())}
                    <CardDescription
                        className={`${descriptionClassName} sm:line-clamp-2 md:line-clamp-4 pt-1 break-words whitespace-pre-wrap max-w-full text-center`}
                    >
                        {data.body}
                    </CardDescription>
                    <XCircle className="w-6 h-6 text-muted-foreground inline-flex ml-1" />
                </CardContent>
            </div>
        </Card>
    );
}
