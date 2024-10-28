"use client";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";

import styles from "./suggestions.module.css";
import { converColorToBgGradient } from "@/app/common/colorUtils";
import { convertSuggestionTitleToIconClass } from "./suggestionsData";

interface SuggestionCardProps {
    title: string;
    body: string;
    link: string;
    color: string;
}

export default function SuggestionCard(data: SuggestionCardProps) {
    const bgColors = converColorToBgGradient(data.color);
    const cardClassName = `${styles.card} ${bgColors} md:w-full md:h-fit sm:w-full h-fit md:w-[200px] md:h-[180px] cursor-pointer md:p-2`;
    const descriptionClassName = `${styles.text} dark:text-white`;

    const cardContent = (
        <Card className={cardClassName}>
            <div className="flex">
                <CardContent className="m-0 p-2">
                    {convertSuggestionTitleToIconClass(data.title, data.color.toLowerCase())}
                    <CardDescription
                        className={`${descriptionClassName} sm:line-clamp-2 md:line-clamp-4 pt-1`}
                    >
                        {data.body}
                    </CardDescription>
                </CardContent>
            </div>
        </Card>
    );

    return data.link ? (
        <a href={data.link} className="no-underline">
            {cardContent}
        </a>
    ) : (
        cardContent
    );
}
