'use client'
import {
    Card,
    CardContent,
    CardDescription,
    CardHeader,
    CardTitle,
} from "@/components/ui/card"

import styles from "./suggestions.module.css";
import { getIconFromIconName } from "@/app/common/iconUtils";


function convertSuggestionColorToIconClass(color: string) {
    if (color.includes('blue')) return getIconFromIconName('Robot', 'blue', 'w-8', 'h-8');
    if (color.includes('yellow')) return getIconFromIconName('Globe', 'yellow', 'w-8', 'h-8');
    if (color.includes('green')) return getIconFromIconName('Palette', 'green', 'w-8', 'h-8');
    else return getIconFromIconName('Lightbulb', 'orange', 'w-8', 'h-8');
}


interface SuggestionCardProps {
    title: string;
    body: string;
    link: string;
    image: string;
    color: string;
}

export default function SuggestionCard(data: SuggestionCardProps) {
    const cardClassName = `${styles.card} ${data.color} md:w-full md:h-fit sm:w-full sm:h-fit lg:w-[200px] lg:h-[200px]`;
    const titleClassName = `${styles.title} pt-2 dark:text-white dark:font-bold`;
    const descriptionClassName = `${styles.text} dark:text-white`;

    const cardContent = (
        <Card className={cardClassName}>
            <CardHeader className="m-0 p-2 pb-1 relative">
                {convertSuggestionColorToIconClass(data.image)}
                <CardTitle className={titleClassName}>{data.title}</CardTitle>
            </CardHeader>
            <CardContent className="m-0 p-2 pr-4 pt-1">
                <CardDescription className={descriptionClassName}>{data.body}</CardDescription>
            </CardContent>
        </Card>
    );

    return data.link ? (
        <a href={data.link} className="no-underline">
            {cardContent}
        </a>
    ) : cardContent;
}
