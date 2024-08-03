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
import { converColorToBgGradient } from "@/app/common/colorUtils";


function convertSuggestionTitleToIconClass(title: string, color: string) {
    if (title.includes('automation')) return getIconFromIconName('Robot', color, 'w-8', 'h-8');
    if (title.includes('online')) return getIconFromIconName('Globe', color, 'w-8', 'h-8');
    if (title.includes('paint')) return getIconFromIconName('Palette', color, 'w-8', 'h-8');
    if (title.includes('pop')) return getIconFromIconName('Confetti', color, 'w-8', 'h-8');
    if (title.includes('travel')) return getIconFromIconName('Jeep', color, 'w-8', 'h-8');
    if (title.includes('learn')) return getIconFromIconName('Book', color, 'w-8', 'h-8');
    if (title.includes('health')) return getIconFromIconName('Asclepius', color, 'w-8', 'h-8');
    if (title.includes('fun')) return getIconFromIconName('Island', color, 'w-8', 'h-8');
    if (title.includes('home')) return getIconFromIconName('House', color, 'w-8', 'h-8');
    if (title.includes('language')) return getIconFromIconName('Translate', color, 'w-8', 'h-8');
    if (title.includes('code')) return getIconFromIconName('Code', color, 'w-8', 'h-8');

    else return getIconFromIconName('Lightbulb', color, 'w-8', 'h-8');
}


interface SuggestionCardProps {
    title: string;
    body: string;
    link: string;
    color: string;
}

export default function SuggestionCard(data: SuggestionCardProps) {
    const bgColors = converColorToBgGradient(data.color);
    const cardClassName = `${styles.card} ${bgColors} md:w-full md:h-fit sm:w-full sm:h-fit lg:w-[200px] lg:h-[200px] cursor-pointer`;
    const titleClassName = `${styles.title} pt-2 dark:text-white dark:font-bold`;
    const descriptionClassName = `${styles.text} dark:text-white`;

    const cardContent = (
        <Card className={cardClassName}>
            <CardHeader className="m-0 p-2 pb-1 relative">
                <div className="flex flex-row md:flex-col">
                    {convertSuggestionTitleToIconClass(data.title.toLowerCase(), data.color.toLowerCase())}
                    <CardTitle className={titleClassName}>{data.title}</CardTitle>
                </div>
            </CardHeader>
            <CardContent className="m-0 p-2 pr-4 pt-1">
                <CardDescription className={`${descriptionClassName} sm:line-clamp-2 md:line-clamp-4`}>
                    {data.body}
                </CardDescription>
            </CardContent>
        </Card>
    );

    return data.link ? (
        <a href={data.link} className="no-underline">
            {cardContent}
        </a>
    ) : cardContent;
}
