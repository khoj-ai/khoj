'use client'
import {
	Card,
	CardContent,
	CardDescription,
	CardHeader,
	CardTitle,
} from "@/components/ui/card"

import styles from "./suggestions.module.css";
import { useRef } from "react";

import {
    Lightbulb,
    Robot,
    Aperture,
    GraduationCap,
    Jeep,
    Island,
    MathOperations,
    Asclepius,
    Couch,
    Code,
    Atom,
    ClockCounterClockwise,
    PaperPlaneTilt,
    Info,
    UserCircle,
    Globe,
    Palette,
} from "@phosphor-icons/react";
import { get } from "http";

interface IconMap {
    [key: string]: (color: string, width: string, height: string) => JSX.Element | null;
}

const iconMap: IconMap = {
    Lightbulb: (color: string, width: string, height: string) => <Lightbulb className={`${width} ${height} ${color} mr-2`} />,
    Robot: (color: string, width: string, height: string) => <Robot className={`${width} ${height} ${color} mr-2`} />,
    Aperture: (color: string, width: string, height: string) => <Aperture className={`${width} ${height} ${color} mr-2`} />,
    GraduationCap: (color: string, width: string, height: string) => <GraduationCap className={`${width} ${height} ${color} mr-2`} />,
    Jeep: (color: string, width: string, height: string) => <Jeep className={`${width} ${height} ${color} mr-2`} />,
    Island: (color: string, width: string, height: string) => <Island className={`${width} ${height} ${color} mr-2`} />,
    MathOperations: (color: string, width: string, height: string) => <MathOperations className={`${width} ${height} ${color} mr-2`} />,
    Asclepius: (color: string, width: string, height: string) => <Asclepius className={`${width} ${height} ${color} mr-2`} />,
    Couch: (color: string, width: string, height: string) => <Couch className={`${width} ${height} ${color} mr-2`} />,
    Code: (color: string, width: string, height: string) => <Code className={`${width} ${height} ${color} mr-2`} />,
    Atom: (color: string, width: string, height: string) => <Atom className={`${width} ${height} ${color} mr-2`} />,
    ClockCounterClockwise: (color: string, width: string, height: string) => <ClockCounterClockwise className={`${width} ${height} ${color} mr-2`} />,
    Globe: (color: string, width: string, height: string) => <Globe className={`${width} ${height} ${color} mr-2`} />,
    Palette: (color: string, width: string, height: string) => <Palette className={`${width} ${height} ${color} mr-2`} />,
};

function convertColorToTextClass(color: string) {
    if (color === 'red') return `text-red-500`;
    if (color === 'yellow') return `text-yellow-500`;
    if (color === 'green') return `text-green-500`;
    if (color === 'blue') return `text-blue-500`;
    if (color === 'orange') return `text-orange-500`;
    if (color === 'purple') return `text-purple-500`;
    if (color === 'pink') return `text-pink-500`;
    if (color === 'teal') return `text-teal-500`;
    if (color === 'cyan') return `text-cyan-500`;
    if (color === 'lime') return `text-lime-500`;
    if (color === 'indigo') return `text-indigo-500`;
    if (color === 'fuschia') return `text-fuschia-500`;
    if (color === 'rose') return `text-rose-500`;
    if (color === 'sky') return `text-sky-500`;
    if (color === 'amber') return `text-amber-500`;
    if (color === 'emerald') return `text-emerald-500`;
    return `text-gray-500`;
}

function getIconFromIconName(iconName: string, color: string = 'gray', width: string = 'w-8', height: string = 'h-8') {
    const icon = iconMap[iconName];
    const colorName = color.toLowerCase();
    const colorClass = convertColorToTextClass(colorName);

    return icon ? icon(colorClass, width, height) : null;
}

function convertSuggestionColorToIconClass(color: string) {
    if (color.includes('sky')) return getIconFromIconName('Robot', 'blue', 'w-8', 'h-8');
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
    if (data.link !== "") {
        return (
            <a href={data.link} className="no-underline">
            <Card className={`${styles.card} ${data.color}`}>
                <CardHeader className="m-0 p-2 pb-1 relative">
                    {convertSuggestionColorToIconClass(data.image)}
                    <CardTitle className={`${styles.title} pt-2`}>{data.title}</CardTitle>
                </CardHeader>
                <CardContent className="m-0 p-2 pr-4 pt-1 w-[200px] h-[100px]">
                    <CardDescription className={styles.text}>{data.body}</CardDescription>
                </CardContent>
            </Card>
            </a>
        );
    }
    return (
        <Card className={`${styles.card} ${data.color}`}>
            <CardHeader className="m-0 p-2 pb-1 relative">
                {convertSuggestionColorToIconClass(data.image)}
                <CardTitle className={`${styles.title} pt-2`}>{data.title}</CardTitle>
            </CardHeader>
            <CardContent className="m-0 p-2 pr-4 pt-1 w-[200px] h-[100px]">
                <CardDescription className={styles.text}>{data.body}</CardDescription>
            </CardContent>
        </Card>
    );
}
