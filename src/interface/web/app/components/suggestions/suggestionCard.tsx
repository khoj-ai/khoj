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

interface SuggestionCardProps {
	title: string;
	body: string;
	link: string;
	image: string;
	color: string;
}

export default function SuggestionCard(data: SuggestionCardProps) {
	return (
		<a href={data.link} className="no-underline">
		<Card className={styles.card} id={data.color}>
            <CardHeader className="m-0 p-2 pb-1 relative">
				<img className="w-8 h-8" src={data.image} alt="Icon"/>
                <CardTitle className={styles.title}>{data.title}</CardTitle>
            </CardHeader>
            <CardContent className="m-0 p-2 pr-4 pt-1 w-[200px] h-[100px]">
                <CardDescription className={styles.text}>{data.body}</CardDescription>
            </CardContent>
        </Card>
		</a>
	);
}
