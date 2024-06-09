'use client'

import styles from "./suggestions.module.css";

interface SuggestionCardProps {
	title: string;
	body: string;
	link: string;
	styleClass: string;
}

export default function SuggestionCard(data: SuggestionCardProps) {

	return (
		<div className={styles[data.styleClass] + " " + styles.card}>
			<div className={styles.title}>
				{data.title}
			</div>
			<div className={styles.body}>
				{data.body}
			</div>
			<div>
				<a
					href={data.link}
					target="_blank"
					rel="noopener noreferrer"
				>click me
				</a>
			</div>
		</div>
	);
}
