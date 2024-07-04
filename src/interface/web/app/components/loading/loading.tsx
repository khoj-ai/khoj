import styles from './loading.module.css';

import { CircleNotch } from '@phosphor-icons/react';

export default function Loading() {
    return (
        <div className={`${styles.loader} h-[100vh] flex items-center justify-center`}></div>
    );
}

interface InlineLoadingProps {
    className?: string;
}

export function InlineLoading(props: InlineLoadingProps) {
    return (
        <button className={`${props.className}`}>
            <CircleNotch className={`animate-spin h-5 w-5 mr-3`} />
        </button>
    )
}
