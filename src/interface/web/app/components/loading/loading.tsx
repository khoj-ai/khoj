import { CircleNotch } from '@phosphor-icons/react';

interface LoadingProps {
    className?: string;
    message?: string;
}

export default function Loading(props: LoadingProps) {
    return (
        // NOTE: We can display usage tips here for casual learning moments.
        <div className={props.className || "bg-background opacity-50 flex items-center justify-center h-screen"}>
            <div>{props.message || "Loading" } <span><CircleNotch className="inline animate-spin h-5 w-5" /></span></div>
        </div>
    );
}

export function InlineLoading(props: LoadingProps) {
    return (
        <button className={`${props.className}`}>
            <span>{props.message} <CircleNotch className="inline animate-spin h-5 w-5 mx-3" /></span>
        </button>
    )
}
