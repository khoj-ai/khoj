import { CircleNotch } from '@phosphor-icons/react';

export default function Loading() {
    return (
        // NOTE: We can display usage tips here for casual learning moments.
        <div className={`bg-background opacity-50 flex items-center justify-center h-screen`}>
            <div>Loading <span><CircleNotch className={`inline animate-spin h-5 w-5`} /></span></div>
        </div>
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
