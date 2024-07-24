export function convertColorToTextClass(color: string) {
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

function convertToBGGradientClass(color: string) {
    if (color === 'red') return `bg-gradient-to-b from-[hsl(var(--background))] to-red-100/70 dark:from-[hsl(var(--background))] dark:to-red-950/30 `;
    if (color === 'yellow') return `bg-gradient-to-b from-[hsl(var(--background))] to-yellow-100/70 dark:from-[hsl(var(--background))] dark:to-yellow-950/30 `;
    if (color === 'green') return `bg-gradient-to-b from-[hsl(var(--background))] to-green-100/90 dark:from-[hsl(var(--background))] dark:to-green-950/30 `;
    if (color === 'blue') return `bg-gradient-to-b from-[hsl(var(--background))] to-blue-100/70 dark:from-[hsl(var(--background))] dark:to-blue-950/30 `;
    if (color === 'orange') return `bg-gradient-to-b from-[hsl(var(--background))] to-orange-100/70 dark:from-[hsl(var(--background))] dark:to-orange-950/30 `;
    if (color === 'purple') return `bg-gradient-to-b from-[hsl(var(--background))] to-purple-100/70 dark:from-[hsl(var(--background))] dark:to-purple-950/30 `;
    if (color === 'pink') return `bg-gradient-to-b from-[hsl(var(--background))] to-pink-100/70 dark:from-[hsl(var(--background))] dark:to-pink-950/30 `;
    if (color === 'teal') return `bg-gradient-to-b from-[hsl(var(--background))] to-teal-100/70 dark:from-[hsl(var(--background))] dark:to-teal-950/30 `;
    if (color === 'cyan') return `bg-gradient-to-b from-[hsl(var(--background))] to-cyan-100/70 dark:from-[hsl(var(--background))] dark:to-cyan-950/30 `;
    if (color === 'lime') return `bg-gradient-to-b from-[hsl(var(--background))] to-lime-100/70 dark:from-[hsl(var(--background))] dark:to-lime-950/30 `;
    if (color === 'indigo') return `bg-gradient-to-b from-[hsl(var(--background))] to-indigo-100/70 dark:from-[hsl(var(--background))] dark:to-indigo-950/30 `;
    if (color === 'fuschia') return `bg-gradient-to-b from-[hsl(var(--background))] to-fuschia-100/70 dark:from-[hsl(var(--background))] dark:to-fuschia-950/30 `;
    if (color === 'rose') return `bg-gradient-to-b from-[hsl(var(--background))] to-rose-100/70 dark:from-[hsl(var(--background))] dark:to-rose-950/30 `;
    if (color === 'sky') return `bg-gradient-to-b from-[hsl(var(--background))] to-sky-100/70 dark:from-[hsl(var(--background))] dark:to-sky-950/30 `;
    if (color === 'amber') return `bg-gradient-to-b from-[hsl(var(--background))] to-amber-100/70 dark:from-[hsl(var(--background))] dark:to-amber-950/30 `;
    if (color === 'emerald') return `bg-gradient-to-b from-[hsl(var(--background))] to-emerald-100/70 dark:from-[hsl(var(--background))] dark:to-emerald-950/30 `;
    return `bg-gradient-to-b from-white to-orange-50`;
}

export function convertSuggestionColorToTextClass(color: string) {
    return `${convertToBGGradientClass(color)} dark:border dark:border-neutral-700`;
}

export function convertColorToBorderClass(color: string) {
    if (color === 'red') return `border-red-500`;
    if (color === 'yellow') return `border-yellow-500`;
    if (color === 'green') return `border-green-500`;
    if (color === 'blue') return `border-blue-500`;
    if (color === 'orange') return `border-orange-500`;
    if (color === 'purple') return `border-purple-500`;
    if (color === 'pink') return `border-pink-500`;
    if (color === 'teal') return `border-teal-500`;
    if (color === 'cyan') return `border-cyan-500`;
    if (color === 'lime') return `border-lime-500`;
    if (color === 'indigo') return `border-indigo-500`;
    if (color === 'fuschia') return `border-fuschia-500`;
    if (color === 'rose') return `border-rose-500`;
    if (color === 'sky') return `border-sky-500`;
    if (color === 'amber') return `border-amber-500`;
    if (color === 'emerald') return `border-emerald-500`;
    return `border-gray-500`;
}

export const colorMap: Record<string, string> = {
    'red': 'border-red-500',
    'blue': 'border-blue-500',
    'green': 'border-green-500',
    'yellow': 'border-yellow-500',
    'purple': 'border-purple-500',
    'pink': 'border-pink-500',
    'indigo': 'border-indigo-500',
    'gray': 'border-gray-500',
    'orange': 'border-orange-500',
};
