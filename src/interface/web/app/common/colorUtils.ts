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

export function convertSuggestionColorToTextClass(color: string) {
    const colors = ['blue', 'yellow', 'green', 'pink', 'purple'];
    if (colors.includes(color)) {
        return "" + `bg-gradient-to-b from-[hsl(var(--background))] to-${color}-100/${color == "green" ? "90" : "70"} dark:from-[hsl(var(--background))] dark:to-${color}-950/30 dark:border dark:border-neutral-700`;
    }
    return `bg-gradient-to-b from-white to-orange-50`;
}

export function convertColorToBorderClass(color: string) {
    console.log("Color:", color);
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
