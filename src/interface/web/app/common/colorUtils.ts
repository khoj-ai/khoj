export const tailwindColors = [
    "red",
    "yellow",
    "green",
    "blue",
    "orange",
    "purple",
    "pink",
    "teal",
    "cyan",
    "lime",
    "indigo",
    "fuchsia",
    "rose",
    "sky",
    "amber",
    "emerald",
];

export function convertColorToTextClass(color: string) {
    if (tailwindColors.includes(color)) {
        return `text-${color}-500`;
    }
    return `text-gray-500`;
}

export function convertToBGGradientClass(color: string) {
    if (tailwindColors.includes(color)) {
        return `bg-gradient-to-b from-[hsl(var(--background))] to-${color}-100/70 dark:from-[hsl(var(--background))] dark:to-${color}-950/30 `;
    }
    return `bg-gradient-to-b from-white to-orange-50`;
}

export function convertToBGClass(color: string) {
    if (tailwindColors.includes(color)) {
        return `bg-${color}-500 dark:bg-${color}-900 hover:bg-${color}-400 dark:hover:bg-${color}-800`;
    }
    return `bg-background`;
}

export function converColorToBgGradient(color: string) {
    return `${convertToBGGradientClass(color)} dark:border dark:border-neutral-700`;
}

export function convertColorToCaretClass(color: string | undefined) {
    if (color && tailwindColors.includes(color)) {
        return `caret-${color}-500`;
    }
    return `caret-orange-500`;
}

export function convertColorToRingClass(color: string | undefined) {
    if (color && tailwindColors.includes(color)) {
        return `focus-visible:ring-${color}-500`;
    }
    return `focus-visible:ring-orange-500`;
}

export function convertColorToBorderClass(color: string) {
    if (tailwindColors.includes(color)) {
        return `border-${color}-500`;
    }
    return `border-gray-500`;
}
//rewrite colorMap using the convertColorToBorderClass function and iteration through the tailwindColors array
export const colorMap: Record<string, string> = {};
for (const color of tailwindColors) {
    colorMap[color] = convertColorToBorderClass(color);
}
