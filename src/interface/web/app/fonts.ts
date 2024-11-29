import { Noto_Sans, Noto_Sans_Arabic } from "next/font/google";

export const noto_sans = Noto_Sans({
    subsets: ["latin", "latin-ext", "cyrillic", "cyrillic-ext", "devanagari", "vietnamese"],
    display: "swap",
    variable: "--font-noto-sans",
});

export const noto_sans_arabic = Noto_Sans_Arabic({
    subsets: ["arabic"],
    display: "swap",
    variable: "--font-noto-sans-arabic",
});
