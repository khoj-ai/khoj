import { useEffect, useState } from "react";
import useSWR from "swr";

export interface LocationData {
    city?: string;
    region?: string;
    country?: string;
    countryCode?: string;
    timezone: string;
}

const locationFetcher = () =>
    window
        .fetch("https://ipapi.co/json")
        .then((res) => res.json())
        .catch((err) => console.log(err));

export const toTitleCase = (str: string) =>
    str.replace(/\w\S*/g, (txt) => txt.charAt(0).toUpperCase() + txt.slice(1).toLowerCase());

export function welcomeConsole() {
    console.log(
        `%c %s`,
        "font-family:monospace",
        `
 __  __     __  __     ______       __        _____      __
/\\ \\/ /    /\\ \\_\\ \\   /\\  __ \\     /\\ \\      /\\  __ \\   /\\ \\
\\ \\  _"-.  \\ \\  __ \\  \\ \\ \\/\\ \\   _\\_\\ \\     \\ \\  __ \\  \\ \\ \\
 \\ \\_\\ \\_\\  \\ \\_\\ \\_\\  \\ \\_____\\ /\\_____\\     \\ \\_\\ \\_\\  \\ \\_\\
  \\/_/\\/_/   \\/_/\\/_/   \\/_____/ \\/_____/      \\/_/\\/_/   \\/_/


Greetings traveller,

I am ✨Khoj✨, your open-source, personal AI copilot.

See my source code at https://github.com/khoj-ai/khoj
Read my operating manual at https://docs.khoj.dev
`,
    );
}

export function useIPLocationData() {
    const {
        data: locationData,
        error: locationDataError,
        isLoading: locationDataLoading,
    } = useSWR<LocationData>("/api/ip", locationFetcher, { revalidateOnFocus: false });
    return { locationData, locationDataError, locationDataLoading };
}

export function useIsMobileWidth() {
    const [isMobileWidth, setIsMobileWidth] = useState(false);

    useEffect(() => {
        const handleResize = () => {
            if (window.innerWidth <= 768) {
                setIsMobileWidth(true);
            } else {
                setIsMobileWidth(false);
            }
        };

        handleResize();
        window.addEventListener("resize", handleResize);
        return () => window.removeEventListener("resize", handleResize);
    }, []);

    return isMobileWidth;
}

export const convertBytesToText = (fileSize: number) => {
    if (fileSize < 1024) {
        return `${fileSize} B`;
    } else if (fileSize < 1024 * 1024) {
        return `${(fileSize / 1024).toFixed(2)} KB`;
    } else {
        return `${(fileSize / (1024 * 1024)).toFixed(2)} MB`;
    }
};

export function useDebounce<T>(value: T, delay: number): T {
    const [debouncedValue, setDebouncedValue] = useState<T>(value);

    useEffect(() => {
        const handler = setTimeout(() => {
            setDebouncedValue(value);
        }, delay);

        return () => {
            clearTimeout(handler);
        };
    }, [value, delay]);

    return debouncedValue;
}
