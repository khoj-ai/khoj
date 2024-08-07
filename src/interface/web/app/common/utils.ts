import { useEffect, useState } from "react";
import useSWR from "swr";

export interface LocationData {
    ip: string;
    city: string;
    region: string;
    country: string;
    postal: string;
    latitude: number;
    longitude: number;
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
    const { data: locationData, error: locationDataError } = useSWR<LocationData>(
        "/api/ip",
        locationFetcher,
        { revalidateOnFocus: false },
    );

    if (locationDataError) return null;
    if (!locationData) return null;

    return locationData;
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
