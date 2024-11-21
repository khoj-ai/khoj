"use client";

import { useState, useEffect } from "react";

import dynamic from "next/dynamic";

import { ExcalidrawProps } from "@excalidraw/excalidraw/types/types";
import { ExcalidrawElement } from "@excalidraw/excalidraw/types/element/types";
import { ExcalidrawElementSkeleton } from "@excalidraw/excalidraw/types/data/transform";

const Excalidraw = dynamic<ExcalidrawProps>(
    async () => (await import("@excalidraw/excalidraw")).Excalidraw,
    {
        ssr: false,
    },
);

import { convertToExcalidrawElements } from "@excalidraw/excalidraw";

import { Button } from "@/components/ui/button";

import { ArrowsInSimple, ArrowsOutSimple } from "@phosphor-icons/react";

interface ExcalidrawWrapperProps {
    data: ExcalidrawElementSkeleton[];
}

export default function ExcalidrawWrapper(props: ExcalidrawWrapperProps) {
    const [excalidrawElements, setExcalidrawElements] = useState<ExcalidrawElement[]>([]);
    const [expanded, setExpanded] = useState<boolean>(false);

    const isValidExcalidrawElement = (element: ExcalidrawElementSkeleton): boolean => {
        return (
            element.x !== undefined &&
            element.y !== undefined &&
            element.id !== undefined &&
            element.type !== undefined
        );
    };

    useEffect(() => {
        if (expanded) {
            onkeydown = (e) => {
                if (e.key === "Escape") {
                    setExpanded(false);
                    // Trigger a resize event to make Excalidraw adjust its size
                    window.dispatchEvent(new Event("resize"));
                }
            };
        } else {
            onkeydown = null;
        }
    }, [expanded]);

    useEffect(() => {
        // Do some basic validation
        const basicValidSkeletons: ExcalidrawElementSkeleton[] = [];

        for (const element of props.data) {
            if (isValidExcalidrawElement(element as ExcalidrawElementSkeleton)) {
                basicValidSkeletons.push(element);
            }
        }

        const validSkeletons: ExcalidrawElementSkeleton[] = [];
        for (const element of basicValidSkeletons) {
            if (element.type === "frame") {
                continue;
            }
            if (element.type === "arrow") {
                if (element.start) {
                    const start = basicValidSkeletons.find(
                        (child) => child.id === element.start?.id,
                    );
                    if (!start) {
                        continue;
                    }
                }
                if (element.end) {
                    const end = basicValidSkeletons.find((child) => child.id === element.end?.id);
                    if (!end) {
                        continue;
                    }
                }

                validSkeletons.push(element);
            } else {
                validSkeletons.push(element);
            }
        }

        for (const element of basicValidSkeletons) {
            if (element.type === "frame") {
                const children = element.children?.map((childId) => {
                    return validSkeletons.find((child) => child.id === childId);
                });
                // Get the valid children, filter out any undefined values
                const validChildrenIds: readonly string[] = children
                    ?.map((child) => child?.id)
                    .filter((id) => id !== undefined) as string[];

                if (validChildrenIds === undefined || validChildrenIds.length === 0) {
                    continue;
                }

                validSkeletons.push({
                    ...element,
                    children: validChildrenIds,
                });
            }
        }

        const elements = convertToExcalidrawElements(validSkeletons);
        setExcalidrawElements(elements);
    }, []);

    return (
        <div className="relative">
            <div
                className={`${expanded ? "fixed inset-0 bg-black bg-opacity-50 backdrop-blur-sm z-50 flex items-center justify-center" : ""}`}
            >
                <Button
                    onClick={() => {
                        setExpanded(!expanded);
                        // Trigger a resize event to make Excalidraw adjust its size
                        window.dispatchEvent(new Event("resize"));
                    }}
                    variant={"outline"}
                    className={`${expanded ? "absolute top-2 left-2 z-[60]" : ""}`}
                >
                    {expanded ? (
                        <ArrowsInSimple className="h-4 w-4" />
                    ) : (
                        <ArrowsOutSimple className="h-4 w-4" />
                    )}
                </Button>
                <div
                    className={`
                    ${expanded ? "w-[80vw] h-[80vh]" : "w-full h-[500px]"}
                    bg-white overflow-hidden rounded-lg relative
                `}
                >
                    <Excalidraw
                        initialData={{
                            elements: excalidrawElements,
                            appState: { zenModeEnabled: true },
                            scrollToContent: true,
                        }}
                        // TODO - Create a common function to detect if the theme is dark?
                        theme={localStorage.getItem("theme") === "dark" ? "dark" : "light"}
                        validateEmbeddable={true}
                        renderTopRightUI={(isMobile, appState) => {
                            return <></>;
                        }}
                    />
                </div>
            </div>
        </div>
    );
}
