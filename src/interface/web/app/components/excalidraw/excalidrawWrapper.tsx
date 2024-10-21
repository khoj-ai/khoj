"use client";

import { useState, useEffect } from "react";

import dynamic from "next/dynamic";

import {
    AppState,
    BinaryFiles,
    ExcalidrawImperativeAPI,
    ExcalidrawProps,
} from "@excalidraw/excalidraw/types/types";
import {
    ExcalidrawElement,
    ExcalidrawLinearElement,
} from "@excalidraw/excalidraw/types/element/types";
import { ExcalidrawElementSkeleton } from "@excalidraw/excalidraw/types/data/transform";

const Excalidraw = dynamic<ExcalidrawProps>(
    async () => (await import("@excalidraw/excalidraw")).Excalidraw,
    {
        ssr: false,
    },
);

import { convertToExcalidrawElements } from "@excalidraw/excalidraw";

import { Button } from "@/components/ui/button";

import { useIsMobileWidth } from "../../common/utils";
import { ArrowsInSimple, ArrowsOutSimple } from "@phosphor-icons/react";

function convertFromExcalidrawElements(elements: readonly ExcalidrawElement[]) {
    return elements.map((element): ExcalidrawElementSkeleton => {
        const elementType = element.type as Exclude<typeof element.type, "selection">;

        const baseElement = {
            type: elementType,
            x: element.x,
            y: element.y,
            width: element.width,
            height: element.height,
            id: element.id,
        };

        switch (element.type) {
            case "text":
                return {
                    ...baseElement,
                    text: element.text,
                    fontSize: element.fontSize,
                } as ExcalidrawElementSkeleton;
            case "line":
            case "arrow":
                const directionElement = element as ExcalidrawLinearElement;
                return {
                    ...baseElement,
                    points: element.points,
                    start: {
                        id: directionElement.startBinding?.elementId,
                    },
                    end: {
                        id: directionElement.endBinding?.elementId,
                    },
                } as ExcalidrawElementSkeleton;
            case "frame":
                return {
                    ...baseElement,
                    children: [],
                } as ExcalidrawElementSkeleton;
            default:
                return {
                    ...baseElement,
                    type: elementType,
                } as ExcalidrawElementSkeleton;
        }
    });
}

interface ExcalidrawWrapperProps {
    data: ExcalidrawElementSkeleton[];
}

export default function ExcalidrawWrapper(props: ExcalidrawWrapperProps) {
    const isMobileWidth = useIsMobileWidth();
    const [excalidrawElements, setExcalidrawElements] = useState<ExcalidrawElement[]>([]);
    const [excalidrawCurrentState, setExcalidrawCurrentState] = useState<
        readonly ExcalidrawElement[]
    >([]);
    const [excalidrawAPI, setExcalidrawAPI] = useState<ExcalidrawImperativeAPI | null>(null);
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
        // Do some basic validation
        const basicValidSkeletons: ExcalidrawElementSkeleton[] = [];

        for (const element of props.data) {
            if (isValidExcalidrawElement(element as ExcalidrawElementSkeleton)) {
                basicValidSkeletons.push(element as ExcalidrawElementSkeleton);
            }
        }

        const validSkeletons: ExcalidrawElementSkeleton[] = [];
        for (const element of basicValidSkeletons) {
            if (element.type === "frame") {
                continue;
            }
            if (element.type === "arrow" || element.type === "line") {
                const start = basicValidSkeletons.find((child) => child.id === element.start?.id);
                const end = basicValidSkeletons.find((child) => child.id === element.end?.id);
                if (start && end) {
                    validSkeletons.push(element);
                }
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

        console.log("Valid skeletons", validSkeletons);

        const elements = convertToExcalidrawElements(validSkeletons);
        setExcalidrawElements(elements);
    }, []);

    const onExcalidrawChange = (
        elements: readonly ExcalidrawElement[],
        appState: AppState,
        files: BinaryFiles,
    ) => {
        for (const element of elements) {
            if (!element.isDeleted) {
            }
        }
        setExcalidrawCurrentState(elements);
    };

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
                        validateEmbeddable={true}
                        renderTopRightUI={(isMobile, appState) => {
                            return <></>;
                        }}
                        excalidrawAPI={(api) => setExcalidrawAPI(api)}
                        onChange={onExcalidrawChange}
                    />
                </div>
            </div>
        </div>
    );
}
