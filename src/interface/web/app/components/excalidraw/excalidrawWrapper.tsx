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

import SidePanel from "../../components/sidePanel/chatHistorySidePanel";
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

    useEffect(() => {
        const elements = convertToExcalidrawElements(props.data);
        // console.log("EXCALIDRAW ELEMENTS", elements);
        setExcalidrawElements(elements);
    }, []);

    const onExcalidrawChange = (
        elements: readonly ExcalidrawElement[],
        appState: AppState,
        files: BinaryFiles,
    ) => {
        // console.log(elements, appState, files);
        // console.log(convertFromExcalidrawElements(elements));
        // console.log(convertFromExcalidrawElements(elements).map((element) => JSON.stringify(element)));
        // console.log("EXCALIDRAW CHANGE");
        for (const element of elements) {
            if (!element.isDeleted) {
                // console.log(JSON.stringify(convertFromExcalidrawElements([element])));
            }
        }
        setExcalidrawCurrentState(elements);
    };

    return (
        <div className={`flex flex-col ${expanded ? "fixed inset-0 z-50 bg-white" : ""}`}>
            <Button
                onClick={() => {
                    setExpanded(!expanded);
                    // Trigger a resize event to make Excalidraw adjust its size
                    window.dispatchEvent(new Event("resize"));
                }}
                variant={"outline"}
                className={`${expanded ? "absolute top-2 right-2 z-10" : ""}`}
            >
                {expanded ? (
                    <ArrowsInSimple className="h-4 w-4" />
                ) : (
                    <ArrowsOutSimple className="h-4 w-4" />
                )}
            </Button>
            <div className={`flex ${expanded ? "h-screen" : "h-[500px]"}`}>
                {/* <Button
                onClick={() => {
                    setExcalidrawElements(
                        convertToExcalidrawElements(
                            convertFromExcalidrawElements(excalidrawCurrentState),
                            ),
                            );
                            const sceneData = {
                                elements: [
                                    {
                                        type: "rectangle",
                                        version: 141,
                                        versionNonce: 361174001,
                                        isDeleted: false,
                                        id: "oDVXy8D6rom3H1-LLH2-f",
                                        fillStyle: "hachure",
                                        strokeWidth: 1,
                                        strokeStyle: "solid",
                                        roughness: 1,
                                        opacity: 100,
                                        angle: 0,
                                        x: 100.50390625,
                                        y: 93.67578125,
                                        strokeColor: "#c92a2a",
                                        backgroundColor: "transparent",
                                        width: 186.47265625,
                                        height: 141.9765625,
                                        seed: 1968410350,
                                        groupIds: [],
                                        boundElements: null,
                                        locked: false,
                                        link: null,
                                        updated: 1,
                                        roundness: {
                                            type: 3,
                                            value: 32,
                                            },
                                            frameId: null,
                                            } as ExcalidrawElement,
                                            ],
                                            appState: {
                                                viewBackgroundColor: "#edf2ff",
                                                },
                                                };
                                                excalidrawAPI?.updateScene(sceneData);
                                                }}
                                                variant="default"
                                                >
                                                Refresh
                                                </Button> */}

                <div
                    className={`flex-grow overflow-hidden ${expanded ? "w-full h-full" : "h-[500px]"}`}
                >
                    <Excalidraw
                        initialData={{
                            elements: excalidrawElements,
                            appState: { zenModeEnabled: true },
                            scrollToContent: true,
                        }}
                        excalidrawAPI={(api) => setExcalidrawAPI(api)}
                        onChange={onExcalidrawChange}
                    />
                </div>
            </div>
        </div>
    );
}
