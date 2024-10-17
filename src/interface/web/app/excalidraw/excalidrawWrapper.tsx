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

import SidePanel from "../components/sidePanel/chatHistorySidePanel";
import { useIsMobileWidth } from "../common/utils";

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

export default function ExcalidrawWrapper() {
    const isMobileWidth = useIsMobileWidth();
    const [excalidrawElements, setExcalidrawElements] = useState<ExcalidrawElement[]>([]);
    const [excalidrawCurrentState, setExcalidrawCurrentState] = useState<
        readonly ExcalidrawElement[]
    >([]);
    const [excalidrawAPI, setExcalidrawAPI] = useState<ExcalidrawImperativeAPI | null>(null);

    useEffect(() => {
        const elements = convertToExcalidrawElements([
            {
                type: "frame",
                x: -200,
                y: -200,
                width: 500,
                height: 400,
                children: [
                    "plant_ellipse",
                    "water_ellipse",
                    "fertilize_ellipse",
                    "plant_to_water_arrow",
                    "water_to_fertilize_arrow",
                ],
            },
            {
                type: "text",
                x: -150,
                y: -180,
                fontSize: 24,
                text: "Process for Growing Healthy Plants",
            },
            {
                type: "ellipse",
                x: -150,
                y: -100,
                width: 100,
                height: 50,
                id: "plant_ellipse",
                label: {
                    text: "Plant",
                },
            },
            {
                type: "ellipse",
                x: 0,
                y: -100,
                width: 100,
                height: 50,
                id: "water_ellipse",
                label: {
                    text: "Water",
                },
            },
            {
                type: "ellipse",
                x: 150,
                y: -100,
                width: 100,
                height: 50,
                id: "fertilize_ellipse",
                label: {
                    text: "Fertilize",
                },
            },
            {
                type: "arrow",
                x: 0,
                y: -75,
                width: 20,
                height: 20,
                id: "plant_to_water_arrow",
                points: [
                    [0, 0],
                    [50, 0],
                ],
                start: {
                    id: "plant_ellipse",
                },
                end: {
                    id: "water_ellipse",
                },
            },
            {
                type: "arrow",
                x: 100,
                y: -75,
                width: 20,
                height: 20,
                id: "water_to_fertilize_arrow",
                points: [
                    [0, 0],
                    [50, 0],
                ],
                start: {
                    id: "water_ellipse",
                },
                end: {
                    id: "fertilize_ellipse",
                },
            },
            {
                type: "frame",
                x: 100,
                y: -75,
                width: 20,
                height: 20,
                id: "full_frame",
                name: "Full Frame",
                children: [
                    "plant_ellipse",
                    "water_ellipse",
                    "fertilize_ellipse",
                    "plant_to_water_arrow",
                    "water_to_fertilize_arrow",
                ],
            },
        ]);

        setExcalidrawElements(elements);
    }, []);

    const onExcalidrawChange = (
        elements: readonly ExcalidrawElement[],
        appState: AppState,
        files: BinaryFiles,
    ) => {
        console.log(elements, appState, files);
        // console.log(convertFromExcalidrawElements(elements));
        // console.log(convertFromExcalidrawElements(elements).map((element) => JSON.stringify(element)));
        console.log("EXCALIDRAW CHANGE");
        for (const element of elements) {
            if (!element.isDeleted) {
                console.log(JSON.stringify(convertFromExcalidrawElements([element])));
            }
        }
        setExcalidrawCurrentState(elements);
    };

    return (
        <div className="flex h-screen">
            <div className="flex-shrink-0">
                <SidePanel conversationId={null} uploadedFiles={[]} isMobileWidth={isMobileWidth} />
            </div>
            <Button
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
            </Button>
            <div className="flex-grow overflow-hidden">
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
    );
}
