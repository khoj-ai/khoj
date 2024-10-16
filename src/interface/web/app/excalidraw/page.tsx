"use client";

import styles from "./factChecker.module.css";
import { useAuthenticatedData } from "@/app/common/auth";
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

import ChatMessage, {
    Context,
    OnlineContext,
    OnlineContextData,
    WebPage,
} from "../components/chatMessage/chatMessage";
import { ModelPicker, Model } from "../components/modelPicker/modelPicker";
import ShareLink from "../components/shareLink/shareLink";

import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";

import { Card, CardContent, CardFooter, CardHeader, CardTitle } from "@/components/ui/card";
import Link from "next/link";
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

export default function ExcalidrawPage() {
    const isMobileWidth = useIsMobileWidth();
    const [excalidrawElements, setExcalidrawElements] = useState<ExcalidrawElement[]>([]);
    const [excalidrawCurrentState, setExcalidrawCurrentState] = useState<
        readonly ExcalidrawElement[]
    >([]);
    const [excalidrawAPI, setExcalidrawAPI] = useState<ExcalidrawImperativeAPI | null>(null);

    useEffect(() => {
        const elements = convertToExcalidrawElements([
            {
                type: "text",
                x: 150,
                y: 30,
                fontSize: 24,
                text: "Simple Chicken Soup Recipe",
                id: "title_text",
            },
            {
                type: "ellipse",
                x: 180,
                y: 80,
                width: 100,
                height: 50,
                strokeColor: "#000000",
                backgroundColor: "#FFFFFF",
                id: "start_ellipse",
                label: {
                    text: "Start",
                },
            },
            {
                type: "arrow",
                x: 220,
                y: 130,
                width: 0,
                height: 50,
                strokeColor: "#000000",
                id: "arrow_1",
                start: {
                    id: "start_ellipse",
                },
                end: {
                    id: "rectangle_1",
                },
                points: [
                    [0, 0],
                    [0, 50],
                ],
            },
            {
                type: "rectangle",
                x: 150,
                y: 180,
                width: 200,
                height: 70,
                strokeColor: "#000000",
                backgroundColor: "#FFFFFF",
                id: "rectangle_1",
                label: {
                    text: "1. Gather Ingredients: Chicken, Carrots, Onions, Celery, Broth, Salt, Pepper",
                },
            },
            {
                type: "arrow",
                x: 220,
                y: 250,
                width: 0,
                height: 50,
                strokeColor: "#000000",
                id: "arrow_2",
                start: {
                    id: "rectangle_1",
                },
                end: {
                    id: "rectangle_2",
                },
                points: [
                    [0, 0],
                    [0, 50],
                ],
            },
            {
                type: "rectangle",
                x: 150,
                y: 300,
                width: 200,
                height: 50,
                strokeColor: "#000000",
                backgroundColor: "#FFFFFF",
                id: "rectangle_2",
                label: {
                    text: "2. Chop Vegetables: Carrots, Onions, Celery",
                },
            },
            {
                type: "arrow",
                x: 220,
                y: 350,
                width: 0,
                height: 50,
                strokeColor: "#000000",
                id: "arrow_3",
                start: {
                    id: "rectangle_2",
                },
                end: {
                    id: "rectangle_3",
                },
                points: [
                    [0, 0],
                    [0, 50],
                ],
            },
            {
                type: "rectangle",
                x: 150,
                y: 400,
                width: 200,
                height: 50,
                strokeColor: "#000000",
                backgroundColor: "#FFFFFF",
                id: "rectangle_3",
                label: {
                    text: "3. Boil Broth with Chicken: Simmer for 20 mins",
                },
            },
            {
                type: "arrow",
                x: 220,
                y: 450,
                width: 0,
                height: 50,
                strokeColor: "#000000",
                id: "arrow_4",
                start: {
                    id: "rectangle_3",
                },
                end: {
                    id: "rectangle_4",
                },
                points: [
                    [0, 0],
                    [0, 50],
                ],
            },
            {
                type: "rectangle",
                x: 150,
                y: 500,
                width: 200,
                height: 50,
                strokeColor: "#000000",
                backgroundColor: "#FFFFFF",
                id: "rectangle_4",
                label: {
                    text: "4. Add Vegetables: Cook for additional 20 mins",
                },
            },
            {
                type: "arrow",
                x: 220,
                y: 550,
                width: 0,
                height: 50,
                strokeColor: "#000000",
                id: "arrow_5",
                start: {
                    id: "rectangle_4",
                },
                end: {
                    id: "rectangle_5",
                },
                points: [
                    [0, 0],
                    [0, 50],
                ],
            },
            {
                type: "rectangle",
                x: 150,
                y: 600,
                width: 200,
                height: 50,
                strokeColor: "#000000",
                backgroundColor: "#FFFFFF",
                id: "rectangle_5",
                label: {
                    text: "5. Season to Taste: Add Salt and Pepper",
                },
            },
            {
                type: "arrow",
                x: 220,
                y: 650,
                width: 0,
                height: 50,
                strokeColor: "#000000",
                id: "arrow_6",
                start: {
                    id: "rectangle_5",
                },
                end: {
                    id: "end_ellipse",
                },
                points: [
                    [0, 0],
                    [0, 50],
                ],
            },
            {
                type: "ellipse",
                x: 180,
                y: 700,
                width: 100,
                height: 50,
                strokeColor: "#000000",
                backgroundColor: "#FFFFFF",
                id: "end_ellipse",
                label: {
                    text: "End",
                },
            },
            {
                type: "frame",
                x: 100,
                y: 0,
                width: 300,
                height: 800,
                children: [
                    "title_text",
                    "start_ellipse",
                    "rectangle_1",
                    "rectangle_2",
                    "rectangle_3",
                    "rectangle_4",
                    "rectangle_5",
                    "end_ellipse",
                    "arrow_1",
                    "arrow_2",
                    "arrow_3",
                    "arrow_4",
                    "arrow_5",
                    "arrow_6",
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
