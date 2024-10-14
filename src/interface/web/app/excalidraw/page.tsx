"use client";

import styles from "./factChecker.module.css";
import { useAuthenticatedData } from "@/app/common/auth";
import { useState, useEffect } from "react";

import dynamic from "next/dynamic";
const Excalidraw = dynamic(async () => (await import("@excalidraw/excalidraw")).Excalidraw, {
    ssr: false,
});

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

export default function ExcalidrawPage() {
    const isMobileWidth = useIsMobileWidth();

    return (
        <div className="flex h-screen">
            <div className="flex-shrink-0">
                <SidePanel conversationId={null} uploadedFiles={[]} isMobileWidth={isMobileWidth} />
            </div>
            <div className="flex-grow overflow-hidden">
                <Excalidraw />
            </div>
        </div>
    );
}
