import React, { useEffect, useState, useRef } from "react";
import mermaid from "mermaid";
import { Info } from "@phosphor-icons/react";

interface MermaidProps {
    chart: string;
}

const Mermaid: React.FC<MermaidProps> = ({ chart }) => {
    const [mermaidError, setMermaidError] = useState<string | null>(null);
    const [mermaidId] = useState(`mermaid-chart-${Math.random().toString(12).substring(7)}`);
    const elementRef = useRef<HTMLDivElement>(null);

    useEffect(() => {
        mermaid.initialize({
            startOnLoad: false,
        });

        mermaid.parseError = (error) => {
            console.error("Mermaid errors:", error);
            // Extract error message from error object
            // Parse error message safely
            let errorMessage;
            try {
                errorMessage = typeof error === "string" ? JSON.parse(error) : error;
            } catch (e) {
                errorMessage = error?.toString() || "Unknown error";
            }

            console.log("Mermaid error message:", errorMessage);

            if (errorMessage.str !== "element is null") {
                setMermaidError(
                    "Something went wrong while rendering the diagram. Please try again later or downvote the message if the issue persists.",
                );
            } else {
                setMermaidError(null);
            }
        };

        mermaid.contentLoaded();
    }, []);

    useEffect(() => {
        console.log("Rendering mermaid chart:", chart);
        if (elementRef.current) {
            elementRef.current.removeAttribute("data-processed");

            mermaid
                .run({
                    nodes: [elementRef.current],
                })
                .then(() => {
                    setMermaidError(null);
                })
                .catch((error) => {});
        }
    }, [chart]);

    return (
        <div>
            {mermaidError ? (
                <div className="flex items-center gap-2 bg-red-100 border border-red-500 rounded-md p-3 mt-3 text-red-900 text-sm">
                    <Info className="w-12 h-12" />
                    <span>Error rendering diagram: {mermaidError}</span>
                </div>
            ) : (
                <div
                    id={mermaidId}
                    ref={elementRef}
                    className="mermaid"
                    style={{
                        width: "auto",
                        height: "auto",
                        boxSizing: "border-box",
                        overflow: "auto",
                    }}
                >
                    {chart}
                </div>
            )}
        </div>
    );
};

export default Mermaid;
