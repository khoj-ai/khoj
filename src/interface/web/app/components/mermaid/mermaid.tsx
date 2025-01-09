import React, { useEffect, useState, useRef } from "react";
import mermaid from "mermaid";

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
            const errorMessage = JSON.stringify(error);
            setMermaidError(errorMessage);
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
                .catch((error) => {
                    console.error("Mermaid error:", error);
                    // Extract error message from error object
                    const errorMessage = error?.str || error?.message || JSON.stringify(error);
                    console.log("Mermaid error message:", errorMessage);
                });
        }
    }, [chart]);

    return (
        <div>
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
        </div>
    );
};

export default Mermaid;
