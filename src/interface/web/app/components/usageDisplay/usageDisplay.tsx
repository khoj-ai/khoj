"use client";

import React, { useState, useEffect } from "react";
import { CurrencyCircleDollar, Coins } from "@phosphor-icons/react";
import styles from "./usageDisplay.module.css";

export interface UsageData {
    input_tokens?: number;
    output_tokens?: number;
    total_tokens?: number;
    cost?: number;
}

interface UsageDisplayProps {
    usage: UsageData;
    isStreaming?: boolean;
    className?: string;
}

interface AnimatedCounterProps {
    value: number;
    formatValue?: (value: number) => string;
    className?: string;
    duration?: number;
}

function AnimatedCounter({ value, formatValue, className, duration = 1000 }: AnimatedCounterProps) {
    const [displayValue, setDisplayValue] = useState(0);
    const [isAnimating, setIsAnimating] = useState(false);

    useEffect(() => {
        if (value === displayValue) return;

        setIsAnimating(true);
        const startValue = displayValue;
        const difference = value - startValue;
        const startTime = Date.now();

        const animate = () => {
            const elapsed = Date.now() - startTime;
            const progress = Math.min(elapsed / duration, 1);

            // Use easeOutCubic for smoother animation
            const easeProgress = 1 - Math.pow(1 - progress, 3);

            const currentValue = startValue + difference * easeProgress;
            setDisplayValue(currentValue);

            if (progress < 1) {
                requestAnimationFrame(animate);
            } else {
                setDisplayValue(value);
                setIsAnimating(false);
            }
        };

        requestAnimationFrame(animate);
    }, [value, duration]);

    const formattedValue = formatValue
        ? formatValue(displayValue)
        : Math.round(displayValue).toString();

    return (
        <span className={`${className} ${isAnimating ? styles.animating : ""}`}>
            {formattedValue}
        </span>
    );
}

export default function UsageDisplay({
    usage,
    isStreaming = false,
    className = "",
}: UsageDisplayProps) {
    const formatCost = (cost: number) => `$${cost.toFixed(5)}`;
    const formatTokens = (tokens: number) => Math.round(tokens).toLocaleString();

    const hasUsageData = usage && (usage.total_tokens || usage.cost);

    if (!hasUsageData) {
        return null;
    }

    return (
        <div
            className={`${styles.usageContainer} ${className} ${isStreaming ? styles.streaming : ""}`}
        >
            <div className={styles.usageSection}>
                {usage.total_tokens && (
                    <div className={styles.usageItem}>
                        <Coins className={styles.usageIcon} size={14} />
                        <span className={styles.usageLabel}>Tokens:</span>
                        <AnimatedCounter
                            value={usage.total_tokens}
                            formatValue={formatTokens}
                            className={styles.usageValue}
                            duration={isStreaming ? 500 : 1000}
                        />
                    </div>
                )}

                {usage.cost && (
                    <div className={styles.usageItem}>
                        <CurrencyCircleDollar className={styles.usageIcon} size={14} />
                        <span className={styles.usageLabel}>Cost:</span>
                        <AnimatedCounter
                            value={usage.cost}
                            formatValue={formatCost}
                            className={styles.usageValue}
                            duration={isStreaming ? 500 : 1000}
                        />
                    </div>
                )}
            </div>

            {usage.input_tokens && usage.output_tokens && (
                <div className={styles.usageBreakdown}>
                    <span className={styles.breakdownText}>
                        <AnimatedCounter
                            value={usage.input_tokens}
                            formatValue={formatTokens}
                            className={styles.inputTokens}
                            duration={isStreaming ? 300 : 800}
                        />
                        {" in + "}
                        <AnimatedCounter
                            value={usage.output_tokens}
                            formatValue={formatTokens}
                            className={styles.outputTokens}
                            duration={isStreaming ? 300 : 800}
                        />
                        {" out"}
                    </span>
                </div>
            )}
        </div>
    );
}
