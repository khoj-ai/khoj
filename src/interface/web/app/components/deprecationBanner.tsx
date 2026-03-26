"use client";

import React, { useState, useEffect } from "react";
import Link from "next/link";
import { X, Warning } from "@phosphor-icons/react";
import { useUserConfig } from "@/app/common/auth";

const DISMISS_KEY = "khoj-cloud-deprecation-dismissed";

export function DeprecationBanner() {
    const [isDismissed, setIsDismissed] = useState(true);
    const { data: userConfig } = useUserConfig(true);

    useEffect(() => {
        setIsDismissed(localStorage.getItem(DISMISS_KEY) === "true");
    }, []);

    function dismiss() {
        localStorage.setItem(DISMISS_KEY, "true");
        setIsDismissed(true);
    }

    if (isDismissed || !userConfig?.billing_enabled) return null;

    return (
        <div className="w-full px-4 py-2.5 bg-orange-600/90 shadow-sm flex items-center justify-center gap-2 text-sm text-orange-50 z-0 relative">
            <Warning className="h-4 w-4 shrink-0 text-orange-200" weight="bold" />
            <p>
                <strong>Khoj Cloud is being deprecated on April 15, 2026.</strong>{" "}
                Please{" "}
                <Link href="/settings#account" className="underline font-medium hover:text-white">
                    export your data
                </Link>
                {" "}before then. To continue using Khoj, you can{" "}
                <a
                    href="https://docs.khoj.dev/get-started/setup"
                    target="_blank"
                    rel="noopener noreferrer"
                    className="underline font-medium hover:text-white"
                >
                    self-host it
                </a>.
            </p>
            <button
                onClick={dismiss}
                className="ml-2 p-0.5 rounded hover:bg-orange-700 shrink-0"
                aria-label="Dismiss banner"
            >
                <X className="h-4 w-4" />
            </button>
        </div>
    );
}
