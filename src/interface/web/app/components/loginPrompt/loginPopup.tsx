"use client";

import { Button } from "@/components/ui/button";
import { Card, CardDescription, CardHeader } from "@/components/ui/card";
import { KhojLogo } from "../logo/khojLogo";

export interface LoginPopupProps {
    isMobileWidth?: boolean;
    setShowLoginPrompt: (show: boolean) => void;
}

export default function LoginPopup(props: LoginPopupProps) {
    return (
        <Card className="absolute rounded-lg inset-x-0 bottom-2 md:bottom-8 left-1/2 transform -translate-x-1/2 p-6 flex flex-col md:flex-row items-center justify-between bg-gradient-to-b from-slate-50 dark:from-slate-900 to-bg-secondary z-30 shadow-lg gap-8 w-full md:w-fit">
            {!props.isMobileWidth && <KhojLogo className="w-12 h-auto" />}
            <div className="flex flex-col items-start justify-center">
                <CardHeader className="p-0 text-xl font-bold">Welcome to Khoj!</CardHeader>
                <CardDescription>
                    Get started with Khoj, your AI-powered knowledge assistant.
                </CardDescription>
            </div>
            <Button
                variant={"default"}
                className="p-8 text-lg"
                onClick={() => props.setShowLoginPrompt(true)}
            >
                Get started for free
            </Button>
        </Card>
    );
}
