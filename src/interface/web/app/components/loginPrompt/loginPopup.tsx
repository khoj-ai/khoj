"use client";

import { Button } from "@/components/ui/button";
import { Card, CardDescription, CardHeader } from "@/components/ui/card";
import { KhojLogo } from "../logo/khojLogo";
import { Drawer, DrawerContent } from "@/components/ui/drawer";

export interface LoginPopupProps {
    isMobileWidth?: boolean;
    setShowLoginPrompt: (show: boolean) => void;
}

export default function LoginPopup(props: LoginPopupProps) {
    if (props.isMobileWidth) {
        return (
            <Drawer open={true} onClose={() => props.setShowLoginPrompt(false)}>
                <DrawerContent>
                    <PopUpContent {...props} />
                </DrawerContent>
            </Drawer>
        );
    }

    return (
        <div className="fixed inset-x-0 bottom-8 z-30 flex items-center justify-center">
            <PopUpContent {...props} />
        </div>
    );
}

function PopUpContent(props: LoginPopupProps) {
    return (
        <Card className="rounded-lg p-6 flex flex-col items-center justify-between gap-8 md:w-fit border-none md:flex-row md:z-30 md:shadow-lg">
            {!props.isMobileWidth && <KhojLogo className="w-12 h-auto" />}
            <div className="flex flex-col items-start justify-center gap-8 md:gap-2">
                <CardHeader className="p-0 text-xl font-bold">Welcome to Khoj!</CardHeader>
                <CardDescription>
                    Sign in to get started with Khoj, your AI research assistant.
                </CardDescription>
            </div>
            <Button
                variant={"default"}
                className="p-6 text-lg"
                onClick={() => props.setShowLoginPrompt(true)}
            >
                Get started for free
            </Button>
        </Card>
    );
}
