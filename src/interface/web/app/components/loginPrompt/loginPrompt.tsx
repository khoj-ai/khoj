import {
    AlertDialog,
    AlertDialogAction,
    AlertDialogCancel,
    AlertDialogContent,
    AlertDialogDescription,
    AlertDialogFooter,
    AlertDialogHeader,
    AlertDialogTitle,
} from "@/components/ui/alert-dialog";
import { Button } from "@/components/ui/button";
import {
    Dialog,
    DialogContent,
    DialogDescription,
    DialogFooter,
    DialogHeader,
    DialogTitle,
} from "@/components/ui/dialog";
import { Input } from "@/components/ui/input";
import { ArrowLeft, GoogleCardboardLogo, GoogleLogo, Spinner } from "@phosphor-icons/react";
import Link from "next/link";
import { useState } from "react";
import useSWR from "swr";

export interface LoginPromptProps {
    loginRedirectMessage: string;
    onOpenChange: (open: boolean) => void;
}

const fetcher = (url: string) => fetch(url).then((res) => res.json());

interface Provider {
    client_id: string;
    redirect_uri: string;
}

interface CredentialsData {
    [provider: string]: Provider;
}

export default function LoginPrompt(props: LoginPromptProps) {
    const { data, error, isLoading } = useSWR<CredentialsData>("/auth/oauth/metadata", fetcher);

    const [useEmailSignIn, setUseEmailSignIn] = useState(false);

    const [email, setEmail] = useState("");
    const [checkEmail, setCheckEmail] = useState(false);

    const handleGoogleSignIn = () => {
        if (!data?.google?.client_id || !data?.google?.redirect_uri) return;

        // Create full redirect URL using current origin
        const fullRedirectUri = `${window.location.origin}${data.google.redirect_uri}`;

        const params = new URLSearchParams({
            client_id: data.google.client_id,
            redirect_uri: fullRedirectUri,
            response_type: "code",
            scope: "email profile openid",
            state: window.location.pathname,
            access_type: "offline",
            prompt: "consent select_account",
            include_granted_scopes: "true",
        });

        window.location.href = `https://accounts.google.com/o/oauth2/v2/auth?${params}`;
    };

    function handleMagicLinkSignIn() {
        fetch("/auth/magic", {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify({ email: email }),
        })
            .then((res) => {
                if (res.ok) {
                    setCheckEmail(true);
                    return res.json();
                } else {
                    throw new Error("Failed to send magic link");
                }
            })
            .then((data) => {
                console.log(data);
            })
            .catch((err) => {
                console.error(err);
            });
    }

    return (
        <Dialog open={true} onOpenChange={props.onOpenChange}>
            <DialogContent className="flex flex-row gap-4 max-w-3xl">
                <div>
                    <DialogHeader>
                        <DialogTitle>Sign in to Khoj to continue</DialogTitle>
                    </DialogHeader>
                    <DialogDescription className="py-4">
                        {props.loginRedirectMessage}.
                    </DialogDescription>
                    {useEmailSignIn && (
                        <div className="flex flex-col gap-4 py-4">
                            <Button
                                variant="ghost"
                                className="w-fit p-0 m-0 flex gap-2 items-center justify-center text-sm"
                                onClick={() => {
                                    setUseEmailSignIn(false);
                                }}
                            >
                                <ArrowLeft className="h-6 w-6" />
                            </Button>
                            <Input
                                placeholder="Email"
                                value={email}
                                onChange={(e) => setEmail(e.target.value)}
                            />
                            <Button
                                variant="default"
                                onClick={handleMagicLinkSignIn}
                                disabled={isLoading || checkEmail}
                            >
                                {checkEmail ? "Check your email" : "Send magic link"}
                            </Button>
                        </div>
                    )}
                    {!useEmailSignIn && (
                        <div className="flex flex-col gap-4 py-4">
                            <Button
                                variant="outline"
                                className="w-full flex gap-2 items-center justify-center"
                                onClick={handleGoogleSignIn}
                                disabled={isLoading || !data?.google}
                            >
                                {isLoading ? (
                                    <Spinner className="h-6 w-6" />
                                ) : (
                                    <GoogleLogo className="h-6 w-6" />
                                )}
                                Continue with Google
                            </Button>

                            <Button
                                variant="default"
                                onClick={() => {
                                    setUseEmailSignIn(true);
                                }}
                            >
                                Continue with Email
                            </Button>
                        </div>
                    )}
                    <DialogDescription>
                        By logging in, you agree to our{" "}
                        <Link href="https://khoj.dev/terms-of-service">Terms of Service.</Link>
                    </DialogDescription>
                </div>
                <div className="flex flex-col gap-4">
                    <img src="https://i.giphy.com/media/v1.Y2lkPTc5MGI3NjExNGl0NHR5Nm0wdmFreGRoYjJmanJqYnZ1dzd3OHBqNGY3OGxiczZldyZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9dg/SVZ7jzFPStbMsnjDWA/giphy.gif" />
                </div>
            </DialogContent>
        </Dialog>
    );
}
