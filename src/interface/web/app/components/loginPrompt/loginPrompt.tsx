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
import {
    ArrowLeft,
    ArrowsClockwise,
    GoogleCardboardLogo,
    GoogleLogo,
    LineVertical,
    PaperPlane,
    PaperPlaneTilt,
    PencilSimple,
    Spinner,
    CaretLeft,
    CaretRight,
} from "@phosphor-icons/react";
import Link from "next/link";
import { useEffect, useState } from "react";
import useSWR from "swr";
import { GoogleSignIn } from "./GoogleSignIn";

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
    const [recheckEmail, setRecheckEmail] = useState(false);

    const [currentTip, setCurrentTip] = useState(0);
    const [autoRotate, setAutoRotate] = useState(true);

    // Add these handler functions in your component
    const nextSlide = () => {
        setCurrentTip((prev) => (prev + 1) % tips.length);
        setAutoRotate(false);
    };
    const prevSlide = () => {
        setCurrentTip((prev) => (prev - 1 + tips.length) % tips.length);
        setAutoRotate(false);
    };

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

    const handleGoogleScriptLoad = () => {
        // Initialize Google Sign In after script loads
        window.google.accounts.id.initialize({
            client_id: "blahblahblah.apps.googleusercontent.com",
            callback: handleGoogleSignIn,
            auto_select: false,
            login_uri: "http://localhost:42110/auth/redirect",
        });

        // Render the button
        window.google.accounts.id.renderButton(document.getElementById("g_id_signin")!, {
            theme: "outline",
            size: "large",
            width: "100%",
        });
    };

    const tips = [
        { src: "/documents_tip.png", alt: "Documents tip" },
        { src: "/personalize_tip.png", alt: "Personalize tip" },
        { src: "/automate_tip.png", alt: "Automate tip" },
    ];

    useEffect(() => {
        if (!autoRotate) return;

        const timer = setInterval(() => {
            setCurrentTip((prev) => (prev + 1) % tips.length);
        }, 3000); // Rotate every 3 seconds

        return () => clearInterval(timer);
    }, [autoRotate]);

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
                    if (checkEmail) {
                        setRecheckEmail(true);
                    }
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
            <DialogContent
                className={`flex flex-col gap-4 max-w-xl ${!useEmailSignIn ? "p-0 pb-4 m-0" : ""}`}
            >
                <div>
                    {/* <DialogHeader>
                        <DialogTitle>Sign in to Khoj to continue</DialogTitle>
                    </DialogHeader>
                    <DialogDescription className="py-4">
                        {props.loginRedirectMessage}.
                    </DialogDescription> */}

                    {useEmailSignIn && (
                        <div className="flex flex-col gap-4 py-4">
                            <Button
                                variant="ghost"
                                className="w-fit p-0 m-0 flex gap-2 items-center justify-center text-sm absolute top-5 left-5"
                                onClick={() => {
                                    setUseEmailSignIn(false);
                                }}
                            >
                                <ArrowLeft className="h-6 w-6" />
                            </Button>
                            <DialogHeader className="text-center">
                                <DialogTitle className="text-center">
                                    {checkEmail ? "Check your email" : "Sign in with Email"}
                                </DialogTitle>
                            </DialogHeader>
                            <DialogDescription className="text-center">
                                {checkEmail
                                    ? recheckEmail
                                        ? "A new link has been sent. Click on the link in your email to sign-in"
                                        : "Click on the link in your email to sign-in"
                                    : "You will receive a sign-in link on the email address you provide below"}
                            </DialogDescription>
                            <Input
                                placeholder="Email"
                                className="p-6"
                                disabled={isLoading || checkEmail}
                                value={email}
                                onChange={(e) => setEmail(e.target.value)}
                            />
                            <Button
                                variant="default"
                                className="p-6"
                                onClick={handleMagicLinkSignIn}
                                disabled={isLoading || checkEmail || !email}
                            >
                                <PaperPlaneTilt className="h-6 w-6 mr-2" />
                                {checkEmail ? "Check your email" : "Send sign in link"}
                            </Button>
                            {checkEmail && (
                                <DialogDescription className="text-center flex items-center justify-center gap-4">
                                    <Button
                                        variant="ghost"
                                        className="p-0"
                                        disabled={recheckEmail}
                                        onClick={() => {
                                            handleMagicLinkSignIn();
                                        }}
                                    >
                                        <ArrowsClockwise className="h-6 w-6 mr-2" />
                                        Resend email
                                    </Button>
                                    <LineVertical className="h-6 w-6" />
                                    <Button
                                        variant="ghost"
                                        className="p-0"
                                        disabled={recheckEmail}
                                        onClick={() => {
                                            setEmail("");
                                            setCheckEmail(false);
                                        }}
                                    >
                                        <PencilSimple className="h-6 w-6 mr-2" />
                                        Use a different email
                                    </Button>
                                </DialogDescription>
                            )}
                        </div>
                    )}
                    {!useEmailSignIn && (
                        <div>
                            <div className="relative w-full h-80 overflow-hidden rounded-lg">
                                {tips.map((tip, index) => (
                                    <img
                                        key={tip.src}
                                        src={tip.src}
                                        alt={tip.alt}
                                        className={`absolute w-full h-full object-cover transition-all duration-500 ease-in-out ${
                                            index === currentTip
                                                ? "opacity-100 translate-x-0"
                                                : "opacity-0 translate-x-full"
                                        }`}
                                    />
                                ))}
                                <Button
                                    onClick={prevSlide}
                                    className="absolute left-2 top-1/2 -translate-y-1/2 bg-white/80 hover:bg-white/90 rounded-full p-2 shadow-lg"
                                >
                                    <CaretLeft className="text-black h-6 w-6" />
                                </Button>
                                <Button
                                    onClick={nextSlide}
                                    className="absolute right-2 top-1/2 -translate-y-1/2 bg-white/80 hover:bg-white/90 rounded-full p-2 shadow-lg"
                                >
                                    <CaretRight className="text-black h-6 w-6" />
                                </Button>
                            </div>
                            <DialogHeader className="flex flex-col gap-4 text-center p-4">
                                <DialogTitle className="text-base text-center font-bold">
                                    Sign In for free to start using Khoj: Your AI-powered second
                                    brain
                                </DialogTitle>
                            </DialogHeader>
                            <div className="flex flex-col gap-4 py-4 text-center align-middle items-center">
                                <GoogleSignIn onLoad={handleGoogleScriptLoad} />
                                {/* <div id="g_id_signin" /> */}
                                <Button
                                    variant="outline"
                                    className="w-[300px] p-6 flex gap-2 items-center justify-center"
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
                                    className="w-[300px] p-6 flex gap-2 items-center justify-center"
                                    onClick={() => {
                                        setUseEmailSignIn(true);
                                    }}
                                >
                                    <PaperPlaneTilt className="h-6 w-6" />
                                    Continue with Email
                                </Button>
                            </div>
                            <DialogDescription className="text-center">
                                By logging in, you agree to our{" "}
                                <Link href="https://khoj.dev/terms-of-service">
                                    Terms of Service.
                                </Link>
                            </DialogDescription>
                        </div>
                    )}
                </div>
            </DialogContent>
        </Dialog>
    );
}
