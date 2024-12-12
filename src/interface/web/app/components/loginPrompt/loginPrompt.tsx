"use client";

import styles from "./loginPrompt.module.css";
import { Button } from "@/components/ui/button";
import { Dialog, DialogContent } from "@/components/ui/dialog";
import { Input } from "@/components/ui/input";
import {
    ArrowLeft,
    ArrowsClockwise,
    LineVertical,
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
import { Drawer, DrawerContent } from "@/components/ui/drawer";

export interface LoginPromptProps {
    loginRedirectMessage: string;
    onOpenChange: (open: boolean) => void;
    isMobileWidth?: boolean;
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

    useEffect(() => {
        const google = (window as any).google;

        if (!google) return;

        // Initialize Google Sign In after script loads
        google.accounts.id.initialize({
            client_id: data?.google?.client_id,
            callback: handleGoogleSignIn,
            auto_select: false,
            login_uri: data?.google?.redirect_uri,
        });

        // Render the button
        google.accounts.id.renderButton(document.getElementById("g_id_signin")!, {
            theme: "outline",
            size: "large",
            width: "100%",
        });
    }, [data]);

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
        const google = (window as any).google;

        if (!data?.google?.client_id || !data?.google?.redirect_uri) return;

        // Initialize Google Sign In after script loads
        google.accounts.id.initialize({
            client_id: data?.google?.client_id,
            callback: handleGoogleSignIn,
            auto_select: false,
            login_uri: data?.google?.redirect_uri,
        });

        // Render the button
        google.accounts.id.renderButton(document.getElementById("g_id_signin")!, {
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

    if (props.isMobileWidth) {
        return (
            <Drawer open={true} onOpenChange={props.onOpenChange}>
                <DrawerContent className={`flex flex-col gap-4 max-w-xl`}>
                    <div>
                        {useEmailSignIn && (
                            <EmailSignInContext
                                email={email}
                                setEmail={setEmail}
                                checkEmail={checkEmail}
                                setCheckEmail={setCheckEmail}
                                setUseEmailSignIn={setUseEmailSignIn}
                                recheckEmail={recheckEmail}
                                setRecheckEmail={setRecheckEmail}
                                handleMagicLinkSignIn={handleMagicLinkSignIn}
                            />
                        )}
                        {!useEmailSignIn && (
                            <MainSignInContext
                                tips={tips}
                                currentTip={currentTip}
                                nextSlide={nextSlide}
                                prevSlide={prevSlide}
                                handleGoogleScriptLoad={handleGoogleScriptLoad}
                                handleGoogleSignIn={handleGoogleSignIn}
                                isLoading={isLoading}
                                data={data}
                                setUseEmailSignIn={setUseEmailSignIn}
                                isMobileWidth={props.isMobileWidth}
                            />
                        )}
                    </div>
                </DrawerContent>
            </Drawer>
        );
    }

    return (
        <Dialog open={true} onOpenChange={props.onOpenChange}>
            <DialogContent
                className={`flex flex-col gap-4 max-w-xl ${!useEmailSignIn ? "p-0 pb-4 m-0" : ""}`}
            >
                <div>
                    {useEmailSignIn && (
                        <EmailSignInContext
                            email={email}
                            setEmail={setEmail}
                            checkEmail={checkEmail}
                            setCheckEmail={setCheckEmail}
                            setUseEmailSignIn={setUseEmailSignIn}
                            recheckEmail={recheckEmail}
                            setRecheckEmail={setRecheckEmail}
                            handleMagicLinkSignIn={handleMagicLinkSignIn}
                        />
                    )}
                    {!useEmailSignIn && (
                        <MainSignInContext
                            tips={tips}
                            currentTip={currentTip}
                            nextSlide={nextSlide}
                            prevSlide={prevSlide}
                            handleGoogleScriptLoad={handleGoogleScriptLoad}
                            handleGoogleSignIn={handleGoogleSignIn}
                            isLoading={isLoading}
                            data={data}
                            setUseEmailSignIn={setUseEmailSignIn}
                            isMobileWidth={props.isMobileWidth ?? false}
                        />
                    )}
                </div>
            </DialogContent>
        </Dialog>
    );
}

function EmailSignInContext({
    email,
    setEmail,
    checkEmail,
    setCheckEmail,
    setUseEmailSignIn,
    recheckEmail,
    handleMagicLinkSignIn,
}: {
    email: string;
    setEmail: (email: string) => void;
    checkEmail: boolean;
    setCheckEmail: (checkEmail: boolean) => void;
    setUseEmailSignIn: (useEmailSignIn: boolean) => void;
    recheckEmail: boolean;
    setRecheckEmail: (recheckEmail: boolean) => void;
    handleMagicLinkSignIn: () => void;
}) {
    return (
        <div className="flex flex-col gap-4 p-4">
            <Button
                variant="ghost"
                className="w-fit p-0 m-0 flex gap-2 items-center justify-center text-sm absolute top-5 left-5"
                onClick={() => {
                    setUseEmailSignIn(false);
                }}
            >
                <ArrowLeft className="h-6 w-6" />
            </Button>
            <div>
                <div className="text-center font-bold text-lg">Sign in with Email</div>
            </div>
            <div className="text-center text-sm text-muted-foreground">
                {checkEmail
                    ? recheckEmail
                        ? "A new link has been sent. Click on the link in your email to sign-in"
                        : "Click on the link in your email to sign-in"
                    : "You will receive a sign-in link on the email address you provide below"}
            </div>
            <Input
                placeholder="Email"
                className="p-6"
                disabled={checkEmail}
                value={email}
                onChange={(e) => setEmail(e.target.value)}
            />
            <Button
                variant="default"
                className="p-6"
                onClick={handleMagicLinkSignIn}
                disabled={!email}
            >
                <PaperPlaneTilt className="h-6 w-6 mr-2" />
                {checkEmail ? "Check your email" : "Send sign in link"}
            </Button>
            {checkEmail && (
                <div className="flex items-center justify-center gap-4 text-muted-foreground">
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
                </div>
            )}
        </div>
    );
}

function MainSignInContext({
    tips,
    currentTip,
    nextSlide,
    prevSlide,
    handleGoogleScriptLoad,
    handleGoogleSignIn,
    isLoading,
    data,
    setUseEmailSignIn,
    isMobileWidth,
}: {
    tips: { src: string; alt: string }[];
    currentTip: number;
    nextSlide: () => void;
    prevSlide: () => void;
    handleGoogleScriptLoad: () => void;
    handleGoogleSignIn: () => void;
    isLoading: boolean;
    data: CredentialsData | undefined;
    setUseEmailSignIn: (useEmailSignIn: boolean) => void;
    isMobileWidth: boolean;
}) {
    return (
        <div className="flex flex-col gap-4 p-4">
            {!isMobileWidth && (
                <div className="relative w-full h-80 overflow-hidden rounded-t-lg">
                    {tips.map((tip, index) => (
                        <img
                            key={tip.src}
                            src={tip.src}
                            alt={tip.alt}
                            className={`absolute w-full h-full object-cover transition-all duration-500 ease-in-out ${
                                index === currentTip
                                    ? "opacity-100 translate-x-0"
                                    : index < currentTip
                                      ? "opacity-0 -translate-x-full"
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
            )}
            <div className="flex flex-col gap-4 text-center p-4">
                <div className="text-center font-bold text-lg">
                    Sign In for free to start using Khoj: Your AI-powered second brain
                </div>
            </div>
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
                        <button className={`${styles.gsiMaterialButton}`}>
                            <div className={styles.gsiMaterialButtonState}></div>
                            <div className={styles.gsiMaterialButtonContentWrapper}>
                                <div
                                    className={`${styles.gsiMaterialButtonIcon} flex items-center justify-center`}
                                >
                                    <svg
                                        version="1.1"
                                        xmlns="http://www.w3.org/2000/svg"
                                        viewBox="0 0 48 48"
                                        xmlnsXlink="http://www.w3.org/1999/xlink"
                                    >
                                        <path
                                            fill="#EA4335"
                                            d="M24 9.5c3.54 0 6.71 1.22 9.21 3.6l6.85-6.85C35.9 2.38 30.47 0 24 0 14.62 0 6.51 5.38 2.56 13.22l7.98 6.19C12.43 13.72 17.74 9.5 24 9.5z"
                                        ></path>
                                        <path
                                            fill="#4285F4"
                                            d="M46.98 24.55c0-1.57-.15-3.09-.38-4.55H24v9.02h12.94c-.58 2.96-2.26 5.48-4.78 7.18l7.73 6c4.51-4.18 7.09-10.36 7.09-17.65z"
                                        ></path>
                                        <path
                                            fill="#FBBC05"
                                            d="M10.53 28.59c-.48-1.45-.76-2.99-.76-4.59s.27-3.14.76-4.59l-7.98-6.19C.92 16.46 0 20.12 0 24c0 3.88.92 7.54 2.56 10.78l7.97-6.19z"
                                        ></path>
                                        <path
                                            fill="#34A853"
                                            d="M24 48c6.48 0 11.93-2.13 15.89-5.81l-7.73-6c-2.15 1.45-4.92 2.3-8.16 2.3-6.26 0-11.57-4.22-13.47-9.91l-7.98 6.19C6.51 42.62 14.62 48 24 48z"
                                        ></path>
                                        <path fill="none" d="M0 0h48v48H0z"></path>
                                    </svg>
                                </div>
                            </div>
                        </button>
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
            <div className="text-center text-muted-foreground text-xs">
                By logging in, you agree to our{" "}
                <Link href="https://khoj.dev/terms-of-service">Terms of Service.</Link>
            </div>
        </div>
    );
}
