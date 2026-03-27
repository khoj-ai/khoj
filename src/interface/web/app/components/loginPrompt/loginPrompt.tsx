"use client";

import { Button } from "@/components/ui/button";
import { Dialog, DialogContent, DialogTitle } from "@/components/ui/dialog";
import { Input } from "@/components/ui/input";
import Autoplay from "embla-carousel-autoplay";
import {
    ArrowLeft,
    ArrowsClockwise,
    LineVertical,
    PaperPlaneTilt,
    PencilSimple,
    Spinner,
} from "@phosphor-icons/react";
import Link from "next/link";
import { useRef, useState } from "react";
import useSWR from "swr";
import { Drawer, DrawerContent } from "@/components/ui/drawer";
import {
    Carousel,
    CarouselContent,
    CarouselItem,
    CarouselNext,
    CarouselPrevious,
} from "@/components/ui/carousel";
import { Card, CardContent } from "@/components/ui/card";
import { InputOTP, InputOTPGroup, InputOTPSlot } from "@/components/ui/input-otp";
import * as VisuallyHidden from "@radix-ui/react-visually-hidden";

export interface LoginPromptProps {
    onOpenChange: (open: boolean) => void;
    isMobileWidth?: boolean;
}

const fetcher = (url: string) => fetch(url).then((res) => res.json());

const ALLOWED_OTP_ATTEMPTS = 5;

interface Provider {
    client_id: string;
    redirect_uri: string;
    provider_name?: string;
    button_label?: string;
}

interface CredentialsData {
    [key: string]: Provider | undefined;
}

export default function LoginPrompt(props: LoginPromptProps) {
    const { data, error, isLoading } = useSWR<CredentialsData>("/auth/oauth/metadata", fetcher);

    const [useEmailSignIn, setUseEmailSignIn] = useState(false);

    const handleOAuthSignIn = (providerKey: string) => {
        // Redirect to backend login endpoint which handles OAuth flow
        window.location.href = `/auth/login/${providerKey}`;
    };

    if (props.isMobileWidth) {
        return (
            <Drawer open={true} onOpenChange={props.onOpenChange}>
                <DrawerContent className={`flex flex-col gap-4 w-full mb-4`}>
                    <div>
                        {useEmailSignIn ? (
                            <EmailSignInContext setUseEmailSignIn={setUseEmailSignIn} />
                        ) : (
                            <MainSignInContext
                                handleOAuthSignIn={handleOAuthSignIn}
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
                className={`flex flex-col gap-4 ${!useEmailSignIn ? "p-0 pb-4 m-0 max-w-xl" : "w-fit"}`}
            >
                <VisuallyHidden.Root>
                    <DialogTitle>Login Dialog</DialogTitle>
                </VisuallyHidden.Root>
                <div>
                    {useEmailSignIn ? (
                        <EmailSignInContext setUseEmailSignIn={setUseEmailSignIn} />
                    ) : (
                        <MainSignInContext
                            handleOAuthSignIn={handleOAuthSignIn}
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
    setUseEmailSignIn,
}: {
    setUseEmailSignIn: (useEmailSignIn: boolean) => void;
}) {
    const [otp, setOTP] = useState("");
    const [otpError, setOTPError] = useState("");
    const [numFailures, setNumFailures] = useState(0);
    const [email, setEmail] = useState("");
    const [checkEmail, setCheckEmail] = useState(false);
    const [recheckEmail, setRecheckEmail] = useState(false);
    const [sendEmailError, setSendEmailError] = useState("");

    function checkOTPAndRedirect() {
        const verifyUrl = `/auth/magic?code=${encodeURIComponent(otp)}&email=${encodeURIComponent(email)}`;

        if (numFailures >= ALLOWED_OTP_ATTEMPTS) {
            setOTPError("Too many failed attempts. Please try again tomorrow.");
            return;
        }

        fetch(verifyUrl, {
            method: "GET",
            headers: {
                "Content-Type": "application/json",
            },
        })
            .then((res) => {
                if (res.ok) {
                    // Check if the response is a redirect
                    if (res.redirected) {
                        window.location.href = res.url;
                    }
                } else if (res.status === 401) {
                    setOTPError("Invalid OTP.");
                    setNumFailures(numFailures + 1);
                    if (numFailures + 1 >= ALLOWED_OTP_ATTEMPTS) {
                        setOTPError("Too many failed attempts. Please try again tomorrow.");
                    }
                } else if (res.status === 429) {
                    setOTPError("Too many failed attempts. Please try again tomorrow.");
                    setNumFailures(ALLOWED_OTP_ATTEMPTS);
                } else if (res.status === 403) {
                    setOTPError("OTP expired. Please request a new one.");
                } else {
                    throw new Error("Failed to verify OTP");
                }
            })
            .catch((err) => {
                console.error(err);
            });
    }

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
                } else if (res.status === 429 || res.status === 404) {
                    res.json().then((data) => {
                        setSendEmailError(data.detail);
                        throw new Error(data.detail);
                    });
                } else {
                    setSendEmailError("Failed to send email. Contact developers for assistance.");
                    throw new Error("Failed to send magic link via email.");
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
        <div className="flex flex-col gap-4 p-4">
            <Button
                variant="ghost"
                className="w-fit p-1 m-0 flex gap-2 items-center justify-center text-sm absolute top-5 left-5 h-fit rounded-full border border-gray-200"
                onClick={() => {
                    setUseEmailSignIn(false);
                }}
            >
                <ArrowLeft className="h-6 w-6" />
            </Button>
            <div>
                <div className="text-center font-bold text-xl">Get started with Khoj</div>
            </div>
            <div className="text-center text-sm text-muted-foreground">
                {checkEmail
                    ? recheckEmail
                        ? `A new link has been sent to ${email}.`
                        : `A one time sign in code has been sent to ${email}.`
                    : "You will receive a sign-in code on the email address you provide below"}
            </div>
            {!checkEmail && (
                <div className="flex items-center justify-center gap-4 text-muted-foreground flex-col">
                    <Input
                        placeholder="Email"
                        className="p-6 w-[300px] mx-auto rounded-lg"
                        disabled={checkEmail}
                        value={email}
                        autoFocus={true}
                        onKeyDown={(e) => {
                            if (e.key === "Enter") {
                                handleMagicLinkSignIn();
                            }
                        }}
                        onChange={(e) => setEmail(e.target.value)}
                    />
                    <Button
                        variant="default"
                        className="p-6 w-[300px] mx-auto flex gap-2 items-center justify-center rounded-lg"
                        onClick={handleMagicLinkSignIn}
                        disabled={checkEmail}
                    >
                        <PaperPlaneTilt className="h-6 w-6 mr-2 font-bold" />
                        {checkEmail ? "Check your email" : "Send sign in code"}
                    </Button>
                    {sendEmailError && <div className="text-red-500 text-sm">{sendEmailError}</div>}
                </div>
            )}
            {checkEmail && (
                <div className="flex items-center justify-center gap-4 text-muted-foreground flex-col">
                    <InputOTP
                        autoFocus={true}
                        maxLength={6}
                        value={otp || ""}
                        onChange={setOTP}
                        disabled={numFailures >= ALLOWED_OTP_ATTEMPTS}
                        onComplete={() =>
                            setTimeout(() => {
                                checkOTPAndRedirect();
                            }, 1000)
                        }
                    >
                        <InputOTPGroup>
                            <InputOTPSlot index={0} />
                            <InputOTPSlot index={1} />
                            <InputOTPSlot index={2} />
                            <InputOTPSlot index={3} />
                            <InputOTPSlot index={4} />
                            <InputOTPSlot index={5} />
                        </InputOTPGroup>
                    </InputOTP>
                    {otpError && (
                        <div className="text-red-500 text-sm">
                            {otpError} {ALLOWED_OTP_ATTEMPTS - numFailures} remaining attempts.
                        </div>
                    )}
                </div>
            )}

            {checkEmail && (
                <div className="flex items-center justify-center gap-4 text-muted-foreground flex-col md:flex-row">
                    <Button
                        variant="ghost"
                        className="p-0 text-orange-500"
                        disabled={recheckEmail}
                        onClick={handleMagicLinkSignIn}
                    >
                        <ArrowsClockwise className="h-6 w-6 mr-2 text-gray-500" />
                        Resend email
                    </Button>
                    <LineVertical className="h-6 w-6 hidden md:block opacity-50" />
                    <Button
                        variant="ghost"
                        className="p-0 text-orange-500"
                        disabled={recheckEmail}
                        onClick={() => {
                            setEmail("");
                            setCheckEmail(false);
                        }}
                    >
                        <PencilSimple className="h-6 w-6 mr-2 text-gray-500" />
                        Use a different email
                    </Button>
                </div>
            )}
        </div>
    );
}

function MainSignInContext({
    handleOAuthSignIn,
    isLoading,
    data,
    setUseEmailSignIn,
    isMobileWidth,
}: {
    handleOAuthSignIn: (providerKey: string) => void;
    isLoading: boolean;
    data: CredentialsData | undefined;
    setUseEmailSignIn: (useEmailSignIn: boolean) => void;
    isMobileWidth: boolean;
}) {
    const plugin = useRef(Autoplay({ delay: 4000, stopOnInteraction: true }));
    const [showArrows, setShowArrows] = useState(false);

    const tips = [
        {
            src: "https://assets.khoj.dev/sign_in_demos/research_mode.gif",
            alt: "Research tip",
            description: "Simplify Deep Work",
        },
        {
            src: "https://assets.khoj.dev/sign_in_demos/custom_agents.gif",
            alt: "Personalize tip",
            description: "Personalize your AI",
        },
        {
            src: "https://assets.khoj.dev/sign_in_demos/docment_questions.gif",
            alt: "Document tip",
            description: "Ask Anything",
        },
    ];

    return (
        <div className="flex flex-col gap-4 p-4 md:p-0">
            {!isMobileWidth && (
                <Carousel
                    plugins={[plugin.current]}
                    className="w-full"
                    onMouseEnter={() => {
                        plugin.current.stop();
                        setShowArrows(true);
                    }}
                    onMouseLeave={() => {
                        plugin.current.play();
                        setShowArrows(false);
                    }}
                >
                    <CarouselContent>
                        {tips.map((tip, index) => (
                            <CarouselItem key={index}>
                                <div className="relative p-0">
                                    <Card>
                                        <CardContent className="flex flex-col items-center justify-center rounded-b-none rounded-t-lg p-0">
                                            <img
                                                src={tip.src}
                                                alt={tip.alt}
                                                className="w-full h-auto rounded-b-none rounded-t-lg"
                                            />
                                            <div className="absolute bottom-0 flex items-center justify-center text-white bg-gradient-to-t from-black to-transparent text-center w-full p-4 py-6">
                                                {tip.description}
                                            </div>
                                        </CardContent>
                                    </Card>
                                </div>
                            </CarouselItem>
                        ))}
                    </CarouselContent>
                    {showArrows && (
                        <>
                            <CarouselPrevious className="absolute left-0" />
                            <CarouselNext className="absolute right-0" />
                        </>
                    )}
                </Carousel>
            )}
            <div className="flex flex-col gap-4 text-center p-2">
                <div className="text-center font-bold text-xl">
                    Sign in to unlock your second brain
                </div>
            </div>
            <div className="flex flex-col gap-8 pb-4 text-center align-middle items-center">
                {Object.entries(data || {}).map(([providerKey, providerData]) => {
                    if (!providerData) return null;

                    const providerName =
                        providerData.provider_name ||
                        providerKey.charAt(0).toUpperCase() + providerKey.slice(1);

                    return (
                        <Button
                            key={providerKey}
                            variant="outline"
                            className="w-[300px] p-8 flex gap-2 items-center justify-center rounded-lg font-bold"
                            onClick={() => handleOAuthSignIn(providerKey)}
                            disabled={
                                isLoading ||
                                !providerData?.client_id ||
                                !providerData?.redirect_uri
                            }
                        >
                            {isLoading ? (
                                <Spinner className="h-6 w-6" />
                            ) : (
                                <svg
                                    viewBox="0 0 24 24"
                                    fill="none"
                                    stroke="currentColor"
                                    strokeWidth="2"
                                    strokeLinecap="round"
                                    strokeLinejoin="round"
                                    className="h-6 w-6"
                                >
                                    <path d="M15 3h4a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2h-4" />
                                    <polyline points="10 17 15 12 10 7" />
                                    <line x1="15" y1="12" x2="3" y2="12" />
                                </svg>
                            )}
                            {providerData.button_label || `Continue with ${providerName}`}
                        </Button>
                    );
                })}

                <Button
                    variant="outline"
                    className="w-[300px] p-8 flex gap-2 items-center justify-center rounded-lg font-bold"
                    onClick={() => {
                        setUseEmailSignIn(true);
                    }}
                >
                    <PaperPlaneTilt className="h-6 w-6" />
                    Continue with Email
                </Button>
            </div>
            <div className="text-center text-muted-foreground text-sm mb-[20px]">
                By logging in, you agree to our{" "}
                <Link href="https://khoj.dev/terms-of-service">Terms of Service</Link>. See{" "}
                <Link href="https://khoj.dev/privacy-policy">Privacy Policy</Link>.
            </div>
        </div>
    );
}
