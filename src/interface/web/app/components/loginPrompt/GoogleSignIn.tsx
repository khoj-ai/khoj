// GoogleSignIn.tsx
"use client";

import Script from "next/script";

interface GoogleSignInProps {
    onLoad?: () => void;
}

export function GoogleSignIn({ onLoad }: GoogleSignInProps) {
    return (
        <Script
            id="google-signin"
            src="https://accounts.google.com/gsi/client"
            async
            defer
            onLoad={onLoad}
        />
    );
}
