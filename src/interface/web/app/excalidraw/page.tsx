"use client";

import dynamic from "next/dynamic";
import { Suspense } from "react";
import Loading from "../components/loading/loading";

// Since client components get prerenderd on server as well hence importing
// the excalidraw stuff dynamically with ssr false

const ExcalidrawWrapper = dynamic(() => import("./excalidrawWrapper").then((mod) => mod.default), {
    ssr: false,
});

export default function Page() {
    return (
        <Suspense fallback={<Loading />}>
            <ExcalidrawWrapper />
        </Suspense>
    );
}
