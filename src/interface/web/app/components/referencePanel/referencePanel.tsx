"use client";

import { useEffect, useState } from "react";

import { ArrowCircleDown, ArrowRight, Code, Note } from "@phosphor-icons/react";

import markdownIt from "markdown-it";
const md = new markdownIt({
    html: true,
    linkify: true,
    typographer: true,
});

import {
    Context,
    WebPage,
    OnlineContext,
    CodeContext,
    CodeContextFile,
} from "../chatMessage/chatMessage";
import { Card } from "@/components/ui/card";

import {
    Sheet,
    SheetContent,
    SheetDescription,
    SheetHeader,
    SheetTitle,
    SheetTrigger,
} from "@/components/ui/sheet";
import { Popover, PopoverContent, PopoverTrigger } from "@/components/ui/popover";
import DOMPurify from "dompurify";
import { getIconFromFilename } from "@/app/common/iconUtils";

interface NotesContextReferenceData {
    title: string;
    content: string;
}

interface NotesContextReferenceCardProps extends NotesContextReferenceData {
    showFullContent: boolean;
}

function extractSnippet(props: NotesContextReferenceCardProps): string {
    const hierarchicalFileExtensions = ["org", "md", "markdown"];
    const extension = props.title.split(".").pop() || "";
    const cleanContent = hierarchicalFileExtensions.includes(extension)
        ? props.content.split("\n").slice(1).join("\n")
        : props.content;
    return props.showFullContent
        ? DOMPurify.sanitize(md.render(cleanContent))
        : DOMPurify.sanitize(cleanContent);
}

function NotesContextReferenceCard(props: NotesContextReferenceCardProps) {
    const fileIcon = getIconFromFilename(
        props.title || ".txt",
        "w-6 h-6 text-muted-foreground inline-flex mr-2",
    );
    const fileName = props.title.split("/").pop() || props.title;
    const snippet = extractSnippet(props);
    const [isHovering, setIsHovering] = useState(false);

    return (
        <>
            <Popover open={isHovering && !props.showFullContent} onOpenChange={setIsHovering}>
                <PopoverTrigger asChild>
                    <Card
                        onMouseEnter={() => setIsHovering(true)}
                        onMouseLeave={() => setIsHovering(false)}
                        className={`${props.showFullContent ? "w-auto" : "w-[200px]"} overflow-hidden break-words text-balance rounded-lg border-none p-2 bg-muted`}
                    >
                        <h3
                            className={`${props.showFullContent ? "block" : "line-clamp-1"} text-muted-foreground}`}
                        >
                            {fileIcon}
                            {props.showFullContent ? props.title : fileName}
                        </h3>
                        <p
                            className={`text-sm ${props.showFullContent ? "overflow-x-auto block" : "overflow-hidden line-clamp-2"}`}
                            dangerouslySetInnerHTML={{ __html: snippet }}
                        ></p>
                    </Card>
                </PopoverTrigger>
                <PopoverContent className="w-[400px] mx-2">
                    <Card
                        className={`w-auto overflow-hidden break-words text-balance rounded-lg border-none p-2`}
                    >
                        <h3 className={`line-clamp-2 text-muted-foreground}`}>
                            {fileIcon}
                            {props.title}
                        </h3>
                        <p
                            className={`border-t mt-1 pt-1 text-sm overflow-hidden line-clamp-5`}
                            dangerouslySetInnerHTML={{ __html: snippet }}
                        ></p>
                    </Card>
                </PopoverContent>
            </Popover>
        </>
    );
}

interface CodeContextReferenceCardProps {
    code: string;
    output: string;
    output_files: CodeContextFile[];
    error: string;
    showFullContent: boolean;
}

function CodeContextReferenceCard(props: CodeContextReferenceCardProps) {
    const fileIcon = getIconFromFilename(".py", "!w-4 h-4 text-muted-foreground flex-shrink-0");
    const sanitizedCodeSnippet = DOMPurify.sanitize(props.code);
    const [isHovering, setIsHovering] = useState(false);
    const [isDownloadHover, setIsDownloadHover] = useState(false);

    const handleDownload = (file: CodeContextFile) => {
        // Determine MIME type
        let mimeType = "text/plain";
        let byteString = file.b64_data;
        if (file.filename.match(/\.(png|jpg|jpeg|webp)$/)) {
            mimeType = `image/${file.filename.split(".").pop()}`;
            byteString = atob(file.b64_data);
        } else if (file.filename.endsWith(".json")) {
            mimeType = "application/json";
        } else if (file.filename.endsWith(".csv")) {
            mimeType = "text/csv";
        }

        const arrayBuffer = new ArrayBuffer(byteString.length);
        const bytes = new Uint8Array(arrayBuffer);

        for (let i = 0; i < byteString.length; i++) {
            bytes[i] = byteString.charCodeAt(i);
        }

        const blob = new Blob([arrayBuffer], { type: mimeType });
        const url = URL.createObjectURL(blob);
        const a = document.createElement("a");
        a.href = url;
        a.download = file.filename;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(url);
    };

    const renderOutputFiles = (files: CodeContextFile[], hoverCard: boolean) => {
        if (files?.length == 0) return null;
        return (
            <div
                className={`${hoverCard || props.showFullContent ? "border-t mt-1 pt-1" : undefined}`}
            >
                {files.slice(0, props.showFullContent ? undefined : 1).map((file, index) => {
                    return (
                        <div key={`${file.filename}-${index}`}>
                            <h4 className="text-sm text-muted-foreground flex items-center">
                                <span
                                    className={`overflow-hidden mr-2 font-bold ${props.showFullContent ? undefined : "line-clamp-1"}`}
                                >
                                    {file.filename}
                                </span>
                                <button
                                    className={`${hoverCard ? "hidden" : undefined}`}
                                    onClick={(e) => {
                                        e.preventDefault();
                                        handleDownload(file);
                                    }}
                                    onMouseEnter={() => setIsDownloadHover(true)}
                                    onMouseLeave={() => setIsDownloadHover(false)}
                                    title={`Download file: ${file.filename}`}
                                >
                                    <ArrowCircleDown
                                        className={`w-4 h-4`}
                                        weight={isDownloadHover ? "fill" : "regular"}
                                    />
                                </button>
                            </h4>
                            {file.filename.match(/\.(txt|org|md|csv|json)$/) ? (
                                <pre
                                    className={`${props.showFullContent ? "block" : "line-clamp-2"} text-sm mt-1 p-1 bg-background rounded overflow-x-auto`}
                                >
                                    {file.b64_data}
                                </pre>
                            ) : file.filename.match(/\.(png|jpg|jpeg|webp)$/) ? (
                                <img
                                    src={`data:image/${file.filename.split(".").pop()};base64,${file.b64_data}`}
                                    alt={file.filename}
                                    className="mt-1 max-h-32 rounded"
                                />
                            ) : null}
                        </div>
                    );
                })}
            </div>
        );
    };

    return (
        <>
            <Popover open={isHovering && !props.showFullContent} onOpenChange={setIsHovering}>
                <PopoverTrigger asChild>
                    <Card
                        onMouseEnter={() => setIsHovering(true)}
                        onMouseLeave={() => setIsHovering(false)}
                        className={`${props.showFullContent ? "w-auto" : "w-[200px]"} overflow-hidden break-words text-balance rounded-lg border-none p-2 bg-muted`}
                    >
                        <div className="flex flex-col px-1">
                            <div className="flex items-center gap-2">
                                {fileIcon}
                                <h3
                                    className={`overflow-hidden ${props.showFullContent ? "block" : "line-clamp-1"} text-muted-foreground flex-grow`}
                                >
                                    code {props.output_files?.length > 0 ? "artifacts" : ""}
                                </h3>
                            </div>
                            <pre
                                className={`text-xs pb-2 ${props.showFullContent ? "block overflow-x-auto" : props.output_files?.length > 0 ? "hidden" : "overflow-hidden line-clamp-3"}`}
                            >
                                {sanitizedCodeSnippet}
                            </pre>
                            {props.output_files?.length > 0 &&
                                renderOutputFiles(props.output_files, false)}
                        </div>
                    </Card>
                </PopoverTrigger>
                <PopoverContent className="w-[400px] mx-2">
                    <Card
                        className={`w-auto overflow-hidden break-words text-balance rounded-lg border-none p-2`}
                    >
                        <div className="flex items-center gap-2">
                            {fileIcon}
                            <h3
                                className={`overflow-hidden ${props.showFullContent ? "block" : "line-clamp-1"} text-muted-foreground flex-grow`}
                            >
                                code {props.output_files?.length > 0 ? "artifact" : ""}
                            </h3>
                        </div>
                        {(props.output_files?.length > 0 &&
                            renderOutputFiles(props.output_files?.slice(0, 1), true)) || (
                                <pre className="text-xs border-t mt-1 pt-1 overflow-hidden line-clamp-10">
                                    {sanitizedCodeSnippet}
                                </pre>
                            )}
                    </Card>
                </PopoverContent>
            </Popover>
        </>
    );
}

export interface CodeReferenceData {
    code: string;
    output: string;
    output_files: CodeContextFile[];
    error: string;
}

interface OnlineReferenceData {
    title: string;
    description: string;
    link: string;
}

interface OnlineReferenceCardProps extends OnlineReferenceData {
    showFullContent: boolean;
}

function GenericOnlineReferenceCard(props: OnlineReferenceCardProps) {
    const [isHovering, setIsHovering] = useState(false);

    if (!props.link || props.link.split(" ").length > 1) {
        return null;
    }

    let favicon = `https://www.google.com/s2/favicons?domain=globe`;
    let domain = "unknown";
    try {
        domain = new URL(props.link).hostname;
        favicon = `https://www.google.com/s2/favicons?domain=${domain}`;
    } catch (error) {
        console.warn(`Error parsing domain from link: ${props.link}`);
        return null;
    }

    const handleMouseEnter = () => {
        setIsHovering(true);
    };

    const handleMouseLeave = () => {
        setIsHovering(false);
    };

    return (
        <>
            <Popover open={isHovering && !props.showFullContent} onOpenChange={setIsHovering}>
                <PopoverTrigger asChild>
                    <Card
                        onMouseEnter={handleMouseEnter}
                        onMouseLeave={handleMouseLeave}
                        className={`${props.showFullContent ? "w-auto" : "w-[200px]"} overflow-hidden break-words text-balance rounded-lg border-none p-2 bg-muted`}
                    >
                        <div className="flex flex-col">
                            <a
                                href={props.link}
                                target="_blank"
                                rel="noreferrer"
                                className="!no-underline px-1"
                            >
                                <div className="flex items-center gap-2">
                                    <img src={favicon} alt="" className="!w-4 h-4 flex-shrink-0" />
                                    <h3
                                        className={`overflow-hidden ${props.showFullContent ? "block" : "line-clamp-1"} text-muted-foreground flex-grow`}
                                    >
                                        {domain}
                                    </h3>
                                </div>
                                <h3
                                    className={`overflow-hidden ${props.showFullContent ? "block" : "line-clamp-1"} font-bold`}
                                >
                                    {props.title}
                                </h3>
                                <p
                                    className={`overflow-hidden text-sm ${props.showFullContent ? "block" : "line-clamp-2"}`}
                                >
                                    {props.description}
                                </p>
                            </a>
                        </div>
                    </Card>
                </PopoverTrigger>
                <PopoverContent className="w-[400px] mx-2">
                    <Card
                        className={`w-auto overflow-hidden break-words text-balance rounded-lg border-none`}
                    >
                        <div className="flex flex-col">
                            <a
                                href={props.link}
                                target="_blank"
                                rel="noreferrer"
                                className="!no-underline px-1"
                            >
                                <div className="flex items-center gap-2">
                                    <img src={favicon} alt="" className="!w-4 h-4 flex-shrink-0" />
                                    <h3
                                        className={`overflow-hidden ${props.showFullContent ? "block" : "line-clamp-2"} text-muted-foreground flex-grow`}
                                    >
                                        {domain}
                                    </h3>
                                </div>
                                <h3
                                    className={`border-t mt-1 pt-1 overflow-hidden ${props.showFullContent ? "block" : "line-clamp-2"} font-bold`}
                                >
                                    {props.title}
                                </h3>
                                <p
                                    className={`overflow-hidden text-sm ${props.showFullContent ? "block" : "line-clamp-5"}`}
                                >
                                    {props.description}
                                </p>
                            </a>
                        </div>
                    </Card>
                </PopoverContent>
            </Popover>
        </>
    );
}

export function constructAllReferences(
    contextData: Context[],
    onlineData: OnlineContext,
    codeContext: CodeContext,
) {
    const onlineReferences: OnlineReferenceData[] = [];
    const contextReferences: NotesContextReferenceData[] = [];
    const codeReferences: CodeReferenceData[] = [];

    if (codeContext) {
        for (const [key, value] of Object.entries(codeContext)) {
            if (!value.results) {
                continue;
            }
            codeReferences.push({
                code: value.code,
                output: value.results.std_out,
                output_files: value.results.output_files,
                error: value.results.std_err,
            });
        }
    }

    if (onlineData) {
        let localOnlineReferences = [];
        for (const [key, value] of Object.entries(onlineData)) {
            if (value.answerBox) {
                localOnlineReferences.push({
                    title: value.answerBox.title,
                    description: value.answerBox.answer,
                    link: value.answerBox.source,
                });
            }
            if (value.knowledgeGraph) {
                localOnlineReferences.push({
                    title: value.knowledgeGraph.title,
                    description: value.knowledgeGraph.description,
                    link: value.knowledgeGraph.descriptionLink,
                });
            }

            if (value.webpages) {
                // If webpages is of type Array, iterate through it and add each webpage to the localOnlineReferences array
                if (value.webpages instanceof Array) {
                    let webPageResults = value.webpages.map((webPage) => {
                        return {
                            title: webPage.query,
                            description: webPage.snippet,
                            link: webPage.link,
                        };
                    });
                    localOnlineReferences.push(...webPageResults);
                } else {
                    let singleWebpage = value.webpages as WebPage;

                    // If webpages is an object, add the object to the localOnlineReferences array
                    localOnlineReferences.push({
                        title: singleWebpage.query,
                        description: singleWebpage.snippet,
                        link: singleWebpage.link,
                    });
                }
            }

            if (value.organic) {
                let organicResults = value.organic.map((organicContext) => {
                    return {
                        title: organicContext.title,
                        description: organicContext.snippet,
                        link: organicContext.link,
                    };
                });

                localOnlineReferences.push(...organicResults);
            }
        }

        onlineReferences.push(...localOnlineReferences);
    }

    if (contextData) {
        let localContextReferences = contextData.map((context) => {
            if (!context.compiled) {
                const fileContent = context as unknown as string;
                const title = fileContent.split("\n")[0];
                const content = fileContent.split("\n").slice(1).join("\n");
                return {
                    title: title,
                    content: content,
                };
            }
            return {
                title: context.file,
                content: context.compiled,
            };
        });

        contextReferences.push(...localContextReferences);
    }

    return {
        notesReferenceCardData: contextReferences,
        onlineReferenceCardData: onlineReferences,
        codeReferenceCardData: codeReferences,
    };
}

interface SimpleIconProps {
    type: string;
    link?: string;
}

function SimpleIcon(props: SimpleIconProps) {

    let favicon = ``;
    let domain = "unknown";

    if (props.link) {
        try {
            domain = new URL(props.link).hostname;
            favicon = `https://www.google.com/s2/favicons?domain=${domain}`;
        } catch (error) {
            console.warn(`Error parsing domain from link: ${props.link}`);
            return null;
        }
    }

    let symbol = null;

    const itemClasses = "!w-4 !h-4 text-muted-foreground inline-flex mr-2 rounded-lg";

    switch (props.type) {
        case "code":
            symbol = <Code className={`${itemClasses}`} />;
            break;
        case "online":
            symbol =
                <img
                    src={favicon}
                    alt=""
                    className={`${itemClasses}`}
                />;
            break;
        case "notes":
            symbol =
                <Note className={`${itemClasses}`} />;
            break;
        default:
            symbol = null;
    }

    console.log("symbol", symbol);

    if (!symbol) {
        return null;
    }

    return (
        <div className="flex items-center gap-2">
            {symbol}
        </div>
    );
}

export interface TeaserReferenceSectionProps {
    notesReferenceCardData: NotesContextReferenceData[];
    onlineReferenceCardData: OnlineReferenceData[];
    codeReferenceCardData: CodeReferenceData[];
    isMobileWidth: boolean;
}

export function TeaserReferencesSection(props: TeaserReferenceSectionProps) {
    const shouldShowShowMoreButton =
        props.notesReferenceCardData.length > 0 ||
        props.codeReferenceCardData.length > 0 ||
        props.onlineReferenceCardData.length > 0;

    const numReferences =
        props.notesReferenceCardData.length +
        props.codeReferenceCardData.length +
        props.onlineReferenceCardData.length;

    if (numReferences === 0) {
        return null;
    }

    return (
        <div className="pt-0 px-4 pb-4">
            <h3 className="inline-flex items-center">
                <p className="text-gray-400 m-2">{numReferences} sources</p>
                <div className={`flex flex-wrap gap-2 w-auto m-2`}>
                    {shouldShowShowMoreButton && (
                        <ReferencePanel
                            notesReferenceCardData={props.notesReferenceCardData}
                            onlineReferenceCardData={props.onlineReferenceCardData}
                            codeReferenceCardData={props.codeReferenceCardData}
                            isMobileWidth={props.isMobileWidth}
                        />
                    )}
                </div>
            </h3>
        </div>
    );
}

interface ReferencePanelDataProps {
    notesReferenceCardData: NotesContextReferenceData[];
    onlineReferenceCardData: OnlineReferenceData[];
    codeReferenceCardData: CodeReferenceData[];
    isMobileWidth: boolean;
}

export default function ReferencePanel(props: ReferencePanelDataProps) {
    const [numTeaserSlots, setNumTeaserSlots] = useState(3);

    useEffect(() => {
        setNumTeaserSlots(props.isMobileWidth ? 1 : 3);
    }, [props.isMobileWidth]);

    if (!props.notesReferenceCardData && !props.onlineReferenceCardData) {
        return null;
    }

    const codeDataToShow = props.codeReferenceCardData.slice(0, numTeaserSlots);
    const notesDataToShow = props.notesReferenceCardData.slice(
        0,
        numTeaserSlots - codeDataToShow.length,
    );
    const onlineDataToShow =
        notesDataToShow.length + codeDataToShow.length < numTeaserSlots
            ? props.onlineReferenceCardData.filter((online) => online.link).slice(
                0,
                numTeaserSlots - codeDataToShow.length - notesDataToShow.length,
            )
            : [];

    return (
        <Sheet>
            <SheetTrigger className="text-balance w-auto md:w-[200px] justify-start overflow-hidden break-words p-0 bg-transparent border-none text-gray-400 align-middle items-center m-0 inline-flex">
                {codeDataToShow.map((code, index) => {
                    return (
                        <SimpleIcon type="code" key={`code-${index}`} />
                    );
                })}
                {notesDataToShow.map((note, index) => {
                    return (
                        <SimpleIcon type="notes" key={`${note.title}-${index}`} />
                    );
                })}
                {onlineDataToShow.map((online, index) => {
                    return (
                        <SimpleIcon type="online" key={`${online.title}-${index}`} link={online.link} />
                    );
                })}
                <ArrowRight className="m-0" />
            </SheetTrigger>
            <SheetContent className="overflow-y-scroll">
                <SheetHeader>
                    <SheetTitle>References</SheetTitle>
                    <SheetDescription>View all references for this response</SheetDescription>
                </SheetHeader>
                <div className="flex flex-wrap gap-2 w-auto mt-2">
                    {props.codeReferenceCardData.map((code, index) => {
                        return (
                            <CodeContextReferenceCard
                                showFullContent={true}
                                {...code}
                                key={`code-${index}`}
                            />
                        );
                    })}
                    {props.notesReferenceCardData.map((note, index) => {
                        return (
                            <NotesContextReferenceCard
                                showFullContent={true}
                                {...note}
                                key={`${note.title}-${index}`}
                            />
                        );
                    })}
                    {props.onlineReferenceCardData.map((online, index) => {
                        return (
                            <GenericOnlineReferenceCard
                                showFullContent={true}
                                {...online}
                                key={`${online.title}-${index}`}
                            />
                        );
                    })}
                </div>
            </SheetContent>
        </Sheet>
    );
}
