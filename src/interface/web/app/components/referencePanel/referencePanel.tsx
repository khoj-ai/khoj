'use client'

import { useEffect, useState } from "react";

import { ArrowRight, File } from "@phosphor-icons/react";

import markdownIt from "markdown-it";
const md = new markdownIt({
    html: true,
    linkify: true,
    typographer: true
});

import { Context, WebPage, OnlineContextData } from "../chatMessage/chatMessage";
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
import * as DomPurify from 'dompurify';

interface NotesContextReferenceData {
    title: string;
    content: string;
}

interface NotesContextReferenceCardProps extends NotesContextReferenceData {
    showFullContent: boolean;
}


function NotesContextReferenceCard(props: NotesContextReferenceCardProps) {
    const snippet = props.showFullContent ? DomPurify.sanitize(md.render(props.content)) : DomPurify.sanitize(props.content);
    const [isHovering, setIsHovering] = useState(false);

    return (
        <>
            <Popover
                open={isHovering && !props.showFullContent}
                onOpenChange={setIsHovering}
            >
                <PopoverTrigger asChild>
                    <Card
                        onMouseEnter={() => setIsHovering(true)}
                        onMouseLeave={() => setIsHovering(false)}
                        className={`${props.showFullContent ? 'w-auto' : 'w-[200px]'} overflow-hidden break-words text-balance rounded-lg p-2 bg-muted border-none`}
                    >
                        <h3 className={`${props.showFullContent ? 'block' : 'line-clamp-1'} text-muted-foreground}`}>
                            <File className='w-6 h-6 text-muted-foreground inline-flex' />
                            {props.title}
                        </h3>
                        <p className={`${props.showFullContent ? 'block' : 'overflow-hidden line-clamp-2'}`} dangerouslySetInnerHTML={{ __html: snippet }}></p>
                    </Card>
                </PopoverTrigger>
                <PopoverContent
                    className="w-[400px] mx-2">
                    <Card className={`w-auto overflow-hidden break-words text-balance rounded-lg p-2 border-none`}>
                        <h3 className={`line-clamp-2 text-muted-foreground}`}>
                            <File className='w-6 h-6 text-muted-foreground inline-flex' />
                            {props.title}
                        </h3>
                        <p className={`overflow-hidden line-clamp-3`} dangerouslySetInnerHTML={{ __html: snippet }}></p>
                    </Card>
                </PopoverContent>
            </Popover>
        </>
    )
}

export interface ReferencePanelData {
    notesReferenceCardData: NotesContextReferenceData[];
    onlineReferenceCardData: OnlineReferenceData[];
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

    if (!props.link || props.link.split(' ').length > 1) {
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
    }

    const handleMouseLeave = () => {
        setIsHovering(false);
    }

    return (
        <>
            <Popover
                open={isHovering && !props.showFullContent}
                onOpenChange={setIsHovering}
            >
                <PopoverTrigger asChild>
                    <Card
                        onMouseEnter={handleMouseEnter}
                        onMouseLeave={handleMouseLeave}
                        className={`${props.showFullContent ? 'w-auto' : 'w-[200px]'} overflow-hidden break-words rounded-lg text-balance p-2 bg-muted border-none`}>

                        <div className='flex flex-col'>
                            <a href={props.link} target="_blank" rel="noreferrer" className='!no-underline p-2'>
                                <div className='flex items-center'>
                                    <img src={favicon} alt="" className='!w-4 h-4 mr-2' />
                                    <h3 className={`overflow-hidden ${props.showFullContent ? 'block' : 'line-clamp-1'} text-muted-foreground`}>{domain}</h3>
                                </div>
                                <h3 className={`overflow-hidden ${props.showFullContent ? 'block' : 'line-clamp-1'} font-bold`}>{props.title}</h3>
                                <p className={`overflow-hidden ${props.showFullContent ? 'block' : 'line-clamp-2'}`}>{props.description}</p>
                            </a>
                        </div>
                    </Card>
                </PopoverTrigger>
                <PopoverContent
                    className="w-[400px] mx-2">
                    <Card
                        className={`w-auto overflow-hidden break-words text-balance rounded-lg border-none`}>

                        <div className='flex flex-col'>
                            <a href={props.link} target="_blank" rel="noreferrer" className='!no-underline p-2'>
                                <div className='flex items-center'>
                                    <img src={favicon} alt="" className='!w-4 h-4 mr-2' />
                                    <h3 className={`overflow-hidden ${props.showFullContent ? 'block' : 'line-clamp-2'} text-muted-foreground`}>{domain}</h3>
                                </div>
                                <h3 className={`overflow-hidden ${props.showFullContent ? 'block' : 'line-clamp-2'} font-bold`}>{props.title}</h3>
                                <p className={`overflow-hidden ${props.showFullContent ? 'block' : 'line-clamp-3'}`}>{props.description}</p>
                            </a>
                        </div>
                    </Card>
                </PopoverContent>
            </Popover>

        </>
    )
}

export function constructAllReferences(contextData: Context[], onlineData: { [key: string]: OnlineContextData }) {

    const onlineReferences: OnlineReferenceData[] = [];
    const contextReferences: NotesContextReferenceData[] = [];

    if (onlineData) {
        let localOnlineReferences = [];
        for (const [key, value] of Object.entries(onlineData)) {
            if (value.answerBox) {
                localOnlineReferences.push({
                    title: value.answerBox.title,
                    description: value.answerBox.answer,
                    link: value.answerBox.source
                });
            }
            if (value.knowledgeGraph) {
                localOnlineReferences.push({
                    title: value.knowledgeGraph.title,
                    description: value.knowledgeGraph.description,
                    link: value.knowledgeGraph.descriptionLink
                });
            }

            if (value.webpages) {
                // If webpages is of type Array, iterate through it and add each webpage to the localOnlineReferences array
                if (value.webpages instanceof Array) {
                    let webPageResults = value.webpages.map((webPage) => {
                        return {
                            title: webPage.query,
                            description: webPage.snippet,
                            link: webPage.link
                        }
                    });
                    localOnlineReferences.push(...webPageResults);
                } else {
                    let singleWebpage = value.webpages as WebPage;

                    // If webpages is an object, add the object to the localOnlineReferences array
                    localOnlineReferences.push({
                        title: singleWebpage.query,
                        description: singleWebpage.snippet,
                        link: singleWebpage.link
                    });
                }
            }

            if (value.organic) {
                let organicResults = value.organic.map((organicContext) => {
                    return {
                        title: organicContext.title,
                        description: organicContext.snippet,
                        link: organicContext.link
                    }
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
                const title = fileContent.split('\n')[0];
                const content = fileContent.split('\n').slice(1).join('\n');
                return {
                    title: title,
                    content: content
                };
            }
            return {
                title: context.file,
                content: context.compiled
            }
        });

        contextReferences.push(...localContextReferences);
    }

    return {
        notesReferenceCardData: contextReferences,
        onlineReferenceCardData: onlineReferences
    }
}


export interface TeaserReferenceSectionProps {
    notesReferenceCardData: NotesContextReferenceData[];
    onlineReferenceCardData: OnlineReferenceData[];
    isMobileWidth: boolean;
}

export function TeaserReferencesSection(props: TeaserReferenceSectionProps) {
    const [numTeaserSlots, setNumTeaserSlots] = useState(3);

    useEffect(() => {
        setNumTeaserSlots(props.isMobileWidth ? 1 : 3);
    }, [props.isMobileWidth]);

    const notesDataToShow = props.notesReferenceCardData.slice(0, numTeaserSlots);
    const onlineDataToShow = notesDataToShow.length < numTeaserSlots ? props.onlineReferenceCardData.slice(0, numTeaserSlots - notesDataToShow.length) : [];

    const shouldShowShowMoreButton = props.notesReferenceCardData.length > 0 || props.onlineReferenceCardData.length > 0;

    const numReferences = props.notesReferenceCardData.length + props.onlineReferenceCardData.length;

    if (numReferences === 0) {
        return null;
    }

    return (
        <div className={`${props.isMobileWidth ? 'p-0' : 'p-4'}`}>
            <h3 className="inline-flex items-center">
                References
                <p className="text-gray-400 m-2">
                    {numReferences} sources
                </p>
            </h3>
            <div className={`flex ${props.isMobileWidth ? 'w-[90vw]' : 'w-auto'} space-x-4 mt-2`}>
                {
                    notesDataToShow.map((note, index) => {
                        return <NotesContextReferenceCard showFullContent={false} {...note} key={`${note.title}-${index}`} />
                    })
                }
                {
                    onlineDataToShow.map((online, index) => {
                        return <GenericOnlineReferenceCard showFullContent={false} {...online} key={`${online.title}-${index}`} />
                    })
                }
                {
                    shouldShowShowMoreButton &&
                    <ReferencePanel
                        notesReferenceCardData={props.notesReferenceCardData}
                        onlineReferenceCardData={props.onlineReferenceCardData} />
                }
            </div>
        </div>
    )
}


interface ReferencePanelDataProps {
    notesReferenceCardData: NotesContextReferenceData[];
    onlineReferenceCardData: OnlineReferenceData[];
}

export default function ReferencePanel(props: ReferencePanelDataProps) {

    if (!props.notesReferenceCardData && !props.onlineReferenceCardData) {
        return null;
    }

    return (
        <Sheet>
            <SheetTrigger
                className='text-balance w-[200px] overflow-hidden break-words p-0 bg-transparent border-none text-gray-400 align-middle justify-center items-center !m-0 inline-flex'>
                View references <ArrowRight className='m-1' />
            </SheetTrigger>
            <SheetContent className="overflow-y-scroll">
                <SheetHeader>
                    <SheetTitle>References</SheetTitle>
                    <SheetDescription>View all references for this response</SheetDescription>
                </SheetHeader>
                <div className="flex flex-col w-auto gap-2 mt-2">
                    {
                        props.notesReferenceCardData.map((note, index) => {
                            return <NotesContextReferenceCard showFullContent={true} {...note} key={`${note.title}-${index}`} />
                        })
                    }
                    {
                        props.onlineReferenceCardData.map((online, index) => {
                            return <GenericOnlineReferenceCard showFullContent={true} {...online} key={`${online.title}-${index}`} />
                        })
                    }
                </div>
            </SheetContent>
        </Sheet>
    );
}
