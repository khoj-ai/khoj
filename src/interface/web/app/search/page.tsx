'use client'

import { Input } from '@/components/ui/input';

import { useAuthenticatedData } from '../common/auth';
import { useEffect, useState } from 'react';
import SidePanel from '../components/sidePanel/chatHistorySidePanel';
import NavMenu from '../components/navMenu/navMenu';
import styles from './search.module.css';
import { ScrollArea } from '@/components/ui/scroll-area';
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from '@/components/ui/card';
import { ArrowLeft, ArrowRight, Folder, FolderOpen, GithubLogo, LinkSimple, MagnifyingGlass, NoteBlank, NotionLogo } from '@phosphor-icons/react';
import { Button } from '@/components/ui/button';

interface AdditionalData {
    file: string;
    source: string;
    compiled: string;
    heading: string;
}

interface SearchResult {
    type: string;
    additional: AdditionalData;
    entry: string;
    score: number;
    "corpus-id": string;
}

function getNoteTypeIcon(source: string) {
    if (source === 'notion') {
        return <NotionLogo className='text-muted-foreground' />;
    }
    if (source === 'github') {
        return <GithubLogo className='text-muted-foreground' />;
    }
    return <NoteBlank className='text-muted-foreground' />;
}

interface NoteResultProps {
    note: SearchResult;
    setFocusSearchResult: (note: SearchResult) => void;
}

function Note(props: NoteResultProps) {
    const note = props.note;
    const isFileNameURL = (note.additional.file || '').startsWith('http');
    const fileName = isFileNameURL ? note.additional.heading : note.additional.file.split('/').pop();

    return (
        <Card className='bg-secondary h-full shadow-sm rounded-lg bg-gradient-to-b from-background to-slate-50 dark:to-gray-950 border border-muted mb-4'>
            <CardHeader>
                <CardDescription>
                    <div className='p-1 border-muted border w-fit rounded-lg mb-2'>
                        {getNoteTypeIcon(note.additional.source)}
                    </div>
                </CardDescription>
                <CardTitle>
                    {fileName}
                </CardTitle>
            </CardHeader>
            <CardContent>
                <div className='text-sm line-clamp-4'>
                    {note.entry}
                </div>
                <Button onClick={() => props.setFocusSearchResult(note)} className='mt-2'>See content<ArrowRight className='inline ml-2' /></Button>
            </CardContent>
            <CardFooter>
                {
                    isFileNameURL ?
                        <a href={note.additional.file} target="_blank" className='underline text-sm bg-muted p-1 rounded-lg text-muted-foreground'>
                            <LinkSimple className='inline m-2' />{note.additional.file}
                        </a>
                        :
                        <div className='bg-muted p-1 text-sm rounded-lg text-muted-foreground'>
                            <FolderOpen className='inline m-2' />{note.additional.file}
                        </div>
                }
            </CardFooter>
        </Card>
    );
}

function focusNote(note: SearchResult) {
    const isFileNameURL = (note.additional.file || '').startsWith('http');
    const fileName = isFileNameURL ? note.additional.heading : note.additional.file.split('/').pop();
    return (
        <Card className='bg-secondary h-full shadow-sm rounded-lg bg-gradient-to-b from-background to-slate-50 dark:to-gray-950 border border-muted mb-4'>
            <CardHeader>
                <CardTitle>
                    {fileName}
                </CardTitle>
                <CardDescription className='mt-4'>
                    {
                        isFileNameURL ?
                            <a href={note.additional.file} target="_blank" className='underline text-sm bg-muted p-1 rounded-lg text-muted-foreground'>
                                <LinkSimple className='inline m-2' />{note.additional.file}
                            </a>
                            :
                            <div className='bg-muted p-1 text-sm rounded-lg text-muted-foreground'>
                                <FolderOpen className='inline m-2' />{note.additional.file}
                            </div>
                    }
                </CardDescription>

            </CardHeader>
            <CardContent>
                <div className='text-m'>
                    {note.entry}
                </div>
            </CardContent>
        </Card>
    );
}

export default function Search() {
    const authenticatedData = useAuthenticatedData();
    const [searchQuery, setSearchQuery] = useState('');
    const [isMobileWidth, setIsMobileWidth] = useState(false);
    const [title, setTitle] = useState('Search');
    const [searchResults, setSearchResults] = useState<SearchResult[]>([]);
    const [searchResultsLoading, setSearchResultsLoading] = useState(false);
    const [focusSearchResult, setFocusSearchResult] = useState<SearchResult | null>(null);

    useEffect(() => {
        setIsMobileWidth(window.innerWidth < 786);

        window.addEventListener('resize', () => {
            setIsMobileWidth(window.innerWidth < 786);
        });

    }, []);

    useEffect(() => {
        setTitle(isMobileWidth ? '' : 'Search');
    }, [isMobileWidth]);

    function search(query: string) {
        if (searchResultsLoading) {
            return;
        }

        if (!searchQuery.trim()) {
            return;
        }

        const apiUrl = `/api/search?q=${encodeURIComponent(searchQuery)}&client=web`;
        fetch(apiUrl, {
            method: 'GET',
            headers: {
                'Content-Type': 'application/json',
            }
        }).then(response => response.json())
            .then(data => {
                setSearchResults(data);
                setSearchResultsLoading(false);
            }).catch((error) => {
                console.error('Error:', error);
            });
    }

    console.log('searchResults', searchResults);

    return (
        <div className={`${styles.searchLayout}`}>
            <div className='h-full'>
                <SidePanel
                    webSocketConnected={true}
                    conversationId={null}
                    uploadedFiles={[]}
                    isMobileWidth={isMobileWidth}
                />
            </div>
            <div className="md:w-3/4 sm:w-full mr-auto ml-auto">
                <NavMenu title={title} selected='Chat' />
                <div className='p-4'>
                    {
                        isMobileWidth && <div className='font-bold'>Search</div>
                    }
                    <div className='flex justify-between items-center border-2 border-muted p-2 gap-4 rounded-lg'>
                        <MagnifyingGlass className='inline m-2' />
                        <Input
                            className='border-none'
                            onChange={(e) => setSearchQuery(e.target.value)}
                            type="search"
                            placeholder="Search Documents" />
                        <button className='px-4 rounded' onClick={() => search(searchQuery)}>
                            Search
                        </button>
                    </div>
                    {
                        focusSearchResult &&
                        <div className='mt-4'>
                            <Button onClick={() => setFocusSearchResult(null)} className='mb-4' variant={'outline'}>
                                <ArrowLeft className='inline mr-2' />
                                Back
                            </Button>
                            {focusNote(focusSearchResult)}
                        </div>
                    }
                    {
                        !focusSearchResult && searchResults.length > 0 &&
                        <div className='mt-4'>
                            <ScrollArea className="h-[80vh]">
                                {
                                    searchResults.map((result, index) => {
                                        return (
                                            <Note key={result["corpus-id"]}
                                                note={result}
                                                setFocusSearchResult={setFocusSearchResult} />
                                        );
                                    })
                                }
                            </ScrollArea>
                        </div>
                    }
                </div>
            </div>
        </div>
    );
}
