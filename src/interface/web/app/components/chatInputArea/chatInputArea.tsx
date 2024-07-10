
import styles from './chatInputArea.module.css';
import React, { useEffect, useRef, useState } from 'react';

import { uploadDataForIndexing } from '../../common/chatFunctions';
import { Progress } from "@/components/ui/progress"

import 'katex/dist/katex.min.css';
import {
    ArrowCircleUp,
    ArrowRight,
    Browser,
    ChatsTeardrop,
    FileArrowUp,
    GlobeSimple,
    Gps,
    Image,
    Microphone,
    Notebook,
    Question,
    Robot,
    Shapes
} from '@phosphor-icons/react';

import {
    Command,
    CommandEmpty,
    CommandGroup,
    CommandInput,
    CommandItem,
    CommandList,
    CommandSeparator,
} from "@/components/ui/command"

import { Textarea } from "@/components/ui/textarea"
import { Button } from '@/components/ui/button';
import {
    AlertDialog,
    AlertDialogAction,
    AlertDialogContent,
    AlertDialogDescription,
    AlertDialogHeader,
    AlertDialogTitle
} from '@/components/ui/alert-dialog';
import { Popover, PopoverContent } from '@/components/ui/popover';
import { PopoverTrigger } from '@radix-ui/react-popover';

export interface ChatOptions {
    [key: string]: string
}

interface ChatInputProps {
    sendMessage: (message: string) => void;
    sendDisabled: boolean;
    setUploadedFiles?: (files: string[]) => void;
    conversationId?: string | null;
    chatOptionsData?: ChatOptions | null;
    isMobileWidth?: boolean;
    isLoggedIn: boolean;
}

export default function ChatInputArea(props: ChatInputProps) {
    const [message, setMessage] = useState('');
    const fileInputRef = useRef<HTMLInputElement>(null);

    const [warning, setWarning] = useState<string | null>(null);
    const [error, setError] = useState<string | null>(null);
    const [uploading, setUploading] = useState(false);

    const [progressValue, setProgressValue] = useState(0);

    useEffect(() => {
        if (!uploading) {
            setProgressValue(0);
        }

        if (uploading) {
            const interval = setInterval(() => {
                setProgressValue((prev) => {
                    const increment = Math.floor(Math.random() * 5) + 1; // Generates a random number between 1 and 5
                    const nextValue = prev + increment;
                    return nextValue < 100 ? nextValue : 100; // Ensures progress does not exceed 100
                });
            }, 800);
            return () => clearInterval(interval);
        }
    }, [uploading]);

    function onSendMessage() {
        props.sendMessage(message);
        setMessage('');
    }

    function handleSlashCommandClick(command: string) {
        setMessage(`/${command} `);
    }

    function handleFileButtonClick() {
        if (!fileInputRef.current) return;
        fileInputRef.current.click();
    }

    function handleFileChange(event: React.ChangeEvent<HTMLInputElement>) {
        if (!event.target.files) return;

        uploadDataForIndexing(
            event.target.files,
            setWarning,
            setUploading,
            setError,
            props.setUploadedFiles,
            props.conversationId);
    }

    function getIconForSlashCommand(command: string) {
        if (command.includes('summarize')) {
            return <Gps className='h-4 w-4 mr-2' />
        }

        if (command.includes('help')) {
            return <Question className='h-4 w-4 mr-2' />
        }

        if (command.includes('automation')) {
            return <Robot className='h-4 w-4 mr-2' />
        }

        if (command.includes('webpage')) {
            return <Browser className='h-4 w-4 mr-2' />
        }

        if (command.includes('notes')) {
            return <Notebook className='h-4 w-4 mr-2' />
        }

        if (command.includes('image')) {
            return <Image className='h-4 w-4 mr-2' />
        }

        if (command.includes('default')) {
            return <Shapes className='h-4 w-4 mr-2' />
        }

        if (command.includes('general')) {
            return <ChatsTeardrop className='h-4 w-4 mr-2' />
        }

        if (command.includes('online')) {
            return <GlobeSimple className='h-4 w-4 mr-2' />
        }
        return <ArrowRight className='h-4 w-4 mr-2' />
    }

    return (
        <>
            {
                uploading && (
                    <AlertDialog
                        open={uploading}>
                        <AlertDialogContent>
                            <AlertDialogHeader>
                                <AlertDialogTitle>Uploading data. Please wait.</AlertDialogTitle>
                            </AlertDialogHeader>
                            <AlertDialogDescription>
                                <Progress
                                    indicatorColor='bg-slate-500'
                                    className='w-full h-2 rounded-full'
                                    value={progressValue} />
                            </AlertDialogDescription>
                            <AlertDialogAction className='bg-slate-400 hover:bg-slate-500' onClick={() => setUploading(false)}>Dismiss</AlertDialogAction>
                        </AlertDialogContent>
                    </AlertDialog>
                )}
            {
                warning && (
                    <AlertDialog
                        open={warning !== null}>
                        <AlertDialogContent>
                            <AlertDialogHeader>
                                <AlertDialogTitle>Data Upload Warning</AlertDialogTitle>
                            </AlertDialogHeader>
                            <AlertDialogDescription>{warning}</AlertDialogDescription>
                            <AlertDialogAction className='bg-slate-400 hover:bg-slate-500' onClick={() => setWarning(null)}>Close</AlertDialogAction>
                        </AlertDialogContent>
                    </AlertDialog>
                )
            }
            {
                error && (
                    <AlertDialog
                        open={error !== null}>
                        <AlertDialogContent>
                            <AlertDialogHeader>
                                <AlertDialogTitle>Oh no!</AlertDialogTitle>
                                <AlertDialogDescription>Something went wrong while uploading your data</AlertDialogDescription>
                            </AlertDialogHeader>
                            <AlertDialogDescription>{error}</AlertDialogDescription>
                            <AlertDialogAction className='bg-slate-400 hover:bg-slate-500' onClick={() => setError(null)}>Close</AlertDialogAction>
                        </AlertDialogContent>
                    </AlertDialog>
                )
            }
            {
                (message.startsWith('/') && message.split(' ').length === 1) &&
                <div className='flex justify-center text-center'>
                    <Popover
                        open={message.startsWith('/')}>
                        <PopoverTrigger className='flex justify-center text-center'>

                        </PopoverTrigger>
                        <PopoverContent
                            onOpenAutoFocus={(e) => e.preventDefault()}
                            className={`${props.isMobileWidth ? 'w-[100vw]' : 'w-full'} rounded-md`}>
                            <Command className='max-w-full'>
                                <CommandInput placeholder="Type a command or search..." value={message} className='hidden' />
                                <CommandList>
                                    <CommandEmpty>No matching commands.</CommandEmpty>
                                    <CommandGroup heading="Agent Tools">
                                        {props.chatOptionsData && Object.entries(props.chatOptionsData).map(([key, value]) => (
                                            <CommandItem
                                                key={key}
                                                className={`text-md`}
                                                onSelect={() => handleSlashCommandClick(key)}>
                                                <div
                                                    className='grid grid-cols-1 gap-1'>
                                                    <div
                                                        className='font-bold flex items-center'>
                                                        {getIconForSlashCommand(key)}
                                                        /{key}
                                                    </div>
                                                    <div>
                                                        {value}
                                                    </div>
                                                </div>
                                            </CommandItem>
                                        ))}
                                    </CommandGroup>
                                    <CommandSeparator />
                                </CommandList>
                            </Command>
                        </PopoverContent>
                    </Popover>
                </div>
            }
            <div className={`${styles.actualInputArea} flex items-center justify-between`}>

                <input
                    type="file"
                    multiple={true}
                    ref={fileInputRef}
                    onChange={handleFileChange}
                    style={{ display: 'none' }}
                />
                <Button
                    variant={'ghost'}
                    className="!bg-none p-1 h-auto text-3xl rounded-full text-gray-300 hover:text-gray-500"
                    disabled={props.sendDisabled}
                    onClick={handleFileButtonClick}>
                    <FileArrowUp weight='fill' />
                </Button>
                <div className="grid w-full gap-1.5 relative">
                    <Textarea
                        className='border-none min-h-[20px]'
                        placeholder="Type / to see a list of commands"
                        id="message"
                        value={message}
                        onKeyDown={(e) => {
                            if (e.key === 'Enter' && !e.shiftKey) {
                                e.preventDefault();
                                onSendMessage();
                            }
                        }}
                        onChange={(e) => setMessage(e.target.value)}
                        disabled={props.sendDisabled} />
                </div>
                <Button
                    variant={'ghost'}
                    className="!bg-none p-1 h-auto text-3xl rounded-full text-gray-300 hover:text-gray-500"
                    disabled={props.sendDisabled}>
                    <Microphone weight='fill' />
                </Button>
                <Button
                    className="bg-orange-300 hover:bg-orange-500 rounded-full p-0 h-auto text-3xl transition transform hover:-translate-y-1"
                    onClick={onSendMessage}
                    disabled={props.sendDisabled}>
                    <ArrowCircleUp />
                </Button>
            </div>
        </>
    )
}
