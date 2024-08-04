
import styles from './chatInputArea.module.css';
import React, { useEffect, useRef, useState } from 'react';

import { uploadDataForIndexing } from '../../common/chatFunctions';
import { Progress } from "@/components/ui/progress"

import 'katex/dist/katex.min.css';
import {
    ArrowRight,
    ArrowUp,
    Browser,
    ChatsTeardrop,
    GlobeSimple,
    Gps,
    Image,
    Microphone,
    Notebook,
    Paperclip,
    Question,
    Robot,
    Shapes,
    Stop,
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
import LoginPrompt from '../loginPrompt/loginPrompt';
import { Tooltip, TooltipContent, TooltipProvider, TooltipTrigger } from '@/components/ui/tooltip';
import { InlineLoading } from '../loading/loading';
import { convertToBGClass } from '@/app/common/colorUtils';

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
    agentColor?: string;
}

export default function ChatInputArea(props: ChatInputProps) {
    const [message, setMessage] = useState('');
    const fileInputRef = useRef<HTMLInputElement>(null);

    const [warning, setWarning] = useState<string | null>(null);
    const [error, setError] = useState<string | null>(null);
    const [uploading, setUploading] = useState(false);
    const [loginRedirectMessage, setLoginRedirectMessage] = useState<string | null>(null);
    const [showLoginPrompt, setShowLoginPrompt] = useState(false);

    const [recording, setRecording] = useState(false);
    const [mediaRecorder, setMediaRecorder] = useState<MediaRecorder | null>(null);

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
        if (!message.trim()) return;

        if (!props.isLoggedIn) {
            setLoginRedirectMessage('Hey there, you need to be signed in to send messages to Khoj AI');
            setShowLoginPrompt(true);
            return;
        }

        props.sendMessage(message.trim());
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

        if (!props.isLoggedIn) {
            setLoginRedirectMessage('Whoa! You need to login to upload files');
            setShowLoginPrompt(true);
            return;
        }

        uploadDataForIndexing(
            event.target.files,
            setWarning,
            setUploading,
            setError,
            props.setUploadedFiles,
            props.conversationId);
    }

    function getIconForSlashCommand(command: string) {
        const className = 'h-4 w-4 mr-2';
        if (command.includes('summarize')) {
            return <Gps className={className} />
        }

        if (command.includes('help')) {
            return <Question className={className} />
        }

        if (command.includes('automation')) {
            return <Robot className={className} />
        }

        if (command.includes('webpage')) {
            return <Browser className={className} />
        }

        if (command.includes('notes')) {
            return <Notebook className={className} />
        }

        if (command.includes('image')) {
            return <Image className={className} />
        }

        if (command.includes('default')) {
            return <Shapes className={className} />
        }

        if (command.includes('general')) {
            return <ChatsTeardrop className={className} />
        }

        if (command.includes('online')) {
            return <GlobeSimple className={className} />
        }
        return <ArrowRight className={className} />
    }

    // Assuming this function is added within the same context as the provided excerpt
    async function startRecordingAndTranscribe() {
        try {
            const microphone = await navigator.mediaDevices.getUserMedia({ audio: true });
            const mediaRecorder = new MediaRecorder(microphone, { mimeType: 'audio/webm' });

            const audioChunks: Blob[] = [];

            mediaRecorder.ondataavailable = async (event) => {
                audioChunks.push(event.data);
                const audioBlob = new Blob(audioChunks, { type: 'audio/webm' });
                const formData = new FormData();
                formData.append('file', audioBlob);

                // Send the incremental audio blob to the server
                try {
                    const response = await fetch('/api/transcribe', {
                        method: 'POST',
                        body: formData,
                    });

                    if (!response.ok) {
                        throw new Error('Network response was not ok');
                    }

                    const transcription = await response.json();
                    setMessage(transcription.text.trim());
                } catch (error) {
                    console.error('Error sending audio to server:', error);
                }
            };

            // Send an audio blob every 1.5 seconds
            mediaRecorder.start(1500);

            mediaRecorder.onstop = async () => {
                const audioBlob = new Blob(audioChunks, { type: 'audio/webm' });
                const formData = new FormData();
                formData.append('file', audioBlob);

                // Send the audio blob to the server
                try {
                    const response = await fetch('/api/transcribe', {
                        method: 'POST',
                        body: formData,
                    });

                    if (!response.ok) {
                        throw new Error('Network response was not ok');
                    }

                    const transcription = await response.json();
                    mediaRecorder.stream.getTracks().forEach(track => track.stop());
                    setMediaRecorder(null);
                    setMessage(transcription.text.trim());
                } catch (error) {
                    console.error('Error sending audio to server:', error);
                }
            };

            setMediaRecorder(mediaRecorder);
        } catch (error) {
            console.error("Error getting microphone", error);
        }
    }

    useEffect(() => {
        if (!recording && mediaRecorder) {
            mediaRecorder.stop();
        }

        if (recording && !mediaRecorder) {
            startRecordingAndTranscribe();
        }

    }, [recording, mediaRecorder]);

    const chatInputRef = useRef<HTMLTextAreaElement>(null);
    useEffect(() => {
        if (!chatInputRef.current) return;
        chatInputRef.current.style.height = 'auto';
        chatInputRef.current.style.height = Math.max(chatInputRef.current.scrollHeight - 24, 64) + 'px';
    }, [message]);

    return (
        <>
            {
                showLoginPrompt && loginRedirectMessage && (
                    <LoginPrompt
                        onOpenChange={setShowLoginPrompt}
                        loginRedirectMessage={loginRedirectMessage} />
                )
            }
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
            <div className={`${styles.actualInputArea} items-center justify-between dark:bg-neutral-700`}>
                <input
                    type="file"
                    multiple={true}
                    ref={fileInputRef}
                    onChange={handleFileChange}
                    style={{ display: 'none' }}
                />
                <Button
                    variant={'ghost'}
                    className="!bg-none p-0 m-2 h-auto text-3xl rounded-full text-gray-300 hover:text-gray-500"
                    disabled={props.sendDisabled}
                    onClick={handleFileButtonClick}>
                    <Paperclip className='w-8 h-8' />
                </Button>
                <div className="grid w-full gap-1.5 relative">
                    <Textarea
                        ref={chatInputRef}
                        className={`border-none w-full h-16 min-h-16 max-h-[128px] md:py-4 rounded-lg resize-none dark:bg-neutral-700 ${props.isMobileWidth ? 'text-md' : 'text-lg'}`}
                        placeholder="Type / to see a list of commands"
                        id="message"
                        autoFocus={true}
                        value={message}
                        onKeyDown={(e) => {
                            if (e.key === 'Enter' && !e.shiftKey) {
                                e.preventDefault();
                                onSendMessage();
                            }
                        }}
                        onChange={(e) => setMessage(e.target.value)}
                        disabled={props.sendDisabled || recording} />
                </div>
                {
                    recording ?
                        <TooltipProvider>
                            <Tooltip>
                                <TooltipTrigger asChild>
                                    <Button
                                        variant='default'
                                        className={`${!recording && 'hidden'} ${props.agentColor ? convertToBGClass(props.agentColor) : 'bg-orange-300 hover:bg-orange-500'} rounded-full p-1 m-2 h-auto text-3xl transition transform md:hover:-translate-y-1`}
                                        onClick={() => {
                                            setRecording(!recording);
                                        }}
                                        disabled={props.sendDisabled}
                                    >
                                        <Stop weight='fill' className='w-6 h-6' />
                                    </Button>
                                </TooltipTrigger>
                                <TooltipContent>
                                    Click to stop recording and transcribe your voice.
                                </TooltipContent>
                            </Tooltip>
                        </TooltipProvider>
                        :
                        (
                            mediaRecorder ?
                                <InlineLoading />
                                :
                                < TooltipProvider >
                                    <Tooltip>
                                        <TooltipTrigger asChild>
                                            <Button
                                                variant='default'
                                                className={`${(!message || recording) || 'hidden'} ${props.agentColor ? convertToBGClass(props.agentColor) : 'bg-orange-300 hover:bg-orange-500'} rounded-full p-1 m-2 h-auto text-3xl transition transform md:hover:-translate-y-1`}
                                                onClick={() => {
                                                    setMessage("Listening...");
                                                    setRecording(!recording);
                                                }}
                                                disabled={props.sendDisabled}
                                            >
                                                <Microphone weight='fill' className='w-6 h-6' />
                                            </Button>
                                        </TooltipTrigger>
                                        <TooltipContent>
                                            Click to transcribe your message with voice.
                                        </TooltipContent>
                                    </Tooltip>
                                </TooltipProvider>
                        )
                }
                <Button
                    className={`${(!message || recording) && 'hidden'} ${props.agentColor ? convertToBGClass(props.agentColor) : 'bg-orange-300 hover:bg-orange-500'} rounded-full p-1 m-2 h-auto text-3xl transition transform md:hover:-translate-y-1`}
                    onClick={onSendMessage}
                    disabled={props.sendDisabled}>
                    <ArrowUp className='w-6 h-6' weight='bold' />
                </Button>
            </div >
        </>
    )
}
