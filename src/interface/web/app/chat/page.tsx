'use client'

import styles from './chat.module.css';
import React, { Suspense, useEffect, useRef, useState } from 'react';

import SuggestionCard from '../components/suggestions/suggestionCard';
import SidePanel from '../components/sidePanel/chatHistorySidePanel';
import ChatHistory from '../components/chatHistory/chatHistory';
import NavMenu from '../components/navMenu/navMenu';
import { useSearchParams } from 'next/navigation'
import Loading from '../components/loading/loading';

import { handleCompiledReferences, handleImageResponse, setupWebSocket, uploadDataForIndexing } from '../common/chatFunctions';
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
    CommandDialog,
    CommandEmpty,
    CommandGroup,
    CommandInput,
    CommandItem,
    CommandList,
    CommandSeparator,
    CommandShortcut,
} from "@/components/ui/command"

import { Textarea } from "@/components/ui/textarea"
import { Button } from '@/components/ui/button';
import { StreamMessage } from '../components/chatMessage/chatMessage';
import { AlertDialog, AlertDialogAction, AlertDialogContent, AlertDialogDescription, AlertDialogHeader, AlertDialogTitle, AlertDialogTrigger } from '@/components/ui/alert-dialog';
import { Popover, PopoverContent } from '@/components/ui/popover';
import { PopoverTrigger } from '@radix-ui/react-popover';

interface ChatInputProps {
    sendMessage: (message: string) => void;
    sendDisabled: boolean;
    setUploadedFiles?: (files: string[]) => void;
    conversationId?: string | null;
    chatOptionsData?: ChatOptions | null;
    isMobileWidth?: boolean;
}

function ChatInputArea(props: ChatInputProps) {
    const [message, setMessage] = useState('');
    const fileInputRef = useRef<HTMLInputElement>(null);

    const [warning, setWarning] = useState<string | null>(null);
    const [error, setError] = useState<string | null>(null);
    const [uploading, setUploading] = useState(false);

    const [progressValue, setProgressValue] = useState(0);

    useEffect(() => {
        if (message.startsWith('/')) {
            const command = message.split(' ')[0].substring(1);
        }
    }, [message]);

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

interface ChatOptions {
    [key: string]: string
}
const styleClassOptions = ['pink', 'blue', 'green', 'yellow', 'purple'];

interface ChatBodyDataProps {
    chatOptionsData: ChatOptions | null;
    setTitle: (title: string) => void;
    onConversationIdChange?: (conversationId: string) => void;
    setQueryToProcess: (query: string) => void;
    streamedMessages: StreamMessage[];
    setUploadedFiles: (files: string[]) => void;
    isMobileWidth?: boolean;
}


function ChatBodyData(props: ChatBodyDataProps) {
    const searchParams = useSearchParams();
    const conversationId = searchParams.get('conversationId');
    const [message, setMessage] = useState('');
    const [processingMessage, setProcessingMessage] = useState(false);

    useEffect(() => {
        if (conversationId) {
            props.onConversationIdChange?.(conversationId);
        }
    }, [conversationId, props.onConversationIdChange]);

    useEffect(() => {
        if (message) {
            setProcessingMessage(true);
            props.setQueryToProcess(message);
        }
    }, [message]);

    useEffect(() => {
        if (props.streamedMessages &&
            props.streamedMessages.length > 0 &&
            props.streamedMessages[props.streamedMessages.length - 1].completed) {

            setProcessingMessage(false);
        } else {
            setMessage('');
        }
    }, [props.streamedMessages]);

    if (!conversationId) {
        return (
            <div className={styles.suggestions}>
                {props.chatOptionsData && Object.entries(props.chatOptionsData).map(([key, value]) => (
                    <SuggestionCard
                        key={key}
                        title={`/${key}`}
                        body={value}
                        link='#' // replace with actual link if available
                        styleClass={styleClassOptions[Math.floor(Math.random() * styleClassOptions.length)]}
                    />
                ))}
            </div>
        );
    }

    return (
        <>
            <div className={false ? styles.chatBody : styles.chatBodyFull}>
                <ChatHistory
                    conversationId={conversationId}
                    setTitle={props.setTitle}
                    pendingMessage={processingMessage ? message : ''}
                    incomingMessages={props.streamedMessages} />
            </div>
            <div className={`${styles.inputBox} bg-background align-middle items-center justify-center px-3`}>
                <ChatInputArea
                    sendMessage={(message) => setMessage(message)}
                    sendDisabled={processingMessage}
                    chatOptionsData={props.chatOptionsData}
                    conversationId={conversationId}
                    isMobileWidth={props.isMobileWidth}
                    setUploadedFiles={props.setUploadedFiles} />
            </div>
        </>
    );
}

export default function Chat() {
    const [chatOptionsData, setChatOptionsData] = useState<ChatOptions | null>(null);
    const [isLoading, setLoading] = useState(true);
    const [title, setTitle] = useState('Khoj AI - Chat');
    const [conversationId, setConversationID] = useState<string | null>(null);
    const [chatWS, setChatWS] = useState<WebSocket | null>(null);
    const [messages, setMessages] = useState<StreamMessage[]>([]);
    const [queryToProcess, setQueryToProcess] = useState<string>('');
    const [processQuerySignal, setProcessQuerySignal] = useState(false);
    const [uploadedFiles, setUploadedFiles] = useState<string[]>([]);
    const [isMobileWidth, setIsMobileWidth] = useState(false);


    const handleWebSocketMessage = (event: MessageEvent) => {
        let chunk = event.data;

        let currentMessage = messages.find(message => !message.completed);

        if (!currentMessage) {
            console.error("No current message found");
            return;
        }

        // Process WebSocket streamed data
        if (chunk === "start_llm_response") {
            console.log("Started streaming", new Date());
        } else if (chunk === "end_llm_response") {
            currentMessage.completed = true;
        } else {
            // Get the current message
            // Process and update state with the new message
            if (chunk.includes("application/json")) {
                chunk = JSON.parse(chunk);
            }

            const contentType = chunk["content-type"];
            if (contentType === "application/json") {
                try {
                    if (chunk.image || chunk.detail) {
                        let responseWithReference = handleImageResponse(chunk);
                        console.log("Image response", responseWithReference);
                        if (responseWithReference.response) currentMessage.rawResponse = responseWithReference.response;
                        if (responseWithReference.online) currentMessage.onlineContext = responseWithReference.online;
                        if (responseWithReference.context) currentMessage.context = responseWithReference.context;
                    } else if (chunk.type == "status") {
                        currentMessage.trainOfThought.push(chunk.message);
                    } else if (chunk.type == "rate_limit") {
                        console.log("Rate limit message", chunk);
                        currentMessage.rawResponse = chunk.message;
                    } else {
                        console.log("any message", chunk);
                    }
                } catch (error) {
                    console.error("Error processing message", error);
                    currentMessage.completed = true;
                } finally {
                    // no-op
                }

            } else {
                // Update the current message with the new chunk
                if (chunk && chunk.includes("### compiled references:")) {
                    let responseWithReference = handleCompiledReferences(chunk, "");
                    currentMessage.rawResponse += responseWithReference.response;

                    if (responseWithReference.response) currentMessage.rawResponse = responseWithReference.response;
                    if (responseWithReference.online) currentMessage.onlineContext = responseWithReference.online;
                    if (responseWithReference.context) currentMessage.context = responseWithReference.context;
                } else {
                    // If the chunk is not a JSON object, just display it as is
                    currentMessage.rawResponse += chunk;
                }

            }
        };
        // Update the state with the new message, currentMessage
        setMessages([...messages]);
    }

    useEffect(() => {
        fetch('/api/chat/options')
            .then(response => response.json())
            .then((data: ChatOptions) => {
                setLoading(false);
                // Render chat options, if any
                if (data) {
                    setChatOptionsData(data);
                }
            })
            .catch(err => {
                console.error(err);
                return;
            });

        setIsMobileWidth(window.innerWidth < 786);

        window.addEventListener('resize', () => {
            setIsMobileWidth(window.innerWidth < 786);
        });

    }, []);

    useEffect(() => {
        if (chatWS && queryToProcess) {
            // Add a new object to the state
            const newStreamMessage: StreamMessage = {
                rawResponse: "",
                trainOfThought: [],
                context: [],
                onlineContext: {},
                completed: false,
                timestamp: (new Date()).toISOString(),
                rawQuery: queryToProcess || "",
            }
            setMessages(prevMessages => [...prevMessages, newStreamMessage]);
            console.log("Messages", messages);
            setProcessQuerySignal(true);
        } else {
            if (!chatWS) {
                console.error("No WebSocket connection available");
            }
            if (!queryToProcess) {
                console.error("No query to process");
            }
        }
    }, [queryToProcess]);

    useEffect(() => {
        if (processQuerySignal && chatWS) {
            setProcessQuerySignal(false);
            chatWS.onmessage = handleWebSocketMessage;
            chatWS?.send(queryToProcess);
        }
    }, [processQuerySignal]);

    useEffect(() => {
        (async () => {
            if (conversationId) {
                const newWS = await setupWebSocket(conversationId);
                setChatWS(newWS);
            }
        })();
    }, [conversationId]);

    const handleConversationIdChange = (newConversationId: string) => {
        setConversationID(newConversationId);
    };


    if (isLoading) {
        return <Loading />;
    }


    return (
        <div className={styles.main + " " + styles.chatLayout}>
            <title>
                {title}
            </title>
            <div className={styles.sidePanel}>
                <SidePanel webSocketConnected={chatWS !== null} conversationId={conversationId} uploadedFiles={uploadedFiles} />
            </div>
            <div className={styles.chatBox}>
                <NavMenu selected="Chat" title={title} />
                <div className={styles.chatBoxBody}>
                    <Suspense fallback={<Loading />}>
                        <ChatBodyData
                            streamedMessages={messages}
                            chatOptionsData={chatOptionsData}
                            setTitle={setTitle}
                            setQueryToProcess={setQueryToProcess}
                            setUploadedFiles={setUploadedFiles}
                            isMobileWidth={isMobileWidth}
                            onConversationIdChange={handleConversationIdChange} />
                    </Suspense>
                </div>
            </div>
        </div>
    )
}
