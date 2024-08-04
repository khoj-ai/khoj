'use client'

import styles from "./settings.module.css";
import "intl-tel-input/styles";

import { Suspense, useEffect, useState } from "react";
import { useToast } from "@/components/ui/use-toast"

import { useUserConfig, ModelOptions, UserConfig } from "../common/auth";
import { toTitleCase } from "../common/utils";

import { isValidPhoneNumber } from 'libphonenumber-js';

import { Button } from "@/components/ui/button";
import { InputOTP, InputOTPGroup, InputOTPSlot } from "@/components/ui/input-otp";
import { Input } from "@/components/ui/input";
import {
    Card,
    CardContent,
    CardFooter,
    CardHeader,
} from "@/components/ui/card";
import {
    DropdownMenu,
    DropdownMenuContent,
    DropdownMenuRadioGroup,
    DropdownMenuRadioItem,
    DropdownMenuTrigger,
} from "@/components/ui/dropdown-menu"
import {
    Table,
    TableBody,
    TableCell,
    TableRow,
} from "@/components/ui/table"
import {
    CommandInput,
    CommandList,
    CommandEmpty,
    CommandGroup,
    CommandItem,
    CommandDialog
} from "@/components/ui/command";

import {
    ArrowRight,
    ChatCircleText,
    Key,
    Palette,
    SpeakerHigh,
    UserCircle,
    FileMagnifyingGlass,
    Trash,
    Copy,
    CreditCard,
    CheckCircle,
    NotionLogo,
    GithubLogo,
    Files,
    WhatsappLogo,
    ExclamationMark,
    Plugs,
    CloudSlash,
    Laptop,
    Plus,
    FloppyDisk,
    PlugsConnected,
    ArrowCircleUp,
    ArrowCircleDown,
    ArrowsClockwise,
    Check,
    CaretDown,
    Waveform,
} from "@phosphor-icons/react";

import SidePanel from "../components/sidePanel/chatHistorySidePanel";
import Loading from "../components/loading/loading";

import IntlTelInput from 'intl-tel-input/react';


const ManageFilesModal: React.FC<{ onClose: () => void }> = ({ onClose }) => {
    const [syncedFiles, setSyncedFiles] = useState<string[]>([]);
    const [selectedFiles, setSelectedFiles] = useState<string[]>([]);
    const [searchQuery, setSearchQuery] = useState('');

    useEffect(() => {
        const fetchFiles = async () => {
            try {
                const response = await fetch('/api/content/computer');
                if (!response.ok) throw new Error('Failed to fetch files');

                // Extract resonse
                const syncedFiles = await response.json();
                // Validate response
                if (Array.isArray(syncedFiles)) {
                    // Set synced files state
                    setSyncedFiles(syncedFiles.toSorted());
                } else {
                    console.error('Unexpected data format from API');
                }
            } catch (error) {
                console.error('Error fetching files:', error);
            }
        };

        fetchFiles();
    }, []);

    const filteredFiles = syncedFiles.filter(file =>
        file.toLowerCase().includes(searchQuery.toLowerCase())
    );

    const deleteSelected = async () => {
        let filesToDelete = selectedFiles.length > 0 ? selectedFiles : filteredFiles;
        console.log("Delete selected files", filesToDelete);

        if (filesToDelete.length === 0) {
            console.log("No files to delete");
            return;
        }

        try {
            const response = await fetch('/api/content/files', {
                method: 'DELETE',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({ files: filesToDelete }),
            });

            if (!response.ok) throw new Error('Failed to delete files');

            // Update the syncedFiles state
            setSyncedFiles(prevFiles => prevFiles.filter(file => !filesToDelete.includes(file)));

            // Reset selectedFiles
            setSelectedFiles([]);

            console.log("Deleted files:", filesToDelete);
        } catch (error) {
            console.error('Error deleting files:', error);
        }
    };

    const deleteFile = async (filename: string) => {
        console.log("Delete selected file", filename);
        try {
            const response = await fetch(`/api/content/file?filename=${encodeURIComponent(filename)}`, {
                method: 'DELETE',
                headers: {
                    'Content-Type': 'application/json',
                },
            });

            if (!response.ok) throw new Error('Failed to delete file');

            // Update the syncedFiles state
            setSyncedFiles(prevFiles => prevFiles.filter(file => file !== filename));

            // Remove the file from selectedFiles if it's there
            setSelectedFiles(prevSelected => prevSelected.filter(file => file !== filename));

            console.log("Deleted file:", filename);
        } catch (error) {
            console.error('Error deleting file:', error);
        }
    };

    return (
        <CommandDialog open={true} onOpenChange={onClose}>
            <div className="flex flex-col h-full">
                <div className="flex-none p-4 bg-background border-b">
                    <CommandInput
                        placeholder="Find synced files"
                        value={searchQuery}
                        onValueChange={setSearchQuery}
                    />
                </div>

                <div className="flex-grow overflow-auto">
                    <CommandList>
                        <CommandEmpty>No such files synced.</CommandEmpty>
                        <CommandGroup heading="Synced files">
                            {filteredFiles.map((filename: string) => (
                                <CommandItem
                                    key={filename}
                                    value={filename}
                                    onSelect={(value) => {
                                        setSelectedFiles(prev =>
                                            prev.includes(value)
                                                ? prev.filter(f => f !== value)
                                                : [...prev, value]
                                        );
                                    }}
                                >
                                    <div className="flex items-center justify-between w-full">
                                        <div className={`flex items-center ${selectedFiles.includes(filename) ? 'font-semibold' : ''}`}>
                                            {selectedFiles.includes(filename) && <Check className="h-4 w-4 mr-2" />}
                                            <span className="break-all">{filename}</span>
                                        </div>
                                        <Button
                                            variant="outline"
                                            size="sm"
                                            onClick={() => deleteFile(filename)}
                                            className="ml-auto"
                                        >
                                            <Trash className="h-4 w-4" />
                                        </Button>
                                    </div>
                                </CommandItem>
                            ))}
                        </CommandGroup>
                    </CommandList>
                </div>

                <div className="flex-none p-4 bg-background border-t">
                    <div className="flex justify-between">
                        <Button
                            variant="outline"
                            size="sm"
                            onClick={deleteSelected}
                            className="mr-2"
                        >
                            <Trash className="h-4 w-4 mr-2" />
                            {selectedFiles.length > 0 ? `Delect Selected (${selectedFiles.length})` : "Delete All"}
                        </Button>
                    </div>
                </div>
            </div>
        </CommandDialog>
    );
}


interface DropdownComponentProps {
    items: ModelOptions[];
    selected: number;
    callbackFunc: (value: string) => Promise<void>;
}

const DropdownComponent: React.FC<DropdownComponentProps> = ({ items, selected, callbackFunc }) => {
    const [position, setPosition] = useState(selected?.toString() ?? "0");

    return !!selected && (
        <div className="overflow-hidden shadow-md rounded-lg">
            <DropdownMenu>
                <DropdownMenuTrigger asChild className="w-full rounded-lg">
                    <Button variant="outline" className="justify-start py-6 rounded-lg">
                        {items.find(item => item.id.toString() === position)?.name} <CaretDown className="h-4 w-4 ml-auto text-muted-foreground" />
                    </Button>
                </DropdownMenuTrigger>
                <DropdownMenuContent>
                    <DropdownMenuRadioGroup
                        value={position}
                        onValueChange={async (value) => { setPosition(value); await callbackFunc(value); }}
                    >
                        {items.map((item) => (
                            <DropdownMenuRadioItem key={item.id.toString()} value={item.id.toString()}>
                                {item.name}
                            </DropdownMenuRadioItem>
                        ))}
                    </DropdownMenuRadioGroup>
                </DropdownMenuContent>
            </DropdownMenu>
        </div>
    );
}

interface TokenObject {
    token: string;
    name: string;
}

const useApiKeys = () => {
    const [apiKeys, setApiKeys] = useState<TokenObject[]>([]);
    const { toast } = useToast();

    const generateAPIKey = async () => {
        try {
            const response = await fetch(`/auth/token`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
            });
            const tokenObj = await response.json();
            setApiKeys(prevKeys => [...prevKeys, tokenObj]);
        } catch (error) {
            console.error('Error generating API key:', error);
        }
    };

    const copyAPIKey = async (token: string) => {
        try {
            await navigator.clipboard.writeText(token);
            toast({
                title: "🔑 API Key",
                description: "Copied to clipboard",
            });
        } catch (error) {
            console.error('Error copying API key:', error);
        }
    };

    const deleteAPIKey = async (token: string) => {
        try {
            const response = await fetch(`/auth/token?token=${token}`, { method: 'DELETE' });
            if (response.ok) {
                setApiKeys(prevKeys => prevKeys.filter(key => key.token !== token));
            }
        } catch (error) {
            console.error('Error deleting API key:', error);
        }
    };

    const listApiKeys = async () => {
        try {
            const response = await fetch(`/auth/token`);
            const tokens = await response.json();
            if (tokens?.length > 0) {
                setApiKeys(tokens);
            }
        } catch (error) {
            console.error('Error listing API keys:', error);
        }
    };

    useEffect(() => {
        listApiKeys();
    }, []);

    return {
        apiKeys,
        generateAPIKey,
        copyAPIKey,
        deleteAPIKey,
    };
};

enum PhoneNumberValidationState {
  Setup = "setup",
  SendOTP = "otp",
  VerifyOTP = "verify",
  Verified = "verified",
}

export default function SettingsView() {
    const [title, setTitle] = useState("Settings");
    const [isMobileWidth, setIsMobileWidth] = useState(false);
    const {apiKeys, generateAPIKey, copyAPIKey, deleteAPIKey} = useApiKeys();
    const {userConfig: initialUserConfig} = useUserConfig(true);
    const [userConfig, setUserConfig] = useState<UserConfig | null>(null);
    const [name, setName] = useState<string | undefined>(undefined);
    const [notionToken, setNotionToken] = useState<string | null>(null);
    const [phoneNumber, setPhoneNumber] = useState<string | undefined>(undefined);
    const [otp, setOTP] = useState("");
    const [numberValidationState, setNumberValidationState] = useState<PhoneNumberValidationState>(PhoneNumberValidationState.Verified);
    const [isManageFilesModalOpen, setIsManageFilesModalOpen] = useState(false);
    const { toast } = useToast();
    const cardClassName = "w-full lg:w-1/3 grid grid-flow-column border border-gray-300 shadow-md rounded-lg bg-gradient-to-b from-background to-gray-50 dark:to-gray-950";

    useEffect(() => {
        setUserConfig(initialUserConfig);
        setPhoneNumber(initialUserConfig?.phone_number);
        setNumberValidationState(
            initialUserConfig?.is_phone_number_verified
            ? PhoneNumberValidationState.Verified
            : initialUserConfig?.phone_number
            ? PhoneNumberValidationState.SendOTP
            : PhoneNumberValidationState.Setup
        );
        setName(initialUserConfig?.given_name);
        setNotionToken(initialUserConfig?.notion_token ?? null);
    }, [initialUserConfig]);

    useEffect(() => {
        setIsMobileWidth(window.innerWidth < 786);
        const handleResize = () => setIsMobileWidth(window.innerWidth < 786);
        window.addEventListener('resize', handleResize);
        return () => window.removeEventListener('resize', handleResize);
    }, []);

    const sendOTP = async () => {
        try {
            const response = await fetch(`/api/phone?phone_number=${phoneNumber}`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
            });
            if (!response.ok) throw new Error('Failed to send OTP');

            setNumberValidationState(PhoneNumberValidationState.VerifyOTP);
        } catch (error) {
            console.error('Error sending OTP:', error);
            toast({
                title: "📱 Phone",
                description: "Failed to send OTP. Try again or contact us at team@khoj.dev",
            });
        }
    };

    const verifyOTP = async () => {
        try {
            const response = await fetch(`/api/phone/verify?code=${otp}`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
            });
            if (!response.ok) throw new Error('Failed to verify OTP');

            setNumberValidationState(PhoneNumberValidationState.Verified);
            toast({
                title: "📱 Phone",
                description: "Phone number verified",
            });
        } catch (error) {
            console.error('Error verifying OTP:', error);
            toast({
                title: "📱 Phone",
                description: "Failed to verify OTP. Try again or contact us at team@khoj.dev",
            });
        }
    };

    const disconnectNumber = async () => {
        try {
            const response = await fetch(`/api/phone`, {
                method: 'DELETE',
                headers: {
                    'Content-Type': 'application/json',
                },
            });
            if (!response.ok) throw new Error('Failed to disconnect phone number');

            setPhoneNumber(undefined);
            setNumberValidationState(PhoneNumberValidationState.Setup);
            toast({
                title: "📱 Phone",
                description: "Phone number disconnected",
            });
        } catch (error) {
            console.error('Error disconnecting phone number:', error);
            toast({
                title: "📱 Phone",
                description: "Failed to disconnect phone number. Try again or contact us at team@khoj.dev",
            });
        }
    }

    const setSubscription = async (state: string) => {
        try {
            const url = `/api/subscription?email=${userConfig?.username}&operation=${state}`;
            const response = await fetch(url, {
                method: 'PATCH',
                headers: {
                    'Content-Type': 'application/json',
                },
            });
            if (!response.ok) throw new Error('Failed to change subscription');

            // Set updated user settings
            if (userConfig) {
                let newUserConfig = userConfig;
                newUserConfig.subscription_state = state === "cancel" ? "unsubscribed" : "subscribed";
                setUserConfig(newUserConfig);
            }

            // Notify user of subscription change
            toast({
                title: "💳 Subscription",
                description: userConfig?.subscription_state === "unsubscribed" ? "Your subscription was cancelled" : "Your Futurist subscription has been renewed",
            });
        } catch (error) {
            console.error('Error changing subscription:', error);
            toast({
                title: "💳 Subscription",
                description: state === "cancel" ? "Failed to cancel subscription. Try again or contact us at team@khoj.dev" : "Failed to renew subscription. Try again or contact us at team@khoj.dev",
            });
        }
    };

    const saveName = async () => {
        if (!name) return;
        try {
            const response = await fetch(`/api/user/name?name=${name}`, {
                method: 'PATCH',
                headers: {
                    'Content-Type': 'application/json',
                },
            });
            if (!response.ok) throw new Error('Failed to update name');

            // Set updated user settings
            if (userConfig) {
                let newUserConfig = userConfig;
                newUserConfig.given_name = name;
                setUserConfig(newUserConfig);
            }

            // Notify user of name change
            toast({
                title: `✅ Updated Profile`,
                description: `You name has been updated to ${name}`,
            });
        } catch (error) {
            console.error('Error updating name:', error);
            toast({
                title: "⚠️ Failed to Update Profile",
                description: "Failed to update name. Try again or contact team@khoj.dev",
            });
        }
    }

    const updateModel = (name: string) => async (id: string) => {
        if (!userConfig?.is_active && name !== "search") return;
        try {
            const response = await fetch(`/api/model/${name}?id=` + id, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                }
            });

            if (!response.ok) throw new Error('Failed to update model');

            toast({
                title: `✅ Updated ${toTitleCase(name)} Model`,
            });
        } catch (error) {
            console.error(`Failed to update ${name} model:`, error);
            toast({
                description: `❌ Failed to update ${toTitleCase(name)} model. Try again.`,
                variant: "destructive",
            });
        }
    };

    const saveNotionToken = async () => {
        if (!notionToken) return;
        // Save Notion API key to server
        try {
            const response = await fetch(`/api/content/notion`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({ token: notionToken }),
            });
            if (!response.ok) throw new Error('Failed to save Notion API key');

            // Set updated user settings
            if (userConfig) {
                let newUserConfig = userConfig;
                newUserConfig.notion_token = notionToken;
                setUserConfig(newUserConfig);
            }

            // Notify user of Notion API key save
            toast({
                title: `✅ Saved Notion Settings`,
                description: `You Notion API key has been saved.`,
            });
        } catch (error) {
            console.error('Error updating name:', error);
            toast({
                title: "⚠️ Failed to Save Notion Settings",
                description: "Failed to save Notion API key. Try again or contact team@khoj.dev",
            });
        }
    }

    const syncContent = async (type: string) => {
        try {
            const response = await fetch(`/api/content?t=${type}`, {
                method: 'PATCH',
                headers: {
                    'Content-Type': 'application/json',
                },
            });
            if (!response.ok) throw new Error(`Failed to sync content from ${type}`);

            toast({
                title: `🔄 Syncing ${type}`,
                description: `Your ${type} content is being synced.`,
            });
        } catch (error) {
            console.error('Error syncing content:', error);
            toast({
                title: `⚠️ Failed to Sync ${type}`,
                description: `Failed to sync ${type} content. Try again or contact team@khoj.dev`,
            });
        }
    }

    const disconnectContent = async (type: string) => {
        try {
            const response = await fetch(`/api/content/${type}`, {
                method: 'DELETE',
                headers: {
                    'Content-Type': 'application/json',
                },
            });
            if (!response.ok) throw new Error(`Failed to disconnect ${type}`);

            // Set updated user settings
            if (userConfig) {
                let newUserConfig = userConfig;
                if (type === "computer") {
                    newUserConfig.enabled_content_source.computer = false;
                } else if (type === "notion") {
                    newUserConfig.enabled_content_source.notion = false;
                    newUserConfig.notion_token = null;
                    setNotionToken(newUserConfig.notion_token);
                } else if (type === "github") {
                    newUserConfig.enabled_content_source.github = false;
                }
                setUserConfig(newUserConfig);
            }

            // Notify user about disconnecting content source
            if (type === "computer") {
                toast({
                    title: `✅ Deleted Synced Files`,
                    description: "Your synced documents have been deleted.",
                });
            } else {
                toast({
                    title: `✅ Disconnected ${type}`,
                    description: `Your ${type} integration to Khoj has been disconnected.`,
                });
            }
        } catch (error) {
            console.error(`Error disconnecting ${type}:`, error);
            toast({
                title: `⚠️ Failed to Disconnect ${type}`,
                description: `Failed to disconnect from ${type}. Try again or contact team@khoj.dev`,
            });
        }
    }

    if (!userConfig) return <Loading />;

    return (
        <div className={styles.page}>
            <title>
                {title}
            </title>
            <div className={styles.sidePanel}>
                <SidePanel
                    conversationId={null}
                    uploadedFiles={[]}
                    isMobileWidth={isMobileWidth}
                />
            </div>
            <div className={styles.content}>
                <div className={`${styles.contentBody} mx-10 my-2`}>
                    <Suspense fallback={<Loading />}>
                        <div id="content" className="grid grid-flow-column sm:grid-flow-row gap-16 m-8">
                            <div className="section grid gap-8">
                                <div className="text-2xl">Profile</div>
                                <div className="cards flex flex-wrap gap-16">
                                    <Card className={cardClassName}>
                                        <CardHeader className="text-xl flex flex-row"><UserCircle className="h-7 w-7 mr-2"/>Name</CardHeader>
                                        <CardContent className="overflow-hidden">
                                            <p className="pb-4 text-gray-400">What should Khoj refer to you as?</p>
                                            <Input
                                                type="text"
                                                onChange={(e) => setName(e.target.value)}
                                                value={name}
                                                className="w-full border border-gray-300 rounded-lg p-4 py-6"
                                            />
                                        </CardContent>
                                        <CardFooter className="flex flex-wrap gap-4">
                                            <Button
                                                variant="outline"
                                                size="sm"
                                                onClick={saveName}
                                                disabled={name === userConfig.given_name}>
                                                    <FloppyDisk className="h-5 w-5 inline mr-2" />
                                                    Save
                                            </Button>
                                        </CardFooter>
                                    </Card>
                                    <Card id="billing" className={cardClassName}>
                                        <CardHeader className="text-xl flex flex-row">
                                            <CreditCard className="h-7 w-7 mr-2"/>
                                            Subscription
                                        </CardHeader>
                                        <CardContent className="grid gap-2 overflow-hidden">
                                            <p className="text-gray-400">Current Plan</p>
                                            {userConfig.subscription_state === "trial" && (
                                                <>
                                                    <p className="text-xl text-primary/80">Futurist (Trial)</p>
                                                    <p className="text-gray-400">You are on a 14 day trial of the Khoj Futurist plan. Check <a href="https://khoj.dev/pricing" target="_blank">pricing page</a> to compare plans.</p>
                                                </>
                                            ) || userConfig.subscription_state === "subscribed" && (
                                                <>
                                                    <p className="text-xl text-primary/80">Futurist</p>
                                                    <p className="text-gray-400">Subscription <b>renews</b> on <b>{ userConfig.subscription_renewal_date }</b></p>
                                                </>
                                            ) || userConfig.subscription_state === "unsubscribed" && (
                                                <>
                                                    <p className="text-xl">Futurist</p>
                                                    <p className="text-gray-400">Subscription <b>ends</b> on <b>{ userConfig.subscription_renewal_date }</b></p>
                                                </>
                                            ) || userConfig.subscription_state === "expired" && (
                                                <>
                                                    <p className="text-xl">Free Plan</p>
                                                    {userConfig.subscription_renewal_date && (
                                                        <p className="text-gray-400">Subscription <b>expired</b> on <b>{ userConfig.subscription_renewal_date }</b></p>
                                                    ) || (
                                                        <p className="text-gray-400">Check <a href="https://khoj.dev/pricing" target="_blank">pricing page</a> to compare plans.</p>
                                                    )}
                                                </>
                                            )}
                                        </CardContent>
                                        <CardFooter className="flex flex-wrap gap-4">
                                            {(userConfig.subscription_state == "subscribed") && (
                                                <Button
                                                    variant="outline"
                                                    className="hover:text-red-400"
                                                    onClick={() => setSubscription("cancel")}
                                                >
                                                    <ArrowCircleDown className="h-5 w-5 mr-2" />Unsubscribe
                                                </Button>
                                            ) || (userConfig.subscription_state == "unsubscribed") && (
                                                <Button
                                                    variant="outline"
                                                    className="text-primary/80 hover:text-primary"
                                                    onClick={() => setSubscription("resubscribe")}
                                                >
                                                    <ArrowCircleUp weight="bold" className="h-5 w-5 mr-2" />Resubscribe
                                                </Button>
                                            ) || (
                                                <Button
                                                    variant="outline"
                                                    className="text-primary/80 hover:text-primary"
                                                    onClick={() => window.open(`${userConfig.khoj_cloud_subscription_url}?prefilled_email=${userConfig.username}`, '_blank', 'noopener,noreferrer')}
                                                >
                                                   <ArrowCircleUp weight="bold" className="h-5 w-5 mr-2" />Subscribe
                                                </Button>
                                            )}
                                        </CardFooter>
                                    </Card>
                                </div>
                            </div>
                            {isManageFilesModalOpen && <ManageFilesModal onClose={() => setIsManageFilesModalOpen(false)} />}
                            <div className="section grid gap-8">
                                <div className="text-2xl">Content</div>
                                <div className="cards flex flex-wrap gap-16">
                                    <Card className={cardClassName}>
                                        <CardHeader className="flex flex-row text-2xl"><Laptop className="h-8 w-8 mr-2" />Files</CardHeader>
                                        <CardContent className="overflow-hidden pb-12 text-gray-400">
                                            Manage your synced files
                                        </CardContent>
                                        <CardFooter className="flex flex-wrap gap-4">
                                            <Button variant="outline" size="sm" onClick={() => setIsManageFilesModalOpen(true)}>
                                                <>
                                                    <Files className="h-5 w-5 inline mr-1" />Manage
                                                </>
                                            </Button>
                                            <Button
                                                variant="outline"
                                                size="sm"
                                                className={`${userConfig.enabled_content_source.computer || "hidden"}`}
                                                onClick={() => disconnectContent("computer")}
                                            >
                                                <CloudSlash className="h-5 w-5 inline mr-1" />Disable
                                            </Button>
                                        </CardFooter>
                                    </Card>
                                    <Card className={`${cardClassName} hidden`}>
                                        <CardHeader className="flex flex-row text-2xl"><GithubLogo className="h-8 w-8 mr-2" />Github</CardHeader>
                                        <CardContent className="overflow-hidden pb-12 text-gray-400">
                                            Set Github repositories to index
                                        </CardContent>
                                        <CardFooter className="flex flex-wrap gap-4">
                                            <Button variant="outline" size="sm">
                                                {userConfig.enabled_content_source.github && (
                                                    <>
                                                        <Files className="h-5 w-5 inline mr-1" />Manage
                                                    </>
                                                ) || (
                                                    <>
                                                        <Plugs className="h-5 w-5 inline mr-1" />Connect
                                                    </>
                                                )}
                                            </Button>
                                            <Button variant="outline" size="sm" className={`${userConfig.enabled_content_source.github || "hidden"}`}>
                                                <CloudSlash className="h-5 w-5 inline mr-1" />Disable
                                            </Button>
                                        </CardFooter>
                                    </Card>
                                    <Card className={cardClassName}>
                                        <CardHeader className="text-xl flex flex-row"><NotionLogo className="h-7 w-7 mr-2" />Notion</CardHeader>
                                        <CardContent className="grid gap-4">
                                            <p className="text-gray-400">Sync your Notion pages. See the <a href="https://docs.khoj.dev/data-sources/notion_integration/">setup instructions</a></p>
                                            {!userConfig.notion_oauth_url && (
                                            <Input
                                                onChange={(e) => setNotionToken(e.target.value)}
                                                value={notionToken || ""}
                                                placeholder="Enter API Key of your Khoj integration on Notion"
                                                className="w-full border border-gray-300 rounded-lg px-4 py-6"
                                            />
                                            )}
                                        </CardContent>
                                        <CardFooter className="flex flex-wrap gap-4">
                                               {(
                                                /* Show connect to notion button if notion oauth url setup and user disconnected*/
                                                userConfig.notion_oauth_url && !userConfig.enabled_content_source.notion
                                                ?
                                                    <Button variant="outline" size="sm" onClick={() => {window.open(userConfig.notion_oauth_url)}}>
                                                        <Plugs className="h-5 w-5 inline mr-1" />Connect
                                                    </Button>
                                                /* Show sync button if user connected to notion and API key unchanged */
                                                : userConfig.enabled_content_source.notion && notionToken === userConfig.notion_token
                                                ?
                                                    <Button variant="outline" size="sm" onClick={() => syncContent("notion")}>
                                                        <ArrowsClockwise className="h-5 w-5 inline mr-1" />Sync
                                                    </Button>
                                                /* Show set API key button notion oauth url not set setup */
                                                : !userConfig.notion_oauth_url
                                                ?
                                                    <Button variant="outline" size="sm" onClick={saveNotionToken} disabled={notionToken === userConfig.notion_token}>
                                                        <FloppyDisk className="h-5 w-5 inline mr-1" />
                                                        {userConfig.enabled_content_source.notion && "Update API Key" || "Set API Key"}
                                                    </Button>
                                                : <></>
                                                )}
                                            <Button
                                                variant="outline"
                                                size="sm"
                                                className={`${userConfig.notion_token || "hidden"}`}
                                                onClick={() => disconnectContent("notion")}
                                            >
                                                <CloudSlash className="h-5 w-5 inline mr-1" />Disconnect
                                            </Button>
                                        </CardFooter>
                                    </Card>
                                </div>
                            </div>
                            <div className="section grid gap-8">
                                <div className="text-2xl">Models</div>
                                <div className="cards flex flex-wrap gap-16">
                                    {userConfig.chat_model_options.length > 0 && (
                                    <Card className={cardClassName}>
                                        <CardHeader className="text-xl flex flex-row"><ChatCircleText className="h-7 w-7 mr-2"/>Chat</CardHeader>
                                        <CardContent className="overflow-hidden pb-12 grid gap-8">
                                            <p className="text-gray-400">Pick the chat model to generate text responses</p>
                                            <DropdownComponent
                                                items={userConfig.chat_model_options}
                                                selected={userConfig.selected_chat_model_config}
                                                callbackFunc={updateModel("chat")}
                                            />
                                        </CardContent>
                                        <CardFooter className="flex flex-wrap gap-4">
                                            {!userConfig.is_active && (
                                                <p className="text-gray-400">Subscribe to switch model</p>
                                            )}
                                        </CardFooter>
                                     </Card>
                                    )}
                                    {userConfig.search_model_options.length > 0 && (
                                    <Card className={cardClassName}>
                                        <CardHeader className="text-xl flex flex-row"><FileMagnifyingGlass className="h-7 w-7 mr-2"/>Search</CardHeader>
                                        <CardContent className="overflow-hidden pb-12 grid gap-8">
                                            <p className="text-gray-400">Pick the search model to find your documents</p>
                                            <DropdownComponent
                                                items={userConfig.search_model_options}
                                                selected={userConfig.selected_search_model_config}
                                                callbackFunc={updateModel("search")}
                                            />
                                        </CardContent>
                                        <CardFooter className="flex flex-wrap gap-4">
                                        </CardFooter>
                                    </Card>
                                    )}
                                    {userConfig.paint_model_options.length > 0 && (
                                    <Card className={cardClassName}>
                                        <CardHeader className="text-xl flex flex-row"><Palette className="h-7 w-7 mr-2"/>Paint</CardHeader>
                                        <CardContent className="overflow-hidden pb-12 grid gap-8">
                                            <p className="text-gray-400">Pick the paint model to generate image responses</p>
                                            <DropdownComponent
                                                items={userConfig.paint_model_options}
                                                selected={userConfig.selected_paint_model_config}
                                                callbackFunc={updateModel("paint")}
                                            />
                                        </CardContent>
                                        <CardFooter className="flex flex-wrap gap-4">
                                            {!userConfig.is_active && (
                                                <p className="text-gray-400">Subscribe to switch model</p>
                                            )}
                                        </CardFooter>
                                    </Card>
                                    )}
                                    {userConfig.voice_model_options.length > 0 && (
                                    <Card className={cardClassName}>
                                        <CardHeader className="text-xl flex flex-row"><Waveform className="h-7 w-7 mr-2"/>Voice</CardHeader>
                                        <CardContent className="overflow-hidden pb-12 grid gap-8">
                                            <p className="text-gray-400">Pick the voice model to generate speech responses</p>
                                            <DropdownComponent
                                                items={userConfig.voice_model_options}
                                                selected={userConfig.selected_voice_model_config}
                                                callbackFunc={updateModel("voice")}
                                            />
                                        </CardContent>
                                        <CardFooter className="flex flex-wrap gap-4">
                                            {!userConfig.is_active && (
                                                <p className="text-gray-400">Subscribe to switch model</p>
                                            )}
                                        </CardFooter>
                                    </Card>
                                    )}
                                </div>
                            </div>
                            <div className="section grid gap-8">
                                <div className="text-2xl">Clients</div>
                                <div className="cards flex flex-wrap gap-8">
                                    {!userConfig.anonymous_mode && (
                                    <Card className="grid grid-flow-column border border-gray-300 shadow-md rounded-lg bg-gradient-to-b from-background to-gray-50 dark:to-gray-950">
                                        <CardHeader className="text-xl grid grid-flow-col grid-cols-[1fr_auto] pb-0">
                                            <span className="flex flex-wrap">
                                                <Key className="h-7 w-7 mr-2" />API Keys
                                            </span>
                                            <Button variant="secondary" className="!mt-0" onClick={generateAPIKey}>
                                                <Plus weight="bold" className='h-5 w-5 mr-2' />Generate Key
                                            </Button>
                                        </CardHeader>
                                        <CardContent className="overflow-hidden grid gap-6">
                                            <p className="text-md text-gray-400">
                                            Access Khoj from the <a href="https://docs.khoj.dev/clients/Desktop" target="_blank">Desktop</a>, <a href="https://docs.khoj.dev/clients/Obsidian">Obsidian</a>, <a href="https://docs.khoj.dev/clients/Emacs">Emacs</a> apps and more.
                                            </p>
                                            <Table>
                                                <TableBody>
                                                    {apiKeys.map((key) => (
                                                        <TableRow key={key.token}>
                                                            <TableCell className="pl-0 py-3">{key.name}</TableCell>
                                                            <TableCell className="grid grid-flow-col grid-cols-[1fr_auto] bg-secondary rounded-xl p-3 m-1">
                                                                <span>
                                                                    {`${key.token.slice(0, 6)}...${key.token.slice(-4)}`}
                                                                </span>
                                                                <div className="grid grid-flow-col">
                                                                    <Copy
                                                                        weight="bold"
                                                                        className="h-4 w-4 mr-2 hover:bg-primary/40"
                                                                        onClick={() => {
                                                                            toast({title: `🔑 Copied API Key: ${key.name}`, description: `Set this API key in the Khoj apps you want to connect to this Khoj account`});
                                                                            copyAPIKey(key.token);
                                                                        }}
                                                                    />
                                                                    <Trash
                                                                        weight="bold"
                                                                        className='h-4 w-4 mr-2 md:ml-4 text-red-400 hover:bg-primary/40'
                                                                        onClick={() => {
                                                                            toast({title: `🔑 Deleted API Key: ${key.name}`, description: `Apps using this API key will no longer connect to this Khoj account`});
                                                                            deleteAPIKey(key.token);
                                                                        }}
                                                                    />
                                                                </div>
                                                            </TableCell>
                                                        </TableRow>
                                                    ))}
                                                </TableBody>
                                            </Table>
                                        </CardContent>
                                        <CardFooter className="flex flex-wrap gap-4">
                                        </CardFooter>
                                    </Card>
                                    )}
                                    <Card className={cardClassName}>
                                        <CardHeader className="text-xl flex flex-row">
                                            <WhatsappLogo className="h-7 w-7 mr-2"/>
                                            Chat on Whatsapp
                                            {numberValidationState === PhoneNumberValidationState.Verified && (
                                                <CheckCircle weight="bold" className="h-4 w-4 ml-1 text-green-400" />
                                            ) || numberValidationState !== PhoneNumberValidationState.Setup && (
                                                <ExclamationMark weight="bold" className="h-4 w-4 ml-1 text-yellow-400" />
                                            )}
                                        </CardHeader>
                                        <CardContent className="grid gap-4">
                                            <p className="text-gray-400">
                                                Connect your number to chat with Khoj on WhatsApp. Learn more about the integration <a href="https://docs.khoj.dev/clients/whatsapp">here</a>.
                                            </p>
                                            <div>
                                            <IntlTelInput
                                                initialValue={phoneNumber || ""}
                                                onChangeNumber={setPhoneNumber}
                                                disabled={numberValidationState === PhoneNumberValidationState.VerifyOTP}
                                                initOptions={{
                                                    separateDialCode: true,
                                                    initialCountry: "af",
                                                    utilsScript: "https://assets.khoj.dev/intl-tel-input%4023.8.0_build_js_utils.js",
                                                    containerClass: `${styles.phoneInput}`
                                            }}
                                            />
                                            {numberValidationState === PhoneNumberValidationState.VerifyOTP && (
                                                <>
                                                    <p>{`Enter the OTP sent to your number: ${phoneNumber}`}</p>
                                                    <InputOTP
                                                        autoFocus={true}
                                                        maxLength={6}
                                                        value={otp || ""}
                                                        onChange={setOTP}
                                                        onComplete={() => setNumberValidationState(PhoneNumberValidationState.VerifyOTP)}
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
                                                </>
                                            )}
                                            </div>
                                        </CardContent>
                                        <CardFooter className="flex flex-wrap gap-4">
                                            {numberValidationState === PhoneNumberValidationState.VerifyOTP && (
                                                <Button
                                                    variant="outline"
                                                    onClick={verifyOTP}
                                                >
                                                    Verify
                                                </Button>
                                            ) || (
                                                <Button
                                                    variant="outline"
                                                    disabled={!phoneNumber || (phoneNumber === userConfig.phone_number &&  numberValidationState === PhoneNumberValidationState.Verified) || !isValidPhoneNumber(phoneNumber)}
                                                    onClick={sendOTP}
                                                >
                                                    {!userConfig.phone_number
                                                    ? (<><Plugs className="inline mr-2" />Setup Whatsapp</>)
                                                    : !phoneNumber || (phoneNumber === userConfig.phone_number && numberValidationState === PhoneNumberValidationState.Verified) || !isValidPhoneNumber(phoneNumber)
                                                    ? (<><PlugsConnected className="inline mr-2 text-green-400" />Switch Number</>)
                                                    : (<>Send OTP <ArrowRight className="inline ml-2" weight="bold"/></>)
                                                    }
                                                </Button>
                                            )}
                                            {
                                                numberValidationState === PhoneNumberValidationState.Verified && (
                                                    <Button
                                                        variant="outline"
                                                        onClick={() => disconnectNumber()}
                                                    >
                                                        <CloudSlash className="h-5 w-5 mr-2" />Disconnect
                                                    </Button>
                                                )
                                            }
                                        </CardFooter>
                                    </Card>
                                </div>
                            </div>
                        </div>
                    </Suspense>
                </div>
            </div>
        </div>
    );
}
