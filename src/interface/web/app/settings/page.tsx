'use client'

import styles from "./settings.module.css";

import { Suspense, useEffect, useState } from "react";
import { useToast } from "@/components/ui/use-toast"

import { useUserConfig, ModelOptions, UserConfig } from "../common/auth";

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
} from "@phosphor-icons/react";

import NavMenu from "../components/navMenu/navMenu";
import SidePanel from "../components/sidePanel/chatHistorySidePanel";
import Loading from "../components/loading/loading";


interface DropdownComponentProps {
    items: ModelOptions[];
    selected: number;
    callbackFunc: (value: string) => Promise<void>;
}

const DropdownComponent: React.FC<DropdownComponentProps> = ({ items, selected, callbackFunc }) => {
    const [position, setPosition] = useState(selected?.toString() ?? "0");

    return !!selected && (
        <div className="overflow-hidden">
            <DropdownMenu>
                <DropdownMenuTrigger asChild className="w-full">
                    <Button variant="outline" className="justify-start py-6">
                        {items.find(item => item.id === Number(position))?.name}
                    </Button>
                </DropdownMenuTrigger>
                <DropdownMenuContent>
                    <DropdownMenuRadioGroup
                        value={position.toString()}
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

export const useApiKeys = () => {
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
    const { apiKeys, generateAPIKey, copyAPIKey, deleteAPIKey } = useApiKeys();
    const initialUserConfig = useUserConfig(true);
    const [userConfig, setUserConfig] = useState<UserConfig | null>(null);
    const [number, setNumber] = useState<string | undefined>(undefined);
    const [otp, setOTP] = useState("");
    const [numberValidationState, setNumberValidationState] = useState<PhoneNumberValidationState>(PhoneNumberValidationState.Verified);
    const { toast } = useToast();
    const cardClassName = "w-full lg:w-1/3 grid grid-flow-column border border-gray-300 shadow-md rounded-lg";

    useEffect(() => {
        setUserConfig(initialUserConfig);
        setNumber(initialUserConfig?.phone_number);
        setNumberValidationState(
            initialUserConfig?.is_phone_number_verified
            ? PhoneNumberValidationState.Verified
            : initialUserConfig?.phone_number
            ? PhoneNumberValidationState.SendOTP
            : PhoneNumberValidationState.Setup
        );
    }, [initialUserConfig]);

    useEffect(() => {
        setIsMobileWidth(window.innerWidth < 786);
        const handleResize = () => setIsMobileWidth(window.innerWidth < 786);
        window.addEventListener('resize', handleResize);
        return () => window.removeEventListener('resize', handleResize);
    }, []);

    const sendOTP = async () => {
        try {
            const response = await fetch(`/api/phone?phone_number=${number}`, {
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
                title: "💳 Billing",
                description: userConfig?.subscription_state === "unsubscribed" ? "Your subscription was cancelled" : "Your Futurist subscription has been renewed",
            });
        } catch (error) {
            console.error('Error changing subscription:', error);
            toast({
                title: "💳 Billing",
                description: state === "cancel" ? "Failed to cancel subscription. Try again or contact us at team@khoj.dev" : "Failed to renew subscription. Try again or contact us at team@khoj.dev",
            });
         }
    };

    const updateModel = (name: string) => async (id: string) => {
        try {
            const response = await fetch(`/api/model/${name}?id=` + id, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                }
            });

            if (!response.ok) throw new Error('Failed to update model');

            toast({
                description: `${name} model updated succesfully`,
            });
        } catch (error) {
            console.error(`Failed to update ${name} model:`, error);
            toast({
                description: `Failed to update ${name} model. Try again.`,
                variant: "destructive",
            });
        }
    };

    if (!userConfig) return <Loading />;

    return (
        <div id="page" className={styles.page}>
            <title>
                {title}
            </title>
            <div className={styles.sidePanel}>
                <SidePanel
                    webSocketConnected={true}
                    conversationId={null}
                    uploadedFiles={[]}
                    isMobileWidth={isMobileWidth}
                />
            </div>
            <div className={styles.content}>
                <NavMenu selected="Settings" title="Settings" showLogo={true} />
                <div className={styles.contentBody}>
                    <Suspense fallback={<Loading />}>
                        <div id="content" className="grid grid-flow-column sm:grid-flow-row gap-16 m-8">
                            <div className="section grid gap-8">
                                <div className="text-2xl">Profile</div>
                                <div className="cards flex flex-wrap gap-16">
                                    <Card className={cardClassName}>
                                        <CardHeader className="text-xl flex flex-row"><UserCircle className="h-7 w-7 mr-2"/>Name</CardHeader>
                                        <CardContent className="overflow-hidden">
                                            <p className="pb-4 text-gray-400">What should Khoj refer to you as?</p>
                                            <Input type="text" className="w-full border border-gray-300 rounded-lg p-4 py-6" defaultValue={userConfig.given_name} />
                                        </CardContent>
                                        <CardFooter className="flex flex-wrap gap-4">
                                            <Button variant="outline" size="sm"><FloppyDisk className="h-5 w-5 inline mr-2" />Save</Button>
                                        </CardFooter>
                                    </Card>
                                </div>
                            </div>
                            <div className="section grid gap-8">
                                <div className="text-2xl">Content</div>
                                <div className="cards flex flex-wrap gap-16">
                                    <Card className={cardClassName}>
                                        <CardHeader className="text-xl flex flex-row text-2xl"><Laptop className="h-8 w-8 mr-2" />Files</CardHeader>
                                        <CardContent className="overflow-hidden pb-12 text-gray-400">
                                            Manage your synced files
                                        </CardContent>
                                        <CardFooter className="flex flex-wrap gap-4">
                                            <Button variant="outline" size="sm">
                                                {userConfig.enabled_content_source.computer && (
                                                    <>
                                                        <Files className="h-5 w-5 inline mr-1" />Manage
                                                    </>
                                                ) || (
                                                    <>
                                                        <Plugs className="h-5 w-5 inline mr-1" />Connect
                                                    </>
                                                )}
                                            </Button>
                                            <Button variant="outline" size="sm" className={`${userConfig.enabled_content_source.computer || "hidden"}`}>
                                                <CloudSlash className="h-5 w-5 inline mr-1" />Disable
                                            </Button>
                                        </CardFooter>
                                    </Card>
                                    <Card className={cardClassName}>
                                        <CardHeader className="text-xl flex flex-row text-2xl"><GithubLogo className="h-8 w-8 mr-2" />Github</CardHeader>
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
                                        <CardContent className="overflow-hidden pb-12 text-gray-400">
                                            Sync your Notion pages
                                        </CardContent>
                                        <CardFooter className="flex flex-wrap gap-4">
                                            <Button variant="outline" size="sm">
                                                {userConfig.enabled_content_source.notion && (
                                                    <>
                                                        <Files className="h-5 w-5 inline mr-1" />Manage
                                                    </>
                                                ) || (
                                                    <>
                                                        <Plugs className="h-5 w-5 inline mr-1" />Connect
                                                    </>
                                                )}
                                            </Button>
                                            <Button variant="outline" size="sm" className={`${userConfig.enabled_content_source.notion || "hidden"}`}>
                                                <CloudSlash className="h-5 w-5 inline mr-1" />Disable
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
                                            {!userConfig.is_active && (
                                                <p className="text-gray-400">Subscribe to switch model</p>
                                            )}
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
                                        <CardHeader className="text-xl flex flex-row"><SpeakerHigh className="h-7 w-7 mr-2"/>Voice</CardHeader>
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
                                    <Card className="grid grid-flow-column border border-gray-300 shadow-md rounded-lg">
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
                                                            <TableCell className="grid grid-flow-col grid-cols-[1fr_auto] bg-secondary rounded-xl p-3">
                                                                <span>
                                                                    {`${key.token.slice(0, 6)}...${key.token.slice(-4)}`}
                                                                </span>
                                                                <div className="grid grid-flow-col">
                                                                    <Copy weight="bold" className="h-4 w-4 mr-2 hover:bg-primary/40" onClick={() => copyAPIKey(key.token)}/>
                                                                    <Trash weight="bold" className='h-4 w-4 mr-2 md:ml-4 text-red-400 hover:bg-primary/40' onClick={() => deleteAPIKey(key.token)}/>
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
                                            {numberValidationState === PhoneNumberValidationState.VerifyOTP && (
                                                <>
                                                    <p>{`Enter the OTP sent to your WhatsApp number: ${number}`}</p>
                                                    <InputOTP
                                                        autoFocus={true}
                                                        maxLength={6}
                                                        value={otp || ""}
                                                        onChange={setOTP}
                                                        onComplete={() => setNumberValidationState(PhoneNumberValidationState.Verified)}
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
                                            ) || (
                                                <>
                                                    <Input
                                                        type="tel"
                                                        onChange={(e) => setNumber(e.target.value)}
                                                        value={number || ""}
                                                        placeholder="Enter your WhatsApp number"
                                                        className="w-full border border-gray-300 rounded-lg px-4 py-6"
                                                    />
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
                                                    disabled={!number || number === userConfig.phone_number || !isValidPhoneNumber(number)}
                                                    onClick={sendOTP}
                                                >
                                                    {!userConfig.phone_number
                                                    ? (<><Plugs className="inline mr-2" />Setup Whatsapp</>)
                                                    : !number || number === userConfig.phone_number || !isValidPhoneNumber(number)
                                                    ? (<><PlugsConnected className="inline mr-2 text-green-400" />Switch Number</>)
                                                    : (<>Send OTP to Whatsapp <ArrowRight className="inline ml-2" weight="bold"/></>)
                                                    }
                                                </Button>
                                            )}
                                        </CardFooter>
                                    </Card>
                                </div>
                            </div>
                            <div className="section grid gap-8">
                                <div className="text-2xl">Billing</div>
                                <div className="cards flex flex-wrap gap-16">
                                    <Card className={cardClassName}>
                                        <CardHeader className="text-xl flex flex-row">
                                            <CreditCard className="h-7 w-7 mr-2"/>
                                            Subscription
                                        </CardHeader>
                                        <CardContent className="grid gap-2 overflow-hidden">
                                            <p className="text-gray-400">Current Plan</p>
                                            {userConfig.subscription_state === "trial" && (
                                                <>
                                                    <p className="text-xl">Futurist Trial</p>
                                                    <p className="text-gray-400">You are on a 14 day trial of the Khoj <i>Futurist</i> plan</p>
                                                    <p className="text-gray-400">See <a href="https://khoj.dev/pricing">pricing</a> for details</p>
                                                </>
                                            ) || userConfig.subscription_state === "subscribed" && (
                                                <>
                                                    <p className="text-xl">Futurist</p>
                                                    <p className="text-gray-400">Subscription <b>renews</b> on <b>{ userConfig.subscription_renewal_date }</b></p>
                                                </>
                                            ) || userConfig.subscription_state === "unsubscribed" && (
                                                <>
                                                    <p className="text-xl">Futurist</p>
                                                    <p className="text-gray-400">Subscription <b>ends</b> on <b>{ userConfig.subscription_renewal_date }</b></p>
                                                </>
                                            ) || userConfig.subscription_state === "expired" && (
                                                <>
                                                    <p className="text-xl">Trial</p>
                                                    {userConfig.subscription_renewal_date && (
                                                        <p className="text-gray-400">Your subscription <b>expired</b> on <b>{ userConfig.subscription_renewal_date }</b></p>
                                                    ) || (
                                                        <p className="text-gray-400">See <a href="https://khoj.dev/pricing">pricing</a> for details</p>
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
                                                    <CloudSlash className="h-5 w-5 mr-2" />Unsubscribe
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
                        </div>
                    </Suspense>
                </div>
            </div>
        </div>
    );
}
