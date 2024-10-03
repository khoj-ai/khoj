"use client";

import useSWR from "swr";
import { InlineLoading } from "../components/loading/loading";
import { Card, CardContent, CardFooter, CardHeader, CardTitle } from "@/components/ui/card";
import { Button, buttonVariants } from "@/components/ui/button";

import {
    Select,
    SelectContent,
    SelectItem,
    SelectTrigger,
    SelectValue,
} from "@/components/ui/select";

interface AutomationsData {
    id: number;
    subject: string;
    query_to_run: string;
    scheduling_request: string;
    schedule: string;
    crontime: string;
    next: string;
}

import cronstrue from "cronstrue";
import { zodResolver } from "@hookform/resolvers/zod";
import { UseFormReturn, useForm } from "react-hook-form";
import { z } from "zod";
import { Suspense, useEffect, useState } from "react";
import {
    Form,
    FormControl,
    FormDescription,
    FormField,
    FormItem,
    FormLabel,
    FormMessage,
} from "@/components/ui/form";
import { Input } from "@/components/ui/input";
import { Dialog, DialogContent, DialogTrigger } from "@/components/ui/dialog";
import { DialogTitle } from "@radix-ui/react-dialog";
import { Textarea } from "@/components/ui/textarea";
import { LocationData, useIPLocationData, useIsMobileWidth } from "../common/utils";

import styles from "./automations.module.css";
import ShareLink from "../components/shareLink/shareLink";
import { useSearchParams } from "next/navigation";
import { Popover, PopoverTrigger, PopoverContent } from "@/components/ui/popover";
import {
    CalendarCheck,
    CalendarDot,
    CalendarDots,
    Clock,
    ClockAfternoon,
    DotsThreeVertical,
    Envelope,
    Lightning,
    MapPinSimple,
    Play,
    Plus,
    Trash,
} from "@phosphor-icons/react";
import { useAuthenticatedData, UserProfile } from "../common/auth";
import LoginPrompt from "../components/loginPrompt/loginPrompt";
import { useToast } from "@/components/ui/use-toast";
import { ToastAction } from "@/components/ui/toast";
import { Alert, AlertDescription } from "@/components/ui/alert";
import SidePanel from "../components/sidePanel/chatHistorySidePanel";
import { Drawer, DrawerContent, DrawerTitle, DrawerTrigger } from "@/components/ui/drawer";

const automationsFetcher = () =>
    window
        .fetch("/api/automations")
        .then((res) => res.json())
        .catch((err) => console.log(err));

// Standard cron format: minute hour dayOfMonth month dayOfWeek

function getEveryBlahFromCron(cron: string) {
    const cronParts = cron.split(" ");
    const dayOfMonth = cronParts[2];
    const dayOfWeek = cronParts[4];

    // If both dayOfMonth and dayOfWeek are '*', it runs every day
    if (dayOfMonth === "*" && dayOfWeek === "*") {
        return "Day";
    }
    // If dayOfWeek is not '*', it suggests a specific day of the week, implying a weekly schedule
    else if (dayOfWeek !== "*") {
        return "Week";
    }
    // If dayOfMonth is not '*', it suggests a specific day of the month, implying a monthly schedule
    else if (dayOfMonth !== "*") {
        return "Month";
    }
    // Default to 'Day' if none of the above conditions are met
    else {
        return "Day";
    }
}

function getDayOfWeekFromCron(cron: string) {
    const cronParts = cron.split(" ");
    if (cronParts[3] === "*" && cronParts[4] !== "*") {
        return Number(cronParts[4]);
    }

    return undefined;
}

function getTimeRecurrenceFromCron(cron: string) {
    const cronParts = cron.split(" ");
    const hour = cronParts[1];
    const minute = cronParts[0];
    const period = Number(hour) >= 12 ? "PM" : "AM";

    let friendlyHour = Number(hour) > 12 ? Number(hour) - 12 : hour;
    if (friendlyHour === "00") {
        friendlyHour = "12";
    }

    let friendlyMinute = minute;
    if (Number(friendlyMinute) < 10 && friendlyMinute !== "00") {
        friendlyMinute = `0${friendlyMinute}`;
    }
    return `${friendlyHour}:${friendlyMinute} ${period}`;
}

function getDayOfMonthFromCron(cron: string) {
    const cronParts = cron.split(" ");

    return String(cronParts[2]);
}

function cronToHumanReadableString(cron: string) {
    return cronstrue.toString(cron);
}

const frequencies = ["Day", "Week", "Month"];

const daysOfMonth = Array.from({ length: 31 }, (_, i) => String(i + 1));

const weekDays = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"];

const timeOptions: string[] = [];

const timePeriods = ["AM", "PM"];

// Populate the time selector with options for each hour of the day
for (var i = 0; i < timePeriods.length; i++) {
    for (var hour = 0; hour < 12; hour++) {
        for (var minute = 0; minute < 60; minute += 15) {
            // Ensure all minutes are two digits
            const paddedMinute = String(minute).padStart(2, "0");
            const friendlyHour = hour === 0 ? 12 : hour;
            timeOptions.push(`${friendlyHour}:${paddedMinute} ${timePeriods[i]}`);
        }
    }
}

const timestamp = Date.now();

const suggestedAutomationsMetadata: AutomationsData[] = [
    {
        subject: "Weekly Newsletter",
        query_to_run:
            "Compile a message including: 1. A recap of news from last week 2. An at-home workout I can do before work 3. A quote to inspire me for the week ahead",
        schedule: "9AM every Monday",
        next: "Next run at 9AM on Monday",
        crontime: "0 9 * * 1",
        id: timestamp,
        scheduling_request: "",
    },
    {
        subject: "Daily Bedtime Story",
        query_to_run:
            "Compose a bedtime story that a five-year-old might enjoy. It should not exceed five paragraphs. Appeal to the imagination, but weave in learnings.",
        schedule: "9PM every night",
        next: "Next run at 9PM today",
        crontime: "0 21 * * *",
        id: timestamp + 1,
        scheduling_request: "",
    },
    {
        subject: "Front Page of Hacker News",
        query_to_run:
            "Summarize the top 5 posts from https://news.ycombinator.com/best and share them with me, including links",
        schedule: "9PM on every Wednesday",
        next: "Next run at 9PM on Wednesday",
        crontime: "0 21 * * 3",
        id: timestamp + 2,
        scheduling_request: "",
    },
    {
        subject: "Market Summary",
        query_to_run:
            "Get the market summary for today and share it with me. Focus on tech stocks and the S&P 500.",
        schedule: "9AM on every weekday",
        next: "Next run at 9AM on Monday",
        crontime: "0 9 * * *",
        id: timestamp + 3,
        scheduling_request: "",
    },
    {
        subject: "Round-up of research papers about AI in healthcare",
        query_to_run:
            "Summarize the top 3 research papers about AI in healthcare that were published in the last week. Include links to the full papers.",
        schedule: "9AM every Friday",
        next: "Next run at 9AM on Friday",
        crontime: "0 9 * * 5",
        id: timestamp + 4,
        scheduling_request: "",
    },
];

function createShareLink(automation: AutomationsData) {
    const encodedSubject = encodeURIComponent(automation.subject);
    const encodedQuery = encodeURIComponent(automation.query_to_run);
    const encodedCrontime = encodeURIComponent(automation.crontime);

    const shareLink = `${window.location.origin}/automations?subject=${encodedSubject}&query=${encodedQuery}&crontime=${encodedCrontime}`;

    return shareLink;
}

function deleteAutomation(automationId: string, setIsDeleted: (isDeleted: boolean) => void) {
    fetch(`/api/automation?automation_id=${automationId}`, { method: "DELETE" })
        .then((response) => response.json())
        .then((data) => {
            setIsDeleted(true);
        });
}

function sendAPreview(automationId: string, setToastMessage: (toastMessage: string) => void) {
    fetch(`/api/trigger/automation?automation_id=${automationId}`, { method: "POST" })
        .then((response) => {
            if (!response.ok) {
                throw new Error("Network response was not ok");
            }
            return response;
        })
        .then((automations) => {
            setToastMessage("Automation triggered. Check your inbox in a few minutes!");
        })
        .catch((error) => {
            setToastMessage("Sorry, something went wrong. Try again later.");
        });
}

interface AutomationsCardProps {
    automation: AutomationsData;
    isMobileWidth: boolean;
    locationData?: LocationData | null;
    suggestedCard?: boolean;
    setNewAutomationData?: (data: AutomationsData) => void;
    isLoggedIn: boolean;
    setShowLoginPrompt: (showLoginPrompt: boolean) => void;
    authenticatedData: UserProfile | null;
}

function AutomationsCard(props: AutomationsCardProps) {
    const [isEditing, setIsEditing] = useState(false);
    const [updatedAutomationData, setUpdatedAutomationData] = useState<AutomationsData | null>(
        null,
    );
    const [isDeleted, setIsDeleted] = useState(false);
    const [toastMessage, setToastMessage] = useState("");
    const { toast } = useToast();

    const automation = props.automation;

    const [timeRecurrence, setTimeRecurrence] = useState("");
    const [intervalString, setIntervalString] = useState("");

    useEffect(() => {
        // The updated automation data, if present, takes priority over the original automation data
        const automationData = updatedAutomationData || automation;
        setTimeRecurrence(getTimeRecurrenceFromCron(automationData.crontime));
        const frequency = getEveryBlahFromCron(automationData.crontime);

        if (frequency === "Day") {
            setIntervalString("Daily");
        } else if (frequency === "Week") {
            const dayOfWeek = getDayOfWeekFromCron(automationData.crontime);
            if (dayOfWeek === undefined) {
                setIntervalString("Weekly");
            } else {
                setIntervalString(`${weekDays[dayOfWeek]}`);
            }
        } else if (frequency === "Month") {
            const dayOfMonth = getDayOfMonthFromCron(automationData.crontime);
            setIntervalString(`Monthly on the ${dayOfMonth}`);
        }
    }, [updatedAutomationData, automation]);

    useEffect(() => {
        const toastTitle = `Automation: ${updatedAutomationData?.subject || automation.subject}`;
        if (toastMessage) {
            toast({
                title: toastTitle,
                description: toastMessage,
                action: <ToastAction altText="Dismiss">Ok</ToastAction>,
            });
            setToastMessage("");
        }
    }, [toastMessage, updatedAutomationData, automation, toast]);

    if (isDeleted) {
        return null;
    }

    return (
        <Card
            className={`bg-secondary h-full shadow-sm rounded-lg bg-gradient-to-b from-background to-slate-50 dark:to-gray-950 border ${styles.automationCard}`}
        >
            <CardHeader>
                <CardTitle className="line-clamp-2 leading-normal flex justify-between">
                    {updatedAutomationData?.subject || automation.subject}
                    <Popover>
                        <PopoverTrigger asChild>
                            <Button className="bg-background" variant={"ghost"}>
                                <DotsThreeVertical className="h-4 w-4" />
                            </Button>
                        </PopoverTrigger>
                        <PopoverContent className="w-auto grid gap-2 text-left bg-secondary">
                            {!props.suggestedCard && props.locationData && (
                                <AutomationComponentWrapper
                                    isMobileWidth={props.isMobileWidth}
                                    callToAction="Edit"
                                    createNew={false}
                                    setIsCreating={setIsEditing}
                                    setShowLoginPrompt={props.setShowLoginPrompt}
                                    setNewAutomationData={setUpdatedAutomationData}
                                    authenticatedData={props.authenticatedData}
                                    isCreating={isEditing}
                                    automation={updatedAutomationData || automation}
                                    ipLocationData={props.locationData}
                                />
                            )}
                            <ShareLink
                                buttonTitle="Share"
                                includeIcon={true}
                                buttonClassName="justify-start px-4 py-2 h-10"
                                buttonVariant={"outline" as keyof typeof buttonVariants}
                                title="Share Automation"
                                description="Copy the link below and share it with your coworkers or friends."
                                url={createShareLink(automation)}
                                onShare={() => {
                                    navigator.clipboard.writeText(createShareLink(automation));
                                }}
                            />
                            {!props.suggestedCard && (
                                <Button
                                    variant={"outline"}
                                    className="justify-start"
                                    onClick={() => {
                                        sendAPreview(automation.id.toString(), setToastMessage);
                                    }}
                                >
                                    <Play className="h-4 w-4 mr-2" />
                                    Run Now
                                </Button>
                            )}
                            <Button
                                variant={"destructive"}
                                className="justify-start"
                                onClick={() => {
                                    if (props.suggestedCard) {
                                        setIsDeleted(true);
                                        return;
                                    }
                                    deleteAutomation(automation.id.toString(), setIsDeleted);
                                }}
                            >
                                <Trash className="h-4 w-4 mr-2" />
                                Delete
                            </Button>
                        </PopoverContent>
                    </Popover>
                </CardTitle>
            </CardHeader>
            <CardContent className="text-secondary-foreground break-all">
                {updatedAutomationData?.query_to_run || automation.query_to_run}
            </CardContent>
            <CardFooter className="flex flex-col items-start md:flex-row md:justify-between md:items-center gap-2">
                <div className="flex gap-2">
                    <div className="flex items-center bg-blue-50 rounded-lg p-1.5 border-blue-200 border dark:bg-blue-800 dark:border-blue-500">
                        <CalendarCheck className="h-4 w-4 mr-2 text-blue-700 dark:text-blue-300" />
                        <div className="text-s text-blue-700 dark:text-blue-300">
                            {timeRecurrence}
                        </div>
                    </div>
                    <div className="flex items-center bg-purple-50 rounded-lg p-1.5 border-purple-200 border dark:bg-purple-800 dark:border-purple-500">
                        <ClockAfternoon className="h-4 w-4 mr-2 text-purple-700 dark:text-purple-300" />
                        <div className="text-s text-purple-700 dark:text-purple-300">
                            {intervalString}
                        </div>
                    </div>
                </div>
                {props.suggestedCard && props.setNewAutomationData && (
                    <AutomationComponentWrapper
                        isMobileWidth={props.isMobileWidth}
                        callToAction="Add"
                        createNew={true}
                        setIsCreating={setIsEditing}
                        setShowLoginPrompt={props.setShowLoginPrompt}
                        setNewAutomationData={props.setNewAutomationData}
                        authenticatedData={props.authenticatedData}
                        isCreating={isEditing}
                        automation={automation}
                        ipLocationData={props.locationData}
                    />
                )}
            </CardFooter>
        </Card>
    );
}

interface SharedAutomationCardProps {
    locationData?: LocationData | null;
    setNewAutomationData: (data: AutomationsData) => void;
    isLoggedIn: boolean;
    setShowLoginPrompt: (showLoginPrompt: boolean) => void;
    authenticatedData: UserProfile | null;
    isMobileWidth: boolean;
}

function SharedAutomationCard(props: SharedAutomationCardProps) {
    const searchParams = useSearchParams();
    const [isCreating, setIsCreating] = useState(true);

    const subject = searchParams.get("subject");
    const query = searchParams.get("query");
    const crontime = searchParams.get("crontime");

    if (!subject || !query || !crontime) {
        return null;
    }

    const automation: AutomationsData = {
        id: 0,
        subject: decodeURIComponent(subject),
        query_to_run: decodeURIComponent(query),
        scheduling_request: "",
        schedule: cronToHumanReadableString(decodeURIComponent(crontime)),
        crontime: decodeURIComponent(crontime),
        next: "",
    };

    return isCreating ? (
        <AutomationComponentWrapper
            isMobileWidth={props.isMobileWidth}
            callToAction="Shared"
            createNew={true}
            setIsCreating={setIsCreating}
            setShowLoginPrompt={props.setShowLoginPrompt}
            setNewAutomationData={props.setNewAutomationData}
            authenticatedData={props.authenticatedData}
            isCreating={isCreating}
            automation={automation}
            ipLocationData={props.locationData}
        />
    ) : null;
}

const EditAutomationSchema = z.object({
    subject: z.optional(z.string()),
    everyBlah: z.string({ required_error: "Every is required" }),
    dayOfWeek: z.optional(z.number()),
    dayOfMonth: z.optional(z.string()),
    timeRecurrence: z.string({ required_error: "Time Recurrence is required" }),
    queryToRun: z.string({ required_error: "Query to Run is required" }),
});

interface EditCardProps {
    automation?: AutomationsData;
    setIsEditing: (completed: boolean) => void;
    setUpdatedAutomationData: (data: AutomationsData) => void;
    locationData?: LocationData | null;
    createNew?: boolean;
    isLoggedIn: boolean;
    setShowLoginPrompt: (showLoginPrompt: boolean) => void;
    authenticatedData: UserProfile | null;
}

function EditCard(props: EditCardProps) {
    const automation = props.automation;

    const form = useForm<z.infer<typeof EditAutomationSchema>>({
        resolver: zodResolver(EditAutomationSchema),
        defaultValues: {
            subject: automation?.subject,
            everyBlah: automation?.crontime ? getEveryBlahFromCron(automation.crontime) : "Day",
            dayOfWeek: automation?.crontime ? getDayOfWeekFromCron(automation.crontime) : undefined,
            timeRecurrence: automation?.crontime
                ? getTimeRecurrenceFromCron(automation.crontime)
                : "12:00 PM",
            dayOfMonth: automation?.crontime ? getDayOfMonthFromCron(automation.crontime) : "1",
            queryToRun: automation?.query_to_run,
        },
    });

    const onSubmit = (values: z.infer<typeof EditAutomationSchema>) => {
        const cronFrequency = convertFrequencyToCron(
            values.everyBlah,
            values.timeRecurrence,
            values.dayOfWeek,
            values.dayOfMonth,
        );

        let updateQueryUrl = `/api/automation?`;
        updateQueryUrl += `q=${encodeURIComponent(values.queryToRun)}`;
        if (automation?.id && !props.createNew) {
            updateQueryUrl += `&automation_id=${encodeURIComponent(automation.id)}`;
        }
        if (values.subject) {
            updateQueryUrl += `&subject=${encodeURIComponent(values.subject)}`;
        }
        updateQueryUrl += `&crontime=${encodeURIComponent(cronFrequency)}`;
        if (props.locationData && props.locationData.city)
            updateQueryUrl += `&city=${encodeURIComponent(props.locationData.city)}`;
        if (props.locationData && props.locationData.region)
            updateQueryUrl += `&region=${encodeURIComponent(props.locationData.region)}`;
        if (props.locationData && props.locationData.country)
            updateQueryUrl += `&country=${encodeURIComponent(props.locationData.country)}`;
        if (props.locationData && props.locationData.timezone)
            updateQueryUrl += `&timezone=${encodeURIComponent(props.locationData.timezone)}`;

        let method = props.createNew ? "POST" : "PUT";

        fetch(updateQueryUrl, { method: method })
            .then((response) => response.json())
            .then((data: AutomationsData) => {
                props.setIsEditing(false);
                props.setUpdatedAutomationData({
                    id: data.id,
                    subject: data.subject || "",
                    query_to_run: data.query_to_run,
                    scheduling_request: data.scheduling_request,
                    schedule: cronToHumanReadableString(data.crontime),
                    crontime: data.crontime,
                    next: data.next,
                });
            });
    };

    function convertFrequencyToCron(
        frequency: string,
        timeRecurrence: string,
        dayOfWeek?: number,
        dayOfMonth?: string,
    ) {
        let cronString = "";

        const minutes = timeRecurrence.split(":")[1].split(" ")[0];
        const period = timeRecurrence.split(":")[1].split(" ")[1];
        const rawHourAsNumber = Number(timeRecurrence.split(":")[0]);
        const hours =
            period === "PM" && rawHourAsNumber < 12
                ? String(rawHourAsNumber + 12)
                : rawHourAsNumber;

        // Convert Sunday to 0th (from 7th) day of week for server cron format
        const dayOfWeekNumber = dayOfWeek !== undefined ? (dayOfWeek === 7 ? 0 : dayOfWeek) : "*";

        switch (frequency) {
            case "Day":
                cronString = `${minutes} ${hours} * * *`;
                break;
            case "Week":
                cronString = `${minutes} ${hours} * * ${dayOfWeekNumber}`;
                break;
            case "Month":
                cronString = `${minutes} ${hours} ${dayOfMonth} * *`;
                break;
        }

        return cronString;
    }

    return (
        <AutomationModificationForm
            authenticatedData={props.authenticatedData}
            locationData={props.locationData || null}
            form={form}
            onSubmit={onSubmit}
            create={props.createNew}
            isLoggedIn={props.isLoggedIn}
            setShowLoginPrompt={props.setShowLoginPrompt}
        />
    );
}

interface AutomationModificationFormProps {
    form: UseFormReturn<z.infer<typeof EditAutomationSchema>>;
    onSubmit: (values: z.infer<typeof EditAutomationSchema>) => void;
    create?: boolean;
    isLoggedIn: boolean;
    setShowLoginPrompt: (showLoginPrompt: boolean) => void;
    authenticatedData: UserProfile | null;
    locationData: LocationData | null;
}

function AutomationModificationForm(props: AutomationModificationFormProps) {
    const [isSaving, setIsSaving] = useState(false);
    const { errors } = props.form.formState;

    function recommendationPill(
        recommendationText: string,
        onChange: (value: any, event: React.MouseEvent<HTMLButtonElement>) => void,
    ) {
        return (
            <Button
                className="text-xs bg-slate-50 dark:bg-slate-950 h-auto p-1.5 m-1 rounded-full"
                variant="ghost"
                key={recommendationText}
                onClick={(event) => {
                    event.preventDefault();
                    onChange({ target: { value: recommendationText } }, event);
                }}
            >
                {recommendationText}...
            </Button>
        );
    }

    const recommendationPills = [
        "Make a picture of",
        "Generate a summary of",
        "Create a newsletter of",
        "Notify me when",
    ];

    return (
        <Form {...props.form}>
            <form
                onSubmit={props.form.handleSubmit((values) => {
                    props.onSubmit(values);
                    setIsSaving(true);
                })}
                className="space-y-6"
            >
                <FormItem className="space-y-1">
                    <FormDescription>
                        Emails will be sent to this address. Timezone and location data will be used
                        to schedule automations.
                        {props.locationData &&
                            metadataMap(props.locationData, props.authenticatedData)}
                    </FormDescription>
                </FormItem>
                {!props.create && (
                    <FormField
                        control={props.form.control}
                        name="subject"
                        render={({ field }) => (
                            <FormItem className="space-y-1">
                                <FormLabel>Subject</FormLabel>
                                <FormDescription>
                                    This is the subject of the email you will receive.
                                </FormDescription>
                                <FormControl>
                                    <Input
                                        placeholder="Digest of Healthcare AI trends"
                                        {...field}
                                    />
                                </FormControl>
                                <FormMessage />
                                {errors.subject && (
                                    <FormMessage>{errors.subject?.message}</FormMessage>
                                )}
                            </FormItem>
                        )}
                    />
                )}

                <FormField
                    control={props.form.control}
                    name="everyBlah"
                    render={({ field }) => (
                        <FormItem className="w-full space-y-1">
                            <FormLabel>Frequency</FormLabel>
                            <FormDescription>How often should this automation run?</FormDescription>
                            <Select onValueChange={field.onChange} defaultValue={field.value}>
                                <FormControl>
                                    <SelectTrigger className="w-[200px]">
                                        <div className="flex items-center">
                                            <CalendarDots className="h-4 w-4 mr-2 inline" />
                                            Every
                                        </div>
                                        <SelectValue placeholder="" />
                                    </SelectTrigger>
                                </FormControl>
                                <SelectContent>
                                    {frequencies.map((frequency) => (
                                        <SelectItem key={frequency} value={frequency}>
                                            {frequency}
                                        </SelectItem>
                                    ))}
                                </SelectContent>
                            </Select>
                            <FormMessage />
                            {errors.subject && (
                                <FormMessage>{errors.everyBlah?.message}</FormMessage>
                            )}
                        </FormItem>
                    )}
                />
                {props.form.watch("everyBlah") === "Week" && (
                    <FormField
                        control={props.form.control}
                        name="dayOfWeek"
                        render={({ field }) => (
                            <FormItem className="w-full space-y-1">
                                <FormDescription>
                                    Every week, on which day should this automation run?
                                </FormDescription>
                                <Select
                                    onValueChange={(value) => field.onChange(Number(value))}
                                    defaultValue={String(field.value)}
                                >
                                    <FormControl>
                                        <SelectTrigger className="w-[200px]">
                                            <div className="flex items-center">
                                                <CalendarDot className="h-4 w-4 mr-2 inline" />
                                                On
                                            </div>
                                            <SelectValue placeholder="" />
                                        </SelectTrigger>
                                    </FormControl>
                                    <SelectContent>
                                        {weekDays.map((day, index) => (
                                            <SelectItem key={day} value={String(index)}>
                                                {day}
                                            </SelectItem>
                                        ))}
                                    </SelectContent>
                                </Select>
                                <FormMessage />
                                {errors.subject && (
                                    <FormMessage>{errors.dayOfWeek?.message}</FormMessage>
                                )}
                            </FormItem>
                        )}
                    />
                )}
                {props.form.watch("everyBlah") === "Month" && (
                    <FormField
                        control={props.form.control}
                        name="dayOfMonth"
                        render={({ field }) => (
                            <FormItem className="w-full space-y-1">
                                <FormDescription>
                                    Every month, on which day should the automation run?
                                </FormDescription>
                                <Select onValueChange={field.onChange} defaultValue={field.value}>
                                    <FormControl>
                                        <SelectTrigger className="w-[200px]">
                                            <div className="flex items-center">
                                                <CalendarDot className="h-4 w-4 mr-2 inline" />
                                                On the
                                            </div>
                                            <SelectValue placeholder="" />
                                        </SelectTrigger>
                                    </FormControl>
                                    <SelectContent>
                                        {daysOfMonth.map((day) => (
                                            <SelectItem key={day} value={day}>
                                                {day}
                                            </SelectItem>
                                        ))}
                                    </SelectContent>
                                </Select>
                                <FormMessage />
                                {errors.subject && (
                                    <FormMessage>{errors.dayOfMonth?.message}</FormMessage>
                                )}
                            </FormItem>
                        )}
                    />
                )}
                {(props.form.watch("everyBlah") === "Day" ||
                    props.form.watch("everyBlah") == "Week" ||
                    props.form.watch("everyBlah") == "Month") && (
                    <FormField
                        control={props.form.control}
                        name="timeRecurrence"
                        render={({ field }) => (
                            <FormItem className="w-full space-y-1">
                                <FormLabel>Time</FormLabel>
                                <FormDescription>
                                    On the days this automation runs, at what time should it run?
                                </FormDescription>
                                <Select onValueChange={field.onChange} defaultValue={field.value}>
                                    <FormControl>
                                        <SelectTrigger className="w-[200px]">
                                            <div className="flex items-center">
                                                <ClockAfternoon className="h-4 w-4 mr-2 inline" />
                                                At
                                            </div>
                                            <SelectValue placeholder="" />
                                        </SelectTrigger>
                                    </FormControl>
                                    <SelectContent>
                                        {timeOptions.map((timeOption) => (
                                            <SelectItem key={timeOption} value={timeOption}>
                                                {timeOption}
                                            </SelectItem>
                                        ))}
                                    </SelectContent>
                                </Select>
                                <FormMessage />
                                {errors.subject && (
                                    <FormMessage>{errors.timeRecurrence?.message}</FormMessage>
                                )}
                            </FormItem>
                        )}
                    />
                )}
                <FormField
                    control={props.form.control}
                    name="queryToRun"
                    render={({ field }) => (
                        <FormItem className="space-y-1">
                            <FormLabel>Instructions</FormLabel>
                            <FormDescription>What do you want Khoj to do?</FormDescription>
                            {props.create && (
                                <div>
                                    {recommendationPills.map((recommendation) =>
                                        recommendationPill(recommendation, field.onChange),
                                    )}
                                </div>
                            )}
                            <FormControl>
                                <Textarea
                                    placeholder="Create a summary of the latest news about AI in healthcare."
                                    value={field.value}
                                    onChange={field.onChange}
                                />
                            </FormControl>
                            <FormMessage />
                            {errors.subject && (
                                <FormMessage>{errors.queryToRun?.message}</FormMessage>
                            )}
                        </FormItem>
                    )}
                />
                <fieldset disabled={isSaving}>
                    {props.isLoggedIn ? (
                        isSaving ? (
                            <Button type="submit" disabled>
                                Saving...
                            </Button>
                        ) : (
                            <Button type="submit">Save</Button>
                        )
                    ) : (
                        <Button
                            onClick={(event) => {
                                event.preventDefault();
                                props.setShowLoginPrompt(true);
                            }}
                            variant={"default"}
                        >
                            Login to Save
                        </Button>
                    )}
                </fieldset>
            </form>
        </Form>
    );
}

function metadataMap(ipLocationData: LocationData, authenticatedData: UserProfile | null) {
    return (
        <div className="flex flex-wrap gap-2 items-center justify-start">
            {authenticatedData ? (
                <span className="rounded-lg text-sm border-secondary border p-1 flex items-center shadow-sm">
                    <Envelope className="h-4 w-4 mr-2 inline text-orange-500 shadow-sm" />
                    {authenticatedData.email}
                </span>
            ) : null}
            {ipLocationData && (
                <span className="rounded-lg text-sm border-secondary border p-1 flex items-center shadow-sm">
                    <MapPinSimple className="h-4 w-4 mr-2 inline text-purple-500" />
                    {ipLocationData
                        ? `${ipLocationData.city}, ${ipLocationData.country}`
                        : "Unknown"}
                </span>
            )}
            {ipLocationData && (
                <span className="rounded-lg text-sm border-secondary border p-1 flex items-center shadow-sm">
                    <Clock className="h-4 w-4 mr-2 inline text-green-500" />
                    {ipLocationData ? `${ipLocationData.timezone}` : "Unknown"}
                </span>
            )}
        </div>
    );
}

interface AutomationComponentWrapperProps {
    isMobileWidth: boolean;
    callToAction: string;
    createNew: boolean;
    setIsCreating: (completed: boolean) => void;
    setShowLoginPrompt: (showLoginPrompt: boolean) => void;
    setNewAutomationData: (data: AutomationsData) => void;
    authenticatedData: UserProfile | null;
    isCreating: boolean;
    ipLocationData: LocationData | null | undefined;
    automation?: AutomationsData;
}

function AutomationComponentWrapper(props: AutomationComponentWrapperProps) {
    return props.isMobileWidth ? (
        <Drawer
            open={props.isCreating}
            onOpenChange={(open) => {
                props.setIsCreating(open);
            }}
        >
            <DrawerTrigger asChild>
                <Button className="shadow-sm justify-start" variant="outline">
                    <Plus className="h-4 w-4 mr-2" />
                    {props.callToAction}
                </Button>
            </DrawerTrigger>
            <DrawerContent className="p-2">
                <DrawerTitle>Automation</DrawerTitle>
                <EditCard
                    createNew={props.createNew}
                    automation={props.automation}
                    setIsEditing={props.setIsCreating}
                    isLoggedIn={props.authenticatedData ? true : false}
                    authenticatedData={props.authenticatedData}
                    setShowLoginPrompt={props.setShowLoginPrompt}
                    setUpdatedAutomationData={props.setNewAutomationData}
                    locationData={props.ipLocationData}
                />
            </DrawerContent>
        </Drawer>
    ) : (
        <Dialog
            open={props.isCreating}
            onOpenChange={(open) => {
                props.setIsCreating(open);
            }}
        >
            <DialogTrigger asChild>
                <Button className="shadow-sm justify-start" variant="outline">
                    <Plus className="h-4 w-4 mr-2" />
                    {props.callToAction}
                </Button>
            </DialogTrigger>
            <DialogContent className="max-h-[98vh] overflow-y-auto">
                <DialogTitle>Automation</DialogTitle>
                <EditCard
                    automation={props.automation}
                    createNew={props.createNew}
                    setIsEditing={props.setIsCreating}
                    isLoggedIn={props.authenticatedData ? true : false}
                    authenticatedData={props.authenticatedData}
                    setShowLoginPrompt={props.setShowLoginPrompt}
                    setUpdatedAutomationData={props.setNewAutomationData}
                    locationData={props.ipLocationData}
                />
            </DialogContent>
        </Dialog>
    );
}

export default function Automations() {
    const authenticatedData = useAuthenticatedData();
    const {
        data: personalAutomations,
        error,
        isLoading,
    } = useSWR<AutomationsData[]>(authenticatedData ? "automations" : null, automationsFetcher, {
        revalidateOnFocus: false,
    });

    const [isCreating, setIsCreating] = useState(false);
    const [newAutomationData, setNewAutomationData] = useState<AutomationsData | null>(null);
    const [allNewAutomations, setAllNewAutomations] = useState<AutomationsData[]>([]);
    const [suggestedAutomations, setSuggestedAutomations] = useState<AutomationsData[]>([]);
    const [showLoginPrompt, setShowLoginPrompt] = useState(false);
    const isMobileWidth = useIsMobileWidth();
    const ipLocationData = useIPLocationData();

    useEffect(() => {
        if (newAutomationData) {
            setAllNewAutomations([...allNewAutomations, newAutomationData]);
            setNewAutomationData(null);
        }
    }, [newAutomationData, allNewAutomations]);

    useEffect(() => {
        const allAutomations = personalAutomations
            ? personalAutomations.concat(allNewAutomations)
            : allNewAutomations;

        if (allAutomations) {
            setSuggestedAutomations(
                suggestedAutomationsMetadata.filter((suggestedAutomation) => {
                    return (
                        allAutomations.find(
                            (automation) => suggestedAutomation.subject === automation.subject,
                        ) === undefined
                    );
                }),
            );
        }
    }, [personalAutomations, allNewAutomations]);

    if (error)
        return <InlineLoading message="Oops, something went wrong. Please refresh the page." />;

    return (
        <main className={`w-full mx-auto`}>
            <div className={`grid w-full mx-auto`}>
                <div className={`${styles.sidePanel} top-0`}>
                    <SidePanel
                        conversationId={null}
                        uploadedFiles={[]}
                        isMobileWidth={isMobileWidth}
                    />
                </div>
                <div className={`${styles.pageLayout} w-full`}>
                    <div className="pt-6 md:pt-8 grid gap-1 md:flex md:justify-between">
                        <h1 className="text-3xl flex items-center">Automations</h1>
                        <div className="flex flex-wrap gap-2 items-center justify-start">
                            {authenticatedData ? (
                                <span className="rounded-lg text-sm border-secondary border p-1 flex items-center shadow-sm">
                                    <Envelope className="h-4 w-4 mr-2 inline text-orange-500 shadow-sm" />
                                    {authenticatedData.email}
                                </span>
                            ) : null}
                            {ipLocationData && (
                                <span className="rounded-lg text-sm border-secondary border p-1 flex items-center shadow-sm">
                                    <MapPinSimple className="h-4 w-4 mr-2 inline text-purple-500" />
                                    {ipLocationData
                                        ? `${ipLocationData.city}, ${ipLocationData.country}`
                                        : "Unknown"}
                                </span>
                            )}
                            {ipLocationData && (
                                <span className="rounded-lg text-sm border-secondary border p-1 flex items-center shadow-sm">
                                    <Clock className="h-4 w-4 mr-2 inline text-green-500" />
                                    {ipLocationData ? `${ipLocationData.timezone}` : "Unknown"}
                                </span>
                            )}
                        </div>
                    </div>
                    {showLoginPrompt && (
                        <LoginPrompt
                            loginRedirectMessage={"Create an account to make your own automation"}
                            onOpenChange={setShowLoginPrompt}
                        />
                    )}
                    <Alert className="bg-secondary border-none my-4">
                        <AlertDescription>
                            <Lightning weight={"fill"} className="h-4 w-4 text-purple-400 inline" />
                            <span className="font-bold">How it works</span> Automations help you
                            structure your time by automating tasks you do regularly. Build your
                            own, or try out our presets. Get results straight to your inbox.
                        </AlertDescription>
                    </Alert>
                    <div className="flex justify-between items-center py-4">
                        <h3 className="text-xl">Your Creations</h3>
                        {authenticatedData ? (
                            <AutomationComponentWrapper
                                isMobileWidth={isMobileWidth}
                                callToAction="Create Automation"
                                createNew={true}
                                setIsCreating={setIsCreating}
                                setShowLoginPrompt={setShowLoginPrompt}
                                setNewAutomationData={setNewAutomationData}
                                authenticatedData={authenticatedData}
                                isCreating={isCreating}
                                ipLocationData={ipLocationData}
                            />
                        ) : (
                            <Button
                                className="shadow-sm"
                                onClick={() => setShowLoginPrompt(true)}
                                variant={"outline"}
                            >
                                <Plus className="h-4 w-4 mr-2" />
                                Create Automation
                            </Button>
                        )}
                    </div>
                    <Suspense>
                        <SharedAutomationCard
                            isMobileWidth={isMobileWidth}
                            authenticatedData={authenticatedData}
                            locationData={ipLocationData}
                            isLoggedIn={authenticatedData ? true : false}
                            setShowLoginPrompt={setShowLoginPrompt}
                            setNewAutomationData={setNewAutomationData}
                        />
                    </Suspense>
                    {(!personalAutomations || personalAutomations.length === 0) &&
                        allNewAutomations.length == 0 &&
                        !isLoading && (
                            <div className="px-4">
                                So empty! Create your own automation to get started.
                                <div className="mt-4">
                                    {authenticatedData ? (
                                        <AutomationComponentWrapper
                                            isMobileWidth={isMobileWidth}
                                            callToAction="Design Automation"
                                            createNew={true}
                                            setIsCreating={setIsCreating}
                                            setShowLoginPrompt={setShowLoginPrompt}
                                            setNewAutomationData={setNewAutomationData}
                                            authenticatedData={authenticatedData}
                                            isCreating={isCreating}
                                            ipLocationData={ipLocationData}
                                        />
                                    ) : (
                                        <Button
                                            onClick={() => setShowLoginPrompt(true)}
                                            variant={"default"}
                                        >
                                            Design
                                        </Button>
                                    )}
                                </div>
                            </div>
                        )}
                    {isLoading && <InlineLoading message="booting up your automations" />}
                    <div className={`${styles.automationsLayout}`}>
                        {personalAutomations &&
                            personalAutomations.map((automation) => (
                                <AutomationsCard
                                    isMobileWidth={isMobileWidth}
                                    key={automation.id}
                                    authenticatedData={authenticatedData}
                                    automation={automation}
                                    locationData={ipLocationData}
                                    isLoggedIn={authenticatedData ? true : false}
                                    setShowLoginPrompt={setShowLoginPrompt}
                                />
                            ))}
                        {allNewAutomations.map((automation) => (
                            <AutomationsCard
                                isMobileWidth={isMobileWidth}
                                key={automation.id}
                                authenticatedData={authenticatedData}
                                automation={automation}
                                locationData={ipLocationData}
                                isLoggedIn={authenticatedData ? true : false}
                                setShowLoginPrompt={setShowLoginPrompt}
                            />
                        ))}
                    </div>
                    <h3 className="text-xl py-4">Try these out</h3>
                    <div className={`${styles.automationsLayout}`}>
                        {suggestedAutomations.map((automation) => (
                            <AutomationsCard
                                isMobileWidth={isMobileWidth}
                                setNewAutomationData={setNewAutomationData}
                                key={automation.id}
                                authenticatedData={authenticatedData}
                                automation={automation}
                                locationData={ipLocationData}
                                isLoggedIn={authenticatedData ? true : false}
                                setShowLoginPrompt={setShowLoginPrompt}
                                suggestedCard={true}
                            />
                        ))}
                    </div>
                </div>
            </div>
        </main>
    );
}
