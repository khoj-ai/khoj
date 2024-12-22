import React from "react";
import { convertColorToTextClass } from "./colorUtils";
import {
    Lightbulb,
    Robot,
    Aperture,
    GraduationCap,
    Jeep,
    Island,
    MathOperations,
    Asclepius,
    Couch,
    Code,
    Atom,
    ClockCounterClockwise,
    File,
    Globe,
    Palette,
    Book,
    Confetti,
    House,
    Translate,
    Image,
    BowlFood,
    Lectern,
    Wallet,
    PencilLine,
    Chalkboard,
    Gps,
    Question,
    Browser,
    Notebook,
    Shapes,
    ChatsTeardrop,
    GlobeSimple,
    ArrowRight,
    Cigarette,
    CraneTower,
    Heart,
    Leaf,
    NewspaperClipping,
    OrangeSlice,
    SmileyMelting,
    YinYang,
    SneakerMove,
    Student,
    Oven,
    Gavel,
    Broadcast,
    KeyReturn,
    FilePdf,
    FileMd,
    MicrosoftWordLogo,
    Microscope,
} from "@phosphor-icons/react";
import { OrgMode } from "@/app/components/logo/fileLogo";

interface IconMap {
    [key: string]: (color: string, width: string, height: string) => JSX.Element | null;
}

const iconMap: IconMap = {
    Lightbulb: (color: string, width: string, height: string) => (
        <Lightbulb className={`${width} ${height} ${color} mr-2`} />
    ),
    Robot: (color: string, width: string, height: string) => (
        <Robot className={`${width} ${height} ${color} mr-2`} />
    ),
    Aperture: (color: string, width: string, height: string) => (
        <Aperture className={`${width} ${height} ${color} mr-2`} />
    ),
    GraduationCap: (color: string, width: string, height: string) => (
        <GraduationCap className={`${width} ${height} ${color} mr-2`} />
    ),
    Jeep: (color: string, width: string, height: string) => (
        <Jeep className={`${width} ${height} ${color} mr-2`} />
    ),
    Island: (color: string, width: string, height: string) => (
        <Island className={`${width} ${height} ${color} mr-2`} />
    ),
    MathOperations: (color: string, width: string, height: string) => (
        <MathOperations className={`${width} ${height} ${color} mr-2`} />
    ),
    Asclepius: (color: string, width: string, height: string) => (
        <Asclepius className={`${width} ${height} ${color} mr-2`} />
    ),
    Couch: (color: string, width: string, height: string) => (
        <Couch className={`${width} ${height} ${color} mr-2`} />
    ),
    Code: (color: string, width: string, height: string) => (
        <Code className={`${width} ${height} ${color} mr-2`} />
    ),
    Atom: (color: string, width: string, height: string) => (
        <Atom className={`${width} ${height} ${color} mr-2`} />
    ),
    ClockCounterClockwise: (color: string, width: string, height: string) => (
        <ClockCounterClockwise className={`${width} ${height} ${color} mr-2`} />
    ),
    Globe: (color: string, width: string, height: string) => (
        <Globe className={`${width} ${height} ${color} mr-2`} />
    ),
    Palette: (color: string, width: string, height: string) => (
        <Palette className={`${width} ${height} ${color} mr-2`} />
    ),
    Book: (color: string, width: string, height: string) => (
        <Book className={`${width} ${height} ${color} mr-2`} />
    ),
    Confetti: (color: string, width: string, height: string) => (
        <Confetti className={`${width} ${height} ${color} mr-2`} />
    ),
    House: (color: string, width: string, height: string) => (
        <House className={`${width} ${height} ${color} mr-2`} />
    ),
    Translate: (color: string, width: string, height: string) => (
        <Translate className={`${width} ${height} ${color} mr-2`} />
    ),
    BowlFood: (color: string, width: string, height: string) => (
        <BowlFood className={`${width} ${height} ${color} mr-2`} />
    ),
    Lectern: (color: string, width: string, height: string) => (
        <Lectern className={`${width} ${height} ${color} mr-2`} />
    ),
    Wallet: (color: string, width: string, height: string) => (
        <Wallet className={`${width} ${height} ${color} mr-2`} />
    ),
    PencilLine: (color: string, width: string, height: string) => (
        <PencilLine className={`${width} ${height} ${color} mr-2`} />
    ),
    Chalkboard: (color: string, width: string, height: string) => (
        <Chalkboard className={`${width} ${height} ${color} mr-2`} />
    ),
    Cigarette: (color: string, width: string, height: string) => (
        <Cigarette className={`${width} ${height} ${color} mr-2`} />
    ),
    CraneTower: (color: string, width: string, height: string) => (
        <CraneTower className={`${width} ${height} ${color} mr-2`} />
    ),
    Heart: (color: string, width: string, height: string) => (
        <Heart className={`${width} ${height} ${color} mr-2`} />
    ),
    Leaf: (color: string, width: string, height: string) => (
        <Leaf className={`${width} ${height} ${color} mr-2`} />
    ),
    NewspaperClipping: (color: string, width: string, height: string) => (
        <NewspaperClipping className={`${width} ${height} ${color} mr-2`} />
    ),
    OrangeSlice: (color: string, width: string, height: string) => (
        <OrangeSlice className={`${width} ${height} ${color} mr-2`} />
    ),
    SmileyMelting: (color: string, width: string, height: string) => (
        <SmileyMelting className={`${width} ${height} ${color} mr-2`} />
    ),
    YinYang: (color: string, width: string, height: string) => (
        <YinYang className={`${width} ${height} ${color} mr-2`} />
    ),
    SneakerMove: (color: string, width: string, height: string) => (
        <SneakerMove className={`${width} ${height} ${color} mr-2`} />
    ),
    Student: (color: string, width: string, height: string) => (
        <Student className={`${width} ${height} ${color} mr-2`} />
    ),
    Oven: (color: string, width: string, height: string) => (
        <Oven className={`${width} ${height} ${color} mr-2`} />
    ),
    Gavel: (color: string, width: string, height: string) => (
        <Gavel className={`${width} ${height} ${color} mr-2`} />
    ),
    Broadcast: (color: string, width: string, height: string) => (
        <Broadcast className={`${width} ${height} ${color} mr-2`} />
    ),
    Image: (color: string, width: string, height: string) => (
        <Image className={`${width} ${height} ${color} mr-2`} />
    ),
    File: (color: string, width: string, height: string) => (
        <File className={`${width} ${height} ${color} mr-2`} />
    ),
};

export function getIconForSlashCommand(command: string, customClassName: string | null = null) {
    const className = customClassName ?? "h-4 w-4";
    if (command.includes("summarize")) {
        return <Gps className={className} />;
    }

    if (command.includes("help")) {
        return <Question className={className} />;
    }

    if (command.includes("automation")) {
        return <Robot className={className} />;
    }

    if (command.includes("webpage")) {
        return <Browser className={className} />;
    }

    if (command.includes("notes")) {
        return <Notebook className={className} />;
    }

    if (command.includes("image")) {
        return <Image className={className} />;
    }

    if (command.includes("default")) {
        return <KeyReturn className={className} />;
    }

    if (command.includes("diagram")) {
        return <Shapes className={className} />;
    }

    if (command.includes("general")) {
        return <ChatsTeardrop className={className} />;
    }

    if (command.includes("online")) {
        return <GlobeSimple className={className} />;
    }

    if (command.includes("text")) {
        return <PencilLine className={className} />;
    }

    if (command.includes("code")) {
        return <Code className={className} />;
    }

    if (command.includes("research")) {
        return <Microscope className={className} />;
    }

    return <ArrowRight className={className} />;
}

function getIconFromIconName(
    iconName: string,
    color: string = "gray",
    width: string = "w-6",
    height: string = "h-6",
) {
    const icon = iconMap[iconName];
    const colorName = color.toLowerCase();
    const colorClass = convertColorToTextClass(colorName);
    return icon ? icon(colorClass, width, height) : null;
}

function getIconFromFilename(
    filename: string,
    className: string = "w-6 h-6 text-muted-foreground inline-flex mr-1",
) {
    const extension = filename.split(".").pop();
    switch (extension) {
        case "org":
            return <OrgMode className={className} />;
        case "markdown":
        case "md":
            return <FileMd className={className} />;
        case "pdf":
            return <FilePdf className={className} />;
        case "doc":
        case "docx":
            return <MicrosoftWordLogo className={className} />;
        case "csv":
        case "json":
            return <MathOperations className={className} />;
        case "txt":
            return <Notebook className={className} />;
        case "py":
            return <Code className={className} />;
        case "jpg":
        case "jpeg":
        case "png":
        case "webp":
            return <Image className={className} weight="fill" />;
        default:
            return <File className={className} weight="fill" />;
    }
}

function getAvailableIcons() {
    return Object.keys(iconMap);
}

export { getIconFromIconName, getIconFromFilename, getAvailableIcons };
