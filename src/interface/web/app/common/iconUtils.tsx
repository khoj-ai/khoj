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
    PaperPlaneTilt,
    Info,
    UserCircle,
    Globe,
    Palette,
    LinkBreak,
    Book,
    Confetti,
    House,
    Translate,
} from "@phosphor-icons/react";

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
};

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

export { getIconFromIconName };
