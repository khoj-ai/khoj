"use client"

import * as React from "react"
import { useState, useEffect } from "react";
import { PopoverProps } from "@radix-ui/react-popover"

import { Check, CaretUpDown } from "@phosphor-icons/react";

import { cn } from "@/lib/utils"
import { useMutationObserver } from "@/app/common/utils";
import { Button } from "@/components/ui/button";
import {
    Command,
    CommandEmpty,
    CommandGroup,
    CommandInput,
    CommandItem,
    CommandList,
} from "@/components/ui/command";
import { Label } from "@/components/ui/label";
import {
    Popover,
    PopoverContent,
    PopoverTrigger,
} from "@/components/ui/popover";

import { ModelOptions, useChatModelOptions } from "./auth";
import { HoverCard, HoverCardContent, HoverCardTrigger } from "@/components/ui/hover-card";
import { Skeleton } from "@/components/ui/skeleton";

interface ModelSelectorProps extends PopoverProps {
    onSelect: (model: ModelOptions) => void;
    selectedModel?: string;
    disabled?: boolean;
}

export function ModelSelector({ ...props }: ModelSelectorProps) {
    const [open, setOpen] = React.useState(false)
    const { models, isLoading, error } = useChatModelOptions();
    const [peekedModel, setPeekedModel] = useState<ModelOptions | undefined>(undefined);
    const [selectedModel, setSelectedModel] = useState<ModelOptions | undefined>(undefined);

    useEffect(() => {
        if (!models?.length) return;

        if (props.selectedModel) {
            const model = models.find(model => model.name === props.selectedModel);
            setSelectedModel(model || models[0]);
            return;
        }

        setSelectedModel(models[0]);
    }, [models, props.selectedModel]);

    if (isLoading) {
        return (
            <Skeleton className="w-full h-10" />
        );
    }

    if (error) {
        return (
            <div className="text-sm text-error">{error.message}</div>
        );
    }

    return (
        <div className="grid gap-2 w-[250px]">
            <Popover open={open} onOpenChange={setOpen} {...props}>
                <PopoverTrigger asChild>
                    <Button
                        variant="outline"
                        role="combobox"
                        aria-expanded={open}
                        aria-label="Select a model"
                        className="w-full justify-between text-left"
                        disabled={props.disabled ?? false}
                    >
                        <p className="truncate">
                            {selectedModel ? selectedModel.name.substring(0, 20) : "Select a model..."}
                        </p>
                        <CaretUpDown className="opacity-50" />
                    </Button>
                </PopoverTrigger>
                <PopoverContent align="end" className="w-[250px] p-0">
                    <HoverCard>
                        <HoverCardContent
                            side="left"
                            align="start"
                            forceMount
                            className="min-h-[280px]"
                        >
                            <div className="grid gap-2">
                                <h4 className="font-medium leading-none">{peekedModel?.name}</h4>
                                <div className="text-sm text-muted-foreground">
                                    {peekedModel?.description}
                                </div>
                                {peekedModel?.strengths ? (
                                    <div className="mt-4 grid gap-2">
                                        <h5 className="text-sm font-medium leading-none">
                                            Strengths
                                        </h5>
                                        <ul className="text-sm text-muted-foreground">
                                            {peekedModel.strengths}
                                        </ul>
                                    </div>
                                ) : null}
                            </div>
                        </HoverCardContent>
                        <div>
                            <HoverCardTrigger />
                            <Command loop>
                                <CommandList className="h-[var(--cmdk-list-height)]">
                                    <CommandInput placeholder="Search Models..." />
                                    <CommandEmpty>No Models found.</CommandEmpty>
                                    <CommandGroup key={"models"} heading={"Models"}>
                                        {models && models.length > 0 && models
                                            .map((model) => (
                                                <ModelItem
                                                    key={model.id}
                                                    model={model}
                                                    isSelected={selectedModel?.id === model.id}
                                                    onPeek={(model) => setPeekedModel(model)}
                                                    onSelect={() => {
                                                        setSelectedModel(model)
                                                        setOpen(false)
                                                    }}
                                                />
                                            ))}
                                    </CommandGroup>
                                </CommandList>
                            </Command>
                        </div>
                    </HoverCard>
                </PopoverContent>
            </Popover>
        </div>
    )
}

interface ModelItemProps {
    model: ModelOptions,
    isSelected: boolean,
    onSelect: () => void,
    onPeek: (model: ModelOptions) => void
}

function ModelItem({ model, isSelected, onSelect, onPeek }: ModelItemProps) {
    const ref = React.useRef<HTMLDivElement>(null)

    useMutationObserver(ref, (mutations) => {
        mutations.forEach((mutation) => {
            if (
                mutation.type === "attributes" &&
                mutation.attributeName === "aria-selected" &&
                ref.current?.getAttribute("aria-selected") === "true"
            ) {
                onPeek(model)
            }
        })
    })

    return (
        <CommandItem
            key={model.id}
            onSelect={onSelect}
            ref={ref}
            className="data-[selected=true]:bg-muted data-[selected=true]:text-secondary-foreground"
        >
            {model.name}
            <Check
                className={cn("ml-auto", isSelected ? "opacity-100" : "opacity-0")}
            />
        </CommandItem>
    )
}
