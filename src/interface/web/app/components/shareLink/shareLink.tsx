import {
    Dialog,
    DialogContent,
    DialogDescription,
    DialogHeader,
    DialogTitle,
    DialogTrigger,
} from "@/components/ui/dialog";

import { Button, buttonVariants } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import { Share } from "@phosphor-icons/react";

interface ShareLinkProps {
    buttonTitle: string;
    title: string;
    description: string;
    url: string;
    onShare: () => void;
    buttonVariant?: keyof typeof buttonVariants;
    includeIcon?: boolean;
}

function copyToClipboard(text: string) {
    const clipboard = navigator.clipboard;
    if (!clipboard) {
        return;
    }
    clipboard.writeText(text);
}

export default function ShareLink(props: ShareLinkProps) {
    return (
        <Dialog>
            <DialogTrigger
                asChild
                onClick={props.onShare}>
                <Button size="sm" className={`px-3`} variant={props.buttonVariant ?? 'default' as const}>
                    {
                        props.includeIcon && (
                            <Share className="w-4 h-4 mr-2" />
                        )
                    }
                    {props.buttonTitle}
                </Button>
            </DialogTrigger>
            <DialogContent>
                <DialogHeader>
                    <DialogTitle>{props.title}</DialogTitle>
                    <DialogDescription>
                        {props.description}
                    </DialogDescription>
                </DialogHeader>
                <div className="flex items-center space-x-2">
                    <div className="grid flex-1 gap-2">
                        <Label htmlFor="link" className="sr-only">
                            Link
                        </Label>
                        <Input
                            id="link"
                            defaultValue={props.url}
                            readOnly
                        />
                    </div>
                    <Button type="submit" size="sm" className="px-3" onClick={() => copyToClipboard(props.url)}>
                        <span>Copy</span>
                    </Button>
                </div>
            </DialogContent>
        </Dialog>
    );
}
