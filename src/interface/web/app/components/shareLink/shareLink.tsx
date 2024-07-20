import {
    Dialog,
    DialogContent,
    DialogDescription,
    DialogHeader,
    DialogTitle,
    DialogTrigger,
} from "@/components/ui/dialog";

import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";

interface ShareLinkProps {
    buttonTitle: string;
    title: string;
    description: string;
    url: string;
    onShare: () => void;
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
                className="inline-flex items-center justify-center whitespace-nowrap rounded-md text-sm font-medium ring-offset-background transition-colors focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 disabled:pointer-events-none disabled:opacity-50 bg-primary text-primary-foreground hover:bg-primary/90 h-10 px-4 py-2"
                onClick={props.onShare}>
                    {props.buttonTitle}
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
