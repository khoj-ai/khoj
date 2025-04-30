import React, { useEffect, useRef, useState } from "react";
import { Button } from "@/components/ui/button";
import { Checkbox } from "@/components/ui/checkbox";
import { Card, CardContent, CardHeader } from "@/components/ui/card";
import { useToast } from "@/components/ui/use-toast";
import {
    Dialog,
    DialogContent,
    DialogHeader,
    DialogTitle,
    DialogTrigger,
} from "@/components/ui/dialog";
import { Loader2, Files } from "lucide-react";

interface Repo {
    name: string;
    owner: string;
    branch: string;
    full_name: string;
    description?: string;
    private: boolean;
    selected?: boolean;
}

export default function GitHubRepoSelector({ openExternally }: { openExternally?: boolean }) {
    const [repos, setRepos] = useState<Repo[]>([]);
    const [selected, setSelected] = useState<Set<string>>(new Set());
    const [open, setOpen] = useState(openExternally || false);
    const [loading, setLoading] = useState(false);
    const [wasAutoOpened, setWasAutoOpened] = useState(false);
    const { toast } = useToast();
    // This is the time we wait for the backend to process the request and create the files for the user

    const hasLoaded = useRef(false);

    useEffect(() => {
        if (hasLoaded.current) return;
        hasLoaded.current = true;

        if (typeof window === "undefined") return;

        const params = new URLSearchParams(window.location.search);
        const shouldOpen = openExternally || params.get("github_connected") === "true";

        setLoading(true);
        fetch("/api/github/repos")
            .then((res) => res.json())
            .then((data) => {
                setRepos(data);
                setSelected(new Set(data.filter((r) => r.selected).map((r) => r.full_name)));

                if (shouldOpen) {
                    setOpen(true);
                    setWasAutoOpened(true);
                    params.delete("github_connected");
                    const newUrl = `${window.location.pathname}?${params.toString()}`;
                    window.history.replaceState({}, "", newUrl);
                }
            })
            .catch(() => toast({ title: "⚠️ Failed to load GitHub repos" }))
            .finally(() => setLoading(false));
    }, []);

    const toggleRepo = (fullName: string) => {
        const next = new Set(selected);
        if (next.has(fullName)) next.delete(fullName);
        else next.add(fullName);
        setSelected(next);
    };

    const submitSelection = async () => {
        const selectedRepos = repos.filter((r) => selected.has(r.full_name));
        const payload = selectedRepos.map(({ name, owner, branch }) => ({ name, owner, branch }));
        const res = await fetch("/api/github/repos/select", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ repos: payload }),
        });

        if (res.ok) {
            toast({
                title: "✅ GitHub repos saved",
                description: `Selected ${payload.length} repos.`,
            });
            setOpen(false);
        } else {
            toast({ title: "❌ Failed to save GitHub repos" });
        }
    };

    return (
        <Dialog
            open={open}
            onOpenChange={(nextOpen) => {
                if (!nextOpen && wasAutoOpened && selected.size === 0) {
                    const confirmed = window.confirm(
                        "You haven't selected any repositories. If you close this window, GitHub integration will remain inactive. Are you sure you want to continue?",
                    );
                    if (!confirmed) return;
                }
                setOpen(nextOpen);
            }}
        >
            <DialogTrigger asChild>
                <Button variant="outline" size="sm">
                    <>
                        <Files className="h-5 w-5 inline mr-1" />
                        Manage
                    </>
                </Button>
            </DialogTrigger>
            <DialogContent className="max-w-2xl">
                <DialogHeader>
                    <DialogTitle>Select GitHub Repositories to Index</DialogTitle>
                </DialogHeader>
                <Card className="w-full">
                    <CardContent className="space-y-4 max-h-[60vh] overflow-y-auto">
                        {loading ? (
                            <div className="flex items-center justify-center py-8">
                                <Loader2 className="h-6 w-6 animate-spin" />
                            </div>
                        ) : (
                            repos.map((repo) => (
                                <div key={repo.full_name} className="flex items-center gap-3">
                                    <Checkbox
                                        checked={selected.has(repo.full_name)}
                                        onCheckedChange={() => toggleRepo(repo.full_name)}
                                    />
                                    <div>
                                        <div className="font-medium">{repo.full_name}</div>
                                        <div className="text-muted-foreground text-sm">
                                            {repo.description || "No description"}
                                        </div>
                                    </div>
                                </div>
                            ))
                        )}
                    </CardContent>
                    {!loading && (
                        <div className="p-4 border-t flex justify-end">
                            <Button onClick={submitSelection} disabled={selected.size === 0}>
                                Save Selection
                            </Button>
                        </div>
                    )}
                </Card>
            </DialogContent>
        </Dialog>
    );
}
