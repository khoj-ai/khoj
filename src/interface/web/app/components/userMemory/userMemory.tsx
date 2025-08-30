import { useState } from "react";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import { Pencil, TrashSimple, FloppyDisk, X } from "@phosphor-icons/react";
import { useToast } from "@/components/ui/use-toast";

export interface UserMemorySchema {
    id: number;
    raw: string;
    created_at: string;
}

interface UserMemoryProps {
    memory: UserMemorySchema;
    onDelete: (id: number) => void;
    onUpdate: (id: number, raw: string) => void;
}

export function UserMemory({ memory, onDelete, onUpdate }: UserMemoryProps) {
    const [isEditing, setIsEditing] = useState(false);
    const [content, setContent] = useState(memory.raw);
    const { toast } = useToast();

    const handleUpdate = () => {
        onUpdate(memory.id, content);
        setIsEditing(false);
        toast({
            title: "Memory Updated",
            description: "Your memory has been successfully updated.",
        });
    };

    const handleDelete = () => {
        onDelete(memory.id);
        toast({
            title: "Memory Deleted",
            description: "Your memory has been successfully deleted.",
        });
    };

    return (
        <div className="flex items-center gap-2 w-full">
            {isEditing ? (
                <>
                    <Input
                        value={content}
                        onChange={(e) => setContent(e.target.value)}
                        className="flex-1"
                    />
                    <Button
                        variant="ghost"
                        size="icon"
                        onClick={handleUpdate}
                        title="Save"
                    >
                        <FloppyDisk className="h-4 w-4" />
                    </Button>
                    <Button
                        variant="ghost"
                        size="icon"
                        onClick={() => setIsEditing(false)}
                        title="Cancel"
                    >
                        <X className="h-4 w-4" />
                    </Button>
                </>
            ) : (
                <>
                    <Input value={memory.raw} readOnly className="flex-1" />
                    <Button
                        variant="ghost"
                        size="icon"
                        onClick={() => setIsEditing(true)}
                        title="Edit"
                    >
                        <Pencil className="h-4 w-4" />
                    </Button>
                    <Button
                        variant="ghost"
                        size="icon"
                        onClick={handleDelete}
                        title="Delete"
                    >
                        <TrashSimple className="h-4 w-4" />
                    </Button>
                </>
            )}
        </div>
    );
}
