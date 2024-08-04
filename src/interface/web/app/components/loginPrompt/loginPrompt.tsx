import {
    AlertDialog,
    AlertDialogAction,
    AlertDialogCancel,
    AlertDialogContent,
    AlertDialogDescription,
    AlertDialogFooter,
    AlertDialogHeader,
    AlertDialogTitle,
} from "@/components/ui/alert-dialog";
import Link from "next/link";

export interface LoginPromptProps {
    loginRedirectMessage: string;
    onOpenChange: (open: boolean) => void;
}

export default function LoginPrompt(props: LoginPromptProps) {
    return (
        <AlertDialog open={true} onOpenChange={props.onOpenChange}>
            <AlertDialogContent>
                <AlertDialogHeader>
                    <AlertDialogTitle>Sign in to Khoj to continue</AlertDialogTitle>
                </AlertDialogHeader>
                <AlertDialogDescription>
                    {props.loginRedirectMessage}. By logging in, you agree to our{" "}
                    <Link href="https://khoj.dev/terms-of-service">Terms of Service.</Link>
                </AlertDialogDescription>
                <AlertDialogFooter>
                    <AlertDialogCancel>Dismiss</AlertDialogCancel>
                    <AlertDialogAction
                        className="bg-slate-400 hover:bg-slate-500"
                        onClick={() => {
                            window.location.href = `/login?next=${encodeURIComponent(window.location.pathname)}`;
                        }}
                    >
                        <Link href={`/login?next=${encodeURIComponent(window.location.pathname)}`}>
                            {" "}
                            {/* Redirect to login page */}
                            Login
                        </Link>
                    </AlertDialogAction>
                </AlertDialogFooter>
            </AlertDialogContent>
        </AlertDialog>
    );
}
