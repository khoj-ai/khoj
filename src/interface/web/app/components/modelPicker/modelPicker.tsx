import { useAuthenticatedData } from '@/app/common/auth';
import React, { useEffect } from 'react';
import useSWR from 'swr';
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


import styles from './modelPicker.module.css';

export interface Model {
    id: number;
    chat_model: string;
}

// Custom fetcher function to fetch options
const fetchOptionsRequest = async (url: string) => {
    const response = await fetch(url, {
      method: 'GET',
      headers: {
        'Content-Type': 'application/json',
      },
    });
    return response.json();
};

export const useOptionsRequest = (url: string) => {
    const { data, error } = useSWR<Model[]>(url, fetchOptionsRequest);

    return {
      data,
      isLoading: !error && !data,
      isError: error,
    };
};

const fetchSelectedModel = async (url: string) => {
    const response = await fetch(url, {
        method: 'GET',
        headers: {
            'Content-Type': 'application/json',
        },
    });
    return response.json();
}

export const useSelectedModel = (url: string) => {
    const { data, error } = useSWR<Model>(url, fetchSelectedModel);

    return {
        data,
        isLoading: !error && !data,
        isError: error,
    }
}

interface ModelPickerProps {
    disabled?: boolean;
    setModelUsed?: (model: Model) => void;
    initialModel?: Model;
}

export const ModelPicker: React.FC<any> = (props: ModelPickerProps) => {
    const { data: models } = useOptionsRequest('/api/model/chat/options');
    const { data: selectedModel } = useSelectedModel('/api/model/chat');
    const [openLoginDialog, setOpenLoginDialog] = React.useState(false);

    let userData = useAuthenticatedData();

    const setModelUsed = props.setModelUsed;

    useEffect(() => {
        if (setModelUsed && selectedModel) {
            setModelUsed(selectedModel);
        }
    }, [selectedModel, setModelUsed]);

    if (!models) {
        return <div>Loading...</div>;
    }

    function onSelect(model: Model) {
        if (!userData) {
            setOpenLoginDialog(true);
            return;
        }

        if (props.setModelUsed) {
            props.setModelUsed(model);
        }

        fetch('/api/model/chat' + '?id=' + String(model.id), { method: 'POST', body: JSON.stringify(model) })
            .then((response) => {
                if (!response.ok) {
                    throw new Error('Failed to select model');
                }
            })
            .catch((error) => {
                console.error('Failed to select model', error);
            });
    }

    function isSelected(model: Model) {
        if (props.initialModel) {
            return model.id === props.initialModel.id;
        }
        return selectedModel?.id === model.id;
    }

    return (
        <div className={styles.modelPicker}>
            <select className={styles.modelPicker} onChange={(e) => {
                const selectedModelId = Number(e.target.value);
                const selectedModel = models.find((model) => model.id === selectedModelId);
                if (selectedModel) {
                    onSelect(selectedModel);
                } else {
                    console.error('Selected model not found', e.target.value);
                }
            }} disabled={props.disabled}>
                {models?.map((model) => (
                    <option key={model.id} value={model.id} selected={isSelected(model)}>
                        {model.chat_model}
                    </option>
                ))}
            </select>
            <AlertDialog open={openLoginDialog} onOpenChange={setOpenLoginDialog}>
                <AlertDialogContent>
                    <AlertDialogHeader>
                    <AlertDialogTitle>You must be logged in to configure your model.</AlertDialogTitle>
                    <AlertDialogDescription>Once you create an account with Khoj, you can configure your model and use a whole suite of other features. Check out our <a href="https://docs.khoj.dev/">documentation</a> to learn more.
                    </AlertDialogDescription>
                    </AlertDialogHeader>
                    <AlertDialogFooter>
                    <AlertDialogCancel>Cancel</AlertDialogCancel>
                    <AlertDialogAction
                        onClick={() => {
                            window.location.href = window.location.origin + '/login';
                        }}>
                        Sign in
                    </AlertDialogAction>
                    </AlertDialogFooter>
                </AlertDialogContent>
            </AlertDialog>
        </div>
    );
};
