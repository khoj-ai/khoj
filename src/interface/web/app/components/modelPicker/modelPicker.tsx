// Write a component that reads the model list from the server and displays it in a dropdown. The component should have a prop `onSelect` that is called with the selected model when the user selects a model from the dropdown. Use the swr library to fetch the model list from the server.

import { useAuthenticatedData } from '@/app/common/auth';
import React from 'react';
import useSWR from 'swr';

import styles from './modelPicker.module.css';

export interface Model {
    id: number;
    chat_model: string;
}

// Custom fetcher that makes an OPTIONS request
const fetchOptionsRequest = async (url: string) => {
    const response = await fetch(url, {
      method: 'GET',
      // Include any additional headers your request might need
      headers: {
        'Content-Type': 'application/json',
      },
    });
    // Assuming the response is JSON. Adjust if needed.
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

export const ModelPicker: React.FC<any> = () => {
    const { data: models } = useOptionsRequest('/api/config/data/conversation/model/options');
    const { data: selectedModel } = useSelectedModel('/api/config/data/conversation/model');

    let userData = useAuthenticatedData();

    if (!models) {
        return <div>Loading...</div>;
    }

    function onSelect(model: Model) {
        if (!userData) {
            alert('Please sign in to select a model');
            return;
        }

        fetch('/api/config/data/conversation/model' + '?id=' + String(model.id), { method: 'POST', body: JSON.stringify(model) })
            .then((response) => {
                if (!response.ok) {
                    throw new Error('Failed to select model');
                }
            })
            .catch((error) => {
                console.error('Failed to select model', error);
            });
    }

    return (
        <select className={styles.modelPicker} onChange={(e) => {
            const selectedModelId = Number(e.target.value);
            const selectedModel = models.find((model) => model.id === selectedModelId);
            if (selectedModel) {
                onSelect(selectedModel);
            } else {
                console.error('Selected model not found', e.target.value);
            }
        }}>
            {models?.map((model) => (
                <option key={model.id} value={model.id} selected={selectedModel?.id === model.id}>
                    {model.chat_model}
                </option>
            ))}
        </select>
    );
};
