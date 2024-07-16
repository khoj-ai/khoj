'use client'

import useSWR from 'swr'

export interface UserProfile {
    email: string;
    username: string;
    photo: string;
    is_active: boolean;
    has_documents: boolean;
    detail: string;
}

const fetcher = (url: string) =>
    window.fetch(url)
        .then(res => res.json())
        .catch(err => console.warn(err));

export function useAuthenticatedData() {
    const { data, error } = useSWR<UserProfile>('/api/v1/user', fetcher, { revalidateOnFocus: false });

    if (error || !data || data.detail === 'Forbidden') return null;

    return data;
}

export interface ModelOptions {
    id: number;
    name: string;
}
export interface SyncedContent {
    computer: boolean;
    github: boolean;
    notion: boolean;
}
export interface UserConfig {
    username: string | null;
    user_photo: string | null;
    is_active: boolean;
    has_documents: boolean;
    khoj_version: string;
    enabled_content_source: SyncedContent;
    anonymous_mode: boolean;
    given_name: string;
    search_model_options: ModelOptions[];
    selected_search_model_config: number;
    chat_model_options: ModelOptions[];
    selected_chat_model_config: number;
    paint_model_options: ModelOptions[];
    selected_paint_model_config: number;
    billing_enabled: boolean;
    subscription_state: string;
    subscription_renewal_date: string;
    khoj_cloud_subscription_url: string | undefined;
    is_twilio_enabled: boolean;
    is_eleven_labs_enabled: boolean;
    voice_model_options: ModelOptions[];
    selected_voice_config: number;
    phone_number: string;
    is_phone_number_verified: boolean;
    notion_oauth_url: string;
    detail: string;
}


export function useUserConfig(detailed: boolean = false) {
    const url = `/api/settings?detailed=${detailed}`;
    const { data, error } = useSWR<UserConfig>(url, fetcher, { revalidateOnFocus: false });

    if (error || !data || data.detail === 'Forbidden') return null;

    return data;
}
