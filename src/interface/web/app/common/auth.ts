"use client";

import useSWR from "swr";

export interface UserProfile {
    email: string;
    username: string;
    photo: string;
    is_active: boolean;
    has_documents: boolean;
    detail: string;
}

const fetcher = (url: string) =>
    window
        .fetch(url)
        .then((res) => res.json())
        .catch((err) => console.warn(err));

export function useAuthenticatedData() {
    const { data, error } = useSWR<UserProfile>("/api/v1/user", fetcher, {
        revalidateOnFocus: false,
    });

    if (error || !data || data.detail === "Forbidden") return null;

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

export enum SubscriptionStates {
    EXPIRED = "expired",
    TRIAL = "trial",
    SUBSCRIBED = "subscribed",
    UNSUBSCRIBED = "unsubscribed",
    INVALID = "invalid",
}

export interface UserConfig {
    // user info
    username: string;
    user_photo: string | null;
    is_active: boolean;
    given_name: string;
    phone_number: string;
    is_phone_number_verified: boolean;
    // user content settings
    enabled_content_source: SyncedContent;
    has_documents: boolean;
    notion_token: string | null;
    // user model settings
    search_model_options: ModelOptions[];
    selected_search_model_config: number;
    chat_model_options: ModelOptions[];
    selected_chat_model_config: number;
    paint_model_options: ModelOptions[];
    selected_paint_model_config: number;
    voice_model_options: ModelOptions[];
    selected_voice_model_config: number;
    // user billing info
    subscription_state: SubscriptionStates;
    subscription_renewal_date: string;
    // server settings
    khoj_cloud_subscription_url: string | undefined;
    billing_enabled: boolean;
    is_eleven_labs_enabled: boolean;
    is_twilio_enabled: boolean;
    khoj_version: string;
    anonymous_mode: boolean;
    notion_oauth_url: string;
    detail: string;
}

export function useUserConfig(detailed: boolean = false) {
    const url = `/api/settings?detailed=${detailed}`;
    const {
        data: userConfig,
        error,
        isLoading: isLoadingUserConfig,
    } = useSWR<UserConfig>(url, fetcher, { revalidateOnFocus: false });

    if (error || !userConfig || userConfig?.detail === "Forbidden")
        return { userConfig: null, isLoadingUserConfig };

    return { userConfig, isLoadingUserConfig };
}
