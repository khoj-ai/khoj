"use client";

import useSWR from "swr";

export interface UserProfile {
    email: string;
    username: string;
    photo: string;
    is_active: boolean;
    has_documents: boolean;
    detail: string;
    khoj_version: string;
}

const fetcher = (url: string) =>
    window
        .fetch(url)
        .then((res) => res.json())
        .catch((err) => console.warn(err));

export function useAuthenticatedData() {
    const { data, error, isLoading } = useSWR<UserProfile>("/api/v1/user", fetcher, {
        revalidateOnFocus: false,
    });

    if (data?.detail === "Forbidden") {
        return { data: null, error: "Forbidden", isLoading: false };
    }

    return { data, error, isLoading };
}

export interface ModelOptions {
    id: number;
    name: string;
    tier: string;
    description: string;
    strengths: string;
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
    subscription_renewal_date: string | undefined;
    subscription_enabled_trial_at: string | undefined;
    // server settings
    khoj_cloud_subscription_url: string | undefined;
    billing_enabled: boolean;
    is_eleven_labs_enabled: boolean;
    is_twilio_enabled: boolean;
    khoj_version: string;
    anonymous_mode: boolean;
    notion_oauth_url: string;
    detail: string;
    length_of_free_trial: number;
}

export function useUserConfig(detailed: boolean = false) {
    const url = `/api/settings?detailed=${detailed}`;
    const {
        data,
        error,
        isLoading,
    } = useSWR<UserConfig>(url, fetcher, { revalidateOnFocus: false });

    if (error || !data || data?.detail === "Forbidden") {
        return { data: null, error, isLoading };
    }

    return { data, error, isLoading };
}

export function useChatModelOptions() {
    const { data, error, isLoading } = useSWR<ModelOptions[]>(`/api/model/chat/options`, fetcher, {
        revalidateOnFocus: false,
    });

    return { models: data, error, isLoading };
}

export function isUserSubscribed(userConfig: UserConfig | null): boolean {
    return (
        (userConfig?.subscription_state &&
            [
                SubscriptionStates.SUBSCRIBED.valueOf(),
                SubscriptionStates.TRIAL.valueOf(),
                SubscriptionStates.UNSUBSCRIBED.valueOf(),
            ].includes(userConfig.subscription_state)) ||
        false
    );
}
