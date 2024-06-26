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

const userFetcher = () => window.fetch('/api/v1/user').then(res => res.json()).catch(err => console.log(err));

export function useAuthenticatedData() {

    const { data, error } = useSWR<UserProfile>('/api/v1/user', userFetcher, { revalidateOnFocus: false });

    // console.log("auth data returned", data);
    // console.log("error auth data", error);
    if (!data) return null;
    if (data.detail === "Forbidden") return null;
    if (error) return null;

    return data;
}
