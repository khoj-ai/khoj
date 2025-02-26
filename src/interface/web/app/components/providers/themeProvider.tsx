'use client'

import { useIsDarkMode } from '@/app/common/utils'

export function ThemeProvider({ children }: { children: React.ReactNode }) {
    const [darkMode, setDarkMode] = useIsDarkMode();
    return <>{children}</>;
}
