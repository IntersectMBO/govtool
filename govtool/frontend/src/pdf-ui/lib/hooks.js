import { useState, useEffect, useRef } from 'react';
import { useLocation } from 'react-router';

export const useDebounce = (value, delay = 1000) => {
    const [debouncedValue, setDebouncedValue] = useState(value);

    useEffect(() => {
        const timer = setTimeout(() => setDebouncedValue(value), delay);

        return () => {
            clearTimeout(timer);
        };
    }, [value, delay]);

    return debouncedValue;
};

export const ScrollToTop = ({ step }) => {
    const { pathname } = useLocation();

    useEffect(() => {
        window.scrollTo({
            top: 0,
            behavior: 'smooth',
        });
    }, [pathname, step && step]);

    return null;
};

export function useScrollToHashSection(sectionIds = [], visibleSections = []) {
    const sectionRefs = useRef({});
    const [shouldExpand, setShouldExpand] = useState(false);
    const [hash, setHash] = useState(() => window.location.hash.slice(1));

    const setRef = (id) => (el) => {
        if (el) {
            sectionRefs.current[id] = el;
        }
    };

    // Listen to hash changes
    useEffect(() => {
        const handleHashChange = () => {
            const newHash = window.location.hash.slice(1);
            setHash(newHash);
        };

        window.addEventListener('hashchange', handleHashChange);
        handleHashChange(); // Trigger on mount

        return () => {
            window.removeEventListener('hashchange', handleHashChange);
        };
    }, []);

    useEffect(() => {
        if (!hash) return;

        const isHiddenSection = sectionIds.includes(hash);
        const isVisibleSection = visibleSections?.includes?.(hash);

        if (isHiddenSection && !isVisibleSection) {
            setShouldExpand(true);
        }

        let attempt = 0;
        const maxAttempts = 50; // Try for 5 seconds, 100ms * 50

        const interval = setInterval(() => {
            const target = sectionRefs.current?.[hash];
            if (target) {
                const rect = target?.getBoundingClientRect();
                const offsetTop = rect.top + window.scrollY - 20;

                const header = document?.querySelector('header');
                const headerHeight = header ? header?.offsetHeight : 80;

                window.scrollTo({
                    top: offsetTop - headerHeight,
                    behavior: 'smooth',
                });
                clearInterval(interval);
            } else if (++attempt >= maxAttempts) {
                clearInterval(interval);
            }
        }, 100);

        return () => clearInterval(interval);
    }, [hash, sectionIds, visibleSections]);

    const refMap = {};
    [...sectionIds, ...visibleSections].forEach((id) => {
        refMap[id] = setRef(id);
    });

    return {
        sectionRefs: refMap,
        shouldExpand,
        setShouldExpand,
    };
}
