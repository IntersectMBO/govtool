import React, {
  useCallback,
  useContext,
  useEffect,
  useMemo,
  useState,
} from "react";
import { env } from "@/config/env";

type ChatwootSettings = {
  hideMessageBubble?: boolean;
  position?: "left" | "right";
  locale?: string;
  type?: "standard" | "expanded_bubble";
};

type ChatwootAPI = {
  hasLoaded?: boolean;
  toggle: (state?: "open" | "close") => void;
  setUser: (
    identifier: string,
    user: { name?: string; email?: string },
  ) => void;
  setCustomAttributes: (attributes: Record<string, string>) => void;
  reset: () => void;
};

declare global {
  interface Window {
    chatwootSettings?: ChatwootSettings;
    chatwootSDK?: {
      run: (config: { websiteToken: string; baseUrl: string }) => void;
    };
    $chatwoot?: ChatwootAPI;
  }
}

const BASE_URL = env.VITE_CHATWOOT_URL?.replace(/\/+$/, "");
const WEBSITE_TOKEN = env.VITE_CHATWOOT_WEBSITE_TOKEN;
const SCRIPT_ID = "chatwoot-sdk";

const defaultValues = {
  openFeedbackWindow: () => {},
};

export const ChatwootContext = React.createContext(defaultValues);

type ChatwootProviderProps = {
  children?: React.ReactNode;
};

export const ChatwootProvider = ({ children }: ChatwootProviderProps) => {
  const [isReady, setIsReady] = useState(false);

  useEffect(() => {
    if (!BASE_URL || !WEBSITE_TOKEN) return undefined;

    const onReady = () => setIsReady(true);
    window.addEventListener("chatwoot:ready", onReady);

    if (window.$chatwoot?.hasLoaded) {
      setIsReady(true);
      // Injecting the SDK twice resets window.$chatwoot after the widget
      // iframe has already completed its handshake, leaving the widget
      // permanently unloaded (React StrictMode mounts effects twice in dev).
    } else if (!document.getElementById(SCRIPT_ID)) {
      window.chatwootSettings = {
        hideMessageBubble: true,
        position: "right",
        type: "standard",
      };

      const script = document.createElement("script");
      script.id = SCRIPT_ID;
      script.src = `${BASE_URL}/packs/js/sdk.js`;
      script.defer = true;
      script.async = true;
      script.onload = () => {
        window.chatwootSDK?.run({
          websiteToken: WEBSITE_TOKEN,
          baseUrl: BASE_URL,
        });
      };
      script.onerror = (error) => {
        console.error("Failed to load the Chatwoot SDK", error);
      };
      document.head.appendChild(script);
    }

    return () => {
      window.removeEventListener("chatwoot:ready", onReady);
    };
  }, []);

  const openFeedbackWindow = useCallback(() => {
    if (isReady && window.$chatwoot) {
      window.$chatwoot.toggle("open");
    }
  }, [isReady]);

  const value = useMemo(() => ({ openFeedbackWindow }), [openFeedbackWindow]);

  return (
    <ChatwootContext.Provider value={value}>
      {children}
    </ChatwootContext.Provider>
  );
};

export function useChatwoot() {
  return useContext(ChatwootContext);
}
