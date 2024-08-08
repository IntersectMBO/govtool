import {
  createContext,
  PropsWithChildren,
  useContext,
  useEffect,
  useMemo,
} from "react";
import * as Sentry from "@sentry/react";

import { NETWORK_NAMES, CEXPLORER_BASE_URLS } from "@/consts";
import { useGetNetworkMetrics } from "@/hooks";

type AppContextType = {
  networkName: string;
  network: string;
  cExplorerBaseUrl: string;
};

const AppContext = createContext<AppContextType | null>(null);

/**
 * Provides app context to its children components.
 *
 * @param children - The child components to render.
 */
const AppContextProvider = ({ children }: PropsWithChildren) => {
  useEffect(() => {
    Sentry.setTag("component_name", "AppContextProvider");
  }, []);

  const { networkMetrics } = useGetNetworkMetrics();

  const value = useMemo(
    () => ({
      networkName:
        NETWORK_NAMES[
          (networkMetrics?.networkName as keyof typeof NETWORK_NAMES) ||
            "preview"
        ],
      network: networkMetrics?.networkName || "preview",
      cExplorerBaseUrl:
        CEXPLORER_BASE_URLS[
          (networkMetrics?.networkName as keyof typeof NETWORK_NAMES) ||
            "preview"
        ],
    }),
    [networkMetrics],
  );

  return <AppContext.Provider value={value}>{children}</AppContext.Provider>;
};

/**
 * Custom hook that provides access to the app context.
 * Throws an error if used outside of an AppContextProvider.
 * @returns The app context.
 */
const useAppContext = () => {
  const context = useContext(AppContext);

  if (!context) {
    throw new Error("useAppContext must be used within an AppContextProvider");
  }

  return context;
};

export { AppContextProvider, useAppContext };
