import {
  createContext,
  PropsWithChildren,
  useContext,
  useEffect,
  useMemo,
  useState,
} from "react";
import * as Sentry from "@sentry/react";

import { NETWORK_NAMES, CEXPLORER_BASE_URLS } from "@/consts";
import { useGetNetworkMetrics, useGetEpochParams } from "@/hooks";
import {
  NETWORK_METRICS_KEY,
  PROTOCOL_PARAMS_KEY,
  setItemToLocalStorage,
} from "@/utils";
import { EpochParams, NetworkMetrics } from "@/models";

type AppContextType = {
  isAppInitializing: boolean;
  networkName: string;
  network: string;
  cExplorerBaseUrl: string;
  epochParams?: EpochParams;
  networkMetrics?: NetworkMetrics;
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
  const { fetchEpochParams, epochParams } = useGetEpochParams();
  const { fetchNetworkMetrics, networkMetrics } = useGetNetworkMetrics();

  const [isAppInitializing, setIsAppInitializing] = useState(true);

  useEffect(() => {
    const init = async () => {
      try {
        const { data: epochParamsData } = await fetchEpochParams();
        if (epochParamsData) {
          setItemToLocalStorage(PROTOCOL_PARAMS_KEY, epochParamsData);
        }

        const { data: networkMetricsData } = await fetchNetworkMetrics();
        if (networkMetricsData) {
          setItemToLocalStorage(NETWORK_METRICS_KEY, networkMetricsData);
        }

        setIsAppInitializing(false);
      } catch (error) {
        Sentry.captureException(error);
      }
    };

    init();
  }, []);

  const value = useMemo(
    () => ({
      isAppInitializing,
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
      epochParams,
      networkMetrics,
    }),
    [isAppInitializing],
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
