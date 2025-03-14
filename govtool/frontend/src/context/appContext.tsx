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
import { useGetEpochParams, useGetNetworkInfo } from "@/hooks";
import {
  NETWORK_INFO_KEY,
  PROTOCOL_PARAMS_KEY,
  setItemToLocalStorage,
} from "@/utils";
import { EpochParams, Network } from "@/models";
import { adaHandleService } from "@/services/AdaHandle";

const BOOTSTRAPPING_PHASE_MAJOR = 9;

type AppContextType = {
  isAppInitializing: boolean;
  isMainnet: boolean;
  isInBootstrapPhase: boolean;
  isFullGovernance: boolean;
  networkName: string;
  network: Network;
  cExplorerBaseUrl: string;
  epochParams?: EpochParams;
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
  const { fetchNetworkInfo, networkInfo } = useGetNetworkInfo();

  const [isAppInitializing, setIsAppInitializing] = useState(true);

  useEffect(() => {
    const init = async () => {
      try {
        const { data: epochParamsData } = await fetchEpochParams();
        if (epochParamsData) {
          setItemToLocalStorage(PROTOCOL_PARAMS_KEY, epochParamsData);
        }

        const { data: networkInfoData } = await fetchNetworkInfo();
        if (networkInfoData) {
          setItemToLocalStorage(NETWORK_INFO_KEY, networkInfoData);

          // Initialize ada handle service
          adaHandleService.initialize(networkInfoData?.networkName);
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
      isMainnet: networkInfo?.networkName === "mainnet",
      isInBootstrapPhase:
        epochParams?.protocol_major === BOOTSTRAPPING_PHASE_MAJOR,
      isFullGovernance: Number(epochParams?.protocol_major) >= 10,
      networkName:
        NETWORK_NAMES[
          (networkInfo?.networkName as keyof typeof NETWORK_NAMES) || "preview"
        ],
      network: networkInfo?.networkName ?? Network.preview,
      cExplorerBaseUrl:
        CEXPLORER_BASE_URLS[
          (networkInfo?.networkName as keyof typeof NETWORK_NAMES) || "preview"
        ],
      epochParams,
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
