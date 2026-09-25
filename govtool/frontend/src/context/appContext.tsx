import {
  createContext,
  PropsWithChildren,
  useContext,
  useEffect,
  useMemo,
  useState,
} from "react";
import * as Sentry from "@sentry/react";

import type { FeatureSet } from "@/models/featureSet";

import { NETWORK_NAMES, CEXPLORER_BASE_URLS } from "@/consts";
import {
  useGetEpochParams,
  useGetNetworkInfo,
  useGetSystemFeatures,
} from "@/hooks";
import {
  NETWORK_INFO_KEY,
  PROTOCOL_PARAMS_KEY,
  setItemToLocalStorage,
} from "@/utils";
import { EpochParams, Network } from "@/models";
import { adaHandleService } from "@/services/AdaHandle";

const BOOTSTRAPPING_PHASE_MAJOR = 9;

/**
 * Three distinct states, deliberately not a boolean:
 *
 *   loading      the bootstrap fetch has not resolved yet.
 *   unavailable  the fetch failed, or the backend does not serve
 *                `/system/features` at all. Gates FAIL OPEN here — the full UI,
 *                exactly as it behaved before capabilities existed.
 *   ready        a `FeatureSet` is in hand and gates apply.
 *
 * Collapsing `unavailable` into `ready` would turn a transient network error
 * into a permanently feature-less app; collapsing it into `loading` would hang
 * every gated control on a spinner forever.
 */
export type CapabilitiesStatus = "loading" | "unavailable" | "ready";

type AppContextType = {
  isAppInitializing: boolean;
  /** Provider capabilities, derived by the backend. `undefined` unless ready. */
  featureSet?: FeatureSet;
  capabilitiesStatus: CapabilitiesStatus;
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
  const { fetchEpochParams, epochParams } = useGetEpochParams();
  const { fetchNetworkInfo, networkInfo } = useGetNetworkInfo();
  const { fetchSystemFeatures } = useGetSystemFeatures();

  const [isAppInitializing, setIsAppInitializing] = useState(true);
  const [featureSet, setFeatureSet] = useState<FeatureSet | undefined>();
  const [capabilitiesStatus, setCapabilitiesStatus] =
    useState<CapabilitiesStatus>("loading");

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

        // Capabilities are advisory: a backend that does not serve them (or a
        // transient failure) must leave the app fully usable, so this is its
        // own try/catch rather than part of the bootstrap's failure path.
        try {
          const { data: featureSetData } = await fetchSystemFeatures();
          if (featureSetData) {
            setFeatureSet(featureSetData);
            setCapabilitiesStatus("ready");
          } else {
            setCapabilitiesStatus("unavailable");
          }
        } catch (capabilitiesError) {
          Sentry.captureException(capabilitiesError);
          setCapabilitiesStatus("unavailable");
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
      featureSet,
      capabilitiesStatus,
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
    [isAppInitializing, capabilitiesStatus, featureSet],
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
