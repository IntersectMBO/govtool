import { createContext, useContext, PropsWithChildren, useMemo } from "react";
import { HandleObject } from "@/models/adaHandle";
import { adaHandleService } from "@/services/AdaHandle";

type AdaHandleContextType = {
  isValidAdaHandle: (handle: string) => Promise<boolean>;
  getAdaHandleCIP105DRepId: (handle: string) => Promise<string>;
  getAdaHandleCIP129DRepId: (handle: string) => Promise<string>;
  getHandleDetails: (handle: string) => Promise<HandleObject | null>;
};

const AdaHandleContext = createContext<AdaHandleContextType | null>(null);

/**
 * Provides the AdaHandle context to its children components.
 */
export const AdaHandleProvider = ({ children }: PropsWithChildren) => {
  const value = useMemo<AdaHandleContextType>(
    () => ({
      isValidAdaHandle: adaHandleService.isValidAdaHandle,
      getAdaHandleCIP105DRepId: adaHandleService.getAdaHandleCIP105DRepId,
      getAdaHandleCIP129DRepId: adaHandleService.getAdaHandleCIP129DRepId,
      getHandleDetails: adaHandleService.getHandleDetails,
    }),
    [adaHandleService],
  );

  return (
    <AdaHandleContext.Provider value={value}>
      {children}
    </AdaHandleContext.Provider>
  );
};

/**
 * Custom hook that provides access to the AdaHandle context.
 * Throws an error if used outside of an AdaHandleProvider.
 */
export const useAdaHandleContext = () => {
  const context = useContext(AdaHandleContext);

  if (!context) {
    throw new Error(
      "useAdaHandleContext must be used within an AdaHandleProvider",
    );
  }

  return context;
};
