import {
  createContext,
  useContext,
  useMemo,
  useCallback,
  PropsWithChildren,
} from "react";

import { MetadataService } from "@/services";
import { MetadataConfig } from "@/types";

type MetadataContextValues = {
  validate: (data: MetadataConfig) => void;
  build: (config: MetadataConfig) => Promise<MetadataService>;
};

const MetadataContext = createContext<MetadataContextValues | null>(null);

/**
 * Provides metadata validation and building functionality to its children components.
 * @param children - The child components to be wrapped by the MetadataProvider.
 */
export const MetadataProvider = ({ children }: PropsWithChildren) => {
  /**
   * Validates the metadata configuration.
   *
   * @param data - The metadata configuration to validate.
   * @returns A promise that resolves to the validation result.
   */
  const validate = useCallback(
    (data: MetadataConfig) => new MetadataService(data).validateMetadata(),
    []
  );

  /**
   * Builds the metadata using the provided configuration.
   * @param config The configuration for building the metadata.
   * @returns A promise that resolves to the built metadata.
   */
  const build = useCallback(
    (config: MetadataConfig) => new MetadataService(config).build(),
    []
  );

  const value = useMemo(() => ({ validate, build }), [validate, build]);

  return (
    <MetadataContext.Provider value={value}>
      {children}
    </MetadataContext.Provider>
  );
};

/**
 * Custom hook that provides access to the metadata context.
 * @returns The metadata context.
 * @throws {Error} If used outside of a MetadataProvider.
 */
export const useMetadata = () => {
  const context = useContext(MetadataContext);
  if (!context) {
    throw new Error("useMetadata must be used within a MetadataProvider");
  }
  return context;
};
