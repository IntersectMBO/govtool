import {
  PropsWithChildren,
  useMemo,
  createContext,
  useContext,
  useCallback,
  useEffect,
} from "react";
import { NodeObject } from "jsonld";
import { blake2bHex } from "blakejs";
import * as Sentry from "@sentry/react";

import { CIP_108, GOVERNANCE_ACTION_CONTEXT } from "@/consts";
import { generateJsonld, generateMetadataBody } from "@/utils";

type GovActionMetadata = {
  title: string;
  abstract: string;
  motivation: string;
  rationale: string;
  references: { uri: string; label: string }[];
};

type GovernanceActionContextType = {
  createGovernanceActionJsonLD: (
    govActionMetadata: GovActionMetadata,
  ) => Promise<NodeObject | undefined>;
  createHash: (jsonLD: NodeObject) => Promise<string | undefined>;
} | null;

const GovernanceActionContext =
  createContext<GovernanceActionContextType>(null);

/**
 * Provides governance action context to its children components.
 *
 * @param children - The child components to render.
 */
const GovernanceActionProvider = ({ children }: PropsWithChildren) => {
  /**
   * Creates a JSON-LD representation of a governance action.
   * @param {GovActionMetadata} govActionMetadata - The metadata of the governance action.
   * @returns The JSON-LD representation of the governance action.
   */

  useEffect(() => {
    Sentry.setTag("component_name", "GovernanceActionProvider");
  }, []);

  const createGovernanceActionJsonLD = useCallback(
    async (govActionMetadata: GovActionMetadata) => {
      try {
        const metadataBody = generateMetadataBody({
          data: govActionMetadata,
          acceptedKeys: ["title", "abstract", "motivation", "rationale"],
          standardReference: CIP_108,
        });

        const jsonLD = await generateJsonld(
          metadataBody,
          GOVERNANCE_ACTION_CONTEXT,
        );
        return jsonLD;
      } catch (error) {
        Sentry.captureException(error);
        console.error(error);
      }
    },
    [],
  );

  /**
   * Creates a hash of a JSON-LD object.
   * @param {NodeObject} jsonLD - The JSON-LD object to hash.
   * @returns The hash of the JSON-LD object.
   */
  const createHash = useCallback(async (jsonLD: NodeObject) => {
    try {
      const jsonHash = blake2bHex(
        JSON.stringify(jsonLD, null, 2),
        undefined,
        32,
      );
      return jsonHash;
    } catch (error) {
      Sentry.captureException(error);
      console.error(error);
    }
  }, []);

  const value = useMemo(
    () => ({
      createGovernanceActionJsonLD,
      createHash,
    }),
    [createGovernanceActionJsonLD, createHash],
  );

  return (
    <GovernanceActionContext.Provider value={value}>
      {children}
    </GovernanceActionContext.Provider>
  );
};

/**
 * Custom hook that provides access to the governance actions context.
 * Throws an error if used outside of a GovernanceActionsProvider.
 * @returns The governance actions context.
 */
const useGovernanceActions = () => {
  const context = useContext(GovernanceActionContext);

  if (!context) {
    throw new Error(
      "useGovernanceActions must be used within a GovernanceActionsProvider",
    );
  }

  return context;
};

export { GovernanceActionProvider, useGovernanceActions };
