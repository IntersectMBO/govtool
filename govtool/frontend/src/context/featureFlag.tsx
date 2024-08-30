import {
  PropsWithChildren,
  useMemo,
  createContext,
  useContext,
  useCallback,
} from "react";

import { GovernanceActionType } from "@/types/governanceAction";

import { useAppContext } from "./appContext";

const govActionVotingEnabledSinceProtocolVersion = {
  [GovernanceActionType.InfoAction]: 9,
  // TODO: Add minimum protocol versions for the following actions
  [GovernanceActionType.HardForkInitiation]: Number.MAX_SAFE_INTEGER,
  [GovernanceActionType.NewCommittee]: Number.MAX_SAFE_INTEGER,
  [GovernanceActionType.NewConstitution]: Number.MAX_SAFE_INTEGER,
  [GovernanceActionType.NoConfidence]: Number.MAX_SAFE_INTEGER,
  [GovernanceActionType.ParameterChange]: Number.MAX_SAFE_INTEGER,
  [GovernanceActionType.TreasuryWithdrawals]: Number.MAX_SAFE_INTEGER,
};
/**
 * The feature flag context type.
 */
type FeatureFlagContextType = {
  isProposalDiscussionForumEnabled: boolean;
  isVotingOnGovernanceActionEnabled: (
    governanceActionType: GovernanceActionType,
  ) => boolean;
};

const FeatureFlagContext = createContext<FeatureFlagContextType>({
  isProposalDiscussionForumEnabled: false,
  isVotingOnGovernanceActionEnabled: () => false,
});

/**
 * Provides feature flag context to its children components.
 *
 * @param children - The child components to render.
 */
const FeatureFlagProvider = ({ children }: PropsWithChildren) => {
  const { epochParams, isAppInitializing } = useAppContext();

  /**
   * Determines if voting on a governance action is enabled based on the protocol version.
   * @param governanceActionType - The type of governance action.
   * @returns A boolean indicating whether voting is enabled for the specified governance action.
   */
  const isVotingOnGovernanceActionEnabled = useCallback(
    (governanceActionType: GovernanceActionType) =>
      (epochParams?.protocol_major || 0) >=
      govActionVotingEnabledSinceProtocolVersion[governanceActionType],
    [isAppInitializing],
  );

  const value = useMemo(
    () => ({
      isProposalDiscussionForumEnabled:
        import.meta.env.VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED === "true" ||
        false,
      isVotingOnGovernanceActionEnabled,
    }),
    [isVotingOnGovernanceActionEnabled],
  );

  return (
    <FeatureFlagContext.Provider value={value}>
      {children}
    </FeatureFlagContext.Provider>
  );
};

/**
 * Custom hook that provides access to the feature flag context.
 * Throws an error if used outside of a FeatureFlagProvider.
 * @returns The feature flag context.
 */
const useFeatureFlag = () => {
  const context = useContext(FeatureFlagContext);

  if (!context) {
    throw new Error("useFeatureFlag must be used within a FeatureFlagProvider");
  }

  return context;
};

export { FeatureFlagProvider, useFeatureFlag };
