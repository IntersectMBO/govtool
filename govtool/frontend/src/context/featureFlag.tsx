import {
  PropsWithChildren,
  useMemo,
  createContext,
  useContext,
  useCallback,
} from "react";

import { GovernanceActionType } from "@/types/governanceAction";

import { useAppContext } from "./appContext";

/**
 * The feature flag context type.
 */
type FeatureFlagContextType = {
  isProposalDiscussionForumEnabled: boolean;
  isVotingOnGovernanceActionEnabled: (
    governanceActionType: GovernanceActionType,
  ) => boolean;
  areDRepVoteTotalsDisplayed: (
    governanceActionType: GovernanceActionType,
    isSecurityGroup?: boolean,
  ) => boolean;
  areSPOVoteTotalsDisplayed: (
    governanceActionType: GovernanceActionType,
    isSecurityGroup: boolean,
  ) => boolean;
  areCCVoteTotalsDisplayed: (
    governanceActionType: GovernanceActionType,
  ) => boolean;
};

const FeatureFlagContext = createContext<FeatureFlagContextType>({
  isProposalDiscussionForumEnabled: false,
  isVotingOnGovernanceActionEnabled: () => false,
  areDRepVoteTotalsDisplayed: () => false,
  areSPOVoteTotalsDisplayed: () => false,
  areCCVoteTotalsDisplayed: () => false,
});

/**
 * Provides feature flag context to its children components.
 *
 * @param children - The child components to render.
 */
const FeatureFlagProvider = ({ children }: PropsWithChildren) => {
  const { isAppInitializing, isInBootstrapPhase, isFullGovernance } =
    useAppContext();

  /**
   * Determines if voting on a governance action is enabled based on the protocol version.
   * @param governanceActionType - The type of governance action.
   * @returns A boolean indicating whether voting is enabled for the specified governance action.
   */
  const isVotingOnGovernanceActionEnabled = useCallback(
    (governanceActionType: GovernanceActionType) =>
      governanceActionType === GovernanceActionType.InfoAction ||
      !isInBootstrapPhase,
    [isAppInitializing, isInBootstrapPhase],
  );

  /**
   * Determines if DRep vote totals should be displayed based on governance action type and phase.
   * @param governanceActionType - The type of governance action.
   * @returns {boolean} Whether DRep vote totals are displayed.
   */
  const areDRepVoteTotalsDisplayed = useCallback(
    (
      governanceActionType: GovernanceActionType,
      isSecurityGroup: boolean = false,
    ) => {
      if (isInBootstrapPhase) {
        return !(
          governanceActionType === GovernanceActionType.HardForkInitiation ||
          (governanceActionType === GovernanceActionType.ParameterChange &&
            !isSecurityGroup)
        );
      }
      if (isFullGovernance) {
        return ![
          GovernanceActionType.NoConfidence,
          GovernanceActionType.NewCommittee,
          GovernanceActionType.NewConstitution,
        ].includes(governanceActionType);
      }
      return true;
    },
    [isAppInitializing, isInBootstrapPhase, isFullGovernance],
  );

  /**
   * Determines if SPO vote totals should be displayed based on governance action type and phase.
   * @param governanceActionType - The type of governance action.
   * @returns {boolean} Whether SPO vote totals are displayed.
   */
  const areSPOVoteTotalsDisplayed = useCallback(
    (governanceActionType: GovernanceActionType, isSecurityGroup: boolean) => {
      if (isInBootstrapPhase) {
        return governanceActionType !== GovernanceActionType.ParameterChange;
      }
      if (isFullGovernance) {
        return !(
          governanceActionType === GovernanceActionType.NewConstitution ||
          governanceActionType === GovernanceActionType.TreasuryWithdrawals ||
          (governanceActionType === GovernanceActionType.ParameterChange &&
            !isSecurityGroup)
        );
      }
      return true;
    },
    [isAppInitializing, isInBootstrapPhase, isFullGovernance],
  );

  /**
   * Determines if CC vote totals should be displayed based on governance action type and phase.
   * @param governanceActionType - The type of governance action.
   * @returns {boolean} Whether CC vote totals are displayed.
   */
  const areCCVoteTotalsDisplayed = useCallback(
    (governanceActionType: GovernanceActionType) => {
      if (isFullGovernance) {
        return ![
          GovernanceActionType.NoConfidence,
          GovernanceActionType.NewCommittee,
        ].includes(governanceActionType);
      }
      return true;
    },
    [isAppInitializing, isFullGovernance],
  );
  const value = useMemo(
    () => ({
      isProposalDiscussionForumEnabled:
        import.meta.env.VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED === "true" ||
        false,
      isVotingOnGovernanceActionEnabled,
      areDRepVoteTotalsDisplayed,
      areSPOVoteTotalsDisplayed,
      areCCVoteTotalsDisplayed,
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
