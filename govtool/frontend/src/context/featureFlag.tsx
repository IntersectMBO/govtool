import {
  PropsWithChildren,
  useMemo,
  createContext,
  useContext,
  useCallback,
} from "react";
import type { ControlId, FeatureId, FeatureSet } from "@/models/featureSet";
import { allowedOptions, isAvailable } from "@/models/featureSet";

import {
  DREP_DIRECTORY_SORTING,
  DREP_DIRECTORY_SORT_CAPABILITY_KEYS,
  GOVERNANCE_ACTIONS_SORTING,
  GOVERNANCE_ACTIONS_SORT_CAPABILITY_KEYS,
} from "@consts";
import { GovernanceActionType } from "@/types/governanceAction";
import { env } from "@/config/env";

import type { CapabilitiesStatus } from "./appContext";
import { useAppContext } from "./appContext";

type SortOption = { key: string; label: string };

/**
 * A list control narrowed by what the active provider can actually serve.
 *
 * `isAvailable: false` means HIDE the control. The contract is explicit that an
 * empty allow-list is "hide the control, not render an empty menu" — the real
 * Koios case, where every `DRepSort` key is refused.
 */
export type GatedListControl = {
  /** The options to render, in the UI's own order. */
  options: SortOption[];
  isAvailable: boolean;
  /** True when a persisted or hard-coded selection is no longer offered. */
  isSelectionStale: (selected: string) => boolean;
  /** What to select instead; `null` when nothing is offered at all. */
  fallbackSelection: (preferred: string) => string | null;
};

/**
 * Build a gated control by narrowing the UI's own option list to what the
 * provider honours.
 *
 * `featureSet === undefined` is the FAIL-OPEN path: capabilities are still
 * loading, or the fetch failed. Every option stays and nothing is stale — i.e.
 * exactly the behaviour GovTool had before capabilities existed. That is
 * `allowedOptions`' own contract, so there is no branch for it here.
 */
const buildGatedControl = (
  uiOptions: SortOption[],
  keyMap: Readonly<Record<string, string>>,
  control: ControlId,
  featureSet: FeatureSet | undefined,
): GatedListControl => {
  const allowed = allowedOptions(
    featureSet,
    control,
    uiOptions.map((option) => keyMap[option.key] ?? option.key),
  );
  const options = uiOptions.filter((option) =>
    allowed.includes(keyMap[option.key] ?? option.key),
  );

  return {
    options,
    isAvailable: options.length > 0,
    isSelectionStale: (selected) => {
      // Nothing chosen yet is not a stale choice.
      if (!selected) return false;
      return !options.some((option) => option.key === selected);
    },
    fallbackSelection: (preferred) =>
      options.some((option) => option.key === preferred)
        ? preferred
        : (options[0]?.key ?? null),
  };
};

/**
 * The feature flag context type.
 */
type FeatureFlagContextType = {
  isProposalDiscussionForumEnabled: boolean;
  isGovernanceOutcomesPillarEnabled: boolean;
  isCip179Enabled: boolean;
  /** loading / unavailable / ready — see `CapabilitiesStatus`. */
  capabilitiesStatus: CapabilitiesStatus;
  /**
   * Whether the provider can serve a whole named feature. Fails OPEN: `true`
   * while capabilities are loading and `true` if the fetch failed, so an
   * unreachable `/system/features` never hides a working surface.
   */
  isFeatureAvailable: (feature: FeatureId) => boolean;
  dRepDirectorySort: GatedListControl;
  governanceActionsSort: GatedListControl;
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

/** Fail-open controls: used before capabilities load and when they cannot be loaded. */
const UNGATED_DREP_DIRECTORY_SORT = buildGatedControl(
  DREP_DIRECTORY_SORTING,
  DREP_DIRECTORY_SORT_CAPABILITY_KEYS,
  "drepDirectory.sort",
  undefined,
);

const UNGATED_GOVERNANCE_ACTIONS_SORT = buildGatedControl(
  GOVERNANCE_ACTIONS_SORTING,
  GOVERNANCE_ACTIONS_SORT_CAPABILITY_KEYS,
  "govActionList.sort",
  undefined,
);

const FeatureFlagContext = createContext<FeatureFlagContextType>({
  isProposalDiscussionForumEnabled: false,
  isGovernanceOutcomesPillarEnabled: false,
  isCip179Enabled: false,
  // Capability defaults are open, not closed: a missing provider must not hide
  // UI that works.
  capabilitiesStatus: "loading",
  isFeatureAvailable: () => true,
  dRepDirectorySort: UNGATED_DREP_DIRECTORY_SORT,
  governanceActionsSort: UNGATED_GOVERNANCE_ACTIONS_SORT,
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
  const {
    isAppInitializing,
    isInBootstrapPhase,
    isFullGovernance,
    featureSet,
    capabilitiesStatus,
  } = useAppContext();

  // Only a `ready` status gates anything. `loading` and `unavailable` both hand
  // the controls a null state, i.e. the full pre-capability UI.
  const activeFeatureSet =
    capabilitiesStatus === "ready" ? featureSet : undefined;

  const isFeatureAvailable = useCallback(
    (feature: FeatureId) => isAvailable(activeFeatureSet, feature),
    [activeFeatureSet],
  );

  const dRepDirectorySort = useMemo(
    () =>
      buildGatedControl(
        DREP_DIRECTORY_SORTING,
        DREP_DIRECTORY_SORT_CAPABILITY_KEYS,
        "drepDirectory.sort",
        activeFeatureSet,
      ),
    [activeFeatureSet],
  );

  const governanceActionsSort = useMemo(
    () =>
      buildGatedControl(
        GOVERNANCE_ACTIONS_SORTING,
        GOVERNANCE_ACTIONS_SORT_CAPABILITY_KEYS,
        "govActionList.sort",
        activeFeatureSet,
      ),
    [activeFeatureSet],
  );

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

      return true;
    },
    [isAppInitializing, isInBootstrapPhase],
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
        env.VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED === "true" ||
        env.VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED === true ||
        false,
      isGovernanceOutcomesPillarEnabled:
        env.VITE_IS_GOVERNANCE_OUTCOMES_PILLAR_ENABLED === "true" ||
        env.VITE_IS_GOVERNANCE_OUTCOMES_PILLAR_ENABLED === true ||
        false,
      isCip179Enabled:
        env.VITE_IS_CIP179_ENABLED === "true" ||
        env.VITE_IS_CIP179_ENABLED === true ||
        false,
      isVotingOnGovernanceActionEnabled,
      areDRepVoteTotalsDisplayed,
      areSPOVoteTotalsDisplayed,
      areCCVoteTotalsDisplayed,
      capabilitiesStatus,
      isFeatureAvailable,
      dRepDirectorySort,
      governanceActionsSort,
    }),
    [
      isVotingOnGovernanceActionEnabled,
      capabilitiesStatus,
      isFeatureAvailable,
      dRepDirectorySort,
      governanceActionsSort,
    ],
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
