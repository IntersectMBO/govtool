import { describe, it, expect, vi, beforeEach, MockedFunction } from "vitest";
import { renderHook } from "@testing-library/react";
import type { FeatureId, FeatureSet } from "@/models/featureSet";

import {
  DREP_DIRECTORY_SORTING,
  GOVERNANCE_ACTIONS_SORTING,
} from "@consts";

import { FeatureFlagProvider, useFeatureFlag } from "./featureFlag";
import { GovernanceActionType } from "@/types/governanceAction";
import { useAppContext } from "./appContext";
import { Network } from "@/models";
import { env } from "@/config/env";

vi.mock("./appContext");

const mockUseAppContext = useAppContext as MockedFunction<typeof useAppContext>;

const mockUseAppContextReturnValue = {
  cExplorerBaseUrl: "http://mock.cexplorer",
  isAppInitializing: false,
  isInBootstrapPhase: false,
  isFullGovernance: true,
  network: Network.preview,
  networkName: "preview",
  isMainnet: false,
  capabilitiesStatus: "loading" as const,
};

describe("FeatureFlagProvider", () => {
  beforeEach(() => {
    vi.resetAllMocks();
    mockUseAppContext.mockReturnValue(mockUseAppContextReturnValue);
  });

  it("should enable proposal discussion forum based on environment variable", () => {
    env.VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED = "true";

    const { result } = renderHook(() => useFeatureFlag(), {
      wrapper: FeatureFlagProvider,
    });

    expect(result.current.isProposalDiscussionForumEnabled).toBe(true);
  });

  it("should disable proposal discussion forum if environment variable is false", () => {
    env.VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED = "false";

    const { result } = renderHook(() => useFeatureFlag(), {
      wrapper: FeatureFlagProvider,
    });

    expect(result.current.isProposalDiscussionForumEnabled).toBe(false);
  });

  describe("isVotingOnGovernanceActionEnabled", () => {
    it("should return true for InfoAction regardless of bootstrap phase", () => {
      mockUseAppContext.mockReturnValue({
        ...mockUseAppContextReturnValue,
        isAppInitializing: false,
        isInBootstrapPhase: true,
        isFullGovernance: false,
      });

      const { result } = renderHook(() => useFeatureFlag(), {
        wrapper: FeatureFlagProvider,
      });

      expect(
        result.current.isVotingOnGovernanceActionEnabled(
          GovernanceActionType.InfoAction,
        ),
      ).toBe(true);
    });

    it("should return false for other actions in bootstrap phase", () => {
      mockUseAppContext.mockReturnValue({
        ...mockUseAppContextReturnValue,
        isAppInitializing: false,
        isInBootstrapPhase: true,
        isFullGovernance: false,
      });

      const { result } = renderHook(() => useFeatureFlag(), {
        wrapper: FeatureFlagProvider,
      });

      expect(
        result.current.isVotingOnGovernanceActionEnabled(
          GovernanceActionType.ParameterChange,
        ),
      ).toBe(false);
    });
  });

  describe("areDRepVoteTotalsDisplayed", () => {
    it("should hide DRep vote totals for HardForkInitiation in bootstrap phase", () => {
      mockUseAppContext.mockReturnValue({
        ...mockUseAppContextReturnValue,
        isAppInitializing: false,
        isInBootstrapPhase: true,
        isFullGovernance: false,
      });

      const { result } = renderHook(() => useFeatureFlag(), {
        wrapper: FeatureFlagProvider,
      });

      expect(
        result.current.areDRepVoteTotalsDisplayed(
          GovernanceActionType.HardForkInitiation,
        ),
      ).toBe(false);
    });

    it("should display DRep vote totals for ParameterChange when isSecurityGroup is true in bootstrap phase", () => {
      mockUseAppContext.mockReturnValue({
        ...mockUseAppContextReturnValue,
        isAppInitializing: false,
        isInBootstrapPhase: true,
        isFullGovernance: false,
      });

      const { result } = renderHook(() => useFeatureFlag(), {
        wrapper: FeatureFlagProvider,
      });

      expect(
        result.current.areDRepVoteTotalsDisplayed(
          GovernanceActionType.ParameterChange,
          true, // isSecurityGroup
        ),
      ).toBe(true);
    });

    it("should show DRep vote totals for MotionNoConfidence in full governance", () => {
      mockUseAppContext.mockReturnValue({
        ...mockUseAppContextReturnValue,
        isAppInitializing: false,
        isInBootstrapPhase: false,
        isFullGovernance: true,
      });

      const { result } = renderHook(() => useFeatureFlag(), {
        wrapper: FeatureFlagProvider,
      });

      expect(
        result.current.areDRepVoteTotalsDisplayed(
          GovernanceActionType.NoConfidence,
        ),
      ).toBe(true);
    });
  });

  describe("areSPOVoteTotalsDisplayed", () => {
    it("should hide SPO vote totals for ParameterChange in bootstrap phase", () => {
      mockUseAppContext.mockReturnValue({
        ...mockUseAppContextReturnValue,
        isAppInitializing: false,
        isInBootstrapPhase: true,
        isFullGovernance: false,
      });

      const { result } = renderHook(() => useFeatureFlag(), {
        wrapper: FeatureFlagProvider,
      });

      expect(
        result.current.areSPOVoteTotalsDisplayed(
          GovernanceActionType.ParameterChange,
          false,
        ),
      ).toBe(false);
    });

    it("should display SPO vote totals for ParameterChange when isSecurityGroup is true in bootstrap phase", () => {
      mockUseAppContext.mockReturnValue({
        ...mockUseAppContextReturnValue,
        isAppInitializing: false,
        isInBootstrapPhase: true,
        isFullGovernance: false,
      });

      const { result } = renderHook(() => useFeatureFlag(), {
        wrapper: FeatureFlagProvider,
      });

      expect(
        result.current.areSPOVoteTotalsDisplayed(
          GovernanceActionType.ParameterChange,
          true,
        ),
      ).toBe(false);
    });

    it("should hide SPO vote totals for TreasuryWithdrawals in full governance", () => {
      mockUseAppContext.mockReturnValue({
        ...mockUseAppContextReturnValue,
        isAppInitializing: false,
        isInBootstrapPhase: false,
        isFullGovernance: true,
      });

      const { result } = renderHook(() => useFeatureFlag(), {
        wrapper: FeatureFlagProvider,
      });

      expect(
        result.current.areSPOVoteTotalsDisplayed(
          GovernanceActionType.TreasuryWithdrawals,
          true,
        ),
      ).toBe(false);
    });
  });

  describe("areCCVoteTotalsDisplayed", () => {
    it("should hide CC vote totals for MotionNoConfidence in full governance", () => {
      mockUseAppContext.mockReturnValue({
        ...mockUseAppContextReturnValue,
        isAppInitializing: false,
        isInBootstrapPhase: false,
        isFullGovernance: true,
      });

      const { result } = renderHook(() => useFeatureFlag(), {
        wrapper: FeatureFlagProvider,
      });

      expect(
        result.current.areCCVoteTotalsDisplayed(
          GovernanceActionType.NoConfidence,
        ),
      ).toBe(false);
    });

    it("should show CC vote totals for other actions in bootstrap phase", () => {
      mockUseAppContext.mockReturnValue({
        ...mockUseAppContextReturnValue,
        isAppInitializing: false,
        isInBootstrapPhase: true,
        isFullGovernance: false,
      });

      const { result } = renderHook(() => useFeatureFlag(), {
        wrapper: FeatureFlagProvider,
      });

      expect(
        result.current.areCCVoteTotalsDisplayed(
          GovernanceActionType.HardForkInitiation,
        ),
      ).toBe(true);
    });
  });
});

/* ------------------------------------------------------------------------- */
/* Capability-derived flags                                                   */
/* ------------------------------------------------------------------------- */

/**
 * A feature set is declare-by-exception, so the base is "everything works" and
 * a test names only what it wants taken away.
 */
const makeFeatureSet = (overrides: Partial<FeatureSet> = {}): FeatureSet => ({
  provider: "koios",
  network: "preview",
  generatedAt: "2026-01-01T00:00:00Z",
  unavailable: {},
  options: {},
  caveats: [],
  ...overrides,
});

const unavailable = (
  ...features: FeatureId[]
): Pick<FeatureSet, "unavailable"> => ({
  unavailable: Object.fromEntries(
    features.map((feature) => [
      feature,
      { cause: "notInSource" as const, reason: "test" },
    ]),
  ),
});

const withCapabilities = (featureSet: FeatureSet) => {
  mockUseAppContext.mockReturnValue({
    ...mockUseAppContextReturnValue,
    capabilitiesStatus: "ready",
    featureSet,
  });
};

describe("capability-derived feature flags", () => {
  beforeEach(() => {
    vi.resetAllMocks();
    mockUseAppContext.mockReturnValue(mockUseAppContextReturnValue);
  });

  it("fails open while capabilities are still loading", () => {
    const { result } = renderHook(() => useFeatureFlag(), {
      wrapper: FeatureFlagProvider,
    });

    expect(result.current.capabilitiesStatus).toBe("loading");
    expect(result.current.dRepDirectorySort.options).toHaveLength(
      DREP_DIRECTORY_SORTING.length,
    );
    expect(result.current.dRepDirectorySort.isAvailable).toBe(true);
    expect(result.current.governanceActionsSort.options).toHaveLength(
      GOVERNANCE_ACTIONS_SORTING.length,
    );
    expect(result.current.isFeatureAvailable("drep.delegatorList")).toBe(
      true,
    );
  });

  it("fails open when the capability fetch failed", () => {
    mockUseAppContext.mockReturnValue({
      ...mockUseAppContextReturnValue,
      capabilitiesStatus: "unavailable",
    });

    const { result } = renderHook(() => useFeatureFlag(), {
      wrapper: FeatureFlagProvider,
    });

    expect(result.current.capabilitiesStatus).toBe("unavailable");
    expect(result.current.dRepDirectorySort.options).toHaveLength(
      DREP_DIRECTORY_SORTING.length,
    );
    expect(result.current.dRepDirectorySort.isSelectionStale("Activity")).toBe(
      false,
    );
    expect(result.current.isFeatureAvailable("dashboard.committeeThreshold")).toBe(
      true,
    );
  });

  it("ignores a feature set that arrived but is not yet marked ready", () => {
    mockUseAppContext.mockReturnValue({
      ...mockUseAppContextReturnValue,
      capabilitiesStatus: "loading",
      featureSet: makeFeatureSet({ options: { "drepDirectory.sort": [] } }),
    });

    const { result } = renderHook(() => useFeatureFlag(), {
      wrapper: FeatureFlagProvider,
    });

    expect(result.current.dRepDirectorySort.isAvailable).toBe(true);
  });

  describe("DRep directory sort", () => {
    it("hides the control when the provider can order by nothing", () => {
      withCapabilities(
        makeFeatureSet({ options: { "drepDirectory.sort": [] } }),
      );

      const { result } = renderHook(() => useFeatureFlag(), {
        wrapper: FeatureFlagProvider,
      });

      expect(result.current.dRepDirectorySort.options).toEqual([]);
      expect(result.current.dRepDirectorySort.isAvailable).toBe(false);
      expect(result.current.dRepDirectorySort.fallbackSelection("Activity")).toBe(
        null,
      );
    });

    it("keeps only the allowed keys, in the UI's own order", () => {
      withCapabilities(
        makeFeatureSet({ options: { "drepDirectory.sort": ["votingPower", "random"] } }),
      );

      const { result } = renderHook(() => useFeatureFlag(), {
        wrapper: FeatureFlagProvider,
      });

      expect(
        result.current.dRepDirectorySort.options.map(({ key }) => key),
      ).toEqual(["VotingPower", "Random"]);
    });

    it("reports GovTool's forced `Activity` default as stale and falls back", () => {
      withCapabilities(
        makeFeatureSet({ options: { "drepDirectory.sort": ["votingPower", "random"] } }),
      );

      const { result } = renderHook(() => useFeatureFlag(), {
        wrapper: FeatureFlagProvider,
      });

      expect(result.current.dRepDirectorySort.isSelectionStale("Activity")).toBe(
        true,
      );
      expect(
        result.current.dRepDirectorySort.fallbackSelection("Activity"),
      ).toBe("VotingPower");
      expect(
        result.current.dRepDirectorySort.isSelectionStale("VotingPower"),
      ).toBe(false);
      expect(
        result.current.dRepDirectorySort.fallbackSelection("VotingPower"),
      ).toBe("VotingPower");
    });

    it("treats an empty selection as not stale", () => {
      withCapabilities(
        makeFeatureSet({ options: { "drepDirectory.sort": ["votingPower"] } }),
      );

      const { result } = renderHook(() => useFeatureFlag(), {
        wrapper: FeatureFlagProvider,
      });

      expect(result.current.dRepDirectorySort.isSelectionStale("")).toBe(false);
    });
  });

  describe("governance action sort", () => {
    it("drops MostYesVotes when the provider refuses it", () => {
      withCapabilities(
        makeFeatureSet({ options: { "govActionList.sort": [
            "soonestToExpire",
            "newest",
          ] } }),
      );

      const { result } = renderHook(() => useFeatureFlag(), {
        wrapper: FeatureFlagProvider,
      });

      expect(
        result.current.governanceActionsSort.options.map(({ key }) => key),
      ).toEqual(["SoonestToExpire", "NewestCreated"]);
      expect(result.current.governanceActionsSort.isAvailable).toBe(true);
      expect(
        result.current.governanceActionsSort.isSelectionStale("MostYesVotes"),
      ).toBe(true);
      // No `defaultTo` declared: fall back to the first option still offered.
      expect(
        result.current.governanceActionsSort.fallbackSelection("MostYesVotes"),
      ).toBe("SoonestToExpire");
    });

    it("keeps a selection that is still offered", () => {
      withCapabilities(
        makeFeatureSet({
          options: { "govActionList.sort": ["soonestToExpire", "newest"] },
        }),
      );

      const { result } = renderHook(() => useFeatureFlag(), {
        wrapper: FeatureFlagProvider,
      });

      expect(
        result.current.governanceActionsSort.isSelectionStale("NewestCreated"),
      ).toBe(false);
      expect(
        result.current.governanceActionsSort.fallbackSelection("NewestCreated"),
      ).toBe("NewestCreated");
    });
  });

  describe("whole-feature gate", () => {
    it("reports a refused feature as unavailable", () => {
      withCapabilities(
        makeFeatureSet(unavailable("dashboard.committeeThreshold")),
      );

      const { result } = renderHook(() => useFeatureFlag(), {
        wrapper: FeatureFlagProvider,
      });

      expect(
        result.current.isFeatureAvailable("dashboard.committeeThreshold"),
      ).toBe(false);
    });

    it("reports an unlisted feature as available", () => {
      // The feature set is declare-by-exception: a feature the provider did
      // not name works. Refusing another one must not take this one with it.
      withCapabilities(makeFeatureSet(unavailable("network.treasury")));

      const { result } = renderHook(() => useFeatureFlag(), {
        wrapper: FeatureFlagProvider,
      });

      expect(
        result.current.isFeatureAvailable("dashboard.committeeThreshold"),
      ).toBe(true);
    });

    // The delegation surface `DashboardCards` gates.
    it("hides the delegation card's feature when the provider refuses it", () => {
      withCapabilities(makeFeatureSet(unavailable("account.currentDelegation")));

      const { result } = renderHook(() => useFeatureFlag(), {
        wrapper: FeatureFlagProvider,
      });

      expect(result.current.isFeatureAvailable("account.currentDelegation")).toBe(
        false,
      );
    });

    it("keeps the delegation card when the fetch failed", () => {
      mockUseAppContext.mockReturnValue({
        ...mockUseAppContextReturnValue,
        capabilitiesStatus: "unavailable",
      });

      const { result } = renderHook(() => useFeatureFlag(), {
        wrapper: FeatureFlagProvider,
      });

      expect(result.current.isFeatureAvailable("account.currentDelegation")).toBe(
        true,
      );
    });
  });
});
