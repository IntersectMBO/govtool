import { describe, it, expect, vi, beforeEach, MockedFunction } from "vitest";
import { renderHook } from "@testing-library/react";
import { FeatureFlagProvider, useFeatureFlag } from "./featureFlag";
import { GovernanceActionType } from "@/types/governanceAction";
import { useAppContext } from "./appContext";
import { Network } from "@/models";

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
};

describe("FeatureFlagProvider", () => {
  beforeEach(() => {
    vi.resetAllMocks();
    mockUseAppContext.mockReturnValue(mockUseAppContextReturnValue);
  });

  it("should enable proposal discussion forum based on environment variable", () => {
    import.meta.env.VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED = "true";

    const { result } = renderHook(() => useFeatureFlag(), {
      wrapper: FeatureFlagProvider,
    });

    expect(result.current.isProposalDiscussionForumEnabled).toBe(true);
  });

  it("should disable proposal discussion forum if environment variable is false", () => {
    import.meta.env.VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED = "false";

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
