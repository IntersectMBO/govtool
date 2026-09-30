import { useCallback, useMemo } from "react";
import { useQueries } from "@tanstack/react-query";

import { QUERY_KEYS } from "@consts";
import { OutcomeGovernanceAction } from "@models";
import { getOutcomeEpochParams, getOutcomeNetworkMetrics } from "@services";
import { GovernanceActionType } from "@/types/governanceAction";

const BOOTSTRAPPING_PHASE_MAJOR = 9;

/**
 * Network metrics and protocol parameters as of the epoch the action was
 * decided in (current ones while it is live), and which voter groups vote on
 * it in that governance phase.
 */
export const useGetOutcomeNetworkMetrics = (
  action?: OutcomeGovernanceAction,
) => {
  const metricsEpoch = useMemo(
    () =>
      action?.status?.ratified_epoch ||
      action?.status?.expired_epoch ||
      action?.status?.dropped_epoch ||
      null,
    [action],
  );

  const [metricsQuery, epochParamsQuery] = useQueries({
    queries: [
      {
        queryKey: [QUERY_KEYS.useGetOutcomeNetworkMetricsKey, metricsEpoch],
        queryFn: () => getOutcomeNetworkMetrics(metricsEpoch ?? undefined),
        enabled: !!action,
      },
      {
        queryKey: [QUERY_KEYS.useGetOutcomeEpochParamsKey, metricsEpoch],
        queryFn: () => getOutcomeEpochParams(metricsEpoch ?? undefined),
        enabled: !!action,
      },
    ],
  });

  const networkMetrics = metricsQuery.data;
  const epochParams = epochParamsQuery.data;

  const isInBootstrapPhase =
    epochParams?.protocol_major === BOOTSTRAPPING_PHASE_MAJOR;
  const isFullGovernance = Number(epochParams?.protocol_major) >= 10;

  const areDRepVoteTotalsDisplayed = useCallback(
    (governanceActionType: GovernanceActionType, isSecurityGroup = false) => {
      if (isInBootstrapPhase) {
        return !(
          governanceActionType === GovernanceActionType.HardForkInitiation ||
          (governanceActionType === GovernanceActionType.ParameterChange &&
            !isSecurityGroup)
        );
      }
      return true;
    },
    [isInBootstrapPhase],
  );

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
    [isInBootstrapPhase, isFullGovernance],
  );

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
    [isFullGovernance],
  );

  return {
    networkMetrics: networkMetrics ?? null,
    epochParams: epochParams ?? null,
    isLoading: metricsQuery.isLoading || epochParamsQuery.isLoading,
    error: metricsQuery.error || epochParamsQuery.error,
    metricsEpoch,
    isInBootstrapPhase,
    isFullGovernance,
    areDRepVoteTotalsDisplayed,
    areSPOVoteTotalsDisplayed,
    areCCVoteTotalsDisplayed,
  };
};
