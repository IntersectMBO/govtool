import { Box, Divider } from "@mui/material";

import { Typography } from "@atoms";
import { SECURITY_RELEVANT_PARAMS_MAP } from "@consts";
import { useGetOutcomeNetworkMetrics, useTranslation } from "@hooks";
import { EpochParams, OutcomeGovernanceAction } from "@models";
import {
  OutcomeIndicator,
  OutcomeStatusChip,
  OutcomeVoteSection,
  VoterType,
} from "@molecules";
import { getGovActionVotingThreshold } from "@utils";
import { GovernanceActionType } from "@/types/governanceAction";

type OutcomeGovernanceVotingProps = {
  action: OutcomeGovernanceAction;
};

/**
 * The tally of each voter group and whether it passed, computed as the
 * ledger ratifies: always-abstain stake leaves the denominator, and
 * always-no-confidence stake counts as yes on a NoConfidence action and as
 * no on every other.
 */
export const OutcomeGovernanceVoting = ({
  action,
}: OutcomeGovernanceVotingProps) => {
  const { t } = useTranslation();
  const {
    networkMetrics,
    epochParams,
    isLoading,
    areDRepVoteTotalsDisplayed,
    areSPOVoteTotalsDisplayed,
    areCCVoteTotalsDisplayed,
  } = useGetOutcomeNetworkMetrics(action);

  const { proposal_params: proposalParams, status } = action;
  const type = action.type as GovernanceActionType;
  const isNoConfidence = type === GovernanceActionType.NoConfidence;
  const isHardFork = type === GovernanceActionType.HardForkInitiation;
  const isDataReady = !isLoading && !!networkMetrics;

  const isSecurityGroup = Object.values(SECURITY_RELEVANT_PARAMS_MAP).some(
    (paramKey) => proposalParams?.[paramKey as keyof EpochParams] !== null,
  );

  const getThreshold = (voterType: VoterType) =>
    getGovActionVotingThreshold({
      govActionType: type,
      protocolParams: proposalParams,
      voterType,
      epochParams,
    });

  const getStatus = () => {
    if (status.enacted_epoch) return t("outcome.status.enacted");
    if (status.ratified_epoch) return t("outcome.status.ratified");
    if (status.expired_epoch) return t("outcome.status.expired");
    if (status.dropped_epoch) return t("outcome.status.dropped");
    return t("outcome.status.inProgress");
  };

  // Network metrics
  const alwaysAbstain = Number(networkMetrics?.always_abstain_voting_power);
  const alwaysAbstainForSPOs = isHardFork
    ? 0
    : Number(networkMetrics?.spos_abstain_voting_power);
  const noConfidence = Number(
    networkMetrics?.always_no_confidence_voting_power,
  );
  const noConfidenceForSPOs = isHardFork
    ? 0
    : Number(networkMetrics?.spos_no_confidence_voting_power);
  const dRepsStake = Number(
    networkMetrics?.total_stake_controlled_by_active_dreps,
  );
  const sPOsStake = Number(
    networkMetrics?.total_stake_controlled_by_stake_pools,
  );
  const committeeMembers = Number(networkMetrics?.no_of_committee_members);
  const ccThreshold = Number(
    (networkMetrics?.quorum_denominator
      ? Number(networkMetrics.quorum_numerator) /
        Number(networkMetrics.quorum_denominator)
      : 0
    ).toPrecision(2),
  );

  // DReps
  const dRepAbstainVotes = Number(action.abstain_votes) + alwaysAbstain;
  const dRepRatificationStake = dRepsStake - dRepAbstainVotes;
  const dRepYesVotes = isNoConfidence
    ? Number(action.yes_votes) + noConfidence
    : Number(action.yes_votes);
  const dRepNoVotes = isNoConfidence
    ? Number(action.no_votes)
    : Number(action.no_votes) + noConfidence;
  const dRepNotVotedVotes =
    dRepRatificationStake - (dRepYesVotes + dRepNoVotes);

  // SPOs
  const poolAbstainVotes =
    Number(action.pool_abstain_votes) + alwaysAbstainForSPOs;
  const poolRatificationStake = sPOsStake - poolAbstainVotes;
  const poolYesVotes = isNoConfidence
    ? Number(action.pool_yes_votes) + noConfidenceForSPOs
    : Number(action.pool_yes_votes);
  const poolNoVotes = isNoConfidence
    ? Number(action.pool_no_votes)
    : Number(action.pool_no_votes) + noConfidenceForSPOs;
  const poolNotVotedVotes =
    poolRatificationStake - (poolYesVotes + poolNoVotes);

  // Constitutional Committee
  const ccYesVotes = Number(action.cc_yes_votes);
  const ccNoVotes = Number(action.cc_no_votes);
  const ccAbstainVotes = Number(action.cc_abstain_votes);
  const ccNotVotedVotes =
    committeeMembers - (ccYesVotes + ccNoVotes + ccAbstainVotes);

  const percentage = (yes: number, total: number) =>
    (total ? (yes / total) * 100 : undefined);
  const dRepYesPercentage = percentage(dRepYesVotes, dRepRatificationStake);
  const poolYesPercentage = percentage(poolYesVotes, poolRatificationStake);
  const ccYesPercentage = percentage(
    ccYesVotes,
    committeeMembers - ccAbstainVotes,
  );
  const complement = (value?: number) =>
    (value !== undefined ? 100 - value : undefined);

  const dRepThreshold = getThreshold("dReps");
  const sPOThreshold = getThreshold("sPos");
  const hasPassed = (yesPercentage?: number, threshold?: number) =>
    yesPercentage !== undefined &&
    (threshold ? yesPercentage >= threshold * 100 : yesPercentage > 50);

  const isDRepDisplayed = areDRepVoteTotalsDisplayed(type, isSecurityGroup);
  const isSPODisplayed = areSPOVoteTotalsDisplayed(type, isSecurityGroup);
  const isCCDisplayed = areCCVoteTotalsDisplayed(type);

  return (
    <Box>
      <Box
        display="flex"
        justifyContent="space-between"
        alignItems="center"
        width="100%"
        mb={3}
      >
        <Typography sx={{ fontWeight: 500, fontSize: 14, color: "#506288" }}>
          {t("outcome.ratifiedStatus.title")}
        </Typography>
        <OutcomeStatusChip label={getStatus()} />
      </Box>
      <Typography
        sx={{
          fontSize: 22,
          fontWeight: 600,
          lineHeight: "24px",
          wordBreak: "break-word",
          mb: 3,
        }}
      >
        {t("outcome.votes.title")}
      </Typography>

      <OutcomeVoteSection
        title={t("outcome.votes.dReps")}
        yesVotes={dRepYesVotes}
        noVotes={dRepNoVotes}
        noTotalVotes={dRepRatificationStake - dRepYesVotes}
        totalControlled={dRepsStake}
        totalAbstainVotes={dRepAbstainVotes}
        autoAbstainVotes={alwaysAbstain}
        explicitAbstainVotes={action.abstain_votes}
        notVotedVotes={dRepNotVotedVotes}
        noConfidenceVotes={noConfidence}
        threshold={dRepThreshold}
        yesPercentage={dRepYesPercentage}
        noPercentage={complement(dRepYesPercentage)}
        ratificationThreshold={dRepRatificationStake}
        isDisplayed={isDRepDisplayed}
        isDataReady={isDataReady}
        dataTestId="DReps-voting-results-data"
      />

      <Divider sx={{ my: 2 }} />

      <OutcomeVoteSection
        title={t("outcome.votes.sPos")}
        yesVotes={poolYesVotes}
        noVotes={poolNoVotes}
        noTotalVotes={poolRatificationStake - poolYesVotes}
        totalControlled={sPOsStake}
        totalAbstainVotes={poolAbstainVotes}
        autoAbstainVotes={alwaysAbstainForSPOs}
        explicitAbstainVotes={action.pool_abstain_votes}
        notVotedVotes={poolNotVotedVotes}
        noConfidenceVotes={noConfidenceForSPOs}
        threshold={sPOThreshold}
        yesPercentage={poolYesPercentage}
        noPercentage={complement(poolYesPercentage)}
        ratificationThreshold={poolRatificationStake}
        isDisplayed={isSPODisplayed}
        isDataReady={isDataReady}
        dataTestId="SPOs-voting-results-data"
      />

      <Divider sx={{ my: 2 }} />

      <OutcomeVoteSection
        title={t("outcome.votes.cCommitteeFull")}
        yesVotes={ccYesVotes}
        noVotes={ccNoVotes}
        totalControlled={committeeMembers}
        totalAbstainVotes={ccAbstainVotes}
        notVotedVotes={ccNotVotedVotes}
        threshold={ccThreshold}
        yesPercentage={ccYesPercentage}
        noPercentage={complement(ccYesPercentage)}
        isCC
        isDisplayed={isCCDisplayed}
        isDataReady={isDataReady}
        dataTestId="CC-voting-results-data"
      />

      <Divider sx={{ my: 2 }} />

      <Box mb={1}>
        <Typography sx={{ fontWeight: 600, fontSize: 18, mb: 1.875 }}>
          {t("outcome.label")}
        </Typography>
        <Box display="flex" justifyContent="space-between" width="100%" gap={1}>
          <OutcomeIndicator
            title={t("outcome.votes.dReps")}
            passed={hasPassed(dRepYesPercentage, dRepThreshold)}
            isDisplayed={isDRepDisplayed}
            isLoading={isLoading}
            dataTestId="DReps-voting-results-outcome"
          />
          <OutcomeIndicator
            title={t("outcome.votes.sPos")}
            passed={hasPassed(poolYesPercentage, sPOThreshold)}
            isDisplayed={isSPODisplayed}
            isLoading={isLoading}
            dataTestId="SPOs-voting-results-outcome"
          />
          <OutcomeIndicator
            title={t("outcome.votes.cCommitteeShort")}
            passed={
              ccYesPercentage !== undefined &&
              ccYesPercentage >= ccThreshold * 100
            }
            isDisplayed={isCCDisplayed}
            isLoading={isLoading}
            dataTestId="CC-voting-results-outcome"
          />
        </Box>
      </Box>
    </Box>
  );
};
