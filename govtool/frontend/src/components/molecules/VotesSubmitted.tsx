import { useCallback, useEffect } from "react";
import { Box } from "@mui/material";

import { IMAGES, SECURITY_RELEVANT_PARAMS_MAP } from "@consts";
import { Typography, VotePill } from "@atoms";
import {
  useGetNetworkMetrics,
  useGetNetworkTotalStake,
  useTranslation,
} from "@hooks";
import {
  getGovActionVotingThreshold,
  correctAdaFormatWithSuffix,
} from "@utils";
import { SubmittedVotesData } from "@models";
import { useFeatureFlag, useAppContext } from "@/context";
import { GovernanceActionType } from "@/types/governanceAction";

type Props = {
  type: GovernanceActionType;
  votes: SubmittedVotesData;
};

export const VotesSubmitted = ({ votes }: Props) => {
  const { type, protocolParams } = votes;
  // A null figure is one the backend could not get from its data source: that
  // group shows as unavailable rather than as no votes.
  const isDRepUnavailable = [
    votes.dRepYesVotes,
    votes.dRepNoVotes,
    votes.dRepAbstainVotes,
  ].includes(null);
  const isSPOUnavailable = [
    votes.poolYesVotes,
    votes.poolNoVotes,
    votes.poolAbstainVotes,
  ].includes(null);
  const isCCUnavailable = [
    votes.ccYesVotes,
    votes.ccNoVotes,
    votes.ccAbstainVotes,
  ].includes(null);
  const dRepYesVotes = votes.dRepYesVotes ?? 0;
  const dRepNoVotes = votes.dRepNoVotes ?? 0;
  const dRepAbstainVotes = votes.dRepAbstainVotes ?? 0;
  const poolYesVotes = votes.poolYesVotes ?? 0;
  const poolNoVotes = votes.poolNoVotes ?? 0;
  const poolAbstainVotes = votes.poolAbstainVotes ?? 0;
  const ccYesVotes = votes.ccYesVotes ?? 0;
  const ccNoVotes = votes.ccNoVotes ?? 0;
  const ccAbstainVotes = votes.ccAbstainVotes ?? 0;

  const isSecurityGroup = useCallback(
    () =>
      Object.values(SECURITY_RELEVANT_PARAMS_MAP).some(
        (paramKey) =>
          protocolParams?.[paramKey as keyof typeof protocolParams] !== null,
      ),
    [protocolParams],
  );

  const {
    areDRepVoteTotalsDisplayed,
    areSPOVoteTotalsDisplayed,
    areCCVoteTotalsDisplayed,
    isFeatureAvailable,
  } = useFeatureFlag();

  // Whole-feature gate. Every number in the committee block — the member count,
  // each percentage denominator and the quorum threshold — comes from the
  // governance metrics record. A provider that refuses it would make this block
  // render "0 of 0 members" and a 0% threshold as if they were facts, which is
  // worse than not rendering it. Fails OPEN while capabilities are unknown.
  const areCommitteeMetricsAvailable = isFeatureAvailable(
    "dashboard.committeeThreshold",
  );
  const { t } = useTranslation();
  const { networkTotalStake, fetchNetworkTotalStake } =
    useGetNetworkTotalStake();
  const { networkMetrics, fetchNetworkMetrics } = useGetNetworkMetrics();
  const { epochParams } = useAppContext();

  useEffect(() => {
    const init = async () => {
      await fetchNetworkTotalStake();
      await fetchNetworkMetrics();
    };
    init();
  }, []);

  const noOfCommitteeMembers = networkMetrics?.noOfCommitteeMembers ?? 0;
  const ccThreshold = (
    networkMetrics?.quorumDenominator
      ? networkMetrics.quorumNumerator / networkMetrics.quorumDenominator
      : 0
  ).toPrecision(2);

  // Coming from be
  // Equal to: total active drep stake + auto no-confidence stake
  const totalStakeControlledByDReps =
    (networkTotalStake?.totalStakeControlledByDReps ?? 0) -
    // As this being voted for the action becomes part of the total active stake
    dRepAbstainVotes;

  // Governance action abstain votesa + auto abstain votes
  const totalAbstainVotes =
    dRepAbstainVotes + (networkTotalStake?.alwaysAbstainVotingPower ?? 0);

  // TODO: Move this logic to backend

  // DRep votes
  const dRepYesVotesPercentage = totalStakeControlledByDReps
    ? (dRepYesVotes / totalStakeControlledByDReps) * 100
    : undefined;

  const dRepNoVotesPercentage = totalStakeControlledByDReps
    ? (dRepNoVotes / totalStakeControlledByDReps) * 100
    : undefined;

  // The total and the yes and no figures all include the always-no-confidence
  // stake (D153), so what is left of the non-abstaining stake did not vote.
  const dRepNotVotedVotes = totalStakeControlledByDReps
    ? totalStakeControlledByDReps - dRepYesVotes - dRepNoVotes
    : undefined;
  const dRepNotVotedVotesPercentage =
    100 - (dRepYesVotesPercentage ?? 0) - (dRepNoVotesPercentage ?? 0);

  // SPO/Pool votes
  const poolYesVotesPercentage =
    typeof poolYesVotes === "number" &&
    typeof networkTotalStake?.totalStakeControlledBySPOs === "number" &&
    networkTotalStake.totalStakeControlledBySPOs > 0
      ? (poolYesVotes / networkTotalStake.totalStakeControlledBySPOs) * 100
      : undefined;

  const poolNoVotesPercentage =
    typeof poolNoVotes === "number" &&
    typeof networkTotalStake?.totalStakeControlledBySPOs === "number" &&
    networkTotalStake.totalStakeControlledBySPOs > 0
      ? (poolNoVotes / networkTotalStake.totalStakeControlledBySPOs) * 100
      : undefined;

  const poolNotVotedVotes =
    typeof networkTotalStake?.totalStakeControlledBySPOs === "number"
      ? networkTotalStake.totalStakeControlledBySPOs -
        (poolYesVotes + poolNoVotes + poolAbstainVotes)
      : undefined;

  const poolNotVotedVotesPercentage =
    100 -
    (typeof poolYesVotesPercentage === "number" ? poolYesVotesPercentage : 0) -
    (typeof poolNoVotesPercentage === "number" ? poolNoVotesPercentage : 0);

  // Constitutional Commission votes
  const ccYesVotesPercentage = noOfCommitteeMembers
    ? (ccYesVotes / noOfCommitteeMembers) * 100
    : undefined;

    const ccNoVotesPercentage = noOfCommitteeMembers
    ? (ccNoVotes / noOfCommitteeMembers) * 100
    : undefined;

    const ccNotVotedVotes =
    noOfCommitteeMembers - ccYesVotes - ccNoVotes - ccAbstainVotes;

    const ccNotVotedVotesPercentage =
    100 - (ccYesVotesPercentage ?? 0) - (ccNoVotesPercentage ?? 0);

  return (
    <Box
      sx={{
        display: "flex",
        flexDirection: "column",
        flex: 1,
      }}
    >
      <img
        alt="ga icon"
        src={IMAGES.govActionListImage}
        width="64px"
        height="64px"
        style={{ marginBottom: "24px" }}
      />
      <Typography
        sx={{
          fontSize: "22px",
          fontWeight: "600",
          lineHeight: "28px",
        }}
      >
        {t("govActions.voteSubmitted")}
      </Typography>
      <Typography
        sx={{
          fontSize: "22px",
          fontWeight: "500",
          lineHeight: "28px",
          mb: 3,
        }}
      >
        {t("govActions.forGovAction")}
      </Typography>
      <Box
        sx={{
          display: "flex",
          flexDirection: "column",
          gap: 4.5,
        }}
      >
        {areDRepVoteTotalsDisplayed(type, isSecurityGroup()) &&
          isDRepUnavailable && <UnavailableVotesGroup type="dReps" />}
        {areDRepVoteTotalsDisplayed(type, isSecurityGroup()) &&
          !isDRepUnavailable && (
            <VotesGroup
              type="dReps"
              yesVotes={dRepYesVotes}
              yesVotesPercentage={dRepYesVotesPercentage}
              noVotes={dRepNoVotes}
              noVotesPercentage={dRepNoVotesPercentage}
              abstainVotes={totalAbstainVotes}
              notVotedVotes={dRepNotVotedVotes}
              notVotedPercentage={dRepNotVotedVotesPercentage}
              threshold={getGovActionVotingThreshold({
                govActionType: type,
                protocolParams,
                voterType: "dReps",
                epochParams,
              })}
            />
          )}
        {areSPOVoteTotalsDisplayed(type, isSecurityGroup()) &&
          isSPOUnavailable && <UnavailableVotesGroup type="sPos" />}
        {areSPOVoteTotalsDisplayed(type, isSecurityGroup()) &&
          !isSPOUnavailable && (
            <VotesGroup
              type="sPos"
              yesVotes={poolYesVotes}
              yesVotesPercentage={poolYesVotesPercentage}
              noVotes={poolNoVotes}
              noVotesPercentage={poolNoVotesPercentage}
              abstainVotes={poolAbstainVotes}
              notVotedVotes={poolNotVotedVotes}
              notVotedPercentage={poolNotVotedVotesPercentage}
              threshold={getGovActionVotingThreshold({
                govActionType: type,
                protocolParams,
                voterType: "sPos",
                epochParams,
              })}
            />
          )}
        {areCCVoteTotalsDisplayed(type) &&
          areCommitteeMetricsAvailable &&
          isCCUnavailable && <UnavailableVotesGroup type="ccCommittee" />}
        {areCCVoteTotalsDisplayed(type) &&
          areCommitteeMetricsAvailable &&
          !isCCUnavailable && (
            <VotesGroup
              type="ccCommittee"
              yesVotes={ccYesVotes}
              noVotes={ccNoVotes}
              abstainVotes={ccAbstainVotes}
              yesVotesPercentage={ccYesVotesPercentage}
              noVotesPercentage={ccNoVotesPercentage}
              notVotedVotes={ccNotVotedVotes}
              notVotedPercentage={ccNotVotedVotesPercentage}
              threshold={
                type !== GovernanceActionType.InfoAction
                  ? Number(ccThreshold)
                  : null
              }
            />
          )}
      </Box>
    </Box>
  );
};

export type VoterType = "ccCommittee" | "dReps" | "sPos";

/** A group that votes on the action but whose totals are not known. */
const UnavailableVotesGroup = ({ type }: { type: VoterType }) => {
  const { t } = useTranslation();
  return (
    <Box
      sx={{ display: "flex", flexDirection: "column", gap: "12px" }}
      data-testid={`submitted-votes-${type}-unavailable`}
    >
      <Typography
        sx={{ fontSize: "18px", fontWeight: "600", lineHeight: "24px" }}
      >
        {t(`govActions.${type}`)}
      </Typography>
      <Typography sx={{ fontSize: 14 }}>
        {t("govActions.voteTotalsUnavailable")}
      </Typography>
    </Box>
  );
};

type VotesGroupProps = {
  type: VoterType;
  yesVotes: number;
  yesVotesPercentage?: number;
  noVotes: number;
  noVotesPercentage?: number;
  notVotedVotes?: number;
  notVotedPercentage?: number;
  abstainVotes: number;
  threshold?: number | null;
};

const VotesGroup = ({
  type,
  yesVotes,
  yesVotesPercentage,
  noVotes,
  noVotesPercentage,
  notVotedVotes,
  notVotedPercentage,
  abstainVotes,
  threshold,
}: VotesGroupProps) => {
  const { t } = useTranslation();
  return (
    <Box
      sx={{
        display: "flex",
        flexDirection: "column",
        gap: "12px",
      }}
      data-testid={`submitted-votes-${type}`}
    >
      <Typography
        sx={{
          fontSize: "18px",
          fontWeight: "600",
          lineHeight: "24px",
        }}
      >
        {t(`govActions.${type}`)}
      </Typography>
      {threshold !== undefined && threshold !== null && (
        <Box display="flex" flexDirection="row" flex={1} alignItems="center">
          <Typography
            sx={{
              marginRight: 1,
              fontSize: 12,
              lineHeight: "16px",
              fontWeight: "400",
              color: "rgba(36, 34, 50, 1)",
            }}
          >
            {t("govActions.threshold")}
          </Typography>
          <Typography
            sx={{
              fontSize: 12,
              lineHeight: "16px",
              color: "neutralGray",
            }}
          >
            {threshold * 100}%
          </Typography>
        </Box>
      )}
      <Vote
        type={type}
        vote="yes"
        percentage={yesVotesPercentage}
        value={yesVotes}
      />
      <Vote type={type} vote="abstain" value={abstainVotes} />
      <Vote
        type={type}
        vote="no"
        percentage={noVotesPercentage}
        value={noVotes}
      />
      {typeof notVotedVotes === "number" && (
        <Vote
          type={type}
          vote="notVoted"
          percentage={notVotedPercentage}
          value={notVotedVotes}
        />
      )}
    </Box>
  );
};

type VoteProps = {
  type: VoterType;
  vote: VoteType;
  value: number;
  percentage?: number;
};
const Vote = ({ type, vote, value, percentage }: VoteProps) => (
  <Box
    sx={{
      alignItems: "center",
      display: "flex",
      flexWrap: "wrap",
      columnGap: 1.5,
    }}
  >
    <VotePill vote={vote} width={115} isCC={type === "ccCommittee"} />
    <Box
      display="flex"
      flexDirection="row"
      flex={1}
      justifyContent="space-between"
    >
      <Typography
        data-testid={`submitted-votes-${type}-${vote}`}
        sx={{
          fontSize: 16,
          wordBreak: "break-all",
          lineHeight: "24px",
          fontWeight: "500",
        }}
      >
        {type !== "ccCommittee"
          ? `₳ ${correctAdaFormatWithSuffix(value)}`
          : value}
      </Typography>
      {vote !== "abstain" && typeof percentage === "number" && (
        <Typography
          data-testid={`submitted-votes-${type}-${vote}-percentage`}
          sx={{
            ml: 1,
            fontSize: 16,
            lineHeight: "24px",
            fontWeight: "500",
            color: "neutralGray",
          }}
        >
          {typeof percentage === "number" ? `${percentage.toFixed(2)}%` : ""}
        </Typography>
      )}
    </Box>
  </Box>
);
