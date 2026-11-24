import { ReactNode, useCallback, useEffect } from "react";
import { Box } from "@mui/material";
import { LucideIcon, Server, Shield, UserRound } from "lucide-react";

import { SECURITY_RELEVANT_PARAMS_MAP } from "@consts";
import { Typography } from "@atoms";
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
import { ds, RADIUS, SHADOWS } from "@/consts/designTokens";

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
      data-testid="votes-submitted"
      sx={{
        display: "grid",
        flex: 1,
        gap: 2,
        gridTemplateColumns: "repeat(auto-fit, minmax(260px, 1fr))",
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
  );
};

export type VoterType = "ccCommittee" | "dReps" | "sPos";

const GROUP_ICONS: Record<VoterType, LucideIcon> = {
  dReps: UserRound,
  sPos: Server,
  ccCommittee: Shield,
};

const GroupCard = ({
  type,
  dataTestId,
  children,
}: {
  type: VoterType;
  dataTestId: string;
  children: ReactNode;
}) => {
  const { t } = useTranslation();
  const Icon = GROUP_ICONS[type];
  return (
    <Box
      data-testid={dataTestId}
      sx={{
        backgroundColor: "#FFFFFF",
        border: `1px solid ${ds.gray["200"]}`,
        borderRadius: `${RADIUS.lg}px`,
        boxShadow: SHADOWS.card,
        display: "flex",
        flexDirection: "column",
        gap: 1.5,
        minWidth: 0,
        p: 2.75,
      }}
    >
      <Box sx={{ alignItems: "center", display: "flex", gap: 0.75 }}>
        <Icon aria-hidden color="#E9682F" size={16} strokeWidth={2} />
        <Typography sx={{ color: ds.gray["900"], fontSize: 15, fontWeight: 600, lineHeight: "22px" }}>
          {t(`govActions.${type}`)}
        </Typography>
      </Box>
      {children}
    </Box>
  );
};

/** A group that votes on the action but whose totals are not known. */
const UnavailableVotesGroup = ({ type }: { type: VoterType }) => {
  const { t } = useTranslation();
  return (
    <GroupCard dataTestId={`submitted-votes-${type}-unavailable`} type={type}>
      <Typography sx={{ color: ds.gray["600"], fontSize: 14 }}>
        {t("govActions.voteTotalsUnavailable")}
      </Typography>
    </GroupCard>
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
    <GroupCard dataTestId={`submitted-votes-${type}`} type={type}>
      {threshold !== undefined && threshold !== null && (
        <Typography sx={{ color: ds.gray["600"], fontSize: 12, lineHeight: "16px", mt: -1 }}>
          {t("govActions.threshold")}
          <Box component="span" sx={{ color: ds.gray["900"], fontWeight: 600, ml: 0.75 }}>
            {`${Math.round(threshold * 1000) / 10}%`}
          </Box>
        </Typography>
      )}
      <Box sx={{ borderTop: `1px dashed ${ds.gray["200"]}` }} />
      <Box sx={{ display: "flex", flexDirection: "column", gap: 1.75 }}>
        <Vote type={type} vote="yes" percentage={yesVotesPercentage} value={yesVotes} />
        <Vote type={type} vote="no" percentage={noVotesPercentage} value={noVotes} />
        <Vote type={type} vote="abstain" value={abstainVotes} />
        {typeof notVotedVotes === "number" && (
          <Vote type={type} vote="notVoted" percentage={notVotedPercentage} value={notVotedVotes} />
        )}
      </Box>
    </GroupCard>
  );
};

const BAR_COLORS: Record<string, string> = {
  yes: ds.success["500"],
  no: ds.error["500"],
  abstain: ds.gray["400"],
  notVoted: ds.gray["300"],
};

type VoteProps = {
  type: VoterType;
  vote: VoteType;
  value: number;
  percentage?: number;
};
const Vote = ({ type, vote, value, percentage }: VoteProps) => {
  const { t } = useTranslation();
  const label =
    type === "ccCommittee" && vote === "yes"
      ? t("votes.constitutional")
      : type === "ccCommittee" && vote === "no"
        ? t("votes.unconstitutional")
        : t(`votes.${vote}`);
  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 0.75 }}>
      <Box sx={{ alignItems: "baseline", display: "flex", flexWrap: "wrap", gap: 1, justifyContent: "space-between" }}>
        <Typography sx={{ color: ds.gray["700"], fontSize: 14, lineHeight: "20px" }}>
          {label}
        </Typography>
        <Box sx={{ alignItems: "baseline", display: "flex", gap: 0.75 }}>
          <Typography
            data-testid={`submitted-votes-${type}-${vote}`}
            sx={{ color: ds.gray["900"], fontSize: 14, fontWeight: 600, lineHeight: "20px", wordBreak: "break-all" }}
          >
            {type !== "ccCommittee" ? `₳ ${correctAdaFormatWithSuffix(value)}` : value}
          </Typography>
          {vote !== "abstain" && typeof percentage === "number" && (
            <Typography
              data-testid={`submitted-votes-${type}-${vote}-percentage`}
              sx={{ color: ds.gray["500"], fontSize: 12, lineHeight: "20px" }}
            >
              {`· ${percentage.toFixed(2)}%`}
            </Typography>
          )}
        </Box>
      </Box>
      {vote !== "abstain" && typeof percentage === "number" && (
        <Box sx={{ backgroundColor: ds.gray["100"], borderRadius: "3px", height: 6, overflow: "hidden" }}>
          <Box
            sx={{
              backgroundColor: BAR_COLORS[vote],
              borderRadius: "3px",
              height: "100%",
              width: `${Math.min(Math.max(percentage, 0), 100)}%`,
            }}
          />
        </Box>
      )}
    </Box>
  );
};
