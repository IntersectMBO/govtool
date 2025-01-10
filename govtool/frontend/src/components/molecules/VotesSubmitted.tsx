import { useCallback } from "react";
import { Box } from "@mui/material";

import { IMAGES, SECURITY_RELEVANT_PARAMS_MAP } from "@consts";
import { Typography, VotePill } from "@atoms";
import { useTranslation } from "@hooks";
import { correctVoteAdaFormat, getGovActionVotingThresholdKey } from "@utils";
import { SubmittedVotesData } from "@models";
import { useFeatureFlag, useAppContext } from "@/context";

type Props = {
  votes: SubmittedVotesData;
};

export const VotesSubmitted = ({
  votes: {
    dRepYesVotes,
    dRepAbstainVotes,
    dRepNoVotes,
    poolYesVotes,
    poolAbstainVotes,
    poolNoVotes,
    ccYesVotes,
    ccAbstainVotes,
    ccNoVotes,
    type,
    protocolParams,
  },
}: Props) => {
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
  } = useFeatureFlag();
  const { t } = useTranslation();
  const { epochParams } = useAppContext();

  const dRepYesVotesPercentage =
    dRepYesVotes + dRepNoVotes
      ? (dRepYesVotes / (dRepYesVotes + dRepNoVotes)) * 100
      : undefined;
  const dRepNoVotesPercentage = dRepYesVotesPercentage
    ? 100 - dRepYesVotesPercentage
    : dRepNoVotes
    ? 100
    : undefined;

  const poolYesVotesPercentage =
    poolYesVotes + poolNoVotes
      ? (poolYesVotes / (poolYesVotes + poolNoVotes)) * 100
      : undefined;
  const poolNoVotesPercentage = poolYesVotesPercentage
    ? 100 - poolYesVotesPercentage
    : poolNoVotes
    ? 100
    : undefined;

  const ccYesVotesPercentage =
    ccYesVotes + ccNoVotes
      ? (ccYesVotes / (ccYesVotes + ccNoVotes)) * 100
      : undefined;
  const ccNoVotesPercentage = ccYesVotesPercentage
    ? 100 - ccYesVotesPercentage
    : ccNoVotes
    ? 100
    : undefined;

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
        {areDRepVoteTotalsDisplayed(type, isSecurityGroup()) && (
          <VotesGroup
            type="dReps"
            yesVotes={dRepYesVotes}
            yesVotesPercentage={dRepYesVotesPercentage}
            noVotes={dRepNoVotes}
            noVotesPercentage={dRepNoVotesPercentage}
            abstainVotes={dRepAbstainVotes}
            threshold={(() => {
              const votingThresholdKey = getGovActionVotingThresholdKey({
                govActionType: type,
                protocolParams,
                voterType: "dReps",
              });
              return votingThresholdKey && epochParams?.[votingThresholdKey];
            })()}
          />
        )}
        {areSPOVoteTotalsDisplayed(type, isSecurityGroup()) && (
          <VotesGroup
            type="sPos"
            yesVotes={poolYesVotes}
            yesVotesPercentage={poolYesVotesPercentage}
            noVotes={poolNoVotes}
            noVotesPercentage={poolNoVotesPercentage}
            abstainVotes={poolAbstainVotes}
            threshold={(() => {
              const votingThresholdKey = getGovActionVotingThresholdKey({
                govActionType: type,
                protocolParams,
                voterType: "sPos",
              });
              return votingThresholdKey && epochParams?.[votingThresholdKey];
            })()}
          />
        )}
        {areCCVoteTotalsDisplayed(type) && (
          <VotesGroup
            type="ccCommittee"
            yesVotes={ccYesVotes}
            noVotes={ccNoVotes}
            abstainVotes={ccAbstainVotes}
            yesVotesPercentage={ccYesVotesPercentage}
            noVotesPercentage={ccNoVotesPercentage}
          />
        )}
      </Box>
    </Box>
  );
};

export type VoterType = "ccCommittee" | "dReps" | "sPos";

type VotesGroupProps = {
  type: VoterType;
  yesVotes: number;
  yesVotesPercentage?: number;
  noVotes: number;
  noVotesPercentage?: number;
  abstainVotes: number;
  threshold?: number | null;
};

const VotesGroup = ({
  type,
  yesVotes,
  yesVotesPercentage,
  noVotes,
  noVotesPercentage,
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
      {threshold !== undefined && (
        <Box
          display="flex"
          flexDirection="row"
          flex={1}
          borderBottom={1}
          borderColor="neutralGray"
        >
          <Typography
            sx={{
              marginRight: 3,
              fontSize: 16,
              lineHeight: "24px",
              fontWeight: "500",
              color: "rgba(36, 34, 50, 1)",
            }}
          >
            {t("govActions.threshold")}
          </Typography>
          <Typography
            sx={{
              fontSize: 16,
              lineHeight: "24px",
              fontWeight: "500",
              color: "neutralGray",
            }}
          >
            {threshold}
          </Typography>
        </Box>
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
        {type !== "ccCommittee" ? `₳ ${correctVoteAdaFormat(value)}` : value}
      </Typography>
      {vote !== "abstain" && typeof percentage === "number" && (
        <Typography
          data-testid={`submitted-votes-${type}-${vote}-percentage`}
          sx={{
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
