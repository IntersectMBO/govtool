import { useCallback } from "react";
import { Box } from "@mui/material";

import { IMAGES, SECURITY_RELEVANT_PARAMS_MAP } from "@consts";
import { Typography, VotePill } from "@atoms";
import { useTranslation } from "@hooks";
import { correctVoteAdaFormat } from "@utils";
import { SubmittedVotesData } from "@models";
import { useFeatureFlag } from "@/context";

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

  return (
    <Box
      sx={{
        display: "flex",
        flexDirection: "column",
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
            noVotes={dRepNoVotes}
            abstainVotes={dRepAbstainVotes}
          />
        )}
        {areSPOVoteTotalsDisplayed(type, isSecurityGroup()) && (
          <VotesGroup
            type="sPos"
            yesVotes={poolYesVotes}
            noVotes={poolNoVotes}
            abstainVotes={poolAbstainVotes}
          />
        )}
        {areCCVoteTotalsDisplayed(type) && (
          <VotesGroup
            type="ccCommittee"
            yesVotes={ccYesVotes}
            noVotes={ccNoVotes}
            abstainVotes={ccAbstainVotes}
          />
        )}
      </Box>
    </Box>
  );
};

type VoterType = "ccCommittee" | "dReps" | "sPos";

type VotesGroupProps = {
  type: VoterType;
  yesVotes: number;
  noVotes: number;
  abstainVotes: number;
};

const VotesGroup = ({
  type,
  yesVotes,
  noVotes,
  abstainVotes,
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
      <Vote type={type} vote="yes" value={yesVotes} />
      <Vote type={type} vote="abstain" value={abstainVotes} />
      <Vote type={type} vote="no" value={noVotes} />
    </Box>
  );
};

type VoteProps = {
  type: VoterType;
  vote: VoteType;
  value: number;
};
const Vote = ({ type, vote, value }: VoteProps) => (
  <Box
    sx={{
      alignItems: "center",
      display: "flex",
      flexWrap: "wrap",
      columnGap: 1.5,
    }}
  >
    <VotePill vote={vote} width={115} isCC={type === "ccCommittee"} />
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
  </Box>
);
