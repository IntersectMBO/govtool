import { useTranslation } from "react-i18next";
import { Box, Typography } from "@mui/material";

import { Vote } from "@models";
import { PILL_TONES, PillTone, RADIUS } from "@/consts/designTokens";

const VOTE_TONES: Record<string, PillTone> = {
  yes: "success",
  no: "error",
  abstain: "neutral",
  notvoted: "neutral",
};

const voteLabelKey: Record<string, string> = {
  yes: "votes.yes",
  no: "votes.no",
  abstain: "votes.abstain",
  notvoted: "votes.notVoted",
};

const ccVoteLabelKey: Record<string, string> = {
  yes: "votes.constitutional",
  no: "votes.unconstitutional",
  abstain: "votes.abstain",
  notvoted: "votes.notVoted",
};

type VoteExtended = Vote | "notVoted";

export const VotePill = ({
  vote,
  width,
  maxWidth,
  isCC,
}: {
  vote: VoteExtended;
  width?: number;
  maxWidth?: number;
  isCC?: boolean;
}) => {
  const { t } = useTranslation();
  const voteKey = vote.toLocaleLowerCase();

  const tone = PILL_TONES[VOTE_TONES[voteKey] ?? "neutral"];
  const labelKey = isCC ? ccVoteLabelKey[voteKey] : voteLabelKey[voteKey];

  return (
    <Box
      alignItems="center"
      border={1}
      borderColor={tone.border}
      bgcolor={tone.bg}
      borderRadius={`${RADIUS.sm}px`}
      display="inline-flex"
      height={20}
      justifyContent="center"
      minWidth="50px"
      maxWidth={maxWidth ? `${maxWidth}px` : "auto"}
      px={1}
      width={width ? `${width}px` : "auto"}
    >
      <Typography
        color={tone.text}
        fontSize={12}
        fontWeight={600}
        letterSpacing="0.01px"
        lineHeight="18px"
        whiteSpace="nowrap"
        textOverflow="ellipsis"
        overflow="hidden"
      >
        {t(labelKey)}
      </Typography>
    </Box>
  );
};
