import { useTranslation } from "react-i18next";
import { Box, Typography } from "@mui/material";

import { Vote } from "@models";

const borderColorMap: Record<string, string> = {
  yes: "#C0E4BA",
  no: "#EDACAC",
  abstain: "#99ADDE",
  notvoted: "#EAE9F0",
};

const bgColorMap: Record<string, string> = {
  yes: "#F0F9EE",
  no: "#FBEBEB",
  abstain: "#E6EBF7",
  notvoted: "#F5F5F8",
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
  notvoted: "",
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

  const bgColor = bgColorMap[voteKey];
  const borderColor = borderColorMap[voteKey];
  const labelKey = isCC ? ccVoteLabelKey[voteKey] : voteLabelKey[voteKey];

  return (
    <Box
      py={0.75}
      px={2.25}
      border={1}
      borderColor={borderColor}
      bgcolor={bgColor}
      borderRadius={100}
      textAlign="center"
      minWidth="50px"
      maxWidth={maxWidth ? `${maxWidth}px` : "auto"}
      width={width ? `${width}px` : "auto"}
    >
      <Typography
        textTransform="uppercase"
        fontSize={12}
        fontWeight={400}
        lineHeight="16px"
        whiteSpace="nowrap"
        textOverflow="ellipsis"
        overflow="hidden"
      >
        {t(labelKey)}
      </Typography>
    </Box>
  );
};
