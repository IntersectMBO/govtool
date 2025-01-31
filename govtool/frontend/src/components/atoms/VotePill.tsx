import { useTranslation } from "react-i18next";
import { Box, Typography } from "@mui/material";

import { Vote } from "@models";

const borderColorMap = {
  yes: "#C0E4BA",
  no: "#EDACAC",
  abstain: "#99ADDE",
  notVoted: "#EAE9F0",
};

const bgColorMap = {
  yes: "#F0F9EE",
  no: "#FBEBEB",
  abstain: "#E6EBF7",
  notVoted: "#F5F5F8",
};

const voteLabelKey = {
  yes: "votes.yes",
  no: "votes.no",
  abstain: "votes.abstain",
  notVoted: "votes.notVoted",
};

const ccVoteLabelKey = {
  yes: "votes.constitutional",
  no: "votes.unconstitutional",
  abstain: "votes.abstain",
  notVoted: "",
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

  const bgColor = bgColorMap[vote];
  const borderColor = borderColorMap[vote];
  const labelKey = isCC ? ccVoteLabelKey[vote] : voteLabelKey[vote];

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
