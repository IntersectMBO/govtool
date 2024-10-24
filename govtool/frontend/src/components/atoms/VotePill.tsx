import { useTranslation } from "react-i18next";
import { Box, Typography } from "@mui/material";

import { Vote } from "@models";

export const VotePill = ({
  vote,
  width,
  maxWidth,
  isCC,
}: {
  vote: Vote;
  width?: number;
  maxWidth?: number;
  isCC?: boolean;
}) => {
  const { t } = useTranslation();
  const VOTE = vote.toLowerCase() as "yes" | "no" | "abstain";
  return (
    <Box
      py={0.75}
      px={2.25}
      border={1}
      borderColor={
        VOTE === "yes" ? "#C0E4BA" : VOTE === "no" ? "#EDACAC" : "#99ADDE"
      }
      bgcolor={
        VOTE === "yes" ? "#F0F9EE" : VOTE === "no" ? "#FBEBEB" : "#E6EBF7"
      }
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
        {t(
          `votes.${
            isCC
              ? VOTE === "yes"
                ? "constitutional"
                : vote === "no"
                ? "unconstitutional"
                : VOTE
              : VOTE
          }`,
        )}
      </Typography>
    </Box>
  );
};
