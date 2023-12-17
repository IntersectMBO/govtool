import { Vote } from "@models";
import { Box, Typography } from "@mui/material";

export const VotePill = ({
  vote,
  width,
  maxWidth,
}: {
  vote: Vote;
  width?: number;
  maxWidth?: number;
}) => {
  const VOTE = vote.toLowerCase();
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
      maxHeight="14px"
    >
      <Typography
        textTransform={"uppercase"}
        fontSize={12}
        fontWeight={400}
        lineHeight={"16px"}
      >
        {vote}
      </Typography>
    </Box>
  );
};
