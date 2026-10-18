import { Box } from "@mui/material";
import {
  IconThumbDown,
  IconThumbUp,
} from "@intersect.mbo/intersectmbo.org-icons-set";

import { Typography } from "@atoms";
import { errorRed, successGreen } from "@consts";

type GovernanceActionIndicatorProps = {
  title: string;
  passed?: boolean;
  isDisplayed: boolean;
  isLoading: boolean;
  dataTestId?: string;
};

/**
 * Whether one voter group reached its threshold: green thumbs-up, red
 * thumbs-down, or grey when the group does not vote on this action.
 */
export const GovernanceActionIndicator = ({
  title,
  passed,
  isDisplayed,
  isLoading,
  dataTestId,
}: GovernanceActionIndicatorProps) => {
  const bgcolor =
    isLoading || !isDisplayed || passed === undefined
      ? "gray"
      : passed
        ? successGreen.c600
        : errorRed.c500;

  return (
    <Box data-testid={dataTestId} width="100%">
      <Box
        data-testid="vote-result-icon"
        sx={{
          alignItems: "center",
          bgcolor,
          borderRadius: 10,
          display: "flex",
          gap: 0.5,
          height: 24,
          justifyContent: "center",
          mb: 1,
          opacity: isDisplayed ? 1 : 0.6,
          py: 0.625,
          transition: "background-color 0.3s ease-in-out",
        }}
      >
        <Box
          display="flex"
          alignItems="center"
          justifyContent="center"
          width={20}
          height={20}
          color="white"
        >
          {isLoading || !isDisplayed || passed === undefined ? (
            "-"
          ) : passed ? (
            <IconThumbUp />
          ) : (
            <IconThumbDown />
          )}
        </Box>
        <Typography
          data-testid="voter-type-label"
          color="white"
          sx={{ fontWeight: 500, fontSize: 13, lineHeight: 1.75 }}
        >
          {title}
        </Typography>
      </Box>
    </Box>
  );
};
