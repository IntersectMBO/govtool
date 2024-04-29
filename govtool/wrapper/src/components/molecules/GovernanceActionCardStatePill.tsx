import { Box } from "@mui/material";
import CheckIcon from "@mui/icons-material/Check";

import { Typography } from "@atoms";
import { useTranslation } from "@hooks";

export const GovernanceActionCardStatePill = ({
  variant = "voteSubmitted",
}: {
  variant?: "inProgress" | "voteSubmitted";
}) => {
  const { t } = useTranslation();

  return (
    <Box
      sx={{
        // TODO: To decide on final state pill design
        position: "absolute",
        top: -15,
        right: 30,
        bgcolor: variant === "inProgress" ? "#F8ECD4" : "#E0F2DC",
        border: 1,
        borderColor: variant === "inProgress" ? "#DEA029" : "#62BC52",
        px: 2.25,
        py: 0.5,
        borderRadius: 100,
      }}
    >
      <Typography
        sx={{
          color: variant === "inProgress" ? "#DEA029" : "#62BC52",
          display: "flex",
          alignItems: "center",
          gap: 0.5,
        }}
        variant="body2"
      >
        {variant === "voteSubmitted" && (
          <CheckIcon
            sx={{
              fontSize: "17px",
            }}
          />
        )}
        {variant === "inProgress"
          ? t("inProgress")
          : t("govActions.voteSubmitted")}
      </Typography>
    </Box>
  );
};
