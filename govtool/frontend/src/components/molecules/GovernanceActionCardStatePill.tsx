import { Box } from "@mui/material";
import CheckIcon from "@mui/icons-material/Check";

import { Typography } from "@atoms";
import { useTranslation } from "@hooks";
import { PILL_TONES, RADIUS } from "@/consts/designTokens";

export const GovernanceActionCardStatePill = ({
  variant = "voteSubmitted",
}: {
  variant?: "inProgress" | "voteSubmitted";
}) => {
  const { t } = useTranslation();
  const tone = PILL_TONES[variant === "inProgress" ? "warning" : "success"];

  return (
    <Box
      data-testid="action-state-pill"
      sx={{
        position: "absolute",
        top: -10,
        right: 30,
        bgcolor: tone.bg,
        border: 1,
        borderColor: tone.border,
        px: 1,
        py: 0.25,
        borderRadius: `${RADIUS.sm}px`,
      }}
    >
      <Typography
        sx={{
          color: tone.text,
          display: "flex",
          alignItems: "center",
          gap: 0.5,
        }}
        variant="tagline"
        fontWeight={600}
      >
        {variant === "voteSubmitted" && (
          <CheckIcon
            sx={{
              fontSize: "14px",
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
