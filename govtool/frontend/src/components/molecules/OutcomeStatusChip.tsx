import { Chip } from "@mui/material";

import { errorRed, primaryBlue, successGreen } from "@consts";
import { useTranslation } from "@hooks";

type OutcomeStatusChipProps = {
  label: string;
  bgColor?: string;
};

/** A rounded label; status labels get their colour, anything else `bgColor`. */
export const OutcomeStatusChip = ({
  label,
  bgColor,
}: OutcomeStatusChipProps) => {
  const { t } = useTranslation();

  const statusColors: Record<string, string> = {
    [t("outcome.status.inProgress")]: successGreen.c100,
    [t("outcome.status.ratified")]: successGreen.c100,
    [t("outcome.status.enacted")]: successGreen.c100,
    [t("outcome.status.expired")]: errorRed.c100,
    [t("outcome.status.dropped")]: primaryBlue.c100,
  };

  return (
    <Chip
      label={label}
      sx={{
        backgroundColor: bgColor ?? statusColors[label],
        borderRadius: 100,
        height: "auto",
        py: 0.75,
        px: 2.25,
        "& .MuiChip-label": {
          fontSize: 12,
          fontWeight: 500,
          whiteSpace: "nowrap",
          textOverflow: "ellipsis",
          overflow: "hidden",
          color: "textBlack",
          px: 0,
          py: 0,
        },
      }}
    />
  );
};
