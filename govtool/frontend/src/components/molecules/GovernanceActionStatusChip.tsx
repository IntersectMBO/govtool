import { Chip } from "@mui/material";

import { useTranslation } from "@hooks";
import { PILL_TONES, PillTone, RADIUS } from "@/consts/designTokens";

type GovernanceActionStatusChipProps = {
  label: string;
  bgColor?: string;
};

/** A rounded label; status labels get their colour, anything else `bgColor`. */
export const GovernanceActionStatusChip = ({
  label,
  bgColor,
}: GovernanceActionStatusChipProps) => {
  const { t } = useTranslation();

  const statusTones: Record<string, PillTone> = {
    [t("actionRecord.status.inProgress")]: "success",
    [t("actionRecord.status.ratified")]: "primary",
    [t("actionRecord.status.enacted")]: "primary",
    [t("actionRecord.status.expired")]: "error",
    [t("actionRecord.status.dropped")]: "neutral",
  };
  const tone = PILL_TONES[statusTones[label] ?? "neutral"];

  return (
    <Chip
      label={label}
      sx={{
        backgroundColor: bgColor ?? tone.bg,
        border: `1px solid ${bgColor ?? tone.border}`,
        borderRadius: `${RADIUS.sm}px`,
        height: 20,
        "& .MuiChip-label": {
          color: tone.text,
          fontSize: 12,
          fontWeight: 600,
          letterSpacing: "0.01px",
          lineHeight: "18px",
          overflow: "hidden",
          px: 0.5,
          py: 0,
          textOverflow: "ellipsis",
          whiteSpace: "nowrap",
        },
      }}
    />
  );
};
