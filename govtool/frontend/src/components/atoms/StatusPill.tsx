import { Chip, ChipProps, styled } from "@mui/material";

import { ds, RADIUS } from "@/consts/designTokens";
import { DRepStatus } from "@/models";

interface StatusPillProps {
  status: DRepStatus;
  dataTestId?: string;
  label?: string;
  size?: "small" | "medium";
  sx?: ChipProps["sx"];
}

// DRep status pills as the redesign's DRep cards draw them.
const COLORS: Record<DRepStatus, { bg: string; text: string }> = {
  [DRepStatus.Active]: { bg: ds.primary["50"], text: ds.primary["600"] },
  [DRepStatus.Inactive]: { bg: ds.error["50"], text: ds.error["600"] },
  [DRepStatus.Retired]: { bg: ds.gray["100"], text: ds.gray["600"] },
  [DRepStatus.Yourself]: { bg: ds.primary["50"], text: ds.primary["600"] },
};

export const StatusPill = ({
  dataTestId,
  status,
  label = status,
  size = "small",
  sx,
}: StatusPillProps) => (
  <StyledChip
    data-testid={dataTestId ?? `${status}-pill`}
    status={status}
    size={size}
    label={label}
    sx={sx}
  />
);

const StyledChip = styled(Chip)<{ status: DRepStatus }>(({ status }) => ({
  backgroundColor: COLORS[status].bg,
  borderRadius: RADIUS.xs,
  color: COLORS[status].text,
  fontSize: 12,
  fontWeight: 600,
  height: 18,
  letterSpacing: "0.01px",
  lineHeight: "18px",
  textTransform: "capitalize",
  "& .MuiChip-label": { padding: "0 4px" },
}));
