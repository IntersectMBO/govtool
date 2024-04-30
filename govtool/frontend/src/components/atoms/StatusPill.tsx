import { Chip, ChipProps, styled } from "@mui/material";
import { cyan, errorRed, successGreen } from "@/consts";
import { DRepStatus } from "@/models";

interface StatusPillProps {
  status: DRepStatus;
  label?: string;
  size?: "small" | "medium";
  sx?: ChipProps["sx"];
}

export const StatusPill = ({
  status,
  label = status,
  size = "small",
  sx,
}: StatusPillProps) => (
  <StyledChip
    status={status}
    size={size}
    label={label}
    sx={{
      px: 0.5,
      py: 0.5,
      height: 24,
      ...sx,
    }}
  />
);

const bgColor = {
  [DRepStatus.Active]: successGreen.c200,
  [DRepStatus.Inactive]: errorRed.c100,
  [DRepStatus.Retired]: cyan.c100,
};

const textColor = {
  [DRepStatus.Active]: successGreen.c700,
  [DRepStatus.Inactive]: errorRed.c500,
  [DRepStatus.Retired]: cyan.c500,
};

const StyledChip = styled(Chip)<{ status: DRepStatus }>(
  ({ theme, status }) => ({
    backgroundColor: bgColor[status],
    color: textColor[status],
    border: `2px solid ${theme.palette.neutralWhite}`,
    fontSize: "0.75rem",
    textTransform: "capitalize",
  }),
);
