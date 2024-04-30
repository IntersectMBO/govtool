import { Chip, ChipProps, styled } from "@mui/material";
import { cyan, errorRed, successGreen } from "@/consts";
import { DRepStatus } from "@/models";
import { theme } from "@/theme";

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
  <StyledChip status={status} size={size} label={label} sx={sx} />
);

const bgColor = {
  [DRepStatus.Active]: successGreen.c200,
  [DRepStatus.Inactive]: errorRed.c100,
  [DRepStatus.Retired]: cyan.c100,
  [DRepStatus.Yourself]: theme.palette.lightBlue,
};

const textColor = {
  [DRepStatus.Active]: successGreen.c700,
  [DRepStatus.Inactive]: errorRed.c500,
  [DRepStatus.Retired]: cyan.c500,
  [DRepStatus.Yourself]: theme.palette.textBlack,
};

const StyledChip = styled(Chip)<{ status: DRepStatus }>(
  ({ theme: themeStyles, status }) => ({
    backgroundColor: bgColor[status],
    color: textColor[status],
    fontSize: "0.75rem",
    textTransform: "capitalize",
    ...(status !== DRepStatus.Yourself && {
      border: `2px solid ${themeStyles.palette.neutralWhite}`,
    }),
  }),
);
