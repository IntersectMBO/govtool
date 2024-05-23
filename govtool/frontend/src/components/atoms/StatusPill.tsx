import { Chip, ChipProps, styled } from "@mui/material";
import { cyan, errorRed, successGreen } from "@/consts";
import { DRepStatus } from "@/models";
import { theme } from "@/theme";

interface StatusPillProps {
  status: DRepStatus;
  dataTestId?: string;
  label?: string;
  size?: "small" | "medium";
  sx?: ChipProps["sx"];
}

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
    sx={{
      px: status === DRepStatus.Yourself ? 0.75 : 0.5,
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
