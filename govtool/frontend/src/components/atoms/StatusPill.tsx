import { Chip, ChipProps, styled } from "@mui/material";
import { cyan, errorRed, successGreen } from "@/consts";
import { DRepStatus } from "@/models";

interface StatusPillProps {
  status: DRepStatus;
  label?: string;
  size?: 'small' | 'medium';
  sx?: ChipProps['sx'];
}

export const StatusPill = ({
  status,
  label = status,
  size = 'small',
  sx
}: StatusPillProps) => (
  <StyledChip
    status={status}
    size={size}
    label={label}
    sx={sx}
  />
);

const bgColor = {
  [DRepStatus.Active]: successGreen.c200,
  [DRepStatus.Inactive]: cyan.c100,
  [DRepStatus.Retired]: errorRed.c100,
};

const textColor = {
  [DRepStatus.Active]: successGreen.c700,
  [DRepStatus.Inactive]: cyan.c500,
  [DRepStatus.Retired]: errorRed.c500,
};

const StyledChip = styled(Chip)<{ status: DRepStatus }>(({ theme, status }) => ({
  backgroundColor: bgColor[status],
  color: textColor[status],
  border: `2px solid ${theme.palette.neutralWhite}`,
  fontSize: '0.75rem',
  textTransform: 'capitalize',
}));
