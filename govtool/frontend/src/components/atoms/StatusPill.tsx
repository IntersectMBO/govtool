import { Chip, ChipProps, styled } from "@mui/material";
import { cyan, errorRed, successGreen } from "@/consts";

type Status = 'Active' | 'Inactive' | 'Retired';

interface StatusPillProps {
  status: Status;
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
  Active: successGreen.c200,
  Inactive: cyan.c100,
  Retired: errorRed.c100,
};

const textColor = {
  Active: successGreen.c700,
  Inactive: cyan.c500,
  Retired: errorRed.c500,
};

const StyledChip = styled(Chip)<{ status: Status }>(({ theme, status }) => ({
  backgroundColor: bgColor[status],
  color: textColor[status],
  border: `2px solid ${theme.palette.neutralWhite}`,
  fontSize: '0.75rem',
  textTransform: 'capitalize',
}));
