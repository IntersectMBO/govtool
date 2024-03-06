import { Chip, ChipProps, styled } from "@mui/material";
import { cyan, errorRed, successGreen } from "@/consts";

type Status = 'active' | 'retired' | 'inactive';

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

const getBgColor = (status: Status): string => {
  switch (status) {
    case 'active':
      return successGreen.c200;
    case 'retired':
      return errorRed.c100;
    case 'inactive':
      return cyan.c100;
    // no default
  }
};

const getTextColor = (status: Status): string => {
  switch (status) {
    case 'active':
      return successGreen.c700;
    case 'retired':
      return errorRed.c500;
    case 'inactive':
      return cyan.c500;
    // no default
  }
};

const StyledChip = styled(Chip)<{ status: Status }>(({ theme, status }) => ({
  backgroundColor: getBgColor(status),
  color: getTextColor(status),
  border: `2px solid ${theme.palette.neutralWhite}`,
  fontSize: '0.75rem',
  textTransform: 'capitalize',
}));
