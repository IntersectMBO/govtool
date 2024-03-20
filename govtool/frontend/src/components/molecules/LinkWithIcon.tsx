import { Box } from '@mui/material';
import ArrowBackIosIcon from '@mui/icons-material/ArrowBackIos';

import { Typography } from '@atoms';

import { LinkWithIconProps } from './types';

export const LinkWithIcon = ({
  label,
  onClick,
  icon,
  sx,
}: LinkWithIconProps) => (
  <Box
    data-testid={`${label.split(' ').join('-')}-link`}
    sx={{
      alignItems: 'center',
      cursor: 'pointer',
      display: 'flex',
      width: 'fit-content',
      ...sx,
    }}
    onClick={onClick}
  >
    {icon || <ArrowBackIosIcon color="primary" sx={{ fontSize: 14 }} />}
    <Typography
      color="primary"
      fontWeight={400}
      sx={{ ml: 0.5 }}
      variant="body2"
    >
      {label}
    </Typography>
  </Box>
);
