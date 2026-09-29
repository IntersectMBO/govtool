import React from 'react';
import { Box } from '@mui/material';

// The centred form box of GovTool's CenteredBoxPageWrapper: radius 20, the
// boxShadow2 shadow and 900px wide from md up; flat and full width on mobile.
// Side padding is below GovTool's 150px so the four stepper buttons fit.
const StepBox = ({ children, sx = {} }) => (
    <Box
        sx={(theme) => ({
            alignSelf: 'center',
            boxSizing: 'border-box',
            width: '100%',
            maxWidth: { xxs: 'none', md: 900 },
            mx: 'auto',
            borderRadius: '20px',
            boxShadow: {
                xxs: 'none',
                md: `2px 2px 20px 0px ${theme.palette.boxShadow2 ?? 'rgba(47, 98, 220, 0.2)'}`,
            },
            px: { xxs: 2, md: 5, lg: 8 },
            py: { xxs: 6, md: 8 },
            display: 'flex',
            flexDirection: 'column',
            ...sx,
        })}
    >
        {children}
    </Box>
);

export default StepBox;
