import ArrowBackIosIcon from '@mui/icons-material/ArrowBackIos';
import { Box, useMediaQuery } from '@mui/material';
import { Button, InfoText, Typography } from '@atoms';
import { theme as govtoolTheme } from '@/theme';

// Layout pieces for the fullscreen create, edit and submit dialogs, copied
// from GovTool's CenteredBoxPageWrapper, DashboardTopNav title band,
// LinkWithIcon back link and CenteredBoxBottomButtons. They are pdf-local
// because the GovTool molecules hard-code their own testids and navigation.

const boxShadow2 = govtoolTheme.palette.boxShadow2;

// DashboardTopNav's title band: a #D6E2FF bottom border, headline5 on
// desktop and title1 on mobile.
export const FlowHeader = ({ title }) => {
    const isMobile = useMediaQuery((theme) => theme.breakpoints.down('md'));
    return (
        <Box
            sx={{
                borderBottom: '1px solid #D6E2FF',
                px: { xxs: 2, md: 5 },
                py: 3,
                position: 'relative',
                zIndex: 10,
            }}
        >
            <Typography
                variant={isMobile ? 'title1' : 'headline5'}
                component='h1'
            >
                {title}
            </Typography>
        </Box>
    );
};

// LinkWithIcon's look (primary 14px arrow, body2 400 primary label) on a
// Button, so the element keeps role=button and its testid.
export const FlowBackLink = ({ children, onClick, ...rest }) => (
    <Button
        variant='text'
        size='small'
        startIcon={<ArrowBackIosIcon color='primary' sx={{ fontSize: 14 }} />}
        onClick={onClick}
        sx={{
            alignSelf: 'flex-start',
            fontSize: 14,
            fontWeight: 400,
            height: 'auto',
            minWidth: 0,
            px: 0,
            py: 0.5,
            '& .MuiButton-startIcon': { mr: 0.5, ml: 0 },
            '&:hover': { backgroundColor: 'transparent' },
        }}
        {...rest}
    >
        {children}
    </Button>
);

// CenteredBoxPageWrapper's page area: 16px/40px side padding, the back link,
// then the content.
export const FlowPage = ({ children, sx }) => (
    <Box
        sx={{
            display: 'flex',
            flex: 1,
            flexDirection: 'column',
            gap: { xxs: 0, md: 1.5 },
            position: 'relative',
            px: { xxs: 2, md: 5 },
            py: { xxs: 3, md: 1.5 },
            zIndex: 10,
            ...sx,
        }}
    >
        {children}
    </Box>
);

// CenteredBoxPageWrapper's box: centred, max 900px wide, radius 20 and the
// boxShadow2 shadow on desktop, flat on mobile.
export const StepBox = ({ children, sx, ...rest }) => (
    <Box
        sx={{
            alignSelf: 'center',
            borderRadius: '20px',
            boxShadow: {
                xxs: 'none',
                md: `2px 2px 20px 0px ${boxShadow2}`,
            },
            boxSizing: 'border-box',
            display: 'flex',
            flexDirection: 'column',
            height: 'auto',
            maxWidth: { xxs: 'none', md: 900 },
            mb: { xxs: 2, md: 1.5 },
            mx: 'auto',
            px: { xxs: 2, md: 8, lg: 18.75 },
            py: { xxs: 6, md: 8 },
            width: '100%',
            ...sx,
        }}
        {...rest}
    >
        {children}
    </Box>
);

// The GovTool form heading: an orange InfoText over a centred headline4.
export const StepHeading = ({
    info,
    title,
    titleVariant = 'headline4',
    titleComponent = 'h4',
    children,
    sx,
}) => (
    <Box sx={{ textAlign: 'center', ...sx }}>
        {info && (
            <InfoText label={info} sx={{ mb: 0.75, textAlign: 'center' }} />
        )}
        <Typography variant={titleVariant} component={titleComponent}>
            {title}
        </Typography>
        {children}
    </Box>
);

// CenteredBoxBottomButtons: back on the left, actions on the right; stacked
// with the primary action on top on mobile.
export const StepButtons = ({ start, end, sx }) => (
    <Box
        sx={{
            display: 'flex',
            flexDirection: { xxs: 'column-reverse', md: 'row' },
            gap: { xxs: 3, md: 2 },
            justifyContent: 'space-between',
            mt: 6,
            ...sx,
        }}
    >
        <Box sx={{ display: 'flex', flexDirection: 'column' }}>{start}</Box>
        {end && (
            <Box
                sx={{
                    display: 'flex',
                    flexDirection: { xxs: 'column-reverse', md: 'row' },
                    gap: { xxs: 3, md: 2 },
                }}
            >
                {end}
            </Box>
        )}
    </Box>
);

// CenteredBoxBottomButtons' button size.
export const stepButtonSx = { px: 6 };
