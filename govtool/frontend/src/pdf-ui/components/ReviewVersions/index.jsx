'use client';

import {
    Dialog,
    Box,
    Card,
    CardContent,
    Grid,
    List,
    ListItem,
    ListItemButton,
    ListItemText,
    Link,
    IconButton,
} from '@mui/material';
import { Button, Typography } from '@atoms';
import { useMediaQuery } from '@mui/material';
import { useTheme } from '@emotion/react';
import {
    IconArchive,
} from '@intersect.mbo/intersectmbo.org-icons-set';
import { ICONS } from '@/consts/icons';
import { formatIsoDate, formatIsoTime, openInNewTab } from '../../lib/utils';
import { useEffect, useState } from 'react';
import { getProposals } from '../../lib/api';
import ReactMarkdown from 'react-markdown';
import { primaryBlue } from '@/consts/colors';

// Layout tokens copied from GovTool's governance action details
// (GovernanceActionDetailsCard / GovernanceActionDetailsCardData /
// GovernanceActionCardElement) and its PagePaddingBox.
const pagePx = { xxs: 2, md: 5 };
const detailsCardSx = {
    width: '100%',
    borderRadius: '20px',
    bgcolor: 'rgba(255, 255, 255, 0.30)',
    boxShadow: '2px 2px 20px 0px rgba(47, 98, 220, 0.20)',
    overflow: 'hidden',
};
const detailsLabelSx = {
    fontSize: 14,
    fontWeight: 600,
    lineHeight: '20px',
    mb: '4px',
};
// GovTool renders markdown paragraphs as 16/400 with a 24px line height.
const markdownComponents = {
    p: ({ children }) => (
        <Typography
            variant='body1'
            fontWeight={400}
            style={{ wordWrap: 'break-word' }}
        >
            {children}
        </Typography>
    ),
};
const backArrow = (
    <img
        src={ICONS.arrowRightIcon}
        alt=''
        style={{ transform: 'rotate(180deg)' }}
    />
);

const ReviewVersions = ({ open, onClose, id }) => {
    const theme = useTheme();
    const openLink = (link) => openInNewTab(link);

    const [versions, setVersions] = useState(null);
    const [selectedVersion, setSelectedVersion] = useState(null);
    const [openVersionsList, setOpenVersionsList] = useState(false);

    const isSmallScreen = useMediaQuery((theme) =>
        theme.breakpoints.down('lg')
    );

    const handleOpenVersionsList = () => setOpenVersionsList(true);
    const handleCloseVersionsList = () => setOpenVersionsList(false);

    const fetchVersions = async () => {
        try {
            let query = `filters[$and][0][prop_id]=${id}&pagination[page]=1&pagination[pageSize]=25&sort[createdAt]=desc&populate[0]=proposal_links&populate[1]=proposal_withdrawals`;
            const { proposals } = await getProposals(query);
            if (!proposals) return;

            setVersions(proposals);
            setSelectedVersion(proposals[0]);
        } catch (error) {
            console.error(error);
        }
    };

    useEffect(() => {
        if (open) {
            fetchVersions();
        }
    }, [open]);

    return (
        <Typography variant='body1' fontWeight={400}>
            <Dialog
                fullScreen
                open={open}
                onClose={onClose}
                data-testid='review-versions'
            >
                {isSmallScreen && openVersionsList ? (
                    <Box>
                        <Box
                            sx={{
                                display: 'flex',
                                flexDirection: 'row',
                                justifyContent: 'flex-start',
                                borderBottom: `1px solid ${theme.palette.lightBlue}`,
                                alignItems: 'center',
                                px: pagePx,
                                py: 3,
                                gap: 2,
                            }}
                        >
                            <IconArchive height='24px' width='24px' />
                            <Typography variant='title1' component='h1'>
                                Versions
                            </Typography>
                        </Box>
                        <List>
                            {versions?.map((version, index) => (
                                <ListItem
                                    key={
                                        version?.attributes?.content?.id ||
                                        index
                                    }
                                    disablePadding
                                    sx={{
                                        backgroundColor:
                                            version?.attributes?.content?.id ===
                                            selectedVersion?.attributes?.content
                                                ?.id
                                                ? primaryBlue.c50
                                                : 'transparent',
                                    }}
                                >
                                    <ListItemButton
                                        onClick={() => {
                                            setSelectedVersion(version);
                                            handleCloseVersionsList();
                                        }}
                                        data-testid='review-versions-list-item-button'
                                    >
                                        <ListItemText
                                            primary={
                                                <Box>
                                                    {`${formatIsoDate(
                                                        version?.attributes
                                                            ?.content
                                                            ?.attributes
                                                            ?.createdAt
                                                    )}  ${formatIsoTime(
                                                        version?.attributes
                                                            ?.content
                                                            ?.attributes
                                                            ?.createdAt
                                                    )} ${
                                                        version?.attributes
                                                            ?.content
                                                            ?.attributes
                                                            ?.prop_rev_active
                                                            ? ' (Live)'
                                                            : ''
                                                    }`}
                                                </Box>
                                            }
                                        />
                                    </ListItemButton>
                                </ListItem>
                            ))}
                        </List>
                    </Box>
                ) : (
                    <Grid
                        container
                        sx={{
                            overflow: 'auto',
                            minHeight: 0,
                        }}
                    >
                        <Grid
                            item
                            xxs={12}
                            sx={{
                                borderBottom: `1px solid ${theme.palette.lightBlue}`,
                                px: pagePx,
                                py: 3,
                            }}
                        >
                            <Typography
                                variant={isSmallScreen ? 'title1' : 'headline3'}
                                component='h1'
                            >
                                View Versions
                            </Typography>
                        </Grid>

                        <Grid item xxs={12} sx={{ px: pagePx, pt: 2, pb: 1 }}>
                            <Button
                                variant='text'
                                size='small'
                                startIcon={backArrow}
                                sx={{ px: 0, minWidth: 0 }}
                                onClick={onClose}
                                data-testid='review-versions-back'
                            >
                                Back
                            </Button>
                        </Grid>

                        <Grid item xxs={12} sx={{ px: pagePx, pb: 5 }}>
                            <Box sx={{ maxWidth: 1290, mx: 'auto' }}>
                            <Grid container spacing={3}>
                                {!isSmallScreen && (
                                    <Grid
                                        item
                                        xxs={12}
                                        lg={3}
                                    >
                                        <Card elevation={0} sx={detailsCardSx}>
                                            <CardContent
                                                sx={{
                                                    padding: 0,
                                                    width: '100%',
                                                }}
                                            >
                                                <Box
                                                    sx={{
                                                        display: 'flex',
                                                        flexDirection: 'row',
                                                        justifyContent:
                                                            'flex-start',
                                                        alignItems: 'center',
                                                        borderBottom: `1px solid ${theme.palette.lightBlue}`,
                                                        gap: 2,
                                                        px: 3,
                                                        py: 2,
                                                    }}
                                                >
                                                    <IconArchive
                                                        height='24px'
                                                        width='24px'
                                                    />
                                                    <Typography
                                                        variant='body1'
                                                        component='h6'
                                                    >
                                                        Versions
                                                    </Typography>
                                                </Box>
                                                {/* Versions */}
                                                <List
                                                    sx={{
                                                        padding: 0,
                                                        maxHeight: '70vh',
                                                        overflowY: 'auto',
                                                    }}
                                                >
                                                    {versions?.map(
                                                        (version, index) => (
                                                            <ListItem
                                                                key={
                                                                    version
                                                                        ?.attributes
                                                                        ?.content
                                                                        ?.id ||
                                                                    index
                                                                }
                                                                disablePadding
                                                                sx={{
                                                                    backgroundColor:
                                                                        version
                                                                            ?.attributes
                                                                            ?.content
                                                                            ?.id ===
                                                                        selectedVersion
                                                                            ?.attributes
                                                                            ?.content
                                                                            ?.id
                                                                            ? primaryBlue.c50
                                                                            : 'transparent',
                                                                }}
                                                            >
                                                                <ListItemButton
                                                                    onClick={() =>
                                                                        setSelectedVersion(
                                                                            version
                                                                        )
                                                                    }
                                                                >
                                                                    <ListItemText
                                                                        primary={
                                                                            <>
                                                                                <div>
                                                                                    {`${formatIsoDate(
                                                                                        version
                                                                                            ?.attributes
                                                                                            ?.content
                                                                                            ?.attributes
                                                                                            ?.createdAt
                                                                                    )}${
                                                                                        version
                                                                                            ?.attributes
                                                                                            ?.content
                                                                                            ?.attributes
                                                                                            ?.prop_rev_active
                                                                                            ? ' (Live)'
                                                                                            : ''
                                                                                    }`}
                                                                                </div>
                                                                                <div>
                                                                                    {formatIsoTime(
                                                                                        version
                                                                                            ?.attributes
                                                                                            ?.content
                                                                                            ?.attributes
                                                                                            ?.createdAt
                                                                                    )}
                                                                                </div>
                                                                            </>
                                                                        }
                                                                    />
                                                                </ListItemButton>
                                                            </ListItem>
                                                        )
                                                    )}
                                                </List>
                                            </CardContent>
                                        </Card>
                                    </Grid>
                                )}
                                {/* Selected version content */}
                                <Grid item xxs={12} lg={9} zIndex={1}>
                                    <Box display={'flex'} width={'100%'}>
                                        <Card
                                            elevation={0}
                                            sx={detailsCardSx}
                                        >
                                            <CardContent
                                                sx={{
                                                    p: { xxs: 3, md: 5 },
                                                    '&:last-child': {
                                                        pb: { xxs: 3, md: 5 },
                                                    },
                                                }}
                                            >
                                                <Box
                                                    sx={{
                                                        display: 'flex',
                                                        flexDirection: 'column',
                                                        gap: 4,
                                                    }}
                                                >
                                                    <Typography
                                                        variant='title1'
                                                        component='h5'
                                                    >
                                                        {
                                                            selectedVersion
                                                                ?.attributes
                                                                ?.content
                                                                ?.attributes
                                                                ?.prop_name
                                                        }
                                                    </Typography>
                                                    {isSmallScreen ? (
                                                        <Box>
                                                            <Typography
                                                                variant='body2'
                                                                sx={detailsLabelSx}
                                                                color={
                                                                    theme.palette.neutralGray
                                                                }
                                                            >
                                                                Version Date
                                                            </Typography>
                                                            <Typography
                                                                variant='body1'
                                                                fontWeight={400}
                                                                gutterBottom
                                                            >
                                                                {`${formatIsoDate(
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.content
                                                                        ?.attributes
                                                                        ?.createdAt
                                                                )}${
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.content
                                                                        ?.attributes
                                                                        ?.prop_rev_active
                                                                        ? ' (Live)'
                                                                        : ''
                                                                }`}
                                                            </Typography>
                                                        </Box>
                                                    ) : null}
                                                    <Box>
                                                        <Typography
                                                            variant='body2'
                                                            sx={detailsLabelSx}
                                                            color={
                                                                theme.palette.neutralGray
                                                            }
                                                        >
                                                            Goverance Action
                                                            Type
                                                        </Typography>
                                                        {/* GovTool shows the action type as a pill. */}
                                                        <Box
                                                            sx={{
                                                                display: 'inline-flex',
                                                                padding: '6px 18px',
                                                                bgcolor: 'lightBlue',
                                                                borderRadius: 100,
                                                            }}
                                                        >
                                                            <Typography variant='caption'>
                                                                {
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.content
                                                                        ?.attributes
                                                                        ?.gov_action_type
                                                                        ?.attributes
                                                                        ?.gov_action_type_name
                                                                }
                                                            </Typography>
                                                        </Box>
                                                    </Box>
                                                    <Box>
                                                        <Typography
                                                            variant='body2'
                                                            sx={detailsLabelSx}
                                                            color={
                                                                theme.palette.neutralGray
                                                            }
                                                        >
                                                            Abstract
                                                        </Typography>
                                                        <ReactMarkdown
                                                            components={markdownComponents}
                                                        >
                                                            {selectedVersion
                                                                ?.attributes
                                                                ?.content
                                                                ?.attributes
                                                                ?.prop_abstract ||
                                                                ''}
                                                        </ReactMarkdown>
                                                    </Box>
                                                    <Box>
                                                        <Typography
                                                            variant='body2'
                                                            sx={detailsLabelSx}
                                                            color={
                                                                theme.palette.neutralGray
                                                            }
                                                        >
                                                            Motivation
                                                        </Typography>
                                                        <ReactMarkdown
                                                            components={markdownComponents}
                                                        >
                                                            {selectedVersion
                                                                ?.attributes
                                                                ?.content
                                                                ?.attributes
                                                                ?.prop_motivation ||
                                                                ''}
                                                        </ReactMarkdown>
                                                    </Box>
                                                    <Box>
                                                        <Typography
                                                            variant='body2'
                                                            sx={detailsLabelSx}
                                                            color={
                                                                theme.palette.neutralGray
                                                            }
                                                        >
                                                            Rationale
                                                        </Typography>

                                                        <ReactMarkdown
                                                            components={markdownComponents}
                                                        >
                                                            {selectedVersion
                                                                ?.attributes
                                                                ?.content
                                                                ?.attributes
                                                                ?.prop_rationale ||
                                                                ''}
                                                        </ReactMarkdown>
                                                    </Box>

                                                    {selectedVersion?.attributes
                                                        ?.content?.attributes
                                                        ?.proposal_links
                                                        ?.length > 0 && (
                                                        <Box>
                                                            <Typography
                                                                variant='body2'
                                                                sx={detailsLabelSx}
                                                                color={
                                                                    theme.palette.neutralGray
                                                                }
                                                            >
                                                                Supporting links
                                                            </Typography>
                                                            <Box
                                                                display='flex'
                                                                flexDirection={
                                                                    isSmallScreen
                                                                        ? 'column'
                                                                        : 'row'
                                                                }
                                                                flexWrap='wrap'
                                                                gap={2}
                                                            >
                                                                {selectedVersion?.attributes?.content?.attributes?.proposal_links?.map(
                                                                    (
                                                                        link,
                                                                        index
                                                                    ) => (
                                                                        <Button
                                                                            key={
                                                                                index
                                                                            }
                                                                            variant='text'
                                                                            size='medium'
                                                                            sx={{
                                                                                px: 0,
                                                                                minWidth: 0,
                                                                                display:
                                                                                    'flex',
                                                                                flexDirection:
                                                                                    'row',
                                                                                alignItems:
                                                                                    'center',
                                                                                whiteSpace:
                                                                                    'normal',
                                                                                height: 'auto',
                                                                                minHeight: 36,
                                                                            }}
                                                                            onClick={() =>
                                                                                openLink(
                                                                                    link?.prop_link
                                                                                )
                                                                            }
                                                                        >
                                                                            <Box
                                                                                mr={
                                                                                    0.5
                                                                                }
                                                                            >
                                                                                <img src={ICONS.link} alt='' style={{ width: '1em', height: '1em' }} />
                                                                            </Box>
                                                                            <Typography
                                                                                variant='body2'
                                                                                component='span'
                                                                                fontWeight={
                                                                                    400
                                                                                }
                                                                                data-testid={`link-${index}-text-content`}
                                                                            >
                                                                                {
                                                                                    link?.prop_link_text
                                                                                }
                                                                            </Typography>
                                                                        </Button>
                                                                    )
                                                                )}
                                                            </Box>
                                                        </Box>
                                                    )}
                                                </Box>

                                                <Box
                                                    sx={{
                                                        display: 'flex',
                                                        flexDirection:
                                                            isSmallScreen
                                                                ? 'column'
                                                                : 'row',
                                                        justifyContent:
                                                            'space-between',
                                                        mt: 5,
                                                    }}
                                                >
                                                    <Box>
                                                        <Button
                                                            variant='outlined'
                                                            sx={{
                                                                mb: {
                                                                    xxs: 2,
                                                                    lg: 0,
                                                                },
                                                            }}
                                                            onClick={onClose}
                                                            data-testid='back-button'
                                                        >
                                                            Back to Proposal
                                                        </Button>
                                                    </Box>
                                                </Box>
                                            </CardContent>
                                        </Card>

                                        {isSmallScreen && (
                                            <Box
                                                ml={2}
                                                sx={{
                                                    display: 'flex',
                                                    justifyContent: 'center',
                                                    alignItems: 'center',
                                                    backgroundColor:
                                                        theme.palette.lightBlue,
                                                    borderRadius: '16px',
                                                    width: '40px',
                                                    height: '40px',
                                                    boxShadow:
                                                        '0px 4px 15px 0px #DDE3F5',
                                                }}
                                            >
                                                <IconButton
                                                    onClick={
                                                        handleOpenVersionsList
                                                    }
                                                    data-testid='versions-button'
                                                >
                                                    <IconArchive />
                                                </IconButton>
                                            </Box>
                                        )}
                                    </Box>
                                </Grid>
                            </Grid>
                            </Box>
                        </Grid>
                    </Grid>
                )}
            </Dialog>
        </Typography>
    );
};

export default ReviewVersions;
