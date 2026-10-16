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
    IconButton,
} from '@mui/material';
import { Button, Typography } from '@atoms';
import { useMediaQuery } from '@mui/material';
import { useTheme } from '@emotion/react';
import {
    IconArchive,
} from '@intersect.mbo/intersectmbo.org-icons-set';
import { ICONS } from '@/consts/icons';
import {
    correctVoteAdaFormat,
    formatIsoDate,
    formatIsoTime,
    openInNewTab,
} from '../../lib/utils';
import { useEffect, useState } from 'react';
import BudgetDiscussionInfoSegment from '../BudgetDiscussionInfoSegment';
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
const backArrow = (
    <img
        src={ICONS.arrowRightIcon}
        alt=''
        style={{ transform: 'rotate(180deg)' }}
    />
);

// Every version of an archived budget proposal, newest first, as the
// archive file holds them.
const BudgetDiscussionReviewVersions = ({ open, onClose, versions = [] }) => {
    const theme = useTheme();
    const openLink = (link) => openInNewTab(link);

    const [selectedVersion, setSelectedVersion] = useState(null);
    const [openVersionsList, setOpenVersionsList] = useState(false);

    const isSmallScreen = useMediaQuery((theme) =>
        theme.breakpoints.down('lg')
    );

    const handleOpenVersionsList = () => setOpenVersionsList(true);
    const handleCloseVersionsList = () => setOpenVersionsList(false);

    useEffect(() => {
        if (open) setSelectedVersion(versions[0] ?? null);
    }, [open, versions]);

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
                                    key={index}
                                    disablePadding
                                    sx={{
                                        backgroundColor:
                                            version?.id === selectedVersion?.id
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
                                                            ?.createdAt
                                                    )}  ${formatIsoTime(
                                                        version?.attributes
                                                            ?.createdAt
                                                    )} ${
                                                        version?.attributes
                                                            ?.is_active
                                                            ? ' (Final)'
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
                                                                key={index}
                                                                disablePadding
                                                                sx={{
                                                                    backgroundColor:
                                                                        version?.id ===
                                                                        selectedVersion?.id
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
                                                                                            ?.createdAt
                                                                                    )}${
                                                                                        version
                                                                                            ?.attributes
                                                                                            ?.is_active
                                                                                            ? ' (Final)'
                                                                                            : ''
                                                                                    }`}
                                                                                </div>
                                                                                <div>
                                                                                    {formatIsoTime(
                                                                                        version
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
                                    <Box display={'flex'} width={'100%'} pb={4}>
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
                                                                        ?.createdAt
                                                                )}${
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.content
                                                                        ?.attributes
                                                                        ?.prop_rev_active
                                                                        ? ' (Final)'
                                                                        : ''
                                                                }`}
                                                            </Typography>
                                                        </Box>
                                                    ) : null}
                                                    <Box>
                                                        <Typography
                                                            variant='headline4'
                                                            component='h4'
                                                            sx={{
                                                                mb: 2,
                                                            }}
                                                        >
                                                            {
                                                                selectedVersion
                                                                    ?.attributes
                                                                    ?.bd_proposal_detail
                                                                    ?.data
                                                                    ?.attributes
                                                                    ?.proposal_name
                                                            }
                                                        </Typography>

                                                        <Box sx={{ mt: 4 }}>
                                                            <BudgetDiscussionInfoSegment
                                                                question={
                                                                    'Budget category'
                                                                }
                                                                answer={
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.bd_psapb
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.type_name
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.type_name ||
                                                                    ''
                                                                }
                                                            />
                                                        </Box>

                                                        <Box
                                                            sx={{
                                                                mt: 3,
                                                            }}
                                                        >
                                                            <Typography
                                                                variant='title1'
                                                                component='h5'
                                                                sx={{
                                                                    mb: 2,
                                                                }}
                                                            >
                                                                Proposal
                                                                Ownership
                                                            </Typography>

                                                            {/* <BudgetDiscussionInfoSegment
                                                                question={
                                                                    'Proposal Public Champion: Who would you like to be the public proposal champion?'
                                                                }
                                                                answer={
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.bd_proposal_ownership
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.proposal_public_champion
                                                                }
                                                            /> */}
                                                            {selectedVersion
                                                                ?.attributes
                                                                ?.bd_proposal_ownership
                                                                ?.data
                                                                ?.attributes
                                                                ?.submited_on_behalf ===
                                                            'Company' ? (
                                                                <Box>
                                                                    <BudgetDiscussionInfoSegment
                                                                        question='Company Name'
                                                                        answer={
                                                                            selectedVersion
                                                                                ?.attributes
                                                                                ?.bd_proposal_ownership
                                                                                ?.data
                                                                                ?.attributes
                                                                                ?.company_name ||
                                                                            ''
                                                                        }
                                                                        answerTestId='company-name-content'
                                                                    />

                                                                    <BudgetDiscussionInfoSegment
                                                                        question='Company Domain Name'
                                                                        answer={
                                                                            selectedVersion
                                                                                ?.attributes
                                                                                ?.bd_proposal_ownership
                                                                                ?.data
                                                                                ?.attributes
                                                                                ?.company_domain_name ||
                                                                            ''
                                                                        }
                                                                        answerTestId='company-domain-name-content'
                                                                    />
                                                                    <BudgetDiscussionInfoSegment
                                                                        question='Country of Incorporation'
                                                                        answer={
                                                                            selectedVersion
                                                                                ?.attributes
                                                                                ?.bd_proposal_ownership
                                                                                ?.data
                                                                                ?.attributes
                                                                                ?.be_country
                                                                                ?.data
                                                                                ?.attributes
                                                                                ?.country_name ||
                                                                            ''
                                                                        }
                                                                        answerTestId='country-of-incorporation-content'
                                                                    />
                                                                </Box>
                                                            ) : (
                                                                ''
                                                            )}
                                                            {selectedVersion
                                                                ?.attributes
                                                                ?.bd_proposal_ownership
                                                                ?.data
                                                                ?.attributes
                                                                ?.submited_on_behalf ===
                                                            'Group' ? (
                                                                <Box>
                                                                    <BudgetDiscussionInfoSegment
                                                                        question='Group Name'
                                                                        answer={
                                                                            selectedVersion
                                                                                ?.attributes
                                                                                ?.bd_proposal_ownership
                                                                                ?.data
                                                                                ?.attributes
                                                                                ?.group_name ||
                                                                            ''
                                                                        }
                                                                        answerTestId='group-name-content'
                                                                    />

                                                                    <BudgetDiscussionInfoSegment
                                                                        question='Type of Group'
                                                                        answer={
                                                                            selectedVersion
                                                                                ?.attributes
                                                                                ?.bd_proposal_ownership
                                                                                ?.data
                                                                                ?.attributes
                                                                                ?.type_of_group ||
                                                                            ''
                                                                        }
                                                                        answerTestId='group-type-content'
                                                                    />
                                                                    <BudgetDiscussionInfoSegment
                                                                        question='Key Information to Identify
                                                                Group'
                                                                        answer={
                                                                            selectedVersion
                                                                                ?.attributes
                                                                                ?.bd_proposal_ownership
                                                                                ?.data
                                                                                ?.attributes
                                                                                ?.key_info_to_identify_group ||
                                                                            ''
                                                                        }
                                                                        answerTestId='group-identity-information-content'
                                                                    />
                                                                </Box>
                                                            ) : (
                                                                ''
                                                            )}

                                                            <BudgetDiscussionInfoSegment
                                                                question={
                                                                    'What social handles would you like to be used? E.g. Github, X'
                                                                }
                                                                answer={
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.bd_proposal_ownership
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.social_handles
                                                                }
                                                                answerTestId='social-handles'
                                                            />
                                                        </Box>

                                                        <Box
                                                            sx={{
                                                                mt: 4,
                                                            }}
                                                        >
                                                            <Typography
                                                                variant='title1'
                                                                component='h5'
                                                                sx={{
                                                                    mb: 2,
                                                                }}
                                                            >
                                                                Problem
                                                                Statements and
                                                                Proposal
                                                                Benefits
                                                            </Typography>

                                                            <BudgetDiscussionInfoSegment
                                                                question={
                                                                    'Problem Statement'
                                                                }
                                                                answer={
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.bd_psapb
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.problem_statement
                                                                }
                                                                answerTestId='problem-statement'
                                                            />

                                                            <BudgetDiscussionInfoSegment
                                                                question={
                                                                    'Proposal Benefit'
                                                                }
                                                                answer={
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.bd_psapb
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.proposal_benefit
                                                                }
                                                                answerTestId='problem-benefit'
                                                            />

                                                            <BudgetDiscussionInfoSegment
                                                                question={
                                                                    'Does this proposal align to the Product Roadmap and Roadmap Goals?'
                                                                }
                                                                answer={
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.bd_psapb
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.roadmap_name
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.roadmap_name
                                                                }
                                                                answerTestId='product-roadmap'
                                                            />
                                                            {selectedVersion
                                                                ?.attributes
                                                                ?.bd_psapb?.data
                                                                ?.attributes
                                                                ?.explain_proposal_roadmap ? (
                                                                <BudgetDiscussionInfoSegment
                                                                    question='Please explain how your proposal supports the Product Roadmap.'
                                                                    answer={
                                                                        selectedVersion
                                                                            ?.attributes
                                                                            ?.bd_psapb
                                                                            ?.data
                                                                            ?.attributes
                                                                            ?.explain_proposal_roadmap ||
                                                                        ''
                                                                    }
                                                                    answerTestId={
                                                                        'explain-roadmap-content'
                                                                    }
                                                                />
                                                            ) : (
                                                                ''
                                                            )}
                                                            <BudgetDiscussionInfoSegment
                                                                question={
                                                                    'Does your proposal align to any of the budget categories?'
                                                                }
                                                                answer={
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.bd_psapb
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.type_name
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.type_name
                                                                }
                                                                answerTestId='budget-discussion-type'
                                                            />

                                                            <BudgetDiscussionInfoSegment
                                                                question={
                                                                    'Does your proposal align with any of the Intersect Committees?'
                                                                }
                                                                answer={
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.bd_psapb
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.committee_name
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.committee_name
                                                                }
                                                                answerTestId='align-proposal-committees'
                                                            />

                                                            <BudgetDiscussionInfoSegment
                                                                question={
                                                                    'If possible provide evidence of wider community endorsement for this proposal?'
                                                                }
                                                                answer={
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.bd_psapb
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.supplementary_endorsement
                                                                }
                                                                answerTestId='evidence'
                                                            />
                                                        </Box>

                                                        <Box
                                                            sx={{
                                                                mt: 4,
                                                            }}
                                                        >
                                                            <Typography
                                                                variant='title1'
                                                                component='h5'
                                                                sx={{
                                                                    mb: 2,
                                                                }}
                                                            >
                                                                Proposal Details
                                                            </Typography>

                                                            <BudgetDiscussionInfoSegment
                                                                question={
                                                                    'What is your proposed name to be used to reference this proposal publicly?'
                                                                }
                                                                answer={
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.bd_proposal_detail
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.proposal_name
                                                                }
                                                                answerTestId='proposal-name'
                                                            />

                                                            <BudgetDiscussionInfoSegment
                                                                question={
                                                                    'Proposal Description'
                                                                }
                                                                answer={
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.bd_proposal_detail
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.proposal_description
                                                                }
                                                                answerTestId='proposal-description'
                                                            />

                                                            <BudgetDiscussionInfoSegment
                                                                question={
                                                                    'Please list any key dependencies (if any) for this proposal?'
                                                                }
                                                                answer={
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.bd_proposal_detail
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.key_dependencies
                                                                }
                                                                answerTestId={`proposal-key-dependencies`}
                                                            />

                                                            <BudgetDiscussionInfoSegment
                                                                question={
                                                                    'How will this proposal be maintained and supported after initial development?'
                                                                }
                                                                answer={
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.bd_proposal_detail
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.maintain_and_support
                                                                }
                                                                answerTestId={`proposal-maintain-and-support`}
                                                            />
                                                            <BudgetDiscussionInfoSegment
                                                                question='How will this proposal be maintained and
                                                                supported after initial development?'
                                                                answer={
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.bd_proposal_detail
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.maintain_and_support ||
                                                                    ''
                                                                }
                                                                answerTestId={
                                                                    'maintain-and-support-content'
                                                                }
                                                            />
                                                            <BudgetDiscussionInfoSegment
                                                                question={
                                                                    'Key Proposal Deliverable(s) and Definition of Done: What tangible milestones or outcomes are to be delivered and what will the community ultimately receive?'
                                                                }
                                                                answer={
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.bd_proposal_detail
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.key_proposal_deliverables
                                                                }
                                                                answerTestId={`proposal-milestone`}
                                                            />

                                                            <BudgetDiscussionInfoSegment
                                                                question={
                                                                    'Resourcing & Duration Estimates: Please provide estimates of team size and duration to achieve the Key Proposal Deliverables outlined above.'
                                                                }
                                                                answer={
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.bd_proposal_detail
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.resourcing_duration_estimates
                                                                }
                                                                answerTestId={`proposal-resources-&-duration-estimates`}
                                                            />

                                                            <BudgetDiscussionInfoSegment
                                                                question={
                                                                    'Experience: Please provide previous experience relevant to complete this project.'
                                                                }
                                                                answer={
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.bd_proposal_detail
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.experience
                                                                }
                                                                answerTestId={`project-experience`}
                                                            />

                                                            <BudgetDiscussionInfoSegment
                                                                question={
                                                                    'Contracting: Please describe how you expect to be contracted.'
                                                                }
                                                                answer={
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.bd_proposal_detail
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.contract_type_name
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.contract_type_name
                                                                }
                                                                answerTestId={`proposal-contracting`}
                                                            />
                                                            {selectedVersion
                                                                ?.attributes
                                                                ?.bd_proposal_detail
                                                                ?.data
                                                                ?.attributes
                                                                ?.contract_type_name
                                                                ?.data
                                                                ?.attributes
                                                                ?.contract_type_name ===
                                                                'Other' && (
                                                                <BudgetDiscussionInfoSegment
                                                                    question='Please describe what you have in mind.'
                                                                    answer={
                                                                        selectedVersion
                                                                            ?.attributes
                                                                            ?.bd_proposal_detail
                                                                            ?.data
                                                                            ?.attributes
                                                                            ?.other_contract_type
                                                                    }
                                                                    answerTestId={`other-contract-description`}
                                                                />
                                                            )}
                                                        </Box>

                                                        <Box
                                                            sx={{
                                                                mt: 4,
                                                            }}
                                                        >
                                                            <Typography
                                                                variant='title1'
                                                                component='h5'
                                                                sx={{
                                                                    mb: 2,
                                                                }}
                                                            >
                                                                Costing
                                                            </Typography>

                                                            <BudgetDiscussionInfoSegment
                                                                question={
                                                                    'ADA Amount'
                                                                }
                                                                answer={`₳ ${correctVoteAdaFormat(
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.bd_costing
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.ada_amount ||
                                                                        0
                                                                )}`}
                                                                answerTestId={`costing-amount`}
                                                            />

                                                            <BudgetDiscussionInfoSegment
                                                                question={
                                                                    'USD to ADA Conversion Rate'
                                                                }
                                                                answer={
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.bd_costing
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.usd_to_ada_conversion_rate
                                                                }
                                                                answerTestId={`costing-conversion-rate`}
                                                            />

                                                            <BudgetDiscussionInfoSegment
                                                                question={
                                                                    'Preferred currency'
                                                                }
                                                                answer={
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.bd_costing
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.preferred_currency
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.currency_name
                                                                }
                                                                answerTestId={`costing-preferred-currency`}
                                                            />

                                                            <BudgetDiscussionInfoSegment
                                                                question={
                                                                    'Amount in preferred currency'
                                                                }
                                                                answer={correctVoteAdaFormat(
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.bd_costing
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.amount_in_preferred_currency ||
                                                                        0
                                                                )}
                                                                answerTestId={`costing-preferred-currency-amount`}
                                                            />

                                                            <BudgetDiscussionInfoSegment
                                                                question={
                                                                    'Cost breakdown'
                                                                }
                                                                answer={
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.bd_costing
                                                                        ?.data
                                                                        ?.attributes
                                                                        ?.cost_breakdown
                                                                }
                                                                answerTestId={`cost-breakdown`}
                                                            />
                                                        </Box>

                                                        <Box
                                                            sx={{
                                                                mt: 4,
                                                            }}
                                                        >
                                                            <Typography
                                                                variant='title1'
                                                                component='h5'
                                                                sx={{
                                                                    mb: 2,
                                                                }}
                                                            >
                                                                Further
                                                                information
                                                            </Typography>
                                                            {selectedVersion
                                                                ?.attributes
                                                                ?.bd_further_information
                                                                ?.data
                                                                ?.attributes
                                                                ?.proposal_links
                                                                ?.length >
                                                                0 && (
                                                                <Box mt={2}>
                                                                    <Typography
                                                                        variant='body2'
                                                                        component='span'
                                                                        sx={{
                                                                            ...detailsLabelSx,
                                                                            display: 'block',
                                                                            color: (
                                                                                theme
                                                                            ) =>
                                                                                theme?.palette?.neutralGray,
                                                                        }}
                                                                    >
                                                                        Supporting
                                                                        links
                                                                    </Typography>

                                                                    <Box
                                                                        sx={{
                                                                            display: 'flex',
                                                                            flexWrap: 'wrap',
                                                                            columnGap: 3,
                                                                        }}
                                                                    >
                                                                        {selectedVersion?.attributes?.bd_further_information?.data?.attributes?.proposal_links?.map(
                                                                            (
                                                                                item,
                                                                                index
                                                                            ) =>
                                                                                item?.prop_link && (
                                                                                    <Button
                                                                                        key={
                                                                                            index
                                                                                        }
                                                                                        variant='text'
                                                                                        size='medium'
                                                                                        sx={{
                                                                                            px: 0,
                                                                                            minWidth: 0,
                                                                                            marginBottom: 2,
                                                                                            whiteSpace:
                                                                                                'normal',
                                                                                            height: 'auto',
                                                                                            minHeight: 36,
                                                                                        }}
                                                                                        startIcon={
                                                                                            <img src={ICONS.link} alt='' width={18} height={18} />
                                                                                        }
                                                                                        onClick={() =>
                                                                                            openLink(
                                                                                                item?.prop_link
                                                                                            )
                                                                                        }
                                                                                        data-testid={
                                                                                            'link-${index}-text-content'
                                                                                        }
                                                                                    >
                                                                                        <Typography
                                                                                            component={
                                                                                                'p'
                                                                                            }
                                                                                            variant='body2'
                                                                                            fontWeight={
                                                                                                400
                                                                                            }
                                                                                            style={{
                                                                                                margin: 0,
                                                                                                textOverflow:
                                                                                                    'ellipsis',
                                                                                                overflow:
                                                                                                    'hidden',
                                                                                                maxWidth:
                                                                                                    '600px',
                                                                                            }}
                                                                                            data-testid={`link-${index}-text-content`}
                                                                                        >
                                                                                            {
                                                                                                item?.prop_link_text
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
                                                                mt: 4,
                                                            }}
                                                        >
                                                            <Typography
                                                                variant='title1'
                                                                component='h5'
                                                                sx={{
                                                                    mb: 2,
                                                                }}
                                                            >
                                                                Administration
                                                                and Auditing
                                                            </Typography>

                                                            <BudgetDiscussionInfoSegment
                                                                question={
                                                                    'Would you like Intersect to be your named Administrator, including acting as the auditor, as per the Cardano Constitution?*'
                                                                }
                                                                answer={
                                                                    selectedVersion
                                                                        ?.attributes
                                                                        ?.intersect_named_administrator
                                                                        ? 'Yes'
                                                                        : 'No'
                                                                }
                                                                answerTestId={`include-as-auditor`}
                                                            />
                                                            {selectedVersion
                                                                ?.attributes
                                                                ?.intersect_named_administrator ? (
                                                                ''
                                                            ) : (
                                                                <BudgetDiscussionInfoSegment
                                                                    question='Please provide further information to help inform DReps. Who is the vendor and what services are they providing?'
                                                                    answer={
                                                                        selectedVersion
                                                                            ?.attributes
                                                                            ?.intersect_admin_further_text ||
                                                                        ''
                                                                    }
                                                                    answerTestId={
                                                                        'intersect-admin-further-text'
                                                                    }
                                                                />
                                                            )}
                                                        </Box>
                                                    </Box>
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

export default BudgetDiscussionReviewVersions;
