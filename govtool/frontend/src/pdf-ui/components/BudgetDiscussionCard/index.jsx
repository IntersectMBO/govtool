'use client';

import { IconChatAlt } from '@intersect.mbo/intersectmbo.org-icons-set';
import InfoOutlinedIcon from '@mui/icons-material/InfoOutlined';
import { ICONS } from '@/consts/icons';
import { Box, Chip, IconButton, Menu } from '@mui/material';
import { Button, Tooltip, Typography } from '@atoms';
import { useEffect, useState } from 'react';

import { Link, useNavigate } from 'react-router';
import { useAppContext } from '../../context/context';
import { correctVoteAdaFormat, formatIsoDate } from '../../lib/utils';
import CreateBudgetDiscussionDialog from '../CreateBudgetDiscussionDialog';
import MarkdownTypography from '../../lib/markdownRenderer';
import {
    cardBodySx,
    cardDatesBoxSx,
    cardDatesRowSx,
    cardDatesTextSx,
    cardElementSx,
    cardFooterSx,
    cardInfoIconSx,
    cardLabelSx,
    cardPillSx,
    cardPillTextSx,
    cardShellSx,
    cardTitleSx,
    cardValueSx,
    commentCountSx,
    shareButtonSx,
    shareContentSx,
    shareCopyButtonSx,
    sharePaperSx,
    statusChipColors,
    statusChipSx,
} from '../ProposalCard/cardStyles';

// Layout follows GovTool's GovernanceActionCard (see ProposalCard): a
// radius-20 shell, label/value elements, the dates box, and a white footer
// holding one full-width button. The share menu follows the Share molecule.

const BudgetDiscussionCard = ({
    budgetDiscussion,
    isDraft = false,
    startEdittinButtonClick = false,
    setShouldRefresh = false,
    startEdittingDraft,
}) => {
    const { user } = useAppContext();
    const navigate = useNavigate();

    const [openEditDialog, setOpenEditDialog] = useState(false);

    const [budgetDiscussionLink, setBudgetDiscussionLink] = useState('');

    useEffect(() => {
        let domain = new URL(window.location.href);
        let origin = domain.origin;
        setBudgetDiscussionLink(`${origin}/budget_discussion/`);
    }, [budgetDiscussionLink]);

    const handleEditProposal = () => {
        // Open edit modal
        setOpenEditDialog(true);
    };

    const handleCloseEditDialog = () => {
        // Close edit modal
        setOpenEditDialog(false);
    };

    const CardContentComponent = ({ budgetDiscussion }) => {
        const disableShareClick = () => {
            setDisableShare(true);
            setTimeout(() => {
                setDisableShare(false);
            }, 2000);
        };

        function copyToClipboard(value) {
            navigator.clipboard.writeText(value);
        }

        const [shareAnchorEl, setShareAnchorEl] = useState(null);
        const [disableShare, setDisableShare] = useState(false);
        const openShare = Boolean(shareAnchorEl);
        const handleShareClick = (event) => {
            setShareAnchorEl(event.currentTarget);
        };

        const handleShareClose = () => {
            setShareAnchorEl(null);
        };
        return (
            <Box
                sx={cardShellSx}
                data-testid={
                    isDraft
                        ? `draft-` +
                          `${budgetDiscussion?.attributes?.master_id}` +
                          `-proposal`
                        : `budget-discussion-` +
                          (budgetDiscussion?.attributes?.bd_psapb?.data
                              ?.attributes?.type_name?.data?.attributes
                              ?.type_name == 'None of these'
                              ? 'no-category'
                              : budgetDiscussion?.attributes?.bd_psapb?.data?.attributes?.type_name?.data?.attributes?.type_name
                                    .replace(/\s+/g, '-')
                                    .toLowerCase()) +
                          `-card`
                }
            >
                <Box sx={cardBodySx}>
                    <Box
                        sx={{
                            display: 'flex',
                            alignItems: 'flex-start',
                            justifyContent: 'space-between',
                            gap: 1,
                            mb: '20px',
                        }}
                    >
                        <Box sx={{ minWidth: 0 }}>
                            <Typography
                                component='h3'
                                sx={cardTitleSx}
                                data-testid={
                                    isDraft
                                        ? `draft-title`
                                        : `budget-discussion-title`
                                }
                            >
                                {isDraft
                                    ? budgetDiscussion?.attributes?.draft_data
                                          ?.bd_proposal_detail?.proposal_name
                                    : budgetDiscussion?.attributes
                                          ?.bd_proposal_detail?.data?.attributes
                                          ?.proposal_name}
                            </Typography>
                            {budgetDiscussion?.attributes?.creator?.data
                                ?.attributes?.govtool_username ? (
                                <Typography
                                    variant='body2'
                                    fontWeight={400}
                                    component={'h5'}
                                    sx={{ color: 'neutralGray' }}
                                    mt={0.5}
                                    data-testid={
                                        isDraft
                                            ? `draft-creator`
                                            : `budget-discussion-creator`
                                    }
                                >
                                    @
                                    {budgetDiscussion?.attributes?.creator?.data
                                        ?.attributes?.govtool_username || ''}
                                </Typography>
                            ) : null}
                        </Box>
                        {isDraft ? null : (
                            <>
                                <Tooltip paragraphOne='Share'>
                                    <IconButton
                                        id='share-button-card'
                                        sx={shareButtonSx(openShare)}
                                        aria-controls={
                                            openShare
                                                ? 'share-menu-card'
                                                : undefined
                                        }
                                        aria-haspopup='true'
                                        aria-expanded={
                                            openShare ? 'true' : undefined
                                        }
                                        onClick={handleShareClick}
                                        data-testid={`budget-discussion-${budgetDiscussion.id}-share-button`}
                                    >
                                        <img
                                            alt=''
                                            src={ICONS.share}
                                            width={24}
                                            height={24}
                                        />
                                    </IconButton>
                                </Tooltip>
                                <Menu
                                    id='share-menu-card'
                                    anchorEl={shareAnchorEl}
                                    open={openShare}
                                    onClose={handleShareClose}
                                    MenuListProps={{
                                        'aria-labelledby': 'share-button-card',
                                        sx: { p: 0 },
                                    }}
                                    slotProps={{
                                        paper: {
                                            elevation: 2,
                                            sx: sharePaperSx,
                                        },
                                    }}
                                    transformOrigin={{
                                        horizontal: 'right',
                                        vertical: 'top',
                                    }}
                                    anchorOrigin={{
                                        horizontal: 'right',
                                        vertical: 'bottom',
                                    }}
                                >
                                    <Box sx={shareContentSx}>
                                        <Typography
                                            component={'p'}
                                            sx={{ alignSelf: 'flex-start' }}
                                        >
                                            Share
                                        </Typography>
                                        <IconButton
                                            onClick={() => {
                                                copyToClipboard(
                                                    `${budgetDiscussionLink}${budgetDiscussion?.attributes?.master_id}`
                                                ),
                                                    disableShareClick();
                                            }}
                                            color='primary'
                                            disabled={disableShare}
                                            data-testid='copy-link'
                                            sx={shareCopyButtonSx(
                                                !disableShare
                                            )}
                                        >
                                            <img
                                                alt=''
                                                src={ICONS.link}
                                                width={24}
                                                height={24}
                                            />
                                        </IconButton>
                                        <Typography
                                            variant='caption'
                                            component={'p'}
                                            sx={{ color: 'textBlack' }}
                                            data-testid='copy-link-text'
                                        >
                                            {disableShare
                                                ? 'Link copied'
                                                : 'Click to copy link'}
                                        </Typography>
                                    </Box>
                                </Menu>
                            </>
                        )}
                    </Box>
                    <Box sx={cardElementSx}>
                        <Typography component={'p'} sx={cardLabelSx}>
                            Budget category
                        </Typography>
                        <Box sx={cardPillSx}>
                            <Typography
                                variant='caption'
                                component='p'
                                sx={cardPillTextSx}
                                data-testid='budget-discussion-type'
                            >
                                {budgetDiscussion?.attributes?.bd_psapb?.data
                                    ?.attributes?.type_name?.data?.attributes
                                    ?.type_name || ''}
                            </Typography>
                        </Box>
                    </Box>
                    <Box sx={cardElementSx}>
                        <Typography component='p' sx={cardLabelSx}>
                            Proposal benefit
                        </Typography>
                        <Box
                            data-testid={
                                isDraft
                                    ? `draft-proposal-benefit`
                                    : 'proposal-benefit'
                            }
                            sx={{
                                display: '-webkit-box',
                                WebkitBoxOrient: 'vertical',
                                WebkitLineClamp: 3,
                                overflow: 'hidden',
                                textOverflow: 'ellipsis',
                                maxHeight: '60px',
                                '& p, & li, & span, & h1, & h2, & h3, & h4, & h5, & h6':
                                    {
                                        fontSize: 14,
                                        lineHeight: '20px',
                                        fontWeight: 400,
                                        m: 0,
                                    },
                            }}
                        >
                            <MarkdownTypography
                                content={
                                    isDraft
                                        ? budgetDiscussion?.attributes
                                              ?.draft_data?.bd_psapb
                                              ?.proposal_benefit
                                        : budgetDiscussion?.attributes
                                              ?.bd_psapb?.data?.attributes
                                              ?.proposal_benefit
                                }
                            />
                        </Box>
                    </Box>
                    <Box sx={cardElementSx}>
                        <Typography component='p' sx={cardLabelSx}>
                            Budget Requested
                        </Typography>
                        <Typography
                            component='p'
                            sx={{ ...cardValueSx, fontWeight: 600 }}
                            data-testid={
                                isDraft
                                    ? 'draft-budget-requested'
                                    : 'budget-requested-amount'
                            }
                        >
                            ₳{' '}
                            {correctVoteAdaFormat(
                                isDraft
                                    ? budgetDiscussion?.attributes?.draft_data
                                          ?.bd_costing?.ada_amount
                                    : budgetDiscussion?.attributes?.bd_costing
                                          ?.data?.attributes?.ada_amount || 0
                            )}
                        </Typography>
                    </Box>
                    <Box sx={cardDatesBoxSx}>
                        <Box sx={cardDatesRowSx}>
                            <Typography
                                variant='caption'
                                component='p'
                                sx={cardDatesTextSx}
                                data-testid={
                                    isDraft
                                        ? 'not-submitted-text'
                                        : 'proposed-date-wrapper'
                                }
                            >
                                {isDraft ? 'Not submitted' : `Proposed on: `}
                                {!isDraft && (
                                    <span
                                        data-testid='proposed-date'
                                        style={{ fontWeight: 600 }}
                                    >
                                        {formatIsoDate(
                                            budgetDiscussion?.attributes
                                                ?.master_proposal_created_at
                                        )}
                                    </span>
                                )}
                            </Typography>
                            <Tooltip paragraphOne={'Proposal Date'}>
                                <Box display={'flex'} alignItems={'center'}>
                                    <InfoOutlinedIcon sx={cardInfoIconSx} />
                                </Box>
                            </Tooltip>
                        </Box>
                    </Box>
                    {isDraft ? null : (
                        <Box
                            display={'flex'}
                            alignItems={'center'}
                            gap={1}
                            mt={'auto'}
                            mb={2}
                        >
                            <Tooltip paragraphOne={'Comments Number'}>
                                <Box display={'flex'} alignItems={'center'}>
                                    <IconButton
                                        disabled={true}
                                        sx={{
                                            borderRadius: 50,
                                            px: 1,
                                            '&.Mui-disabled': {
                                                color: 'textBlack',
                                            },
                                        }}
                                    >
                                        <IconChatAlt width={20} height={20} />
                                        <Box
                                            component='span'
                                            aria-label='comments'
                                            sx={commentCountSx}
                                            data-testid={`budget-discussion-${budgetDiscussion?.id}-comment-count`}
                                        >
                                            {budgetDiscussion?.attributes
                                                ?.prop_comments_number || 0}
                                        </Box>
                                    </IconButton>
                                </Box>
                            </Tooltip>
                            {user &&
                                user?.user?.id?.toString() ===
                                    budgetDiscussion?.attributes?.creator?.data?.id?.toString() &&
                                budgetDiscussion?.attributes
                                    ?.submitted_for_vote == null && (
                                    <Tooltip paragraphOne='Edit'>
                                        <IconButton
                                            aria-label='edit'
                                            onClick={handleEditProposal}
                                            data-testid={`budget-proposals-${budgetDiscussion?.attributes?.master_id}-edit-button`}
                                        >
                                            <img
                                                src={ICONS.editIcon}
                                                alt=''
                                                width={24}
                                                height={24}
                                            />
                                        </IconButton>
                                    </Tooltip>
                                )}
                        </Box>
                    )}
                </Box>
                <Box sx={cardFooterSx}>
                    {isDraft ? (
                        <Button
                            variant='contained'
                            size='large'
                            fullWidth
                            sx={{
                                whiteSpace: 'normal',
                                height: 'auto',
                                minHeight: 40,
                            }}
                            onClick={() => startEdittingDraft(budgetDiscussion)}
                            data-testid={`draft-start-editing`}
                            //`draft-`+budgetDiscussion?.id+`-start-editing`
                        >
                            Start Editing
                        </Button>
                    ) : (
                        <Link
                            to={`/budget_discussion/${budgetDiscussion?.attributes?.master_id}`}
                            data-testid={
                                `budget-discussion-` +
                                (budgetDiscussion?.attributes?.bd_psapb?.data
                                    ?.attributes?.type_name?.data?.attributes
                                    ?.type_name == 'None of these'
                                    ? 'no-category'
                                    : budgetDiscussion?.attributes?.bd_psapb?.data?.attributes?.type_name?.data?.attributes?.type_name
                                          .replace(/\s+/g, '-')
                                          .toLowerCase()) +
                                `-view-details-link-wrapper`
                            }
                            style={{ display: 'block', textDecoration: 'none' }}
                        >
                            <Button
                                variant='contained'
                                size='large'
                                data-testid={
                                    `budget-discussion-` +
                                    (budgetDiscussion?.attributes?.bd_psapb
                                        ?.data?.attributes?.type_name?.data
                                        ?.attributes?.type_name ==
                                    'None of these'
                                        ? 'no-category'
                                        : budgetDiscussion?.attributes?.bd_psapb?.data?.attributes?.type_name?.data?.attributes?.type_name
                                              .replace(/\s+/g, '-')
                                              .toLowerCase()) +
                                    `-view-details`
                                }
                                fullWidth
                                sx={{
                                    whiteSpace: 'normal',
                                    height: 'auto',
                                    minHeight: 40,
                                }}
                            >
                                View Details
                            </Button>
                        </Link>
                    )}
                </Box>
            </Box>
        );
    };

    return isDraft ? (
        <div
            style={{
                position: 'relative',
                height: '100%',
            }}
        >
            <Chip
                label='Draft'
                aria-label='draft-status-badge'
                sx={{ ...statusChipSx, ...statusChipColors.draft }}
            />
            <CardContentComponent budgetDiscussion={budgetDiscussion} />
        </div>
    ) : (
        <div
            style={{
                position: 'relative',
                height: '100%',
            }}
        >
            <CardContentComponent budgetDiscussion={budgetDiscussion} />

            {openEditDialog ? (
                <CreateBudgetDiscussionDialog
                    open={openEditDialog}
                    onClose={handleCloseEditDialog}
                    current_bd_id={budgetDiscussion?.attributes?.master_id}
                />
            ) : null}
        </div>
    );
};

export default BudgetDiscussionCard;
