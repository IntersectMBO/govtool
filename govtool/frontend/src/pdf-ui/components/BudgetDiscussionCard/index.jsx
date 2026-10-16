'use client';

import { IconChatAlt } from '@intersect.mbo/intersectmbo.org-icons-set';
import InfoOutlinedIcon from '@mui/icons-material/InfoOutlined';
import { ICONS } from '@/consts/icons';
import { Box, IconButton, Menu } from '@mui/material';
import { Button, Tooltip, Typography } from '@atoms';
import { useState } from 'react';

import { Link } from 'react-router';
import { correctVoteAdaFormat, formatIsoDate } from '../../lib/utils';
import MarkdownTypography from '../../lib/markdownRenderer';
import { categoryName, categorySlug } from '../../lib/budgetArchive';
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
} from '../ProposalCard/cardStyles';

// Layout follows GovTool's GovernanceActionCard (see ProposalCard): a
// radius-20 shell, label/value elements, the dates box, and a white footer
// holding one full-width button. The share menu follows the Share molecule.
// Read-only: an archived 2025 budget proposal from the static list.

const ShareMenu = ({ masterId }) => {
    const [anchorEl, setAnchorEl] = useState(null);
    const [copied, setCopied] = useState(false);
    const open = Boolean(anchorEl);
    const link = `${window.location.origin}/budget_discussion/${masterId}`;

    return (
        <>
            <Tooltip paragraphOne='Share'>
                <IconButton
                    sx={shareButtonSx(open)}
                    aria-haspopup='true'
                    aria-expanded={open ? 'true' : undefined}
                    onClick={(event) => setAnchorEl(event.currentTarget)}
                    data-testid={`budget-discussion-${masterId}-share-button`}
                >
                    <img alt='' src={ICONS.share} width={24} height={24} />
                </IconButton>
            </Tooltip>
            <Menu
                anchorEl={anchorEl}
                open={open}
                onClose={() => setAnchorEl(null)}
                MenuListProps={{ sx: { p: 0 } }}
                slotProps={{ paper: { elevation: 2, sx: sharePaperSx } }}
                transformOrigin={{ horizontal: 'right', vertical: 'top' }}
                anchorOrigin={{ horizontal: 'right', vertical: 'bottom' }}
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
                            navigator.clipboard.writeText(link);
                            setCopied(true);
                            setTimeout(() => setCopied(false), 2000);
                        }}
                        color='primary'
                        disabled={copied}
                        data-testid='copy-link'
                        sx={shareCopyButtonSx(!copied)}
                    >
                        <img alt='' src={ICONS.link} width={24} height={24} />
                    </IconButton>
                    <Typography
                        variant='caption'
                        component={'p'}
                        sx={{ color: 'textBlack' }}
                        data-testid='copy-link-text'
                    >
                        {copied ? 'Link copied' : 'Click to copy link'}
                    </Typography>
                </Box>
            </Menu>
        </>
    );
};

const BudgetDiscussionCard = ({ budgetDiscussion }) => {
    const attributes = budgetDiscussion?.attributes;
    const masterId = attributes?.master_id;
    const slug = categorySlug(categoryName(budgetDiscussion));
    const costing = attributes?.bd_costing?.data?.attributes;
    const currency =
        costing?.preferred_currency?.data?.attributes?.currency_letter_code;
    const poll = budgetDiscussion?.archive?.poll;
    const username = attributes?.creator?.data?.attributes?.govtool_username;

    return (
        <Box sx={{ position: 'relative', height: '100%' }}>
            <Box
                sx={cardShellSx}
                data-testid={`budget-discussion-${slug}-card`}
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
                                data-testid='budget-discussion-title'
                            >
                                {
                                    attributes?.bd_proposal_detail?.data
                                        ?.attributes?.proposal_name
                                }
                            </Typography>
                            {username ? (
                                <Typography
                                    variant='body2'
                                    fontWeight={400}
                                    component={'h5'}
                                    sx={{ color: 'neutralGray' }}
                                    mt={0.5}
                                    data-testid='budget-discussion-creator'
                                >
                                    @{username}
                                </Typography>
                            ) : null}
                        </Box>
                        <ShareMenu masterId={masterId} />
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
                                {categoryName(budgetDiscussion)}
                            </Typography>
                        </Box>
                    </Box>
                    <Box sx={cardElementSx}>
                        <Typography component='p' sx={cardLabelSx}>
                            Proposal benefit
                        </Typography>
                        <Box
                            data-testid='proposal-benefit'
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
                                    attributes?.bd_psapb?.data?.attributes
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
                            data-testid='budget-requested-amount'
                        >
                            ₳ {correctVoteAdaFormat(costing?.ada_amount || 0)}
                        </Typography>
                        {currency && currency !== 'ADA' ? (
                            <Typography
                                component='p'
                                sx={{ ...cardValueSx, color: 'neutralGray' }}
                                data-testid='budget-requested-preferred-currency'
                            >
                                {correctVoteAdaFormat(
                                    costing?.amount_in_preferred_currency || 0
                                )}{' '}
                                {currency}
                            </Typography>
                        ) : null}
                    </Box>
                    <Box sx={cardDatesBoxSx}>
                        <Box sx={cardDatesRowSx}>
                            <Typography
                                variant='caption'
                                component='p'
                                sx={cardDatesTextSx}
                                data-testid='proposed-date-wrapper'
                            >
                                {`Proposed on: `}
                                <span
                                    data-testid='proposed-date'
                                    style={{ fontWeight: 600 }}
                                >
                                    {formatIsoDate(
                                        attributes?.master_proposal_created_at
                                    )}
                                </span>
                            </Typography>
                            <Tooltip paragraphOne={'Proposal Date'}>
                                <Box display={'flex'} alignItems={'center'}>
                                    <InfoOutlinedIcon sx={cardInfoIconSx} />
                                </Box>
                            </Tooltip>
                        </Box>
                    </Box>
                    <Box
                        display={'flex'}
                        alignItems={'center'}
                        justifyContent={'space-between'}
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
                                        data-testid={`budget-discussion-${masterId}-comment-count`}
                                    >
                                        {attributes?.prop_comments_number || 0}
                                    </Box>
                                </IconButton>
                            </Box>
                        </Tooltip>
                        {poll ? (
                            <Tooltip paragraphOne='Final DRep poll totals'>
                                <Typography
                                    variant='caption'
                                    component='p'
                                    sx={{ color: 'neutralGray' }}
                                    data-testid={`budget-discussion-${masterId}-poll-totals`}
                                >
                                    Poll: Yes {poll.yes} · No {poll.no}
                                </Typography>
                            </Tooltip>
                        ) : null}
                    </Box>
                </Box>
                <Box sx={cardFooterSx}>
                    <Link
                        to={`/budget_discussion/${masterId}`}
                        data-testid={`budget-discussion-${slug}-view-details-link-wrapper`}
                        style={{ display: 'block', textDecoration: 'none' }}
                    >
                        <Button
                            variant='contained'
                            size='large'
                            data-testid={`budget-discussion-${slug}-view-details`}
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
                </Box>
            </Box>
        </Box>
    );
};

export default BudgetDiscussionCard;
