'use client';

import { IconChatAlt } from '@intersect.mbo/intersectmbo.org-icons-set';
import InfoOutlinedIcon from '@mui/icons-material/InfoOutlined';
import { ICONS } from '@/consts/icons';
import { Box, Chip, IconButton, Menu } from '@mui/material';
import { Button, Tooltip, Typography } from '@atoms';
import { useEffect, useState } from 'react';

import { Link, useNavigate } from 'react-router';
import { useAppContext } from '../../context/context';
import { formatIsoDate } from '../../lib/utils';
import EditProposalDialog from '../EditProposalDialog';
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
    commentCountSx,
    shareButtonSx,
    shareContentSx,
    shareCopyButtonSx,
    sharePaperSx,
    statusChipColors,
    statusChipSx,
} from './cardStyles';

// Layout follows GovTool's GovernanceActionCard: a radius-20 shell with the
// #DDE3F5 shadow, label/value elements, the dates box, and a white footer
// holding one full-width button. The share menu follows the Share molecule.

const ProposalCard = ({
    proposal,
    startEdittinButtonClick = false,
    setShouldRefresh = false,
}) => {
    const { user } = useAppContext();
    const navigate = useNavigate();

    const [openEditDialog, setOpenEditDialog] = useState(false);

    const [proposalLink, setProposalLink] = useState('');

    useEffect(() => {
        let domain = new URL(window.location.href);
        let origin = domain.origin;
        setProposalLink(`${origin}/proposal_discussion/`);
    }, [proposalLink]);

    const handleEditProposal = () => {
        setOpenEditDialog(true);
    };

    const handleCloseEditDialog = () => {
        setOpenEditDialog(false);
    };

    const CardStatusChip = ({ label, status, ...rest }) => (
        <Chip
            label={label}
            sx={{ ...statusChipSx, ...statusChipColors[status] }}
            {...rest}
        />
    );

    const CardContentComponent = ({ proposal }) => {
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
            <Box sx={cardShellSx}>
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
                                data-testid={`proposal-${proposal?.id}-title`}
                            >
                                {
                                    proposal?.attributes?.content?.attributes
                                        ?.prop_name
                                }
                            </Typography>
                            <Typography
                                variant='body2'
                                fontWeight={400}
                                component={'h5'}
                                sx={{ color: 'neutralGray' }}
                                mt={0.5}
                            >
                                @{proposal?.attributes?.user_govtool_username}
                            </Typography>
                        </Box>
                        {proposal?.attributes?.content?.attributes
                            ?.is_draft ? null : (
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
                                        data-testid={`proposal-${proposal?.id}-share-button`}
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
                                                    `${proposalLink}${proposal?.id}`
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
                        <Typography component='p' sx={cardLabelSx}>
                            Abstract
                        </Typography>
                        <Box
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
                                    proposal?.attributes?.content?.attributes
                                        ?.prop_abstract || ''
                                }
                                testId={`proposal-${proposal?.id}-abstract-content`}
                            />
                        </Box>
                    </Box>
                    <Box sx={cardElementSx}>
                        <Typography component='p' sx={cardLabelSx}>
                            Governance Action Type
                        </Typography>
                        <Box sx={cardPillSx}>
                            <Typography
                                variant='caption'
                                component='p'
                                sx={cardPillTextSx}
                                data-testid='governance-action-type'
                            >
                                {
                                    proposal?.attributes?.content?.attributes
                                        ?.gov_action_type?.attributes
                                        ?.gov_action_type_name
                                }
                            </Typography>
                        </Box>
                    </Box>
                    <Box sx={cardDatesBoxSx}>
                        <Box sx={cardDatesRowSx}>
                            <Typography
                                variant='caption'
                                component='p'
                                sx={cardDatesTextSx}
                                data-testid={
                                    proposal?.attributes?.content?.attributes
                                        ?.is_draft
                                        ? 'not-submitted-text'
                                        : 'proposed-date-wrapper'
                                }
                            >
                                {proposal?.attributes?.content?.attributes
                                    ?.is_draft
                                    ? 'Not submitted'
                                    : `Proposed on: `}
                                {!proposal?.attributes?.content?.attributes
                                    ?.is_draft && (
                                    <span
                                        data-testid='proposed-date'
                                        style={{ fontWeight: 600 }}
                                    >
                                        {formatIsoDate(
                                            proposal?.attributes?.createdAt
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
                    {proposal?.attributes?.content?.attributes
                        ?.is_draft ? null : (
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
                                        data-testid={`proposal-${proposal?.id}-comment-count`}
                                        disabled={true}
                                        aria-label='comments'
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
                                            sx={commentCountSx}
                                        >
                                            {proposal?.attributes
                                                ?.prop_comments_number || 0}
                                        </Box>
                                    </IconButton>
                                </Box>
                            </Tooltip>
                            {user &&
                                user?.user?.id?.toString() ===
                                    proposal?.attributes?.user_id?.toString() &&
                                !proposal?.attributes?.content?.attributes
                                    ?.prop_submitted && (
                                    <Tooltip paragraphOne='Edit'>
                                        <IconButton
                                            aria-label='edit'
                                            onClick={handleEditProposal}
                                            data-testid={`proposal-${proposal?.id}-edit-button`}
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
                    {proposal?.attributes?.content?.attributes?.is_draft ? (
                        <Button
                            variant='contained'
                            size='large'
                            fullWidth
                            sx={{
                                whiteSpace: 'normal',
                                height: 'auto',
                                minHeight: 40,
                            }}
                            onClick={() => startEdittinButtonClick(proposal)}
                            data-testid={`draft-${proposal?.id}-start-editing`}
                        >
                            Start Editing
                        </Button>
                    ) : (
                        <Link
                            to={`/proposal_discussion/${proposal?.id}`}
                            data-testid={`proposal-${proposal?.id}-view-details-link-wrapper`}
                            style={{ display: 'block', textDecoration: 'none' }}
                        >
                            <Button
                                variant='contained'
                                size='large'
                                data-testid={`proposal-${proposal?.id}-view-details`}
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

    return proposal?.attributes?.content?.attributes?.is_draft ? (
        <div
            data-testid={
                proposal?.attributes?.content?.attributes?.is_draft
                    ? `draft-${proposal?.id}-card`
                    : `proposal-${
                          proposal?.attributes?.content?.attributes
                              ?.gov_action_type?.attributes
                              ?.gov_action_type_name
                              ? proposal?.attributes?.content?.attributes?.gov_action_type?.attributes?.gov_action_type_name?.toLowerCase()
                              : ''
                      }-card`
            }
            style={{
                position: 'relative',
                height: '100%',
            }}
        >
            <CardStatusChip
                label='Draft'
                status='draft'
                aria-label='draft-status-badge'
                data-testid={`proposal-${proposal?.id}-status`}
            />
            <CardContentComponent proposal={proposal} />
        </div>
    ) : (
        <div
            data-testid={
                proposal?.attributes?.content?.attributes?.is_draft
                    ? `draft-${proposal?.id}-card`
                    : `proposal-${
                          proposal?.attributes?.content?.attributes
                              ?.gov_action_type?.attributes
                              ?.gov_action_type_name
                              ? proposal?.attributes?.content?.attributes?.gov_action_type?.attributes?.gov_action_type_name?.toLowerCase()
                              : ''
                      }-card`
            }
            style={{
                position: 'relative',
                height: '100%',
            }}
        >
            <CardStatusChip
                label={
                    proposal?.attributes?.content?.attributes?.prop_submitted
                        ? 'Submitted for vote'
                        : 'Active'
                }
                status={
                    proposal?.attributes?.content?.attributes?.prop_submitted
                        ? 'submitted'
                        : 'active'
                }
                aria-label='status-badge'
                data-testid={`proposal-${proposal?.id}-status`}
            />
            <CardContentComponent proposal={proposal} />

            {openEditDialog && (
                <EditProposalDialog
                    proposal={proposal}
                    openEditDialog={openEditDialog}
                    handleCloseEditDialog={handleCloseEditDialog}
                    setMounted={() => {}}
                    onUpdate={() =>
                        navigate(`/proposal_discussion/${proposal?.id}`)
                    }
                    setShouldRefresh={setShouldRefresh}
                />
            )}
        </div>
    );
};

export default ProposalCard;
