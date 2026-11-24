'use client';

import { useState } from 'react';
import { Link } from 'react-router';
import { Box, ButtonBase } from '@mui/material';
import { Info, MessageCircle, Pencil, TrendingUp } from 'lucide-react';

import { ShareDialog } from '@/components/organisms/DRepDashboard/ShareDRepDialog';
import { ds, PILL_TONES, RADIUS, SHADOWS } from '@/consts/designTokens';
import { removeMarkdown } from '@/utils/removeMarkdown';
import { useAppContext } from '../../context/context';
import { supportPercent } from '../../lib/proposalList';
import { formatIsoDate } from '../../lib/utils';
import EditProposalDialog from '../EditProposalDialog';

const DETAILS_PATH = '/proposals';

const focusRing = {
    '&.Mui-focusVisible, &:focus-visible': {
        outline: `2px solid ${ds.primary['600']}`,
        outlineOffset: 2,
    },
};

const tagSx = (tone) => ({
    backgroundColor: PILL_TONES[tone].bg,
    borderRadius: `${RADIUS.xs}px`,
    color: PILL_TONES[tone].text,
    fontSize: 11,
    fontWeight: 500,
    lineHeight: '16px',
    px: 0.75,
    py: 0.25,
});

const iconButtonSx = {
    borderRadius: `${RADIUS.sm}px`,
    color: ds.gray['800'],
    height: 32,
    width: 32,
    '&:hover': { backgroundColor: ds.gray['100'] },
    ...focusRing,
};

const Stat = ({ label, children, testId }) => (
    <Box sx={{ minWidth: 0 }}>
        <Box
            component='p'
            sx={{
                color: ds.gray['600'],
                fontSize: 11,
                fontWeight: 500,
                letterSpacing: '0.2px',
                m: 0,
                textTransform: 'uppercase',
            }}
        >
            {label}
        </Box>
        <Box
            data-testid={testId}
            sx={{
                alignItems: 'center',
                color: ds.gray['900'],
                display: 'flex',
                fontSize: 14,
                fontWeight: 500,
                gap: 0.75,
                mt: 0.5,
                whiteSpace: 'nowrap',
            }}
        >
            {children}
        </Box>
    </Box>
);

// One proposal as a row: its type and status, date and share, title,
// abstract, proposer, discussion and support. The test ids are the old
// card's, which the forum E2E specs read.
const ProposalRow = ({ proposal, setShouldRefresh = () => {} }) => {
    const { user } = useAppContext();
    const [openEditDialog, setOpenEditDialog] = useState(false);

    const id = proposal?.id;
    const attributes = proposal?.attributes;
    const content = attributes?.content?.attributes;
    const typeName = content?.gov_action_type?.attributes?.gov_action_type_name;
    const isSubmitted = Boolean(content?.prop_submitted);
    const isOwner =
        Boolean(user) &&
        user?.user?.id?.toString() === attributes?.user_id?.toString();
    const comments = attributes?.prop_comments_number || 0;
    const support = supportPercent(
        attributes?.prop_likes,
        attributes?.prop_dislikes
    );
    const username = attributes?.user_govtool_username;
    const detailsPath = `${DETAILS_PATH}/${id}`;


    return (
        <Box
            component='article'
            data-testid={`proposal-${typeName ? typeName.toLowerCase() : ''}-card`}
            sx={{
                backgroundColor: '#FFFFFF',
                border: `1px solid ${ds.gray['200']}`,
                borderRadius: `${RADIUS.lg}px`,
                boxShadow: SHADOWS.card,
                display: 'flex',
                flexDirection: 'column',
                gap: 1.25,
                px: { xxs: 2, md: 2.5 },
                py: { xxs: 2, md: 2.25 },
            }}
        >
            <Box
                sx={{
                    alignItems: 'center',
                    display: 'flex',
                    flexWrap: 'wrap',
                    gap: 1,
                    justifyContent: 'space-between',
                }}
            >
                <Box sx={{ alignItems: 'center', display: 'flex', gap: 0.75 }}>
                    <Box
                        component='span'
                        data-testid='governance-action-type'
                        sx={{ ...tagSx('neutral'), textTransform: 'uppercase' }}
                    >
                        {typeName}
                    </Box>
                    <Box
                        component='span'
                        data-testid={`proposal-${id}-status`}
                        sx={tagSx(isSubmitted ? 'primary' : 'success')}
                    >
                        {isSubmitted ? 'Submitted for vote' : 'Active'}
                    </Box>
                </Box>
                <Box sx={{ alignItems: 'center', display: 'flex', gap: 0.5 }}>
                    <Box
                        component='span'
                        data-testid='proposed-date-wrapper'
                        sx={{
                            alignItems: 'center',
                            backgroundColor: ds.gray['100'],
                            borderRadius: `${RADIUS.pill}px`,
                            color: ds.gray['700'],
                            display: 'inline-flex',
                            fontSize: 12,
                            gap: 0.5,
                            px: 1,
                            py: 0.25,
                        }}
                    >
                        <Info aria-hidden size={13} />
                        Proposed on:{' '}
                        <span data-testid='proposed-date'>
                            {formatIsoDate(attributes?.createdAt)}
                        </span>
                    </Box>
                    {isOwner && !isSubmitted && (
                        <ButtonBase
                            aria-label={`Edit ${content?.prop_name ?? 'proposal'}`}
                            data-testid={`proposal-${id}-edit-button`}
                            onClick={() => setOpenEditDialog(true)}
                            sx={iconButtonSx}
                        >
                            <Pencil aria-hidden size={16} />
                        </ButtonBase>
                    )}
                    <ShareDialog
                        buttonTestId={`proposal-${id}-share-button`}
                        copyTestId={`proposal-${id}-share-copy`}
                        kind='proposal'
                        link={`${window.location.origin}${detailsPath}`}
                    />
                </Box>
            </Box>
            <Box component='h3' sx={{ m: 0 }}>
                <Box
                    component={Link}
                    data-testid={`proposal-${id}-view-details`}
                    to={detailsPath}
                    sx={{
                        borderRadius: `${RADIUS.xs}px`,
                        color: ds.gray['900'],
                        fontSize: 18,
                        fontWeight: 600,
                        lineHeight: '26px',
                        textDecoration: 'none',
                        wordBreak: 'break-word',
                        '&:hover': {
                            color: ds.primary['700'],
                            textDecoration: 'underline',
                        },
                        ...focusRing,
                    }}
                >
                    <span data-testid={`proposal-${id}-title`}>
                        {content?.prop_name}
                    </span>
                </Box>
            </Box>
            {content?.prop_abstract && (
                <Box
                    component='p'
                    data-testid={`proposal-${id}-abstract-content`}
                    sx={{
                        color: ds.gray['600'],
                        display: '-webkit-box',
                        fontSize: 14,
                        lineHeight: '20px',
                        m: 0,
                        overflow: 'hidden',
                        WebkitBoxOrient: 'vertical',
                        WebkitLineClamp: 2,
                        wordBreak: 'break-word',
                    }}
                >
                    {removeMarkdown(content.prop_abstract)}
                </Box>
            )}
            <Box
                sx={{
                    display: 'grid',
                    gap: { xxs: 1.5, sm: 0 },
                    gridTemplateColumns: {
                        xxs: 'repeat(2, minmax(0, 1fr))',
                        sm: 'repeat(3, max-content)',
                    },
                    mt: 0.5,
                    '& > *': {
                        borderLeft: { sm: `1px solid ${ds.gray['200']}` },
                        px: { sm: 3 },
                    },
                    '& > *:first-of-type': { borderLeft: 'none', pl: 0 },
                }}
            >
                <Stat label='Proposer'>
                    <Box
                        aria-hidden
                        sx={{
                            alignItems: 'center',
                            backgroundColor: ds.primary['50'],
                            borderRadius: '50%',
                            color: ds.primary['700'],
                            display: 'flex',
                            flexShrink: 0,
                            fontSize: 11,
                            fontWeight: 600,
                            height: 20,
                            justifyContent: 'center',
                            width: 20,
                        }}
                    >
                        {(username || '?').charAt(0).toUpperCase()}
                    </Box>
                    <Box
                        component='span'
                        sx={{ overflow: 'hidden', textOverflow: 'ellipsis' }}
                    >
                        {username || 'Unknown'}
                    </Box>
                </Stat>
                <Stat label='Discussions' testId={`proposal-${id}-comment-count`}>
                    <MessageCircle aria-hidden color={ds.orange['700']} size={15} />
                    {`${comments} ${comments === 1 ? 'comment' : 'comments'}`}
                </Stat>
                <Stat label='Support' testId={`proposal-${id}-support`}>
                    <TrendingUp aria-hidden color={ds.success['700']} size={15} />
                    {support === null ? 'No votes yet' : `${support}%`}
                </Stat>
            </Box>
            {openEditDialog && (
                <EditProposalDialog
                    proposal={proposal}
                    openEditDialog={openEditDialog}
                    handleCloseEditDialog={() => setOpenEditDialog(false)}
                    setMounted={() => {}}
                    onUpdate={() => setShouldRefresh(true)}
                    setShouldRefresh={setShouldRefresh}
                />
            )}
        </Box>
    );
};

export default ProposalRow;
