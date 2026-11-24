import { useEffect, useState, useRef } from 'react';
import { Link as RouterLink, useNavigate } from 'react-router';
import { Box, ButtonBase, Menu, MenuItem } from '@mui/material';
import {
    ArrowRight,
    ArrowUpDown,
    Check,
    ChevronRight,
    EllipsisVertical,
    ExternalLink,
    MessageCircle,
    Pencil,
    Reply,
    ThumbsDown,
    ThumbsUp,
    Trash2,
} from 'lucide-react';
import { Button, Tooltip, Typography } from '@atoms';
import { ShareDialog } from '@/components/organisms/DRepDashboard/ShareDRepDialog';
import {
    Card,
    CardTitle,
    Divider,
    HEADER_TINTS,
    MonoTag,
} from '@/components/organisms/OutcomeDetails/parts';
import {
    ds,
    FONT_FAMILY,
    PILL_TONES,
    RADIUS,
    SHADOWS,
} from '@/consts/designTokens';
import {
    CommentCard,
    EditProposalDialog,
    Poll,
    ReviewVersions,
    ProposalSubmissionDialog,
    DeleteProposalModal,
} from '../../../components';
import { useAppContext } from '../../../context/context';
import {
    createComment,
    createProposalLikeOrDislike,
    deleteProposal,
    getComments,
    getSingleProposal,
    getUserProposalVote,
    updateProposalLikesOrDislikes,
    createPoll,
    getPolls,
} from '../../../lib/api';
import {
    formatIsoDate,
    GA_SUBMISSION_FEE_ADA,
    getGovActionDepositAda,
    getLovelaceFromCborBalance,
    LOVELACE,
    openInNewTab,
} from '../../../lib/utils';
import ProposalOwnModal from '../../../components/ProposalOwnModal';
import ReactMarkdown from 'react-markdown';
import {
    checkIfDrepIsSignedIn,
    checkShowValidation,
    loginUserToApp,
} from '../../../lib/helpers';
import MarkdownTypography from '../../../lib/markdownRenderer';
import UserValidation from '../../../components/UserValidation/UserValidation';
import { PdfModal } from '../../../components/PdfModal';
import { PdfTextArea } from '../../../components/PdfFields';
import { GOV_ACTION_HISTORY_PATHS } from '@/consts/paths';
import {
    getGovActionCIP129Id,
    getGovActionPath,
} from '@/utils/getGovActionId';
import { useGetGovernanceActionRecordQuery } from '@/hooks/queries/useGetGovernanceActionRecordQuery';
import { getGovernanceActionVoteEnd } from '@/utils/governanceActions';
import {
    cappedCount,
    proposalActionKind,
    proposalStatus,
    sentimentButton,
    typeDetailsKind,
} from './detailsState';

// Laid out as the redesigned governance action pages (OutcomeDetails): a
// tinted header with the breadcrumb, pills, title and dates, then the
// content beside a 340px side column of action, sentiment and information
// cards. Below lg the side column sits between the content and the
// discussion, so the actions stay near the top on a phone.

const focusRing = {
    '&.Mui-focusVisible, &:focus-visible': {
        outline: `2px solid ${ds.primary['600']}`,
        outlineOffset: 2,
    },
};

// The status tag, as on the proposals list (ProposalRow).
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

const breadcrumbLinkSx = {
    borderRadius: `${RADIUS.xs}px`,
    color: 'inherit',
    textDecoration: 'none',
    '&:hover': { textDecoration: 'underline' },
    ...focusRing,
};

// The round header buttons, sized as ShareDialog's.
const roundButtonSx = {
    border: `1px solid ${ds.gray['200']}`,
    borderRadius: '50%',
    color: ds.gray['700'],
    flexShrink: 0,
    height: 40,
    width: 40,
    '&:hover': { backgroundColor: ds.gray['50'] },
    ...focusRing,
};

const textButtonSx = {
    borderRadius: `${RADIUS.xs}px`,
    color: ds.primary['600'],
    fontFamily: 'inherit',
    fontSize: 13,
    fontWeight: 600,
    '&:hover': { textDecoration: 'underline' },
    ...focusRing,
};

const bodyTextSx = {
    color: ds.gray['600'],
    fontSize: 14,
    lineHeight: '20px',
    m: 0,
};

const sectionTitleSx = {
    color: ds.gray['900'],
    fontSize: 20,
    fontWeight: 600,
    lineHeight: '28px',
    m: 0,
};

const stackSx = { display: 'flex', flexDirection: 'column', gap: 2 };

const countPillSx = {
    backgroundColor: ds.gray['100'],
    borderRadius: `${RADIUS.pill}px`,
    color: ds.gray['700'],
    fontSize: 12,
    fontWeight: 600,
    lineHeight: '18px',
    minWidth: 24,
    px: 0.75,
    textAlign: 'center',
};

const monoValueSx = {
    fontFamily: FONT_FAMILY.mono,
    fontSize: 13,
    wordBreak: 'break-all',
};

// The constitution and guardrails values keep their markdown rendering.
const markdownValueSx = {
    ...monoValueSx,
    '& p': { m: 0 },
    '& a': { color: ds.primary['600'] },
};

// The vendored comment and poll cards keep their own shells elsewhere; on
// this page they take the redesign's card.
const cardShellSx = {
    '& > .MuiCard-root': {
        backgroundColor: '#FFFFFF',
        border: `1px solid ${ds.gray['200']}`,
        borderRadius: `${RADIUS.lg}px`,
        boxShadow: SHADOWS.card,
    },
};

const DETAILS_TITLES = {
    withdrawals: 'Treasury withdrawal',
    constitution: 'New constitution',
    hardFork: 'Hard fork',
};

// A header date, as OutcomeDetails' DateItem.
const DateItem = ({ label, children }) => (
    <Box sx={{ minWidth: 0 }}>
        <Box
            sx={{
                color: ds.gray['600'],
                fontSize: 11,
                fontWeight: 500,
                letterSpacing: '0.3px',
                textTransform: 'uppercase',
            }}
        >
            {label}
        </Box>
        <Box
            sx={{
                alignItems: 'baseline',
                color: ds.gray['900'],
                columnGap: 1.5,
                display: 'flex',
                flexWrap: 'wrap',
                fontSize: 13,
                mt: 0.25,
            }}
        >
            {children}
        </Box>
    </Box>
);

// A labelled value in a details card.
const Field = ({ label, children }) => (
    <Box sx={{ minWidth: 0 }}>
        <Box sx={{ color: ds.gray['600'], fontSize: 12, fontWeight: 500 }}>
            {label}
        </Box>
        <Box
            sx={{
                color: ds.gray['900'],
                fontSize: 14,
                lineHeight: '20px',
                mt: 0.5,
                overflowWrap: 'anywhere',
            }}
        >
            {children}
        </Box>
    </Box>
);

// A row of the side column's information card.
const InfoRow = ({ label, children }) => (
    <Box
        sx={{
            alignItems: 'baseline',
            display: 'flex',
            fontSize: 13,
            gap: 2,
            justifyContent: 'space-between',
        }}
    >
        <Box component='dt' sx={{ color: ds.gray['600'], flexShrink: 0 }}>
            {label}
        </Box>
        <Box
            component='dd'
            sx={{
                color: ds.gray['900'],
                fontWeight: 500,
                m: 0,
                minWidth: 0,
                overflow: 'hidden',
                textAlign: 'right',
                textOverflow: 'ellipsis',
                whiteSpace: 'nowrap',
            }}
        >
            {children}
        </Box>
    </Box>
);

// One of Abstract, Motivation and Rationale.
const ReasoningSection = ({ title, children }) => (
    <Box>
        <Box
            component='h2'
            sx={{ color: ds.gray['900'], fontSize: 15, fontWeight: 600, m: 0 }}
        >
            {title}
        </Box>
        <Box sx={{ mt: 0.75, overflowWrap: 'anywhere' }}>{children}</Box>
    </Box>
);

// The like or dislike button with its count. Its own vote shows in the
// success (like) or error (dislike) tone.
const SentimentButton = ({ like, state, count, onClick, dimmed }) => {
    const Icon = like ? ThumbsUp : ThumbsDown;
    const tone = PILL_TONES[like ? 'success' : 'error'];
    return (
        <Tooltip
            paragraphOne={
                <span style={{ whiteSpace: 'pre-line' }}>{state.tooltip}</span>
            }
        >
            <Box component='span' sx={{ display: 'flex', minWidth: 0 }}>
                <ButtonBase
                    aria-pressed={state.active}
                    data-testid={like ? 'like-button' : 'dislike-button'}
                    disabled={state.disabled}
                    onClick={onClick}
                    sx={{
                        alignItems: 'center',
                        backgroundColor: state.active ? tone.bg : '#FFFFFF',
                        border: `1px solid ${
                            state.active ? tone.border : ds.gray['200']
                        }`,
                        borderRadius: `${RADIUS.md}px`,
                        color: state.active ? tone.text : ds.gray['800'],
                        flex: 1,
                        fontFamily: 'inherit',
                        fontSize: 14,
                        fontWeight: 500,
                        gap: 1,
                        height: 44,
                        justifyContent: 'center',
                        opacity: dimmed ? 0.5 : 1,
                        px: 1.5,
                        '&:hover': {
                            backgroundColor: state.active
                                ? tone.bg
                                : ds.gray['50'],
                        },
                        ...focusRing,
                    }}
                >
                    <Icon aria-hidden size={16} />
                    {like ? 'Like' : 'Dislike'}
                    <Box
                        component='span'
                        data-testid={like ? 'like-count' : 'dislike-count'}
                        sx={{
                            ...countPillSx,
                            ...(state.active && {
                                backgroundColor: '#FFFFFF',
                                color: tone.text,
                            }),
                        }}
                    >
                        {cappedCount(count)}
                    </Box>
                </ButtonBase>
            </Box>
        </Tooltip>
    );
};

const SingleGovernanceAction = ({ id }) => {
    const MAX_COMMENT_LENGTH = 15000;
    const navigate = useNavigate();
    const openLink = (link) => openInNewTab(link);

    const {
        user,
        setLoading,
        setOpenUsernameModal,
        setUser,
        walletAPI,
        clearStates,
        addSuccessAlert,
        addErrorAlert,
        addChangesSavedAlert,
        epochParams,
    } = useAppContext();

    // Without epoch params the deposit is unknown, so submission stays blocked.
    const govActionDepositAda = getGovActionDepositAda(epochParams);
    const hasBalanceForGASubmission = (balance) =>
        govActionDepositAda !== null &&
        balance >= govActionDepositAda + GA_SUBMISSION_FEE_ADA;
    const [proposal, setProposal] = useState(null);
    const [mounted, setMounted] = useState(false);
    const [commentsList, setCommentsList] = useState([]);
    const [newCommentText, setNewCommentText] = useState('');
    const [userProposalVote, setUserProposalVote] = useState(null);
    const [openDeleteModal, setOpenDeleteModal] = useState(false);
    const [openEditDialog, setOpenEditDialog] = useState(false);
    const [reviewVersionsOpen, setReviewVersionsOpen] = useState(false);
    const [commentsPageCount, setCommentsPageCount] = useState(0);
    const [commentsCurrentPage, setCommentsCurrentPage] = useState(1);
    const [commentsSortType, setCommentsSortType] = useState('desc');
    const [proposalLink, setProposalLink] = useState('');
    const [disableShare, setDisableShare] = useState(false);
    const [openGASubmissionDialog, setOpenGASubmissionDialog] = useState(false);
    const [ownProposalModal, setOwnProposalModal] = useState(false);
    const [unactivePollList, setUnactivePollList] = useState([]);
    const [activePoll, setActivePoll] = useState(null);
    const [openAlertDialog, setOpenAlertDialog] = useState(false);

    // Once the submitted action is decided, the page reports the vote's end
    // and links to its outcome instead of asking for votes (#3301).
    const submissionTxHash =
        proposal?.attributes?.content?.attributes?.prop_submission_tx_hash;
    const { governanceAction: submittedAction } =
        useGetGovernanceActionRecordQuery(
            submissionTxHash
                ? `${submissionTxHash}#0`
                : ''
        );
    const voteEnd = submittedAction ? getGovernanceActionVoteEnd(submittedAction) : null;
    const seeGovernanceAction = () =>
        navigate(
            `${GOV_ACTION_HISTORY_PATHS.governanceActionHistory}/${getGovActionCIP129Id(submissionTxHash, 0)}`
        );

    const targetRef = useRef();
    const menuRef = useRef();

    const scrollToComponent = () => {
        if (targetRef.current) {
            const top =
                targetRef.current.getBoundingClientRect().top + window.scrollY;
            window.scrollTo({
                top,
                behavior: 'smooth',
            });
        }
    };
    useEffect(() => {
        let domain = new URL(window.location.href);
        let origin = domain.origin;
        setProposalLink(`${origin}/proposals/`);
    }, [proposalLink]);

    const disableShareClick = () => {
        setDisableShare(true);
        setTimeout(() => {
            setDisableShare(false);
        }, 2000);
    };

    // ShareDialog copies the link itself. A click on its copy button reaches
    // this wrapper through React's tree (the dialog is a portal), so the page
    // still confirms the copy beside the share button, as its menu did.
    const handleShareDialogClick = (event) => {
        if (event.target?.closest?.('[data-testid="copy-link"]')) {
            disableShareClick();
        }
    };

    const [anchorEl, setAnchorEl] = useState(null);
    const open = Boolean(anchorEl);
    const handleClick = () => {
        setAnchorEl(menuRef?.current);
    };

    const handleClose = () => {
        setAnchorEl(null);
    };

    // Read More / Show Less logic
    const [showFullText, setShowFullText] = useState(true);
    const [truncatedText, setTruncatedText] = useState('');
    const [totalCharLength, setTotalCharLength] = useState(0);
    const [AbstractMarkdownText, setAbstractMarkdownText] = useState('');
    const [currentWalletBalance, setCurrentWalletBalance] = useState(0);
    const maxLength = 500;
    useEffect(() => {
        if (AbstractMarkdownText.length > maxLength) {
            setTruncatedText(AbstractMarkdownText.slice(0, maxLength) + '...');
        } else {
            setShowFullText(true);
            setTruncatedText(AbstractMarkdownText);
        }
    }, [AbstractMarkdownText, maxLength]);

    const handleEditProposal = () => {
        setOpenEditDialog(true);
    };

    const handleCloseEditDialog = () => {
        setOpenEditDialog(false);
        setAnchorEl(null);
    };

    const handleOpenDeleteModal = () => {
        setOpenDeleteModal(true);
        handleClose();
    };

    const handleCloseDeleteModal = () => {
        setOpenDeleteModal(false);
    };

    const handleOpenReviewVersions = () => setReviewVersionsOpen(true);
    const handleCloseReviewVersions = () => setReviewVersionsOpen(false);

    const handleDeleteProposal = async () => {
        setLoading(true);
        try {
            const response = await deleteProposal(proposal?.id);
            if (!response) return;

            handleCloseDeleteModal();
            navigate('/proposals');
        } catch (error) {
            console.error('Failed to delete proposal:', error);
        } finally {
            setLoading(false);
        }
    };
    const handleAlertDialogClose = () => {
        setOpenAlertDialog(false);
    };

    const fetchProposal = async (id) => {
        setLoading(true);
        try {
            const response = await getSingleProposal(id);
            if (!response) return;

            if (response?.attributes?.content?.attributes?.is_draft) {
                return navigate('/proposals');
            }
            let totalLength =
                response.attributes?.content?.attributes?.prop_abstract.length +
                response.attributes?.content?.attributes?.prop_motivation
                    .length +
                response.attributes?.content?.attributes?.prop_rationale.length;
            setProposal(response);
            setTotalCharLength(totalLength);
            setAbstractMarkdownText(
                response?.attributes?.content?.attributes?.prop_abstract
            );
            if (totalLength > maxLength) setShowFullText(false);
        } catch (error) {
            if (
                error?.response?.data?.error?.details ===
                    'Proposal not found' ||
                error?.response?.data?.error?.details ===
                    'You can not access draft proposal details.'
            ) {
                return navigate('/proposals');
            }
            console.error(error);
        } finally {
            setLoading(false);
        }
    };

    const fetchProposalVote = async (id) => {
        setLoading(true);
        try {
            const response = await getUserProposalVote({ proposalID: id });
            setUserProposalVote(response);
        } catch (error) {
            console.error(error);
        } finally {
            setLoading(false);
        }
    };

    const fetchComments = async (page = 1) => {
        setLoading(true);
        try {
            const query = `filters[$and][0][proposal_id]=${id}&filters[$and][1][comment_parent_id][$null]=true&sort[createdAt]=${commentsSortType}&pagination[page]=${page}&pagination[pageSize]=25&populate[comments_reports][populate][reporter][fields][0]=username&populate[comments_reports][populate][maintainer][fields][0]=username`;
            const { comments, pgCount } = await getComments(query);
            if (!comments) return;
            setCommentsPageCount(pgCount);

            if (page > commentsCurrentPage) {
                setCommentsList((prev) => [...prev, ...comments]);
            } else {
                if (page === 1) {
                    setCommentsCurrentPage(1);
                }
                setCommentsList(comments);
            }
        } catch (error) {
            console.error(error);
        } finally {
            setLoading(false);
        }
    };

    const fetchCurrentWalletBalance = async () => {
        try {
            const bal = await walletAPI.getBalance();
            const balance = getLovelaceFromCborBalance(bal);
            const normalized = balance / LOVELACE;
            setCurrentWalletBalance(normalized);
            return normalized;
        } catch (error) {
            console.error(error);
            return 0; // fallback
        }
    };
    const handleCreateComment = async () => {
        setLoading(true);
        try {
            const newComment = await createComment({
                proposal_id: id,
                comment_text: newCommentText,
                drep_id: walletAPI?.voter?.isRegisteredAsDRep
                    ? walletAPI?.dRepID || ''
                    : '',
            });

            if (!newComment) return;
            setNewCommentText('');
            fetchProposal(id);
            fetchComments(1);
        } catch (error) {
            console.error(error);
        } finally {
            setLoading(false);
        }
    };

    const updateLikesOrDislikes = async ({ like = true, loggedInUser }) => {
        setLoading(true);
        if (
            loggedInUser?.id?.toString() ===
            proposal?.attributes?.user_id?.toString()
        ) {
            return setOwnProposalModal(true);
        }

        try {
            let data = userProposalVote
                ? {
                      vote_result: !userProposalVote?.attributes?.vote_result,
                  }
                : {
                      proposal_id: id,
                      vote_result: like,
                  };

            const response = userProposalVote
                ? await updateProposalLikesOrDislikes({
                      proposalVoteID: userProposalVote?.id,
                      updateData: data,
                  })
                : await createProposalLikeOrDislike({ createData: data });

            if (!response) return;

            setUserProposalVote(response);
            fetchProposal(id);
        } catch (error) {
            console.error(error);
        } finally {
            setLoading(false);
        }
    };

    const handleKeyDown = (event) => {
        if (event.key === 'Enter') {
            //  event.preventDefault();
        }
    };
    const handleBlur = (event) => {
        const cleanedValue = event.target.value
            .replace(/[^\S\n]+/g, ' ')
            .trim();
        setNewCommentText(cleanedValue);
    };
    const handleChange = (event) => {
        let value = event.target.value;

        if (value.startsWith(' ')) {
            value = value.trimStart();
        }

        // value = value.replace(/  +/g, ' ');

        if (value.length <= MAX_COMMENT_LENGTH) {
            setNewCommentText(value);
        }
    };

    const addPoll = async () => {
        try {
            const response = await createPoll({
                pollData: {
                    data: {
                        proposal_id: proposal?.id?.toString(),
                        poll_start_dt: new Date(),
                        is_poll_active: true,
                    },
                },
            });
            if (!response) return;
            fetchActivePoll();
        } catch (error) {
            console.error(error);
        }
    };

    const fetchUnactivePolls = async () => {
        try {
            const query = `filters[$and][0][proposal_id][$eq]=${proposal?.id}&filters[$and][1][is_poll_active]=false&pagination[page]=1&pagination[pageSize]=1&sort[createdAt]=desc`;
            const { polls, pgCount, total } = await getPolls({ query: query });
            if (!polls) return;
            setUnactivePollList(polls);
        } catch (error) {
            console.error(error);
        }
    };

    const fetchActivePoll = async () => {
        try {
            const query = `filters[$and][0][proposal_id][$eq]=${proposal?.id}&filters[$and][1][is_poll_active]=true&pagination[page]=1&pagination[pageSize]=1&sort[createdAt]=desc`;
            const { polls, pgCount, total } = await getPolls({
                query: query,
            });
            if (!polls?.length === 0) return;
            setActivePoll(polls[0]);
        } catch (error) {
            console.error(error);
        }
    };

    const handleSubmitAsGA = async () => {
        const balance = await fetchCurrentWalletBalance();
        if (hasBalanceForGASubmission(balance)) {
            await loginUserToApp({
                wallet: walletAPI,
                setUser,
                setOpenUsernameModal,
                callBackFn: () => {
                    setOpenGASubmissionDialog(true);
                },
                clearStates,
                addErrorAlert,
                addSuccessAlert,
                addChangesSavedAlert,
            });
        } else {
            setOpenAlertDialog(true);
        }
    };

    useEffect(() => {
        if (!mounted) {
            setMounted(true);
        } else {
            fetchProposal(id);
            fetchComments(1);
        }
    }, [id, mounted]);

    useEffect(() => {
        if (proposal && mounted) {
            fetchActivePoll();
            fetchUnactivePolls();
        }
    }, [proposal]);

    useEffect(() => {
        if (mounted && user) {
            if (user) fetchProposalVote(id);
        }
    }, [user, mounted, id]);

    useEffect(() => {
        if (mounted) {
            fetchComments(1);
        }
    }, [commentsSortType]);

    const content = proposal?.attributes?.content?.attributes;
    const isSubmitted = content?.prop_submitted;
    // The page's long-standing ownership check, by forum user id.
    const isAuthor =
        user?.user?.id?.toString() ===
        proposal?.attributes?.user_id?.toString();
    const typeName = content?.gov_action_type?.attributes?.gov_action_type_name;
    const username = proposal?.attributes?.user_govtool_username;
    const status = proposalStatus(isSubmitted);
    const detailsKind = typeDetailsKind(content);
    const constitution = content?.proposal_constitution_content?.data?.attributes;
    const hardFork = content?.proposal_hard_fork_content?.data?.attributes;
    const actionKind = proposalActionKind({
        submitted: isSubmitted,
        hasUser: Boolean(user),
        isAuthor,
        voteEnded: Boolean(voteEnd),
    });
    const needsValidation = checkShowValidation(false, walletAPI, user);
    const showGovernanceValidation = !isSubmitted && needsValidation;
    const showAddPoll =
        !isSubmitted &&
        user &&
        +user?.user?.id === +proposal?.attributes?.user_id &&
        !activePoll;
    const hasPolls = Boolean(activePoll) || unactivePollList?.length > 0;
    const commentsCount = proposal?.attributes?.prop_comments_number;

    const sentimentState = (like) =>
        sentimentButton({
            like,
            submitted: isSubmitted,
            hasWallet: Boolean(walletAPI?.address),
            user,
            isAuthor,
            vote: userProposalVote,
        });
    const likeState = sentimentState(true);
    const dislikeState = sentimentState(false);
    const sentimentClick = (like, state) => {
        if (state.action === 'username') {
            return () =>
                setOpenUsernameModal({
                    open: true,
                    callBackFn: () => {},
                });
        }
        if (state.action === 'vote') {
            return () =>
                updateLikesOrDislikes({
                    like,
                    loggedInUser: user,
                });
        }
        return null;
    };

    const governanceValidation = showGovernanceValidation ? (
        <UserValidation
            type='governance'
            drepCheck={false}
            drepRequired={false}
        />
    ) : null;

    return !proposal ? null : proposal?.attributes?.content?.attributes
          ?.is_draft ? null : (
        <>
            {openEditDialog ? (
                <Typography variant='body1' fontWeight={400}>
                    <EditProposalDialog
                        proposal={proposal}
                        openEditDialog={openEditDialog}
                        handleCloseEditDialog={handleCloseEditDialog}
                        setMounted={setMounted}
                    />
                </Typography>
            ) : (
                <Box
                    sx={{
                        display: 'flex',
                        flex: 1,
                        flexDirection: 'column',
                        gap: 2.5,
                        isolation: 'isolate',
                        position: 'relative',
                    }}
                >
                    {/* The header's full-width tint, as on a live action. */}
                    <Box
                        aria-hidden
                        sx={{
                            background: `linear-gradient(180deg, ${HEADER_TINTS.live} 0%, rgba(255, 255, 255, 0) 100%)`,
                            height: 380,
                            left: '50%',
                            position: 'absolute',
                            top: { xxs: -24, md: -40 },
                            transform: 'translateX(-50%)',
                            width: '100vw',
                            zIndex: -1,
                        }}
                    />

                    <Box
                        aria-label='Breadcrumb'
                        component='nav'
                        sx={{
                            alignItems: 'center',
                            color: ds.gray['600'],
                            display: 'flex',
                            flexWrap: 'wrap',
                            fontSize: 12,
                            gap: 0.75,
                        }}
                    >
                        <Box
                            component={RouterLink}
                            to='/proposals'
                            sx={breadcrumbLinkSx}
                        >
                            Governance Actions
                        </Box>
                        <ChevronRight aria-hidden size={12} />
                        <span>Proposed</span>
                        <ChevronRight aria-hidden size={12} />
                        <Box
                            aria-current='page'
                            component='span'
                            sx={{
                                color: ds.gray['900'],
                                fontWeight: 600,
                                maxWidth: { xxs: '100%', md: 420 },
                                minWidth: 0,
                                overflow: 'hidden',
                                textOverflow: 'ellipsis',
                                whiteSpace: 'nowrap',
                            }}
                        >
                            {content?.prop_name}
                        </Box>
                    </Box>

                    <Box
                        component='header'
                        sx={{
                            display: 'flex',
                            flexDirection: 'column',
                            gap: 1.5,
                        }}
                    >
                        <Box
                            sx={{
                                alignItems: 'center',
                                display: 'flex',
                                flexWrap: 'wrap',
                                gap: 1,
                                lineHeight: '16px',
                            }}
                        >
                            <MonoTag testId='governance-action-type-content'>
                                {typeName}
                            </MonoTag>
                            <Box
                                component='span'
                                data-testid='proposal-status'
                                sx={tagSx(status.tone)}
                            >
                                {status.label}
                            </Box>
                        </Box>
                        <Box
                            sx={{
                                alignItems: 'flex-start',
                                display: 'flex',
                                gap: 2,
                                justifyContent: 'space-between',
                            }}
                        >
                            <Box sx={{ minWidth: 0 }}>
                                <Box
                                    component='h1'
                                    data-testid='title-content'
                                    sx={{
                                        color: ds.gray['900'],
                                        fontSize: { xxs: 24, md: 28 },
                                        fontWeight: 600,
                                        letterSpacing: '-0.5px',
                                        lineHeight: { xxs: '30px', md: '36px' },
                                        m: 0,
                                        wordBreak: 'break-word',
                                    }}
                                >
                                    {content?.prop_name}
                                </Box>
                                <Box
                                    sx={{
                                        alignItems: 'center',
                                        color: ds.gray['700'],
                                        display: 'flex',
                                        fontSize: 14,
                                        gap: 0.75,
                                        mt: 1,
                                    }}
                                >
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
                                            height: 22,
                                            justifyContent: 'center',
                                            width: 22,
                                        }}
                                    >
                                        {(username || '?')
                                            .charAt(0)
                                            .toUpperCase()}
                                    </Box>
                                    <Box
                                        component='span'
                                        sx={{
                                            minWidth: 0,
                                            overflow: 'hidden',
                                            textOverflow: 'ellipsis',
                                        }}
                                    >
                                        @{username}
                                    </Box>
                                </Box>
                            </Box>
                            <Box
                                sx={{
                                    alignItems: 'center',
                                    display: 'flex',
                                    flexShrink: 0,
                                    gap: 1,
                                }}
                            >
                                {disableShare && (
                                    <Box
                                        component='span'
                                        data-testid='copy-link-text'
                                        role='status'
                                        sx={{
                                            alignItems: 'center',
                                            color: ds.success['700'],
                                            display: 'inline-flex',
                                            fontSize: 13,
                                            fontWeight: 500,
                                            gap: 0.5,
                                            whiteSpace: 'nowrap',
                                        }}
                                    >
                                        <Check aria-hidden size={14} />
                                        Link copied
                                    </Box>
                                )}
                                <Box
                                    component='span'
                                    onClick={handleShareDialogClick}
                                    sx={{ display: 'inline-flex' }}
                                >
                                    <ShareDialog
                                        buttonTestId='share-button'
                                        copyTestId='copy-link'
                                        kind='proposal'
                                        link={`${proposalLink}${id}`}
                                    />
                                </Box>
                                {user && isAuthor && !isSubmitted && (
                                    <>
                                        <ButtonBase
                                            id='menu-button'
                                            aria-label='More actions'
                                            aria-controls={
                                                open
                                                    ? 'proposal-menu'
                                                    : undefined
                                            }
                                            aria-haspopup='true'
                                            aria-expanded={
                                                open ? 'true' : undefined
                                            }
                                            ref={menuRef}
                                            onClick={() => {
                                                handleClick();
                                            }}
                                            data-testid='menu-button'
                                            sx={{
                                                ...roundButtonSx,
                                                ...(open && {
                                                    backgroundColor:
                                                        ds.gray['50'],
                                                }),
                                            }}
                                        >
                                            <EllipsisVertical
                                                aria-hidden
                                                size={18}
                                            />
                                        </ButtonBase>
                                        <Menu
                                            id='proposal-menu'
                                            anchorEl={anchorEl}
                                            open={open}
                                            onClose={handleClose}
                                            MenuListProps={{
                                                'aria-labelledby':
                                                    'menu-button',
                                            }}
                                            slotProps={{
                                                paper: {
                                                    elevation: 4,
                                                    sx: {
                                                        borderRadius: `${RADIUS.md}px`,
                                                        minWidth: 200,
                                                        mt: 1,
                                                    },
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
                                            data-testid='proposal-menu'
                                        >
                                            <MenuItem
                                                onClick={() =>
                                                    handleEditProposal()
                                                }
                                                data-testid='edit-proposal'
                                                sx={{
                                                    color: ds.gray['800'],
                                                    fontSize: 14,
                                                    gap: 1.25,
                                                }}
                                            >
                                                <Pencil aria-hidden size={16} />
                                                Edit proposal
                                            </MenuItem>
                                            <MenuItem
                                                onClick={() =>
                                                    handleOpenDeleteModal()
                                                }
                                                data-testid='delete-proposal'
                                                sx={{
                                                    color: ds.error['700'],
                                                    fontSize: 14,
                                                    gap: 1.25,
                                                }}
                                            >
                                                <Trash2 aria-hidden size={16} />
                                                Delete proposal
                                            </MenuItem>
                                        </Menu>
                                    </>
                                )}
                            </Box>
                        </Box>
                        <Box
                            sx={{
                                display: 'flex',
                                flexWrap: 'wrap',
                                gap: { xxs: 2, md: 0 },
                                '& > *': { pr: { md: 3 } },
                                '& > * + *': {
                                    borderLeft: {
                                        md: `1px solid ${ds.gray['200']}`,
                                    },
                                    pl: { md: 3 },
                                },
                            }}
                        >
                            <DateItem label='Proposed on'>
                                {formatIsoDate(proposal?.attributes?.createdAt)}
                            </DateItem>
                            {isSubmitted && (
                                <DateItem label='Submitted on'>
                                    {`${formatIsoDate(content?.prop_submission_date)}${
                                        submittedAction
                                            ? ` (Epoch ${submittedAction.epoch_no})`
                                            : ''
                                    }`}
                                </DateItem>
                            )}
                            <DateItem label='Last edit'>
                                <span>{formatIsoDate(content?.createdAt)}</span>
                                <ButtonBase
                                    onClick={() => handleOpenReviewVersions()}
                                    data-testid='review-version'
                                    sx={textButtonSx}
                                >
                                    Review versions
                                </ButtonBase>
                            </DateItem>
                        </Box>
                        <ReviewVersions
                            open={reviewVersionsOpen}
                            onClose={handleCloseReviewVersions}
                            id={id}
                        />
                    </Box>

                    <Box
                        sx={{
                            alignItems: 'start',
                            display: 'grid',
                            gap: 2.5,
                            gridTemplateAreas: {
                                xxs: '"main" "side" "discussion"',
                                lg: '"main side" "discussion side"',
                            },
                            gridTemplateColumns: {
                                xxs: 'minmax(0, 1fr)',
                                lg: 'minmax(0, 1fr) 340px',
                            },
                            gridTemplateRows: { lg: 'auto 1fr' },
                        }}
                    >
                        <Box
                            sx={{
                                ...stackSx,
                                gap: 2.5,
                                gridArea: 'main',
                                minWidth: 0,
                            }}
                        >
                            <Card sx={stackSx}>
                                <ReasoningSection title='Abstract'>
                                    <MarkdownTypography
                                        content={
                                            showFullText || !maxLength
                                                ? AbstractMarkdownText
                                                : truncatedText
                                        }
                                        testId={`abstract-content`}
                                    />
                                </ReasoningSection>
                                {showFullText && (
                                    <ReasoningSection title='Motivation'>
                                        <MarkdownTypography
                                            content={
                                                content?.prop_motivation || ''
                                            }
                                            testId={`motivation-content`}
                                        />
                                    </ReasoningSection>
                                )}
                                {showFullText && (
                                    <ReasoningSection title='Rationale'>
                                        <MarkdownTypography
                                            content={
                                                content?.prop_rationale || ''
                                            }
                                            testId={`rationale-content`}
                                        />
                                    </ReasoningSection>
                                )}
                                {totalCharLength > maxLength && (
                                    <ButtonBase
                                        aria-expanded={showFullText}
                                        onClick={() =>
                                            setShowFullText(!showFullText)
                                        }
                                        sx={{
                                            ...textButtonSx,
                                            alignSelf: 'flex-start',
                                            fontSize: 14,
                                        }}
                                    >
                                        {showFullText
                                            ? 'Show less'
                                            : 'Read more'}
                                    </ButtonBase>
                                )}
                            </Card>

                            {detailsKind && (
                                <Card sx={stackSx}>
                                    <CardTitle>
                                        {DETAILS_TITLES[detailsKind]}
                                    </CardTitle>
                                    {detailsKind === 'withdrawals' &&
                                        content?.proposal_withdrawals?.map(
                                            (withdrawal, index) => (
                                                <Box
                                                    key={index}
                                                    sx={{
                                                        display: 'grid',
                                                        gap: 2,
                                                        gridTemplateColumns: {
                                                            xxs: 'minmax(0, 1fr)',
                                                            md: 'minmax(0, 1fr) auto',
                                                        },
                                                        ...(index > 0 && {
                                                            borderTop: `1px solid ${ds.gray['200']}`,
                                                            pt: 2,
                                                        }),
                                                    }}
                                                >
                                                    <Field label='Receiving address'>
                                                        <Box
                                                            component='span'
                                                            data-testid={`receiving-address-${index}-content`}
                                                            sx={monoValueSx}
                                                        >
                                                            {
                                                                withdrawal.prop_receiving_address
                                                            }
                                                        </Box>
                                                    </Field>
                                                    <Field label='Amount'>
                                                        <Box
                                                            component='span'
                                                            data-testid={`amount-${index}-content`}
                                                            sx={{
                                                                fontWeight: 600,
                                                                whiteSpace:
                                                                    'nowrap',
                                                            }}
                                                        >
                                                            ₳{' '}
                                                            {
                                                                withdrawal.prop_amount
                                                            }
                                                        </Box>
                                                    </Field>
                                                </Box>
                                            )
                                        )}
                                    {detailsKind === 'constitution' && (
                                        <>
                                            <Field label='New constitution URL'>
                                                <Box
                                                    sx={markdownValueSx}
                                                    data-testid='new-constitution-url-content'
                                                >
                                                    <ReactMarkdown>
                                                        {constitution?.prop_constitution_url ||
                                                            ''}
                                                    </ReactMarkdown>
                                                </Box>
                                            </Field>
                                            {constitution?.prop_have_guardrails_script ===
                                            true ? (
                                                <>
                                                    <Field label='Guardrails script URL'>
                                                        <Box
                                                            sx={markdownValueSx}
                                                            data-testid='guardrails-script-url-content'
                                                        >
                                                            <ReactMarkdown>
                                                                {constitution?.prop_guardrails_script_url ||
                                                                    ''}
                                                            </ReactMarkdown>
                                                        </Box>
                                                    </Field>
                                                    <Field label='Guardrails script hash'>
                                                        <Box
                                                            sx={markdownValueSx}
                                                            data-testid='guardrails-script-hash-content'
                                                        >
                                                            <ReactMarkdown>
                                                                {constitution?.prop_guardrails_script_hash ||
                                                                    ''}
                                                            </ReactMarkdown>
                                                        </Box>
                                                    </Field>
                                                </>
                                            ) : null}
                                        </>
                                    )}
                                    {detailsKind === 'hardFork' && (
                                        <Box
                                            sx={{
                                                display: 'grid',
                                                gap: 2,
                                                gridTemplateColumns: {
                                                    xxs: 'minmax(0, 1fr)',
                                                    md: 'repeat(2, minmax(0, 1fr))',
                                                },
                                            }}
                                        >
                                            <Field label='Previous Gov Action Hash'>
                                                <Box
                                                    component='span'
                                                    data-testid={`previous-gov-action-hash-content`}
                                                    sx={monoValueSx}
                                                >
                                                    {hardFork?.previous_ga_hash ||
                                                        ''}
                                                </Box>
                                            </Field>
                                            <Field label='Previous Gov Action ID'>
                                                <Box
                                                    component='span'
                                                    data-testid={`previous-gov-action-id-content`}
                                                    sx={monoValueSx}
                                                >
                                                    {hardFork?.previous_ga_id ||
                                                        ''}
                                                </Box>
                                            </Field>
                                            <Field label='Major version'>
                                                <Box
                                                    component='span'
                                                    data-testid={`major-version-content`}
                                                >
                                                    {hardFork?.major || ''}
                                                </Box>
                                            </Field>
                                            <Field label='Minor version'>
                                                <Box
                                                    component='span'
                                                    data-testid={`minor-version-content`}
                                                >
                                                    {hardFork?.minor || ''}
                                                </Box>
                                            </Field>
                                        </Box>
                                    )}
                                </Card>
                            )}

                            {content?.proposal_links?.length > 0 && (
                                <Card sx={{ ...stackSx, gap: 1.5 }}>
                                    <CardTitle>Supporting links</CardTitle>
                                    {content?.proposal_links?.map(
                                        (item, index) => (
                                            <ButtonBase
                                                key={index}
                                                onClick={() =>
                                                    openLink(item?.prop_link)
                                                }
                                                sx={{
                                                    alignItems: 'center',
                                                    border: `1px solid ${ds.gray['200']}`,
                                                    borderRadius: `${RADIUS.md}px`,
                                                    fontFamily: 'inherit',
                                                    gap: 1.5,
                                                    justifyContent:
                                                        'space-between',
                                                    px: 1.5,
                                                    py: 1.25,
                                                    textAlign: 'left',
                                                    '&:hover': {
                                                        backgroundColor:
                                                            ds.gray['50'],
                                                    },
                                                    ...focusRing,
                                                }}
                                            >
                                                <Box
                                                    component='span'
                                                    sx={{
                                                        flex: 1,
                                                        minWidth: 0,
                                                    }}
                                                >
                                                    <Box
                                                        component='span'
                                                        data-testid={`link-${index}-text-content`}
                                                        sx={{
                                                            color: ds.gray['900'],
                                                            display: 'block',
                                                            fontSize: 14,
                                                            fontWeight: 500,
                                                            overflow: 'hidden',
                                                            textOverflow:
                                                                'ellipsis',
                                                            whiteSpace:
                                                                'nowrap',
                                                        }}
                                                    >
                                                        {item?.prop_link_text}
                                                    </Box>
                                                    <Box
                                                        component='span'
                                                        data-testid={`link-${index}-url-content`}
                                                        sx={{
                                                            color: ds.primary['600'],
                                                            display: 'block',
                                                            fontSize: 13,
                                                            mt: 0.25,
                                                            overflow: 'hidden',
                                                            textOverflow:
                                                                'ellipsis',
                                                            whiteSpace:
                                                                'nowrap',
                                                        }}
                                                    >
                                                        {item?.prop_link}
                                                    </Box>
                                                </Box>
                                                <ExternalLink
                                                    aria-hidden
                                                    color={ds.gray['600']}
                                                    size={16}
                                                    style={{ flexShrink: 0 }}
                                                />
                                            </ButtonBase>
                                        )
                                    )}
                                </Card>
                            )}
                        </Box>

                        <Box
                            component='aside'
                            aria-label='Proposal actions'
                            sx={{
                                ...stackSx,
                                gap: 2.5,
                                gridArea: 'side',
                                minWidth: 0,
                            }}
                        >
                            {actionKind === 'submit' && (
                                <Card sx={stackSx}>
                                    <CardTitle>Ready to submit?</CardTitle>
                                    <Box component='p' sx={bodyTextSx}>
                                        If you are ready, submit this proposal
                                        as a governance action to get it
                                        voted on.
                                    </Box>
                                    {governanceValidation}
                                    <Button
                                        variant='contained'
                                        fullWidth
                                        data-testid='submit-as-GA-button'
                                        onClick={handleSubmitAsGA}
                                    >
                                        Submit as governance action
                                    </Button>
                                </Card>
                            )}
                            {actionKind === 'discuss' && (
                                <Card sx={stackSx}>
                                    <CardTitle>Join the discussion</CardTitle>
                                    <Box component='p' sx={bodyTextSx}>
                                        Help make this proposal better by
                                        commenting on it.
                                    </Box>
                                    {governanceValidation}
                                </Card>
                            )}
                            {actionKind === 'vote' && (
                                <Card sx={stackSx}>
                                    <CardTitle>Submitted for vote</CardTitle>
                                    <Box component='p' sx={bodyTextSx}>
                                        This proposal has been submitted
                                        on-chain as a governance action to get
                                        voted on.
                                    </Box>
                                    <Button
                                        variant='contained'
                                        fullWidth
                                        data-testid='review-and-vote-link'
                                        endIcon={
                                            <ArrowRight aria-hidden size={16} />
                                        }
                                        onClick={() =>
                                            navigate(
                                                getGovActionPath(
                                                    content?.prop_submission_tx_hash,
                                                    0
                                                )
                                            )
                                        }
                                    >
                                        Review and vote
                                    </Button>
                                </Card>
                            )}
                            {actionKind === 'outcome' && (
                                <Card sx={stackSx}>
                                    <CardTitle>Voting ended</CardTitle>
                                    <Field label='Vote ended on'>
                                        <Box
                                            component='span'
                                            data-testid='vote-ended-date'
                                        >
                                            {`${formatIsoDate(voteEnd.time) || '-'} (Epoch ${voteEnd.epoch ?? '-'})`}
                                        </Box>
                                    </Field>
                                    <Field label='Voting result'>
                                        <Box
                                            component='span'
                                            data-testid='vote-outcome'
                                        >
                                            {`Voting has been completed for this Action. This action was ${voteEnd.outcome.toLowerCase()}.`}
                                        </Box>
                                    </Field>
                                    <Button
                                        variant='outlined'
                                        fullWidth
                                        data-testid='see-outcome-link'
                                        onClick={seeGovernanceAction}
                                    >
                                        See outcome
                                    </Button>
                                </Card>
                            )}

                            <Card sx={stackSx}>
                                <CardTitle>Sentiment</CardTitle>
                                <Box component='p' sx={bodyTextSx}>
                                    Show whether you support this proposal.
                                </Box>
                                <Box
                                    sx={{
                                        display: 'grid',
                                        gap: 1,
                                        gridTemplateColumns:
                                            'repeat(2, minmax(0, 1fr))',
                                    }}
                                >
                                    <SentimentButton
                                        like
                                        state={likeState}
                                        count={proposal?.attributes?.prop_likes}
                                        onClick={sentimentClick(
                                            true,
                                            likeState
                                        )}
                                        dimmed={needsValidation}
                                    />
                                    <SentimentButton
                                        like={false}
                                        state={dislikeState}
                                        count={
                                            proposal?.attributes?.prop_dislikes
                                        }
                                        onClick={sentimentClick(
                                            false,
                                            dislikeState
                                        )}
                                        dimmed={needsValidation}
                                    />
                                </Box>
                                {needsValidation && (
                                    <UserValidation
                                        type='sentiment'
                                        drepCheck={checkIfDrepIsSignedIn()}
                                        drepRequired={false}
                                    />
                                )}
                            </Card>

                            <Card sx={stackSx}>
                                <CardTitle>Proposal information</CardTitle>
                                <Box
                                    component='dl'
                                    sx={{ ...stackSx, gap: 1.25, m: 0 }}
                                >
                                    <InfoRow label='Proposal ID'>
                                        {`#${proposal?.id}`}
                                    </InfoRow>
                                    <InfoRow label='Author'>
                                        @{username}
                                    </InfoRow>
                                    <InfoRow label='Type'>{typeName}</InfoRow>
                                </Box>
                                <Divider />
                                <Box
                                    component='dl'
                                    sx={{ ...stackSx, gap: 1.25, m: 0 }}
                                >
                                    <InfoRow label='Proposed on'>
                                        {formatIsoDate(
                                            proposal?.attributes?.createdAt
                                        )}
                                    </InfoRow>
                                    <InfoRow label='Last edit'>
                                        {formatIsoDate(content?.createdAt)}
                                    </InfoRow>
                                    <InfoRow label='Comments'>
                                        {cappedCount(commentsCount)}
                                    </InfoRow>
                                </Box>
                            </Card>
                        </Box>

                        <Box
                            sx={{
                                ...stackSx,
                                gap: 4,
                                gridArea: 'discussion',
                                minWidth: 0,
                            }}
                        >
                            {(hasPolls || showAddPoll) && (
                                <Box
                                    component='section'
                                    aria-label='Polls'
                                    sx={stackSx}
                                >
                                    <Box component='h2' sx={sectionTitleSx}>
                                        Polls
                                    </Box>
                                    {showAddPoll ? (
                                        <Card
                                            testId='add-poll-card'
                                            sx={{
                                                alignItems: {
                                                    md: 'center',
                                                },
                                                display: 'flex',
                                                flexDirection: {
                                                    xxs: 'column',
                                                    md: 'row',
                                                },
                                                gap: 2,
                                                justifyContent:
                                                    'space-between',
                                            }}
                                        >
                                            <Box sx={{ minWidth: 0 }}>
                                                <CardTitle as='h3'>
                                                    Do you want to check if
                                                    your proposal is ready to
                                                    be submitted as a
                                                    Governance Action?
                                                </CardTitle>
                                                <Box
                                                    component='p'
                                                    sx={{
                                                        ...bodyTextSx,
                                                        mt: 0.75,
                                                    }}
                                                >
                                                    The poll will be pinned to
                                                    the top of your comments
                                                    list, and you can close it
                                                    whenever you like. Opening
                                                    a new poll will
                                                    automatically close the
                                                    previous one, which will
                                                    then appear as a comment in
                                                    the comments feed.
                                                </Box>
                                            </Box>
                                            <Button
                                                variant='contained'
                                                onClick={addPoll}
                                                data-testid='add-poll-button'
                                                sx={{
                                                    alignSelf: {
                                                        xxs: 'flex-end',
                                                        md: 'center',
                                                    },
                                                    flexShrink: 0,
                                                }}
                                            >
                                                Add poll
                                            </Button>
                                        </Card>
                                    ) : null}
                                    {activePoll && (
                                        <Box sx={cardShellSx}>
                                            <Poll
                                                proposalUserId={
                                                    proposal?.attributes
                                                        ?.user_id
                                                }
                                                proposalAuthorUsername={
                                                    username
                                                }
                                                proposalSubmitted={isSubmitted}
                                                poll={activePoll}
                                                fetchActivePoll={
                                                    fetchActivePoll
                                                }
                                                fetchUnactivePolls={
                                                    fetchUnactivePolls
                                                }
                                            />
                                        </Box>
                                    )}
                                    {unactivePollList?.map((poll, index) => (
                                        <Box key={index} sx={cardShellSx}>
                                            <Poll
                                                proposalUserId={
                                                    proposal?.attributes
                                                        ?.user_id
                                                }
                                                proposalAuthorUsername={
                                                    username
                                                }
                                                proposalSubmitted={isSubmitted}
                                                poll={poll}
                                            />
                                        </Box>
                                    ))}
                                </Box>
                            )}

                            <Box
                                component='section'
                                aria-labelledby='proposal-comments-title'
                                sx={stackSx}
                            >
                                <Box
                                    sx={{
                                        alignItems: 'center',
                                        display: 'flex',
                                        flexWrap: 'wrap',
                                        gap: 1.5,
                                        justifyContent: 'space-between',
                                    }}
                                >
                                    <Box
                                        sx={{
                                            alignItems: 'center',
                                            display: 'flex',
                                            gap: 1,
                                        }}
                                    >
                                        <Box
                                            component='h2'
                                            id='proposal-comments-title'
                                            sx={sectionTitleSx}
                                        >
                                            Comments
                                        </Box>
                                        <Box
                                            component='span'
                                            data-testid='comment-count'
                                            sx={countPillSx}
                                        >
                                            {cappedCount(commentsCount)}
                                        </Box>
                                    </Box>
                                    <ButtonBase
                                        aria-label={`Sort comments, ${
                                            commentsSortType === 'desc'
                                                ? 'newest first'
                                                : 'oldest first'
                                        }`}
                                        onClick={() =>
                                            proposal?.attributes
                                                ?.prop_comments_number === 0
                                                ? null
                                                : setCommentsSortType((prev) =>
                                                      prev === 'desc'
                                                          ? 'asc'
                                                          : 'desc'
                                                  )
                                        }
                                        data-testid='sort-comments'
                                        sx={{
                                            alignItems: 'center',
                                            backgroundColor: '#FFFFFF',
                                            border: `1px solid ${ds.gray['200']}`,
                                            borderRadius: `${RADIUS.md}px`,
                                            color: ds.gray['800'],
                                            fontFamily: 'inherit',
                                            fontSize: 13,
                                            fontWeight: 500,
                                            gap: 0.75,
                                            height: 36,
                                            px: 1.5,
                                            '&:hover': {
                                                backgroundColor: ds.gray['50'],
                                            },
                                            ...focusRing,
                                        }}
                                    >
                                        <ArrowUpDown aria-hidden size={15} />
                                        {commentsSortType === 'desc'
                                            ? 'Newest first'
                                            : 'Oldest first'}
                                    </ButtonBase>
                                </Box>

                                {isSubmitted ? null : (
                                    <Card sx={{ ...stackSx, gap: 1.5 }}>
                                        <CardTitle as='h3'>
                                            Add a comment
                                        </CardTitle>
                                        <PdfTextArea
                                            value={newCommentText || ''}
                                            onChange={(e) => handleChange(e)}
                                            onKeyDown={handleKeyDown}
                                            onBlur={handleBlur}
                                            maxLength={MAX_COMMENT_LENGTH}
                                            spellCheck='false'
                                            autoCorrect='off'
                                            autoCapitalize='none'
                                            autoComplete='off'
                                            aria-label='Add a comment'
                                            dataTestId='comment-input'
                                        />
                                        <Box
                                            ref={targetRef}
                                            sx={{
                                                alignItems: {
                                                    xxs: 'stretch',
                                                    md: 'center',
                                                },
                                                display: 'flex',
                                                flexDirection: {
                                                    xxs: 'column',
                                                    md: 'row',
                                                },
                                                gap: 2,
                                                justifyContent: 'flex-end',
                                            }}
                                        >
                                            {checkShowValidation(
                                                true,
                                                walletAPI,
                                                user
                                            ) && (
                                                <Box
                                                    sx={{
                                                        flex: 1,
                                                        minWidth: 0,
                                                    }}
                                                >
                                                    <UserValidation
                                                        type='budget'
                                                        drepCheck={checkIfDrepIsSignedIn(
                                                            walletAPI
                                                        )}
                                                        drepRequired={true}
                                                    />
                                                </Box>
                                            )}
                                            <Button
                                                variant='contained'
                                                onClick={() =>
                                                    user?.user?.govtool_username
                                                        ? handleCreateComment()
                                                        : setOpenUsernameModal({
                                                              open: true,
                                                              callBackFn:
                                                                  () => {},
                                                          })
                                                }
                                                disabled={
                                                    !newCommentText ||
                                                    checkShowValidation(
                                                        false,
                                                        walletAPI,
                                                        user
                                                    )
                                                }
                                                endIcon={
                                                    <Reply
                                                        aria-hidden
                                                        size={16}
                                                    />
                                                }
                                                data-testid='comment-button'
                                                sx={{
                                                    alignSelf: {
                                                        xxs: 'flex-end',
                                                        md: 'center',
                                                    },
                                                    flexShrink: 0,
                                                }}
                                            >
                                                Comment
                                            </Button>
                                        </Box>
                                    </Card>
                                )}

                                {commentsCount === 0 ? (
                                    <Card
                                        sx={{
                                            alignItems: 'center',
                                            display: 'flex',
                                            flexDirection: 'column',
                                            gap: 0.75,
                                            py: { xxs: 3, md: 4 },
                                            textAlign: 'center',
                                        }}
                                    >
                                        <MessageCircle
                                            aria-hidden
                                            color={ds.gray['500']}
                                            size={22}
                                        />
                                        <CardTitle as='h3'>
                                            No comments yet
                                        </CardTitle>
                                        <Box component='p' sx={bodyTextSx}>
                                            Be the first to share your thoughts
                                            on this proposal.
                                        </Box>
                                    </Card>
                                ) : null}

                                {commentsList?.map((comment, index) => (
                                    <Box key={index} sx={cardShellSx}>
                                        <CommentCard
                                            comment={comment}
                                            proposal={proposal}
                                            fetchComments={fetchComments}
                                            checkShowComments={checkShowValidation(
                                                false,
                                                walletAPI,
                                                user
                                            )}
                                            drepCheck={checkIfDrepIsSignedIn(
                                                walletAPI
                                            )}
                                            user={user}
                                        />
                                    </Box>
                                ))}
                                {commentsCurrentPage < commentsPageCount && (
                                    <Box
                                        sx={{
                                            display: 'flex',
                                            justifyContent: 'center',
                                        }}
                                    >
                                        <Button
                                            variant='outlined'
                                            size='medium'
                                            onClick={() => {
                                                fetchComments(
                                                    commentsCurrentPage + 1
                                                );
                                                setCommentsCurrentPage(
                                                    (prev) => prev + 1
                                                );
                                            }}
                                        >
                                            Load more comments
                                        </Button>
                                    </Box>
                                )}
                            </Box>
                        </Box>
                    </Box>

                    <DeleteProposalModal
                        open={openDeleteModal}
                        onClose={handleCloseDeleteModal}
                        handleDeleteProposal={handleDeleteProposal}
                    />

                    <ProposalSubmissionDialog
                        proposal={proposal}
                        openEditDialog={openGASubmissionDialog}
                        handleCloseSubmissionDialog={() =>
                            setOpenGASubmissionDialog(false)
                        }
                        handleGoToDataEdit={() => {
                            setOpenGASubmissionDialog(false);
                            handleEditProposal();
                        }}
                    />

                    <ProposalOwnModal
                        open={ownProposalModal}
                        onClose={() => setOwnProposalModal(false)}
                    />
                </Box>
            )}
            <PdfModal open={openAlertDialog} onClose={handleAlertDialogClose}>
                <Box
                    sx={{
                        display: 'flex',
                        alignItems: 'center',
                        justifyContent: 'between',
                        width: '100%',
                        marginBottom: '16px',
                        pr: 5,
                    }}
                >
                    <Typography
                        variant='headline5'
                        component='h5'
                        fontWeight={600}
                        color='textBlack'
                        width={'100%'}
                        data-testid='insufficient-wallet-balance-title'
                    >
                        Insufficient wallet balance
                    </Typography>
                </Box>
                <Typography
                    variant='body1'
                    fontWeight={400}
                    id='alert-dialog-description'
                >
                    {govActionDepositAda !== null
                        ? `Insufficient wallet balance to submit a Governance Action. To submit a Governance Action on-chain, you require a wallet balance of ₳${govActionDepositAda.toLocaleString(
                              'en-US'
                          )} (refundable deposit) plus a transaction fee of ₳${GA_SUBMISSION_FEE_ADA}.`
                        : 'The governance action deposit could not be loaded, so the wallet balance cannot be checked. Please try again later.'}
                </Typography>
                <Box mt={3}>
                    <Button
                        onClick={handleAlertDialogClose}
                        variant='contained'
                        fullWidth
                        autoFocus
                        data-testid='insufficient-wallet-balance-dialog-button'
                        sx={{
                            whiteSpace: 'normal',
                            height: 'auto',
                            minHeight: 40,
                        }}
                    >
                        Close
                    </Button>
                </Box>
            </PdfModal>
        </>
    );
};

export default SingleGovernanceAction;
