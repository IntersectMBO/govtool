import { useTheme } from '@emotion/react';
import {
    IconChatAlt,
    IconDocumentSearch,
    IconLink,
    IconReply,
    IconShare,
    IconThumbDown,
    IconThumbUp,
} from '@intersect.mbo/intersectmbo.org-icons-set';
import ArrowBackIosIcon from '@mui/icons-material/ArrowBackIos';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import InfoOutlinedIcon from '@mui/icons-material/InfoOutlined';
import MoreVertIcon from '@mui/icons-material/MoreVert';
import { ICONS } from '@/consts/icons';
import {
    Badge,
    Box,
    IconButton,
    Menu,
    MenuItem,
    Stack,
    Link,
} from '@mui/material';
import { Button, Tooltip, Typography } from '@atoms';
import { useEffect, useState, useRef } from 'react';
import { useNavigate } from 'react-router';
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
    correctVoteAdaFormat,
    decodeJWT,
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
import { cyan, errorRed, gray } from '@/consts/colors';
import { GOV_ACTION_HISTORY_PATHS } from '@/consts/paths';
import { useGetGovernanceActionRecordQuery } from '@/hooks/queries/useGetGovernanceActionRecordQuery';
import { getGovernanceActionVoteEnd } from '@/utils/governanceActions';

// Layout follows GovTool's GovernanceActionDetailsCard: a two-column card
// (data on the left, actions on the right) with radius 20 and boxShadow2.
const detailsCardSx = {
    borderRadius: '20px',
    display: 'grid',
    gridTemplateColumns: { xxs: '1fr', xl: '0.55fr 0.45fr' },
    width: '100%',
    position: 'relative',
    boxShadow: '2px 2px 20px 0px rgba(47, 98, 220, 0.20)',
};

const detailsDataColumnSx = {
    borderRadius: { xxs: '20px 20px 0 0', xl: '20px 0 0 20px' },
    bgcolor: 'rgba(255, 255, 255, 0.30)',
    p: { xxs: '32px 24px', md: '40px' },
    overflow: 'hidden',
    minWidth: 0,
};

const detailsActionsColumnSx = {
    borderRadius: { xxs: '0 0 20px 20px', xl: '0 20px 20px 0' },
    bgcolor: 'rgba(255, 255, 255, 0.60)',
    p: { xxs: '32px 24px', md: '40px', xl: '40px 64px' },
    display: 'flex',
    flexDirection: 'column',
    minWidth: 0,
};

// GovTool list-card shell (GovernanceActionCard).
const sectionCardSx = {
    borderRadius: '20px',
    bgcolor: 'rgba(255, 255, 255, 0.30)',
    boxShadow: '0px 4px 15px 0px #DDE3F5',
    p: 3,
};

const commentComposerSx = {
    borderRadius: '20px',
    bgcolor: 'rgba(255, 255, 255, 0.60)',
    boxShadow: '2px 2px 20px 0px rgba(47, 98, 220, 0.20)',
    p: { xxs: 3, md: 4 },
};

// GovernanceActionCardElement value text.
const valueSx = {
    fontSize: 16,
    fontWeight: 400,
    lineHeight: '24px',
    wordBreak: 'break-word',
};

const markdownValueSx = {
    ...valueSx,
    '& p': { m: 0 },
    '& a': { color: 'primaryBlue' },
};

// GovernanceActionCardElement "pill" text variant.
const pillSx = {
    display: 'inline-flex',
    maxWidth: '100%',
    padding: '6px 18px',
    overflow: 'hidden',
    bgcolor: 'lightBlue',
    borderRadius: 100,
};

const readMoreSx = {
    textTransform: 'none',
    padding: '0',
    minWidth: 0,
    marginTop: '8px',
    color: 'primary.main',
    fontWeight: 600,
    '&:hover': {
        backgroundColor: 'transparent',
        textDecoration: 'underline',
    },
};

const textLinkSx = {
    width: 'max-content',
    cursor: 'pointer',
    color: 'primaryBlue',
    fontSize: 14,
    fontWeight: 500,
    lineHeight: '20px',
    textDecorationColor: 'currentColor',
};

// GovTool Share button look: round, light hover with a soft shadow.
const roundIconButtonSx = {
    width: 48,
    height: 48,
    borderRadius: 50,
    transition: 'all 0.3s',
    '&:hover': {
        bgcolor: '#F7F9FB',
        boxShadow: (theme) => theme.shadows[1],
    },
};

const sentimentButtonSx = {
    width: 48,
    height: 48,
    borderRadius: 50,
};

const DetailLabel = ({ children, sx }) => (
    <Typography
        component='p'
        sx={{
            fontSize: 14,
            fontWeight: 600,
            lineHeight: '20px',
            color: 'neutralGray',
            mb: '4px',
            ...sx,
        }}
    >
        {children}
    </Typography>
);

// Label/value row in the GovernanceActionCardElement style.
const DetailRow = ({ label, children, mb = 4 }) => (
    <Box mb={mb}>
        <DetailLabel>{label}</DetailLabel>
        {children}
    </Box>
);

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

    const theme = useTheme();
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
            `${GOV_ACTION_HISTORY_PATHS.governanceActionHistory}/${submissionTxHash}#0`
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
        setProposalLink(`${origin}/proposal_discussion/`);
    }, [proposalLink]);

    const disableShareClick = () => {
        setDisableShare(true);
        setTimeout(() => {
            setDisableShare(false);
        }, 2000);
    };

    function copyToClipboard(value) {
        navigator.clipboard.writeText(value);
    }

    const [anchorEl, setAnchorEl] = useState(null);
    const open = Boolean(anchorEl);
    const handleClick = () => {
        setAnchorEl(menuRef?.current);
    };

    const handleClose = () => {
        setAnchorEl(null);
    };

    const [shareAnchorEl, setShareAnchorEl] = useState(null);
    const openShare = Boolean(shareAnchorEl);
    const handleShareClick = (event) => {
        setShareAnchorEl(event.currentTarget);
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
    const handleShareClose = () => {
        setShareAnchorEl(null);
    };

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
            navigate('/proposal_discussion');
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
                return navigate('/proposal_discussion');
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
                return navigate('/proposal_discussion');
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

    return !proposal ? null : proposal?.attributes?.content?.attributes
          ?.is_draft ? null : (
        <>
            <Typography variant='body1' fontWeight={400}>
                {openEditDialog ? (
                    <EditProposalDialog
                        proposal={proposal}
                        openEditDialog={openEditDialog}
                        handleCloseEditDialog={handleCloseEditDialog}
                        setMounted={setMounted}
                    />
                ) : (
                    <Box>
                        <Box mt={3}>
                            <Button
                                variant='text'
                                size='small'
                                startIcon={
                                    <ArrowBackIosIcon
                                        color='primary'
                                        sx={{ fontSize: 14 }}
                                    />
                                }
                                onClick={() => navigate(`/proposal_discussion`)}
                                sx={{
                                    px: 0,
                                    minWidth: 0,
                                    fontSize: 14,
                                    fontWeight: 400,
                                    lineHeight: '20px',
                                    '& .MuiButton-startIcon': { mr: 0.5 },
                                    '&:hover': {
                                        backgroundColor: 'transparent',
                                        textDecoration: 'underline',
                                    },
                                }}
                            >
                                Back
                            </Button>
                        </Box>

                        {/* <Box mt={4}>
                            <Card
                                variant='outlined'
                                sx={{
                                    backgroundColor: (theme) => alpha(theme.palette.neutralWhite, 0.3),
                                }}
                            >
                                <CardHeader
                                    sx={{
                                        pt: 1,
                                        pb: 1,
                                        backgroundColor: alpha(fadedPurple.c50, 0.7),
                                    }}
                                    title={
                                        <Box
                                            display={'flex'}
                                            justifyContent={'center'}
                                            alignItems={'center'}
                                            flexDirection={'row'}
                                        >
                                            <Typography
                                                variant='caption'
                                                component='p'
                                            >
                                                {proposal?.attributes?.content
                                                    ?.attributes?.prop_submitted
                                                    ? `Submitted for vote on: ${formatIsoDate(proposal?.attributes?.content?.attributes?.prop_submission_date)}`
                                                    : `Proposed on: ${formatIsoDate(
                                                          proposal?.attributes
                                                              ?.createdAt
                                                      )}`}
                                            </Typography>
                                            <Tooltip
                                                title={
                                                    proposal?.attributes
                                                        ?.content?.attributes
                                                        ?.prop_submitted ? (
                                                        <span
                                                            style={{
                                                                whiteSpace:
                                                                    'pre-line',
                                                            }}
                                                        >
                                                            {`Proposal Date\n\nThe date
                                                        when Proposal was
                                                        submitted as Governance
                                                        Action.`}
                                                        </span>
                                                    ) : (
                                                        'Proposal Date'
                                                    )
                                                }
                                            >
                                                <Box>
                                                    <InfoOutlinedIcon sx={{ fontSize: 16, color: gray.c300 }} />
                                                </Box>
                                            </Tooltip>
                                        </Box>
                                    }
                                ></CardHeader>
                                {
                                    <CardContent>
                                        <Box
                                            display='flex'
                                            alignItems='center'
                                            justifyContent='space-between'
                                            flexDirection={{
                                                xxs: 'column',
                                                lg: 'row',
                                            }}
                                            gap={1}
                                        >
                                            <Box
                                                textAlign={{
                                                    xxs: 'center',
                                                    lg: 'left',
                                                }}
                                            >
                                                {proposal?.attributes?.content
                                                    ?.attributes
                                                    ?.prop_submitted ? (
                                                    <Typography
                                                        variant='caption'
                                                        sx={{
                                                            textWrap: 'balance',
                                                        }}
                                                    >
                                                        This proposal has been
                                                        submitted on-chain as a
                                                        Governance Action to get
                                                        voted on.
                                                    </Typography>
                                                ) : (
                                                    <>
                                                        <Typography variant='body2'>
                                                            Your Action:
                                                        </Typography>
                                                        <Typography
                                                            variant='caption'
                                                            sx={{
                                                                textWrap:
                                                                    'balance',
                                                            }}
                                                        >
                                                            {user &&
                                                            user?.user?.id?.toString() ===
                                                                proposal?.attributes?.user_id?.toString()
                                                                ? `If your are ready, submit this proposal as a governance action to get voted on.`
                                                                : `Help make the proposal better by commenting`}
                                                        </Typography>
                                                    </>
                                                )}
                                            </Box>

                                            <Box>
                                                {proposal?.attributes?.content
                                                    ?.attributes
                                                    ?.prop_submitted ? (
                                                    <Button
                                                        variant='outlined'
                                                        data-testid='review-and-vote-button'
                                                        onClick={() =>
                                                            navigate(
                                                                `/connected/governance_actions/${proposal?.attributes?.content?.attributes?.prop_submission_tx_hash}#0`
                                                            )
                                                        }
                                                        endIcon={
                                                            <IconDocumentSearch
                                                                width={18}
                                                                height={18}
                                                                fill={
                                                                    theme
                                                                        .palette
                                                                        .primary
                                                                        .main
                                                                }
                                                            />
                                                        }
                                                        sx={{
                                                            width: 'max-content',
                                                        }}
                                                    >
                                                        Review and Vote
                                                    </Button>
                                                ) : user &&
                                                  user?.user?.id?.toString() ===
                                                      proposal?.attributes?.user_id?.toString() ? (
                                                    <Button
                                                        variant='outlined'
                                                        data-testid='submit-as-GA-button'
                                                        sx={{
                                                            width: 'max-content',
                                                        }}
                                                        onClick={async () => {
                                                            const balance =
                                                                await fetchCurrentWalletBalance();
                                                            if (
                                                                hasBalanceForGASubmission(
                                                                    balance
                                                                )
                                                            ) {
                                                                await loginUserToApp(
                                                                    {
                                                                        wallet: walletAPI,
                                                                        setUser,
                                                                        setOpenUsernameModal,
                                                                        callBackFn:
                                                                            () => {
                                                                                setOpenGASubmissionDialog(
                                                                                    true
                                                                                );
                                                                            },
                                                                        clearStates,
                                                                        addErrorAlert,
                                                                        addSuccessAlert,
                                                                        addChangesSavedAlert,
                                                                    }
                                                                );
                                                            } else {
                                                                setOpenAlertDialog(
                                                                    true
                                                                );
                                                            }
                                                        }}
                                                    >
                                                        Submit as Governance
                                                        Action
                                                    </Button>
                                                ) : (
                                                    <Button
                                                        variant='outlined'
                                                        data-testid='proposal-details-header-comment-button'
                                                        onClick={() =>
                                                            scrollToComponent()
                                                        }
                                                        sx={{
                                                            width: 'max-content',
                                                        }}
                                                    >
                                                        Comment
                                                    </Button>
                                                )}
                                            </Box>
                                        </Box>
                                    </CardContent>
                                }
                            </Card>
                        </Box> */}
                        {!proposal?.attributes?.content?.attributes
                            ?.prop_submitted && (
                            <Box display={'flex'} justifyContent={'flex-end'}>
                                {checkShowValidation(
                                    false,
                                    walletAPI,
                                    user
                                ) && (
                                    <UserValidation
                                        type='governance'
                                        drepCheck={false}
                                        drepRequired={false}
                                    />
                                )}
                            </Box>
                        )}

                        {/* Details card: data column and actions column */}
                        <Box mt={3} sx={detailsCardSx}>
                            <Box sx={detailsDataColumnSx}>
                                {/* HEADER */}
                                <Box
                                    sx={{
                                        display: 'grid',
                                        gridTemplateColumns: '1fr auto',
                                        gap: 2,
                                        alignItems: 'flex-start',
                                        mb: 1,
                                    }}
                                >
                                    <Box sx={{ minWidth: 0 }}>
                                        <Typography
                                            variant='title2'
                                            component='h2'
                                            data-testid='title-content'
                                            sx={{
                                                fontWeight: 600,
                                                wordBreak: 'break-word',
                                            }}
                                        >
                                            {
                                                proposal?.attributes?.content
                                                    ?.attributes?.prop_name
                                            }
                                        </Typography>
                                        <Typography
                                            variant='body2'
                                            fontWeight={400}
                                            component={'h5'}
                                            sx={{
                                                color: (theme) =>
                                                    theme?.palette?.neutralGray,
                                                mt: 0.5,
                                            }}
                                        >
                                            @
                                            {
                                                proposal?.attributes
                                                    ?.user_govtool_username
                                            }
                                        </Typography>
                                    </Box>
                                    <Box
                                        display='flex'
                                        alignItems='center'
                                        justifyContent='flex-end'
                                    >
                                        {/* SHARE BUTTON */}

                                            <Tooltip
                                                heading='Share proposal'
                                                paragraphOne='Click to share this proposal on social media.'
                                            >
                                                <IconButton
                                                    id='share-button'
                                                    data-testid='share-button'
                                                    sx={{
                                                        ...roundIconButtonSx,
                                                        ...(openShare && {
                                                            bgcolor: '#F7F9FB',
                                                            boxShadow: (theme) =>
                                                                theme.shadows[1],
                                                        }),
                                                    }}
                                                    aria-controls={
                                                        openShare
                                                            ? 'share-menu'
                                                            : undefined
                                                    }
                                                    aria-haspopup='true'
                                                    aria-expanded={
                                                        openShare
                                                            ? 'true'
                                                            : undefined
                                                    }
                                                    onClick={handleShareClick}
                                                >
                                                    <IconShare
                                                        width='24'
                                                        height='24'
                                                        fill={
                                                            openShare
                                                                ? theme?.palette
                                                                      ?.primary
                                                                      ?.main
                                                                : theme?.palette?.textBlack
                                                        }
                                                    />
                                                </IconButton>
                                            </Tooltip>
                                            <Menu
                                                id='share-menu'
                                                anchorEl={shareAnchorEl}
                                                open={openShare}
                                                onClose={handleShareClose}
                                                MenuListProps={{
                                                    'aria-labelledby':
                                                        'share-button',
                                                    sx: {
                                                        width: 148,
                                                        py: 1.5,
                                                    },
                                                }}
                                                slotProps={{
                                                    paper: {
                                                        elevation: 4,
                                                        sx: {
                                                            overflow: 'visible',
                                                            mt: 1,
                                                            borderRadius: 3,
                                                            width: 148,
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
                                            >
                                                <Stack
                                                    direction={'column'}
                                                    alignItems={'center'}
                                                    px={3}
                                                >
                                                    <Typography
                                                        variant='body1'
                                                        fontWeight={400}
                                                        component={'p'}
                                                        sx={{
                                                            alignSelf:
                                                                'flex-start',
                                                        }}
                                                    >
                                                        Share
                                                    </Typography>
                                                    <Stack
                                                        direction={'column'}
                                                        alignItems={'center'}
                                                        sx={{
                                                            marginTop:
                                                                '0 !important',
                                                        }}
                                                    >
                                                        <IconButton
                                                            onClick={() => {
                                                                copyToClipboard(
                                                                    `${proposalLink}${id}`
                                                                ),
                                                                    disableShareClick();
                                                            }}
                                                            color='primary'
                                                            disabled={
                                                                disableShare
                                                            }
                                                            sx={{
                                                                width: 48,
                                                                height: 48,
                                                                mt: 1.5,
                                                                mb: 1,
                                                                bgcolor:
                                                                    disableShare
                                                                        ? 'neutralWhite'
                                                                        : 'lightBlue',
                                                                boxShadow: (
                                                                    theme
                                                                ) =>
                                                                    theme
                                                                        .shadows[1],
                                                                '&:hover': {
                                                                    bgcolor:
                                                                        'lightBlue',
                                                                },
                                                                '&.Mui-disabled':
                                                                    {
                                                                        bgcolor:
                                                                            'neutralWhite',
                                                                    },
                                                            }}
                                                            data-testid='copy-link'
                                                        >
                                                            <IconLink
                                                                fill={
                                                                    !disableShare
                                                                        ? theme
                                                                              ?.palette
                                                                              ?.primary
                                                                              ?.main
                                                                        : gray.c300
                                                                }
                                                                height={24}
                                                                width={24}
                                                            />
                                                        </IconButton>
                                                        <Typography
                                                            variant='caption'
                                                            component={'p'}
                                                            sx={{
                                                                color: (
                                                                    theme
                                                                ) =>
                                                                    theme.palette.textBlack,
                                                            }}
                                                            data-testid='copy-link-text'
                                                        >
                                                            {disableShare
                                                                ? 'Link copied'
                                                                : 'Click to copy link'}
                                                        </Typography>
                                                    </Stack>
                                                </Stack>
                                            </Menu>

                                            {user &&
                                                user?.user?.id?.toString() ===
                                                    proposal?.attributes?.user_id?.toString() &&
                                                !proposal?.attributes?.content
                                                    ?.attributes
                                                    ?.prop_submitted && (
                                                    <Box
                                                        display='flex'
                                                        justifyContent='flex-end'
                                                    >
                                                        <IconButton
                                                            id='menu-button'
                                                            sx={
                                                                roundIconButtonSx
                                                            }
                                                            aria-controls={
                                                                open
                                                                    ? 'proposal-menu'
                                                                    : undefined
                                                            }
                                                            aria-haspopup='true'
                                                            aria-expanded={
                                                                open
                                                                    ? 'true'
                                                                    : undefined
                                                            }
                                                            ref={menuRef}
                                                            onClick={() => {
                                                                handleClick();
                                                            }}
                                                            data-testid='menu-button'
                                                        >
                                                            <MoreVertIcon sx={{ fontSize: 24, color: open ? 'primary.main' : 'textBlack' }} />
                                                        </IconButton>
                                                        <Menu
                                                            id='proposal-menu'
                                                            anchorEl={anchorEl}
                                                            open={open}
                                                            onClose={
                                                                handleClose
                                                            }
                                                            MenuListProps={{
                                                                'aria-labelledby':
                                                                    'menu-button',
                                                            }}
                                                            slotProps={{
                                                                paper: {
                                                                    elevation: 4,
                                                                    sx: {
                                                                        overflow:
                                                                            'visible',
                                                                        mt: 1,
                                                                    },
                                                                },
                                                            }}
                                                            transformOrigin={{
                                                                horizontal:
                                                                    'right',
                                                                vertical: 'top',
                                                            }}
                                                            anchorOrigin={{
                                                                horizontal:
                                                                    'right',
                                                                vertical:
                                                                    'bottom',
                                                            }}
                                                            data-testid='proposal-menu'
                                                        >
                                                            <MenuItem
                                                                onClick={() =>
                                                                    handleEditProposal()
                                                                }
                                                                data-testid='edit-proposal'
                                                            >
                                                                <Stack
                                                                    direction={
                                                                        'row'
                                                                    }
                                                                    spacing={2}
                                                                    alignItems={
                                                                        'center'
                                                                    }
                                                                >
                                                                    <img src={ICONS.editIcon} alt='' width={24} height={24} />
                                                                    <Typography variant='body1' fontWeight={400}>
                                                                        Edit
                                                                        Proposal
                                                                    </Typography>
                                                                </Stack>
                                                            </MenuItem>
                                                            <MenuItem
                                                                onClick={() =>
                                                                    handleOpenDeleteModal()
                                                                }
                                                                data-testid='delete-proposal'
                                                            >
                                                                <Stack
                                                                    direction={
                                                                        'row'
                                                                    }
                                                                    spacing={2}
                                                                    alignItems={
                                                                        'center'
                                                                    }
                                                                >
                                                                    <DeleteOutlineIcon sx={{ fontSize: 24, color: 'textBlack' }} />
                                                                    <Typography variant='body1' fontWeight={400}>
                                                                        Delete
                                                                        Proposal
                                                                    </Typography>
                                                                </Stack>
                                                            </MenuItem>
                                                        </Menu>
                                                    </Box>
                                                )}
                                    </Box>
                                </Box>

                                <DetailRow label='Governance Action Type'>
                                    <Box sx={pillSx}>
                                        <Typography
                                            variant='caption'
                                            data-testid='governance-action-type-content'
                                            sx={{
                                                overflow: 'hidden',
                                                textOverflow: 'ellipsis',
                                                whiteSpace: 'nowrap',
                                            }}
                                        >
                                            {
                                                proposal?.attributes?.content
                                                    ?.attributes
                                                    ?.gov_action_type
                                                    ?.attributes
                                                    ?.gov_action_type_name
                                            }
                                        </Typography>
                                    </Box>
                                </DetailRow>

                                <DetailRow label='Abstract'>
                                    <MarkdownTypography
                                        content={
                                            showFullText || !maxLength
                                                ? AbstractMarkdownText
                                                : truncatedText
                                        }
                                        testId={`abstract-content`}
                                    />
                                    {/* <ReactMarkdown data-testid='abstract-content'>
                                            {showFullText || !maxLength
                                                ? AbstractMarkdownText
                                                : truncatedText}
                                        </ReactMarkdown> */}
                                    {!showFullText &&
                                        totalCharLength > maxLength && (
                                            <Box>
                                                <Button
                                                    size='medium'
                                                    variant='text'
                                                    onClick={() =>
                                                        setShowFullText(
                                                            !showFullText
                                                        )
                                                    }
                                                    sx={readMoreSx}
                                                >
                                                    {showFullText
                                                        ? 'Show less'
                                                        : 'Read more'}
                                                </Button>
                                            </Box>
                                        )}
                                </DetailRow>
                                {showFullText && (
                                    <DetailRow label='Motivation'>
                                        <div>
                                            <MarkdownTypography
                                                content={
                                                    proposal?.attributes
                                                        ?.content?.attributes
                                                        ?.prop_motivation || ''
                                                }
                                                testId={`motivation-content`}
                                            />
                                        </div>
                                    </DetailRow>
                                )}
                                {showFullText && (
                                    <DetailRow label='Rationale'>
                                        <div>
                                            <MarkdownTypography
                                                content={
                                                    proposal?.attributes
                                                        ?.content?.attributes
                                                        ?.prop_rationale || ''
                                                }
                                                testId={`rationale-content`}
                                            />
                                        </div>
                                    </DetailRow>
                                )}
                                {showFullText &&
                                proposal?.attributes?.content?.attributes
                                    ?.gov_action_type_id == 2
                                    ? proposal?.attributes?.content?.attributes.proposal_withdrawals?.map(
                                          (withdrawal, index) => (
                                              <Box key={index}>
                                                  <DetailRow label='Receiving address'>
                                                      <Typography
                                                          variant='body1'
                                                          sx={valueSx}
                                                          data-testid={`receiving-address-${index}-content`}
                                                      >
                                                          {
                                                              withdrawal.prop_receiving_address
                                                          }
                                                      </Typography>
                                                  </DetailRow>
                                                  <DetailRow label='Amount'>
                                                      <Typography
                                                          variant='body1'
                                                          sx={valueSx}
                                                          data-testid={`amount-${index}-content`}
                                                      >
                                                          ₳{' '}
                                                          {
                                                              withdrawal.prop_amount
                                                          }
                                                      </Typography>
                                                  </DetailRow>
                                              </Box>
                                          )
                                      )
                                    : null}
                                {showFullText &&
                                proposal?.attributes?.content?.attributes
                                    ?.gov_action_type_id == 3 &&
                                proposal?.attributes?.content?.attributes
                                    ?.proposal_constitution_content?.data ? (
                                    <div>
                                        <DetailRow label='New constitution URL'>
                                            <Box
                                                sx={markdownValueSx}
                                                data-testid='new-constitution-url-content'
                                            >
                                                <ReactMarkdown>
                                                    {proposal?.attributes
                                                        ?.content?.attributes
                                                        ?.proposal_constitution_content
                                                        ?.data?.attributes
                                                        ?.prop_constitution_url ||
                                                        ''}
                                                </ReactMarkdown>
                                            </Box>
                                        </DetailRow>
                                        {proposal?.attributes?.content
                                            ?.attributes
                                            ?.proposal_constitution_content
                                            ?.data?.attributes
                                            ?.prop_have_guardrails_script ===
                                        true ? (
                                            <div>
                                                <DetailRow label='Guardrails script URL'>
                                                    <Box
                                                        sx={markdownValueSx}
                                                        data-testid='guardrails-script-url-content'
                                                    >
                                                        <ReactMarkdown>
                                                            {proposal
                                                                ?.attributes
                                                                ?.content
                                                                ?.attributes
                                                                ?.proposal_constitution_content
                                                                ?.data
                                                                ?.attributes
                                                                ?.prop_guardrails_script_url ||
                                                                ''}
                                                        </ReactMarkdown>
                                                    </Box>
                                                </DetailRow>
                                                <DetailRow label='Guardrails script hash'>
                                                    <Box
                                                        sx={markdownValueSx}
                                                        data-testid='guardrails-script-hash-content'
                                                    >
                                                        <ReactMarkdown>
                                                            {proposal
                                                                ?.attributes
                                                                ?.content
                                                                ?.attributes
                                                                ?.proposal_constitution_content
                                                                ?.data
                                                                ?.attributes
                                                                ?.prop_guardrails_script_hash ||
                                                                ''}
                                                        </ReactMarkdown>
                                                    </Box>
                                                </DetailRow>
                                            </div>
                                        ) : null}
                                    </div>
                                ) : null}
                                {showFullText &&
                                proposal?.attributes?.content?.attributes
                                    ?.gov_action_type_id == 6 ? (
                                    <>
                                        <DetailRow label='Previous Gov Action Hash'>
                                            <Typography
                                                variant='body1'
                                                sx={valueSx}
                                                data-testid={`previous-gov-action-hash-content`}
                                            >
                                                {proposal?.attributes?.content
                                                    ?.attributes
                                                    ?.proposal_hard_fork_content
                                                    ?.data?.attributes
                                                    ?.previous_ga_hash || ''}
                                            </Typography>
                                        </DetailRow>
                                        <DetailRow label='Previous Gov Action ID'>
                                            <Typography
                                                variant='body1'
                                                sx={valueSx}
                                                data-testid={`previous-gov-action-id-content`}
                                            >
                                                {proposal?.attributes?.content
                                                    ?.attributes
                                                    ?.proposal_hard_fork_content
                                                    ?.data?.attributes
                                                    ?.previous_ga_id || ''}
                                            </Typography>
                                        </DetailRow>
                                        <DetailRow label='Major version'>
                                            <Typography
                                                variant='body1'
                                                sx={valueSx}
                                                data-testid={`major-version-content`}
                                            >
                                                {proposal?.attributes?.content
                                                    ?.attributes
                                                    ?.proposal_hard_fork_content
                                                    ?.data?.attributes?.major ||
                                                    ''}
                                            </Typography>
                                        </DetailRow>
                                        <DetailRow label='Minor version'>
                                            <Typography
                                                variant='body1'
                                                sx={valueSx}
                                                data-testid={`minor-version-content`}
                                            >
                                                {proposal?.attributes?.content
                                                    ?.attributes
                                                    ?.proposal_hard_fork_content
                                                    ?.data?.attributes?.minor ||
                                                    ''}
                                            </Typography>
                                        </DetailRow>
                                    </>
                                ) : null}
                                {showFullText &&
                                    totalCharLength > maxLength && (
                                        <Box mb={4}>
                                            <Button
                                                size='medium'
                                                variant='text'
                                                onClick={() =>
                                                    setShowFullText(
                                                        !showFullText
                                                    )
                                                }
                                                sx={readMoreSx}
                                            >
                                                {showFullText
                                                    ? 'Show less'
                                                    : 'Read more'}
                                            </Button>
                                        </Box>
                                    )}
                                {proposal?.attributes?.content?.attributes
                                    ?.proposal_links?.length > 0 && (
                                    <Box>
                                        <DetailLabel sx={{ my: 2 }}>
                                            Supporting links
                                        </DetailLabel>

                                        <Box
                                            display='flex'
                                            flexDirection='column'
                                            alignItems='flex-start'
                                            gap={1}
                                        >
                                            {proposal?.attributes?.content?.attributes?.proposal_links?.map(
                                                (item, index) => (
                                                    <Button
                                                        variant='text'
                                                        size='medium'
                                                        key={index}
                                                        sx={{
                                                            p: 0,
                                                            minWidth: 0,
                                                            maxWidth: '100%',
                                                            justifyContent:
                                                                'flex-start',
                                                            '& .MuiButton-endIcon':
                                                                { ml: 1 },
                                                            '&:hover': {
                                                                backgroundColor:
                                                                    'transparent',
                                                            },
                                                        }}
                                                        endIcon={
                                                            <img
                                                                src={
                                                                    ICONS.externalLinkIcon
                                                                }
                                                                alt=''
                                                                width={20}
                                                                height={20}
                                                            />
                                                        }
                                                        onClick={() =>
                                                            openLink(
                                                                item?.prop_link
                                                            )
                                                        }
                                                        // data-testid={
                                                        //     'link-${index}-text-content'
                                                        // }
                                                    >
                                                        <Typography
                                                            variant='body1'
                                                            fontWeight={400}
                                                            component={'p'}
                                                            style={{
                                                                margin: 0,
                                                                textOverflow:
                                                                    'ellipsis',
                                                                overflow:
                                                                    'hidden',
                                                                whiteSpace:
                                                                    'nowrap',
                                                                maxWidth:
                                                                    '800px',
                                                            }}
                                                            sx={{
                                                                color: 'primaryBlue',
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

                            <Box sx={detailsActionsColumnSx}>
                                <DetailRow
                                    label={
                                        proposal?.attributes?.content
                                            ?.attributes?.prop_submitted
                                            ? `Submitted on:`
                                            : `Proposed on:`
                                    }
                                >
                                    <Typography variant='body1' sx={valueSx}>
                                        {proposal?.attributes?.content
                                            ?.attributes?.prop_submitted
                                            ? `${formatIsoDate(proposal?.attributes?.content?.attributes?.prop_submission_date)}${
                                                  submittedAction
                                                      ? ` (Epoch ${submittedAction.epoch_no})`
                                                      : ''
                                              }`
                                            : `${formatIsoDate(
                                                  proposal?.attributes
                                                      ?.createdAt
                                              )}`}
                                    </Typography>
                                </DetailRow>
                                {voteEnd && (
                                    <>
                                        <DetailRow label='Vote ended on:'>
                                            <Typography
                                                variant='body1'
                                                sx={valueSx}
                                                data-testid='vote-ended-date'
                                            >
                                                {`${formatIsoDate(voteEnd.time) || '-'} (Epoch ${voteEnd.epoch ?? '-'})`}
                                            </Typography>
                                        </DetailRow>
                                        <DetailRow label='Voting result:'>
                                            <Typography
                                                variant='body1'
                                                sx={valueSx}
                                                data-testid='vote-outcome'
                                            >
                                                {`Voting has been completed for this Action. This action was ${voteEnd.outcome.toLowerCase()}.`}
                                            </Typography>
                                        </DetailRow>
                                    </>
                                )}
                                {proposal?.attributes?.content?.attributes
                                    ?.prop_submitted && (
                                    <DetailRow label='Proposed on:'>
                                        <Typography
                                            variant='body1'
                                            sx={valueSx}
                                        >
                                            {formatIsoDate(
                                                proposal?.attributes?.createdAt
                                            )}
                                        </Typography>
                                    </DetailRow>
                                )}
                                <DetailRow label='Last Edit:'>
                                    <Box
                                        display='flex'
                                        alignItems='center'
                                        flexWrap='wrap'
                                        columnGap={2}
                                        rowGap={0.5}
                                    >
                                        <Typography
                                            variant='body1'
                                            sx={valueSx}
                                        >
                                            {formatIsoDate(
                                                proposal?.attributes?.content
                                                    ?.attributes?.createdAt
                                            )}
                                        </Typography>
                                        <Link
                                            variant='outlined'
                                            onClick={() =>
                                                handleOpenReviewVersions()
                                            }
                                            data-testid='review-version'
                                            sx={textLinkSx}
                                        >
                                            Review Versions
                                        </Link>
                                    </Box>

                                    <ReviewVersions
                                        open={reviewVersionsOpen}
                                        onClose={handleCloseReviewVersions}
                                        id={id}
                                    />
                                </DetailRow>

                                <Box
                                    mb={4}
                                    display='flex'
                                    flexDirection='column'
                                    alignItems='flex-start'
                                    gap={1}
                                >
                                            {user &&
                                            proposal?.attributes?.content
                                                ?.attributes?.prop_submitted ===
                                                false &&
                                            //proposal?.attributes?.prop_submitted &&
                                            user?.user?.id?.toString() ===
                                                proposal?.attributes?.user_id?.toString() ? (
                                                <Link
                                                    variant='outlined'
                                                    data-testid='submit-as-GA-button'
                                                    sx={textLinkSx}
                                                    onClick={async () => {
                                                        const balance =
                                                            await fetchCurrentWalletBalance();
                                                        if (
                                                            hasBalanceForGASubmission(
                                                                balance
                                                            )
                                                        ) {
                                                            await loginUserToApp(
                                                                {
                                                                    wallet: walletAPI,
                                                                    setUser,
                                                                    setOpenUsernameModal,
                                                                    callBackFn:
                                                                        () => {
                                                                            setOpenGASubmissionDialog(
                                                                                true
                                                                            );
                                                                        },
                                                                    clearStates,
                                                                    addErrorAlert,
                                                                    addSuccessAlert,
                                                                    addChangesSavedAlert,
                                                                }
                                                            );
                                                        } else {
                                                            setOpenAlertDialog(
                                                                true
                                                            );
                                                        }
                                                    }}
                                                >
                                                    Submit as Governance Action
                                                </Link>
                                            ) : null}
                                            {proposal?.attributes?.content
                                                ?.attributes?.prop_submitted ? (
                                                voteEnd ? (
                                                    <Link
                                                        variant='outlined'
                                                        data-testid='see-outcome-link'
                                                        onClick={seeGovernanceAction}
                                                        sx={textLinkSx}
                                                    >
                                                        See voting result
                                                    </Link>
                                                ) : (
                                                <Link
                                                    variant='outlined'
                                                    data-testid='review-and-vote-link'
                                                    onClick={() =>
                                                        navigate(
                                                            `/connected/governance_actions/${proposal?.attributes?.content?.attributes?.prop_submission_tx_hash}#0`
                                                        )
                                                    }
                                                    sx={textLinkSx}
                                                >
                                                    Vote
                                                </Link>
                                                )
                                            ) : null}
                                </Box>

                                <Box
                                    mt='auto'
                                    pt={3}
                                    sx={{
                                        borderTop: (theme) =>
                                            `1px solid ${theme.palette.lightBlue}`,
                                    }}
                                >
                                    <Box
                                        display={'flex'}
                                        flexDirection={'row'}
                                        flexWrap={'wrap'}
                                        alignItems={'center'}
                                        justifyContent={'space-between'}
                                        gap={2}
                                    >
                                        <Tooltip paragraphOne='Total comments number'>
                                            <span>
                                                <Box
                                                    display={'flex'}
                                                    alignItems={'center'}
                                                >
                                                    <IconButton
                                                        disabled
                                                        sx={sentimentButtonSx}
                                                    >
                                                        <Badge
                                                            slotProps={{
                                                                badge: {
                                                                    'data-testid':
                                                                        'comment-count',
                                                                },
                                                            }}
                                                            badgeContent={
                                                                proposal
                                                                    ?.attributes
                                                                    ?.prop_comments_number ||
                                                                0
                                                            }
                                                            aria-label='proposal comments'
                                                            showZero
                                                            sx={{
                                                                transform:
                                                                    'translate(30px, -20px)',
                                                                '& .MuiBadge-badge':
                                                                    {
                                                                        color: 'neutralWhite',
                                                                        backgroundColor:
                                                                            (
                                                                                theme
                                                                            ) =>
                                                                                theme.palette.primary.main,
                                                                    },
                                                            }}
                                                        ></Badge>
                                                        <IconChatAlt />
                                                    </IconButton>
                                                </Box>
                                            </span>
                                        </Tooltip>
                                        <Box
                                            style={{
                                                display: 'flex',
                                                flexDirection: 'row',
                                                flexWrap: 'wrap',
                                                gap: '12px',
                                                alignItems: 'center',
                                                justifyContent: 'flex-end',
                                            }}
                                        >
                                            {checkShowValidation(
                                                false,
                                                walletAPI,
                                                user
                                            ) && (
                                                <UserValidation
                                                    type='sentiment'
                                                    drepCheck={checkIfDrepIsSignedIn()}
                                                    drepRequired={false}
                                                />
                                            )}
                                            <Box display={'flex'} gap={2}>
                                                {/* LIKE BUTTON */}
                                                <Tooltip
                                                    paragraphOne={
                                                        <span
                                                            style={{
                                                                whiteSpace:
                                                                    'pre-line',
                                                            }}
                                                        >
                                                            {proposal
                                                                ?.attributes
                                                                ?.content
                                                                ?.attributes
                                                                ?.prop_submitted
                                                                ? `Proposal Submitted\n\nYou can't like this proposal`
                                                                : walletAPI?.address
                                                                  ? user
                                                                      ? user?.user?.id?.toString() ===
                                                                        proposal?.attributes?.user_id?.toString()
                                                                          ? `You can't like your proposal`
                                                                          : userProposalVote
                                                                            ? userProposalVote
                                                                                  ?.attributes
                                                                                  ?.vote_result ===
                                                                              true
                                                                                ? `You already liked this proposal`
                                                                                : 'Like this proposal\n\nClick to like this proposal'
                                                                            : 'Like this proposal\n\nClick to like this proposal'
                                                                      : 'Verify yourself to like this proposal'
                                                                  : 'Connect wallet to like this proposal'}
                                                        </span>
                                                    }
                                                >
                                                    <span>
                                                        <IconButton
                                                            sx={{
                                                                ...sentimentButtonSx,
                                                                border: (
                                                                    theme
                                                                ) =>
                                                                    `1px solid ${theme.palette.lightBlue}`,
                                                                opacity:
                                                                    checkShowValidation(
                                                                        false,
                                                                        walletAPI,
                                                                        user
                                                                    )
                                                                        ? 0.5
                                                                        : 1,
                                                            }}
                                                            data-testid='like-button'
                                                            disabled={
                                                                walletAPI?.address
                                                                    ? proposal
                                                                          ?.attributes
                                                                          ?.content
                                                                          ?.attributes
                                                                          ?.prop_submitted
                                                                        ? true
                                                                        : user
                                                                          ? user?.user?.id?.toString() ===
                                                                            proposal?.attributes?.user_id?.toString()
                                                                              ? true
                                                                              : userProposalVote
                                                                                ? userProposalVote
                                                                                      ?.attributes
                                                                                      ?.vote_result ===
                                                                                  true
                                                                                    ? true
                                                                                    : false
                                                                                : false
                                                                          : false
                                                                    : true
                                                            }
                                                            onClick={
                                                                proposal
                                                                    ?.attributes
                                                                    ?.content
                                                                    ?.attributes
                                                                    ?.prop_submitted
                                                                    ? null
                                                                    : user
                                                                      ? !user
                                                                            ?.user
                                                                            ?.govtool_username
                                                                          ? () =>
                                                                                setOpenUsernameModal(
                                                                                    {
                                                                                        open: true,
                                                                                        callBackFn:
                                                                                            () => {},
                                                                                    }
                                                                                )
                                                                          : user?.user?.id?.toString() ===
                                                                              proposal?.attributes?.user_id?.toString()
                                                                            ? null
                                                                            : userProposalVote
                                                                              ? userProposalVote
                                                                                    ?.attributes
                                                                                    ?.vote_result ===
                                                                                null
                                                                                  ? null
                                                                                  : () =>
                                                                                        updateLikesOrDislikes(
                                                                                            {
                                                                                                like: true,
                                                                                                loggedInUser:
                                                                                                    user,
                                                                                            }
                                                                                        )
                                                                              : () =>
                                                                                    updateLikesOrDislikes(
                                                                                        {
                                                                                            like: true,
                                                                                            loggedInUser:
                                                                                                user,
                                                                                        }
                                                                                    )
                                                                      : () =>
                                                                            updateLikesOrDislikes(
                                                                                {
                                                                                    like: true,
                                                                                    loggedInUser:
                                                                                        user,
                                                                                }
                                                                            )
                                                            }
                                                        >
                                                            <Badge
                                                                badgeContent={
                                                                    proposal
                                                                        ?.attributes
                                                                        ?.prop_likes ||
                                                                    0
                                                                }
                                                                data-testid='like-count'
                                                                showZero
                                                                aria-label='proposal likes'
                                                                sx={{
                                                                    transform:
                                                                        'translate(30px, -20px)',
                                                                    '& .MuiBadge-badge':
                                                                        {
                                                                            color: 'neutralWhite',
                                                                            backgroundColor:
                                                                                cyan.c400,
                                                                        },
                                                                }}
                                                            ></Badge>
                                                            <IconThumbUp
                                                                fill={
                                                                    user
                                                                        ? userProposalVote
                                                                            ? userProposalVote
                                                                                  ?.attributes
                                                                                  ?.vote_result ===
                                                                              true
                                                                                ? theme
                                                                                      ?.palette
                                                                                      ?.primary
                                                                                      ?.main
                                                                                : theme?.palette?.textBlack
                                                                            : theme?.palette?.textBlack
                                                                        : theme?.palette?.textBlack
                                                                }
                                                            />
                                                        </IconButton>
                                                    </span>
                                                </Tooltip>
                                                {/* DISLIKE BUTTON */}
                                                <Tooltip
                                                    paragraphOne={
                                                        <span
                                                            style={{
                                                                whiteSpace:
                                                                    'pre-line',
                                                            }}
                                                        >
                                                            {proposal
                                                                ?.attributes
                                                                ?.content
                                                                ?.attributes
                                                                ?.prop_submitted
                                                                ? `Proposal Submitted\n\nYou can't dislike this proposal`
                                                                : walletAPI?.address
                                                                  ? user
                                                                      ? user?.user?.id?.toString() ===
                                                                        proposal?.attributes?.user_id?.toString()
                                                                          ? `You can't dislike your proposal`
                                                                          : userProposalVote
                                                                            ? userProposalVote
                                                                                  ?.attributes
                                                                                  ?.vote_result ===
                                                                              false
                                                                                ? `You already disliked this proposal`
                                                                                : 'Dislike this proposal\n\nClick to dislike this proposal'
                                                                            : 'Dislike this proposal\n\nClick to dislike this proposal'
                                                                      : 'Verify yourself to dislike this proposal'
                                                                  : 'Connect wallet to dislike this proposal'}
                                                        </span>
                                                    }
                                                >
                                                    <span>
                                                        <IconButton
                                                            sx={{
                                                                ...sentimentButtonSx,
                                                                border: (
                                                                    theme
                                                                ) =>
                                                                    `1px solid ${theme.palette.lightBlue}`,
                                                                opacity:
                                                                    checkShowValidation(
                                                                        false,
                                                                        walletAPI,
                                                                        user
                                                                    )
                                                                        ? 0.5
                                                                        : 1,
                                                            }}
                                                            data-testid='dislike-button'
                                                            disabled={
                                                                walletAPI?.address
                                                                    ? proposal
                                                                          ?.attributes
                                                                          ?.content
                                                                          ?.attributes
                                                                          ?.prop_submitted
                                                                        ? true
                                                                        : user
                                                                          ? user?.user?.id?.toString() ===
                                                                            proposal?.attributes?.user_id?.toString()
                                                                              ? true
                                                                              : userProposalVote
                                                                                ? userProposalVote
                                                                                      ?.attributes
                                                                                      ?.vote_result ===
                                                                                  false
                                                                                    ? true
                                                                                    : false
                                                                                : false
                                                                          : false
                                                                    : true
                                                            }
                                                            onClick={
                                                                proposal
                                                                    ?.attributes
                                                                    ?.content
                                                                    ?.attributes
                                                                    ?.prop_submitted
                                                                    ? null
                                                                    : user
                                                                      ? !user
                                                                            ?.user
                                                                            ?.govtool_username
                                                                          ? () =>
                                                                                setOpenUsernameModal(
                                                                                    {
                                                                                        open: true,
                                                                                        callBackFn:
                                                                                            () => {},
                                                                                    }
                                                                                )
                                                                          : user?.user?.id?.toString() ===
                                                                              proposal?.attributes?.user_id?.toString()
                                                                            ? null
                                                                            : userProposalVote
                                                                              ? userProposalVote
                                                                                    ?.attributes
                                                                                    ?.vote_result ===
                                                                                null
                                                                                  ? null
                                                                                  : () =>
                                                                                        updateLikesOrDislikes(
                                                                                            {
                                                                                                like: false,
                                                                                                loggedInUser:
                                                                                                    user,
                                                                                            }
                                                                                        )
                                                                              : () =>
                                                                                    updateLikesOrDislikes(
                                                                                        {
                                                                                            like: false,
                                                                                            loggedInUser:
                                                                                                user,
                                                                                        }
                                                                                    )
                                                                      : () =>
                                                                            updateLikesOrDislikes(
                                                                                {
                                                                                    like: false,
                                                                                    loggedInUser:
                                                                                        user,
                                                                                }
                                                                            )
                                                            }
                                                        >
                                                            <Badge
                                                                badgeContent={
                                                                    proposal
                                                                        ?.attributes
                                                                        ?.prop_dislikes ||
                                                                    0
                                                                }
                                                                data-testid='dislike-count'
                                                                showZero
                                                                aria-label='proposal dislikes'
                                                                sx={{
                                                                    transform:
                                                                        'translate(30px, -20px)',
                                                                    '& .MuiBadge-badge':
                                                                        {
                                                                            color: 'neutralWhite',
                                                                            backgroundColor:
                                                                                errorRed.c300,
                                                                        },
                                                                }}
                                                            ></Badge>
                                                            <IconThumbDown
                                                                fill={
                                                                    user
                                                                        ? userProposalVote
                                                                            ? userProposalVote
                                                                                  ?.attributes
                                                                                  ?.vote_result ===
                                                                              false
                                                                                ? theme
                                                                                      ?.palette
                                                                                      ?.primary
                                                                                      ?.main
                                                                                : theme?.palette?.textBlack
                                                                            : theme?.palette?.textBlack
                                                                        : theme?.palette?.textBlack
                                                                }
                                                            />
                                                        </IconButton>
                                                    </span>
                                                </Tooltip>
                                            </Box>
                                        </Box>
                                    </Box>
                                </Box>
                            </Box>
                        </Box>

                        <Box
                            mt={5}
                            display='flex'
                            alignItems='center'
                            justifyContent='space-between'
                        >
                            {activePoll && (
                                <Typography
                                    variant='title2'
                                    component='h3'
                                    sx={{ fontWeight: 600 }}
                                >
                                    Polls
                                </Typography>
                            )}
                        </Box>
                        {proposal?.attributes?.content?.attributes
                            ?.prop_submitted ? null : user &&
                          +user?.user?.id === +proposal?.attributes?.user_id &&
                          !activePoll ? (
                            <Box mt={3}>
                                <Box
                                    data-testid='add-poll-card'
                                    sx={sectionCardSx}
                                >
                                    <Typography
                                        variant='body1'
                                        fontWeight={600}
                                    >
                                        Do you want to check if your proposal is
                                        ready to be submitted as a Governance
                                        Action?
                                    </Typography>

                                    <Typography
                                        variant='body2'
                                        fontWeight={400}
                                        mt={1.5}
                                        sx={{ color: 'textBlack' }}
                                    >
                                        The poll will be pinned to the top of
                                        your comments list, and you can close it
                                        whenever you like. Opening a new poll
                                        will automatically close the previous
                                        one, which will then appear as a comment
                                        in the comments feed.
                                    </Typography>

                                    <Box
                                        mt={3}
                                        display='flex'
                                        justifyContent='flex-end'
                                    >
                                        <Button
                                            variant='contained'
                                            onClick={addPoll}
                                            data-testid='add-poll-button'
                                        >
                                            Add Poll
                                        </Button>
                                    </Box>
                                </Box>
                            </Box>
                        ) : null}

                        {activePoll && (
                            <Box mt={3}>
                                <Poll
                                    proposalUserId={
                                        proposal?.attributes?.user_id
                                    }
                                    proposalAuthorUsername={
                                        proposal?.attributes
                                            ?.user_govtool_username
                                    }
                                    proposalSubmitted={
                                        proposal?.attributes?.content
                                            ?.attributes?.prop_submitted
                                    }
                                    poll={activePoll}
                                    fetchActivePoll={fetchActivePoll}
                                    fetchUnactivePolls={fetchUnactivePolls}
                                />
                            </Box>
                        )}

                        {unactivePollList?.length > 0 && (
                            <Box mt={3}>
                                {unactivePollList?.map((poll, index) => (
                                    <Box key={index} mb={4}>
                                        <Poll
                                            proposalUserId={
                                                proposal?.attributes?.user_id
                                            }
                                            proposalAuthorUsername={
                                                proposal?.attributes
                                                    ?.user_govtool_username
                                            }
                                            proposalSubmitted={
                                                proposal?.attributes?.content
                                                    ?.attributes?.prop_submitted
                                            }
                                            poll={poll}
                                        />
                                    </Box>
                                ))}
                            </Box>
                        )}
                        <Box
                            mt={5}
                            display='flex'
                            alignItems='center'
                            justifyContent='space-between'
                        >
                            <Typography
                                variant='title2'
                                component='h3'
                                sx={{ fontWeight: 600 }}
                            >
                                Comments
                            </Typography>

                            <IconButton
                                sx={roundIconButtonSx}
                                onClick={() =>
                                    proposal?.attributes
                                        ?.prop_comments_number === 0
                                        ? null
                                        : setCommentsSortType((prev) =>
                                              prev === 'desc' ? 'asc' : 'desc'
                                          )
                                }
                                data-testid='sort-comments'
                            >
                                <img
                                    src={ICONS.sortIcon}
                                    alt=''
                                    width={24}
                                    height={24}
                                />
                            </IconButton>
                        </Box>
                        {proposal?.attributes?.content?.attributes
                            ?.prop_submitted ? null : (
                            <Box mt={3}>
                                <Box sx={commentComposerSx}>
                                    <Typography
                                        component='h6'
                                        sx={{
                                            fontSize: 14,
                                            fontWeight: 500,
                                            lineHeight: '20px',
                                        }}
                                    >
                                        Submit a comment
                                    </Typography>

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
                                        dataTestId='comment-input'
                                        layoutStyles={{ mt: 1.5, mb: 1 }}
                                    />

                                    <Box
                                        mt={2}
                                        display='flex'
                                        justifyContent={'flex-end'}
                                        alignItems={{
                                            xxs: 'stretch',
                                            md: 'center',
                                        }}
                                        flexDirection={{
                                            xxs: 'column',
                                            md: 'row',
                                        }}
                                        gap={2}
                                        ref={targetRef}
                                    >
                                        {checkShowValidation(
                                            true,
                                            walletAPI,
                                            user
                                        ) && (
                                            <UserValidation
                                                type='budget'
                                                drepCheck={checkIfDrepIsSignedIn(
                                                    walletAPI
                                                )}
                                                drepRequired={true}
                                            />
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
                                                    <IconReply
                                                        height={18}
                                                        width={18}
                                                        fill={
                                                            !newCommentText ||
                                                            !walletAPI?.address
                                                                ? gray.c300
                                                                : theme.palette.neutralWhite
                                                        }
                                                    />
                                                }
                                                data-testid='comment-button'
                                            >
                                                Comment
                                            </Button>
                                    </Box>
                                </Box>
                            </Box>
                        )}
                        {proposal?.attributes?.prop_comments_number === 0 ? (
                            <Box sx={{ ...sectionCardSx, my: 3 }}>
                                <Stack
                                    display={'flex'}
                                    direction={'column'}
                                    alignItems={'center'}
                                    justifyContent={'center'}
                                    textAlign={'center'}
                                    gap={1}
                                >
                                    <Typography
                                        variant='title2'
                                        component='h6'
                                        color='textBlack'
                                        fontWeight={600}
                                    >
                                        No Comments yet
                                    </Typography>
                                    <Typography
                                        variant='body1'
                                        fontWeight={400}
                                        color='textBlack'
                                    >
                                        Be the first to share your thoughts on
                                        this proposal.
                                    </Typography>
                                </Stack>
                            </Box>
                        ) : null}

                        {commentsList?.map((comment, index) => (
                            <Box mt={3} key={index}>
                                <CommentCard
                                    comment={comment}
                                    proposal={proposal}
                                    fetchComments={fetchComments}
                                    checkShowComments={checkShowValidation(
                                        false,
                                        walletAPI,
                                        user
                                    )}
                                    drepCheck={checkIfDrepIsSignedIn(walletAPI)}
                                    user={user}
                                />
                            </Box>
                        ))}
                        {commentsCurrentPage < commentsPageCount && (
                            <Box
                                marginY={3}
                                display={'flex'}
                                justifyContent={'center'}
                            >
                                <Button
                                    variant='outlined'
                                    size='medium'
                                    onClick={() => {
                                        fetchComments(commentsCurrentPage + 1);
                                        setCommentsCurrentPage(
                                            (prev) => prev + 1
                                        );
                                    }}
                                >
                                    Load more comments
                                </Button>
                            </Box>
                        )}

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
                        />

                        <ProposalOwnModal
                            open={ownProposalModal}
                            onClose={() => setOwnProposalModal(false)}
                        />
                    </Box>
                )}
            </Typography>
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
