import { useTheme } from '@emotion/react';
import {
    IconChatAlt,
    IconReply,
} from '@intersect.mbo/intersectmbo.org-icons-set';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
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
import { useNavigate, useLocation } from 'react-router';
import {
    CommentCard,
    BudgetDiscussionPoll,
    DeleteProposalModal,
    CreateBudgetDiscussionDialog,
} from '../../../components';
import { useAppContext } from '../../../context/context';
import {
    createComment,
    deleteBudgetDiscussion,
    getComments,
    getBudgetDiscussion,
    getBudgetDiscussionPoll,
    getCountryList,
} from '../../../lib/api';
import {
    correctVoteAdaFormat,
    decodeJWT,
    formatIsoDate,
    openInNewTab,
} from '../../../lib/utils';
import ProposalOwnModal from '../../../components/ProposalOwnModal';
import BudgetDiscussionReviewVersions from '../../../components/BudgetDiscussionReviewVersions';
import MarkdownTypography from '../../../lib/markdownRenderer';
import { useScrollToHashSection } from '../../../lib/hooks';
import UserValidation from '../../../components/UserValidation/UserValidation';
import { PdfTextArea } from '../../../components/PdfFields';
import {
    checkIfDrepIsSignedIn,
    checkShowValidation,
} from '../../../lib/helpers';
import { gray } from '@/consts/colors';

// GovernanceActionDetailsCard shell: radius 20, boxShadow2, translucent body.
const detailsCardSx = {
    borderRadius: '20px',
    boxShadow: '2px 2px 20px 0px rgba(47, 98, 220, 0.20)',
    bgcolor: 'rgba(255, 255, 255, 0.30)',
    p: { xxs: '40px 24px', md: '40px' },
    overflow: 'hidden',
    width: '100%',
};

// The comment composer panel, like the GovernanceActionDetailsCardVotes column.
const composerCardSx = {
    borderRadius: '20px',
    boxShadow: '2px 2px 20px 0px rgba(47, 98, 220, 0.20)',
    bgcolor: 'rgba(255, 255, 255, 0.60)',
    p: { xxs: '32px 24px', md: '40px' },
};

// Round icon button in the Share molecule style.
const roundIconButtonSx = (active) => (theme) => ({
    width: 48,
    height: 48,
    padding: 1.5,
    borderRadius: 50,
    bgcolor: active ? '#F7F9FB' : 'transparent',
    boxShadow: active ? theme.shadows[1] : 'none',
    transition: 'all 0.3s',
    '&:hover': {
        boxShadow: theme.shadows[1],
        bgcolor: '#F7F9FB',
    },
});

// GovernanceActionCardElement label.
const detailLabelSx = {
    fontSize: 14,
    fontWeight: 600,
    lineHeight: '20px',
    color: 'neutralGray',
};

const sectionSx = {
    mt: 4,
    pt: 4,
    borderTop: '1px solid',
    borderColor: 'lightBlue',
};

// A GovernanceActionDetailsCardData label/value row.
const DetailRow = ({ question, answer, show = true, answerTestId }) => {
    if (!show) {
        return null;
    }
    return (
        <Box mb='32px'>
            <Typography component='p' sx={{ ...detailLabelSx, mb: '4px' }}>
                {question}
            </Typography>
            <MarkdownTypography testId={`${answerTestId}`} content={answer} />
        </Box>
    );
};

const SECTIONS = [
    'problem-statement',
    'proposal-details',
    'costing',
    'further-information',
    'administrating-and-auditing',
];

const VISIBLE_SECTIONS = ['problem-ownership']; // Visible on start, without expanding text

const SingleBudgetDiscussion = ({ id }) => {
    const MAX_COMMENT_LENGTH = 15000;
    const navigate = useNavigate();
    const openLink = (link) => openInNewTab(link);

    const {
        user,
        setLoading,
        setOpenUsernameModal,
        walletAPI,
        addErrorAlert,
        addSuccessAlert,
    } = useAppContext();

    const theme = useTheme();
    const [proposal, setProposal] = useState(null);
    const [mounted, setMounted] = useState(false);
    const [commentsList, setCommentsList] = useState([]);
    const [newCommentText, setNewCommentText] = useState('');
    const [openDeleteModal, setOpenDeleteModal] = useState(false);
    const [openEditDialog, setOpenEditDialog] = useState(false);
    const [reviewVersionsOpen, setReviewVersionsOpen] = useState(false);
    const [commentsPageCount, setCommentsPageCount] = useState(0);
    const [commentsCurrentPage, setCommentsCurrentPage] = useState(1);
    const [commentsSortType, setCommentsSortType] = useState('desc');
    const [proposalLink, setProposalLink] = useState('');
    const [disableShare, setDisableShare] = useState(false);
    const [ownProposalModal, setOwnProposalModal] = useState(false);
    const [activePoll, setActivePoll] = useState(null);
    const [showCreateBDDialog, setShowCreateBDDialog] = useState(false);
    const [refetchProposal, setRefetchProposal] = useState(false);
    const [allCountries, setAllCountries] = useState([]);
    const [hoveredSection, setHoveredSection] = useState(null);

    function copyToClipboard(value) {
        navigator.clipboard.writeText(value);
    }

    const handleSectionEnter = (sectionId) => {
        setHoveredSection(sectionId);
    };

    const handleSectionLeave = () => {
        setHoveredSection(null);
    };

    const handleToggleSection = (sectionId) => {
        setHoveredSection(hoveredSection === sectionId ? null : sectionId);
    };

    // Read More / Show Less logic
    const [showFullText, setShowFullText] = useState(false);
    const { sectionRefs, shouldExpand, setShouldExpand } =
        useScrollToHashSection(SECTIONS, VISIBLE_SECTIONS);

    useEffect(() => {
        if (shouldExpand) {
            setShowFullText(true);
        }
    }, [shouldExpand]);

    const targetRef = useRef();
    const menuRef = useRef();

    useEffect(() => {
        let domain = new URL(window.location.href);
        let origin = domain.origin;
        setProposalLink(`${origin}/budget_discussion/`);
    }, [proposalLink]);
    useEffect(() => {
        const fetchData = async () => {
            try {
                if (!allCountries.length) {
                    const countriesResponse = await getCountryList();
                    setAllCountries(countriesResponse?.data || []);
                }
            } catch (error) {
                console.error('Error fetching data:', error);
            }
        };

        fetchData();
    }, []);
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

    const handleShareClose = () => {
        setShareAnchorEl(null);
    };

    const handleEditProposal = () => {
        setOpenEditDialog(true);
        setAnchorEl(null);
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
            const response = await deleteBudgetDiscussion(proposal?.id);
            if (!response) return;

            handleCloseDeleteModal();
            navigate('/budget_discussion');
            addSuccessAlert('Proposal deleted successfully');
        } catch (error) {
            let errorMessage = 'Failed to delete proposal';
            if (error?.response?.data?.error?.message) {
                errorMessage = error?.response?.data?.error?.message;
            }
            console.error('Failed to delete proposal:', error);
            addErrorAlert(errorMessage);
        } finally {
            setLoading(false);
        }
    };

    const fetchProposal = async (id) => {
        setLoading(true);
        let query = `populate[0]=creator&populate[1]=bd_costing.preferred_currency&populate[2]=bd_proposal_detail.contract_type_name&populate[3]=bd_further_information.proposal_links&populate[4]=bd_psapb.type_name&populate[5]=bd_psapb.roadmap_name&populate[6]=bd_psapb.committee_name&populate[7]=bd_proposal_ownership.be_country`;
        try {
            const response = await getBudgetDiscussion({
                id: id,
                query: query,
            });
            if (!response) return;

            setProposal(response);
        } catch (error) {
            if (error?.response?.data?.error?.message === 'Not Found') {
                return navigate('/budget_discussion');
            }
        } finally {
            setLoading(false);
        }
    };

    const fetchComments = async (page = 1) => {
        setLoading(true);
        try {
            const query = `filters[$and][0][bd_proposal_id]=${id}&filters[$and][1][comment_parent_id][$null]=true&sort[createdAt]=${commentsSortType}&pagination[page]=${page}&pagination[pageSize]=25&populate[comments_reports][populate][reporter][fields][0]=username&populate[comments_reports][populate][maintainer][fields][0]=username`;
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

    const handleCreateComment = async () => {
        setLoading(true);
        try {
            const newComment = await createComment({
                bd_proposal_id: id,
                comment_text: newCommentText,
                drep_id: walletAPI?.voter?.isRegisteredAsDRep
                    ? walletAPI?.dRepID || ''
                    : '',
            });

            if (!newComment) return;
            setNewCommentText('');
            fetchProposal(id);
            fetchComments(1);
            addSuccessAlert('Commented successfully');
        } catch (error) {
            addErrorAlert('Failed to comment');
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

    const fetchActivePoll = async () => {
        try {
            const query = `filters[$and][0][bd_proposal_id][$eq]=${id}&filters[$and][1][is_poll_active]=true&pagination[page]=1&pagination[pageSize]=1&sort[createdAt]=desc`;
            const { polls, pgCount, total } = await getBudgetDiscussionPoll({
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
    }, [id, mounted, openEditDialog]);

    useEffect(() => {
        if (mounted && refetchProposal) {
            fetchProposal(id);
            setRefetchProposal(false);
        }
    }, [mounted, refetchProposal, id]);

    useEffect(() => {
        if (mounted) {
            fetchComments(1);
        }
    }, [commentsSortType]);
    useEffect(() => {
        if (!proposal?.id) return;
        fetchActivePoll();
    }, [proposal]);

    const location = useLocation();

    const renderSectionTitle = (sectionId, title) => (
        <Typography
            variant='title1'
            component='h5'
            sx={{
                mb: 3,
                position: 'relative',
            }}
            onClick={() => handleToggleSection(sectionId)}
            onMouseEnter={() => handleSectionEnter(sectionId)}
            onMouseLeave={handleSectionLeave}
            data-section={sectionId}
            ref={sectionRefs[sectionId]}
        >
            {title}
            {hoveredSection === sectionId ? (
                <IconButton
                    sx={{
                        ml: 1,
                        position: 'absolute',
                        top: '50%',
                        transform: 'translateY(-50%)',
                    }}
                    onClick={() =>
                        copyToClipboard(
                            `${proposalLink}${proposal?.attributes?.master_id}#${sectionId}`
                        )
                    }
                >
                    <img src={ICONS.link} alt='' width={20} height={20} />
                </IconButton>
            ) : null}
        </Typography>
    );

    return !proposal ? null : proposal?.attributes?.content?.attributes
          ?.is_draft ? null : (
        <>
            <Typography variant='body1' fontWeight={400}>
                {openEditDialog ? (
                    <CreateBudgetDiscussionDialog
                        open={openEditDialog}
                        onClose={() => setOpenEditDialog(false)}
                        current_bd_id={proposal?.attributes?.master_id}
                    />
                ) : (
                    <Box>
                        {/* Back link, as on GovernanceActionDetails */}
                        <Box mt={3}>
                            <Button
                                variant='text'
                                size='medium'
                                startIcon={
                                    <img
                                        src={ICONS.arrowRightIcon}
                                        alt=''
                                        style={{ transform: 'rotate(180deg)' }}
                                    />
                                }
                                onClick={() => navigate(`/budget_discussion`)}
                                sx={{
                                    px: 0,
                                    fontWeight: 400,
                                    color: 'primary.main',
                                    '& .MuiButton-startIcon': { mr: '12px' },
                                    '&:hover': {
                                        backgroundColor: 'transparent',
                                    },
                                }}
                            >
                                Show all
                            </Button>
                        </Box>

                        <Box mt={3}>
                            {proposal?.attributes?.submitted_for_vote !==
                                null && (
                                <Box
                                    sx={{
                                        mb: 3,
                                        px: 3,
                                        py: 2,
                                        borderRadius: '12px',
                                        border: '1px solid',
                                        borderColor: 'lightBlue',
                                        bgcolor: 'rgba(255, 255, 255, 0.60)',
                                        display: 'flex',
                                        flexDirection: 'column',
                                        alignItems: {
                                            xxs: 'center',
                                            lg: 'flex-start',
                                        },
                                        textAlign: {
                                            xxs: 'center',
                                            lg: 'left',
                                        },
                                        gap: 0.5,
                                    }}
                                >
                                    <Typography
                                        variant='caption'
                                        component='p'
                                        sx={{ fontWeight: 600 }}
                                    >
                                        Submitted for vote
                                    </Typography>
                                    <Typography
                                        variant='caption'
                                        component='span'
                                        sx={{
                                            textWrap: 'balance',
                                        }}
                                    >
                                        Editing and Voting options have been
                                        disabled for this proposal because it
                                        is included in the Intersect Budget
                                        info action
                                    </Typography>
                                </Box>
                            )}

                            <Box sx={detailsCardSx}>
                                {/* Header, as DataMissingHeader: title and share */}
                                <Box
                                    sx={{
                                        display: 'grid',
                                        gridTemplateColumns: '1fr auto',
                                        gap: 2,
                                        alignItems: 'flex-start',
                                        mb: 3,
                                    }}
                                >
                                    <Box sx={{ minWidth: 0 }}>
                                        <Typography
                                            variant='title2'
                                            component='h2'
                                            sx={{
                                                fontWeight: 600,
                                                wordBreak: 'break-word',
                                            }}
                                            data-testid='title-content'
                                        >
                                            {
                                                proposal?.attributes
                                                    ?.bd_proposal_detail?.data
                                                    ?.attributes?.proposal_name
                                            }
                                        </Typography>
                                        {proposal?.attributes?.creator?.data
                                            ?.attributes.govtool_username ? (
                                            <Typography
                                                variant='body2'
                                                fontWeight={400}
                                                component={'h5'}
                                                sx={{
                                                    color: 'neutralGray',
                                                    mt: 1,
                                                }}
                                            >
                                                @
                                                {
                                                    proposal?.attributes
                                                        ?.creator?.data
                                                        ?.attributes
                                                        .govtool_username
                                                }
                                            </Typography>
                                        ) : null}
                                    </Box>

                                    <Box
                                        display='flex'
                                        alignItems='center'
                                        gap={1}
                                    >
                                        {/* SHARE BUTTON */}
                                        <Tooltip
                                            heading='Share proposal'
                                            paragraphOne='Click to share this proposal on social media.'
                                        >
                                            <IconButton
                                                id='share-button'
                                                data-testid='share-button'
                                                sx={roundIconButtonSx(
                                                    openShare
                                                )}
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
                                                <img
                                                    src={ICONS.share}
                                                    alt=''
                                                    width={24}
                                                    height={24}
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
                                                sx: { py: 0 },
                                            }}
                                            slotProps={{
                                                paper: {
                                                    elevation: 4,
                                                    sx: {
                                                        overflow: 'visible',
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
                                        >
                                            {/* Share molecule popover */}
                                            <Box
                                                sx={{
                                                    display: 'flex',
                                                    flexDirection: 'column',
                                                    alignItems: 'center',
                                                    justifyContent: 'center',
                                                    boxSizing: 'border-box',
                                                    padding: '12px 24px',
                                                    width: 148,
                                                }}
                                            >
                                                <Typography
                                                    component={'p'}
                                                    sx={{
                                                        alignSelf: 'flex-start',
                                                    }}
                                                >
                                                    Share
                                                </Typography>
                                                <IconButton
                                                    onClick={() => {
                                                        copyToClipboard(
                                                            `${proposalLink}${id}`
                                                        ),
                                                            disableShareClick();
                                                    }}
                                                    color='primary'
                                                    disabled={disableShare}
                                                    data-testid='copy-link'
                                                    sx={{
                                                        width: 48,
                                                        height: 48,
                                                        mt: 1.5,
                                                        mb: 1,
                                                        borderRadius: 50,
                                                        bgcolor: !disableShare
                                                            ? 'lightBlue'
                                                            : 'neutralWhite',
                                                        boxShadow: (theme) =>
                                                            theme.shadows[1],
                                                        '&.Mui-disabled': {
                                                            bgcolor:
                                                                'neutralWhite',
                                                        },
                                                    }}
                                                >
                                                    <img
                                                        src={ICONS.link}
                                                        alt=''
                                                        height={24}
                                                        width={24}
                                                        style={{
                                                            opacity:
                                                                !disableShare
                                                                    ? 1
                                                                    : 0.5,
                                                        }}
                                                    />
                                                </IconButton>
                                                <Typography
                                                    variant='caption'
                                                    component={'p'}
                                                    data-testid='copy-link-text'
                                                >
                                                    {disableShare
                                                        ? 'Link copied'
                                                        : 'Click to copy link'}
                                                </Typography>
                                            </Box>
                                        </Menu>

                                        {user &&
                                            user?.user?.id?.toString() ===
                                                proposal?.attributes?.creator?.data?.id?.toString() &&
                                            proposal?.attributes
                                                ?.submitted_for_vote ==
                                                null && (
                                                <Box
                                                    display='flex'
                                                    justifyContent='flex-end'
                                                >
                                                    <IconButton
                                                        id='menu-button'
                                                        sx={roundIconButtonSx(
                                                            open
                                                        )}
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
                                                        <MoreVertIcon
                                                            sx={{
                                                                fontSize: 24,
                                                                color: open
                                                                    ? 'primary.main'
                                                                    : 'textBlack',
                                                            }}
                                                        />
                                                    </IconButton>
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
                                                                    overflow:
                                                                        'visible',
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
                                                                <img
                                                                    src={
                                                                        ICONS.editIcon
                                                                    }
                                                                    alt=''
                                                                    width={24}
                                                                    height={24}
                                                                />
                                                                <Typography
                                                                    variant='body1'
                                                                    fontWeight={
                                                                        400
                                                                    }
                                                                >
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
                                                                <DeleteOutlineIcon
                                                                    sx={{
                                                                        fontSize: 24,
                                                                        color: 'textBlack',
                                                                    }}
                                                                />
                                                                <Typography
                                                                    variant='body1'
                                                                    fontWeight={
                                                                        400
                                                                    }
                                                                >
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

                                {/* Label/value rows, as GovernanceActionCardElement */}
                                <Box mb='32px'>
                                    <Typography
                                        component='p'
                                        sx={{ ...detailLabelSx, mb: '4px' }}
                                    >
                                        Budget category
                                    </Typography>
                                    <Box display='flex'>
                                        <Box
                                            sx={{
                                                padding: '6px 18px',
                                                overflow: 'hidden',
                                                bgcolor: 'lightBlue',
                                                borderRadius: 100,
                                            }}
                                        >
                                            <Typography
                                                variant='caption'
                                                fontWeight={400}
                                                data-testid='budget-discussion-type'
                                                sx={{
                                                    overflow: 'hidden',
                                                    textOverflow: 'ellipsis',
                                                    whiteSpace: 'nowrap',
                                                }}
                                            >
                                                {
                                                    proposal?.attributes
                                                        ?.bd_psapb?.data
                                                        ?.attributes?.type_name
                                                        ?.data?.attributes
                                                        ?.type_name
                                                }
                                            </Typography>
                                        </Box>
                                    </Box>
                                </Box>
                                <Box
                                    display='flex'
                                    alignItems='center'
                                    flexWrap='wrap'
                                    gap={2}
                                >
                                    <Typography
                                        variant='caption'
                                        component='span'
                                        sx={{ color: 'neutralGray' }}
                                    >
                                        {`Last Edit: ${formatIsoDate(
                                            proposal?.attributes?.createdAt
                                        )}`}
                                    </Typography>
                                    <Box>
                                        <Link
                                            variant='outlined'
                                            startIcon={
                                                <img
                                                    src={ICONS.link}
                                                    alt=''
                                                    width={18}
                                                    height={18}
                                                />
                                            }
                                            onClick={() =>
                                                handleOpenReviewVersions()
                                            }
                                            data-testid='review-version'
                                            sx={{
                                                cursor: 'pointer',
                                                fontSize: 14,
                                                fontWeight: 500,
                                                color: 'primaryBlue',
                                            }}
                                        >
                                            Review Versions
                                        </Link>
                                        <BudgetDiscussionReviewVersions
                                            open={reviewVersionsOpen}
                                            onClose={handleCloseReviewVersions}
                                            id={proposal?.attributes?.master_id}
                                        />
                                    </Box>
                                </Box>

                                <Box sx={sectionSx}>
                                    {renderSectionTitle(
                                        'problem-ownership',
                                        'Proposal Ownership'
                                    )}

                                    {/* <DetailRow
                                        question={
                                            'Proposal Public Champion: Who would you like to be the public proposal champion?'
                                        }
                                        answer={
                                            proposal?.attributes
                                                ?.bd_proposal_ownership
                                                ?.data?.attributes
                                                ?.proposal_public_champion
                                        }
                                        answerTestId='public-proposal-champion'
                                    /> */}
                                    {proposal?.attributes
                                        ?.bd_proposal_ownership?.data
                                        ?.attributes?.submited_on_behalf ===
                                    'Company' ? (
                                        <Box>
                                            <DetailRow
                                                question='Company Name'
                                                answer={
                                                    proposal?.attributes
                                                        ?.bd_proposal_ownership
                                                        ?.data?.attributes
                                                        ?.company_name || ''
                                                }
                                                answerTestId='company-name-content'
                                            />

                                            <DetailRow
                                                question='Company Domain Name'
                                                answer={
                                                    proposal?.attributes
                                                        ?.bd_proposal_ownership
                                                        ?.data?.attributes
                                                        ?.company_domain_name ||
                                                    ''
                                                }
                                                answerTestId='company-domain-name-content'
                                            />
                                            <DetailRow
                                                question='Country of Incorporation'
                                                answer={
                                                    allCountries.find(
                                                        (country) =>
                                                            country.id ===
                                                            proposal
                                                                ?.attributes
                                                                .bd_proposal_ownership
                                                                .data.attributes
                                                                .be_country.data
                                                                .id
                                                    )?.attributes
                                                        ?.country_name ||
                                                    'Error'
                                                }
                                                answerTestId='country-of-incorporation-content'
                                            />
                                        </Box>
                                    ) : (
                                        ''
                                    )}
                                    {proposal?.attributes
                                        ?.bd_proposal_ownership?.data
                                        ?.attributes?.submited_on_behalf ===
                                    'Group' ? (
                                        <Box>
                                            <DetailRow
                                                question='Group Name'
                                                answer={
                                                    proposal?.attributes
                                                        ?.bd_proposal_ownership
                                                        ?.data?.attributes
                                                        ?.group_name || ''
                                                }
                                                answerTestId='group-name-content'
                                            />

                                            <DetailRow
                                                question='Type of Group'
                                                answer={
                                                    proposal?.attributes
                                                        ?.bd_proposal_ownership
                                                        ?.data?.attributes
                                                        ?.type_of_group || ''
                                                }
                                                answerTestId='group-type-content'
                                            />

                                            <DetailRow
                                                question='Key Information to Identify
                                                Group'
                                                answer={
                                                    proposal?.attributes
                                                        ?.bd_proposal_ownership
                                                        ?.data?.attributes
                                                        ?.key_info_to_identify_group ||
                                                    ''
                                                }
                                                answerTestId='group-identity-information-content'
                                            />
                                        </Box>
                                    ) : (
                                        ''
                                    )}
                                    <DetailRow
                                        question={
                                            'What social handles would you like to be used? E.g. Github, X'
                                        }
                                        answer={
                                            proposal?.attributes
                                                ?.bd_proposal_ownership?.data
                                                ?.attributes?.social_handles
                                        }
                                        answerTestId='social-handles'
                                    />
                                </Box>

                                <Box sx={sectionSx}>
                                    {renderSectionTitle(
                                        'problem-statement',
                                        'Problem Statements and Proposal Benefits'
                                    )}

                                    <DetailRow
                                        question={'Problem Statement'}
                                        answer={
                                            proposal?.attributes?.bd_psapb?.data
                                                ?.attributes?.problem_statement
                                        }
                                        answerTestId='problem-statement'
                                    />

                                    <DetailRow
                                        question={'Proposal Benefit'}
                                        answer={
                                            proposal?.attributes?.bd_psapb?.data
                                                ?.attributes?.proposal_benefit
                                        }
                                        show={showFullText}
                                        answerTestId='problem-benefit'
                                    />

                                    <DetailRow
                                        question={
                                            'Does this proposal align to the Product Roadmap and Roadmap Goals?'
                                        }
                                        answer={
                                            proposal?.attributes?.bd_psapb?.data
                                                ?.attributes?.roadmap_name?.data
                                                ?.attributes?.roadmap_name
                                        }
                                        show={showFullText}
                                        answerTestId='product-roadmap'
                                    />
                                    {proposal?.attributes?.bd_psapb?.data
                                        ?.attributes
                                        ?.explain_proposal_roadmap ? (
                                        <DetailRow
                                            question='Please explain how your proposal supports the Product Roadmap.'
                                            answer={
                                                proposal?.attributes?.bd_psapb
                                                    ?.data?.attributes
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
                                    <DetailRow
                                        question={
                                            'Does your proposal align to any of the budget categories?'
                                        }
                                        answer={
                                            proposal?.attributes?.bd_psapb?.data
                                                ?.attributes?.type_name?.data
                                                ?.attributes?.type_name
                                        }
                                        show={showFullText}
                                        answerTestId='budget-discussion-type'
                                    />

                                    <DetailRow
                                        question={
                                            'Does your proposal align with any of the Intersect Committees?'
                                        }
                                        answer={
                                            proposal?.attributes?.bd_psapb?.data
                                                ?.attributes?.committee_name
                                                ?.data?.attributes
                                                ?.committee_name
                                        }
                                        show={showFullText}
                                        answerTestId='align-proposal-committees'
                                    />

                                    <DetailRow
                                        question={
                                            'If possible provide evidence of wider community endorsement for this proposal?'
                                        }
                                        answer={
                                            proposal?.attributes?.bd_psapb?.data
                                                ?.attributes
                                                ?.supplementary_endorsement
                                        }
                                        show={showFullText}
                                        answerTestId='evidence'
                                    />
                                </Box>

                                {showFullText && (
                                    <Box sx={sectionSx}>
                                        {renderSectionTitle(
                                            'proposal-details',
                                            'Proposal Details'
                                        )}

                                        <DetailRow
                                            question={
                                                'What is your proposed name to be used to reference this proposal publicly?'
                                            }
                                            answer={
                                                proposal?.attributes
                                                    ?.bd_proposal_detail?.data
                                                    ?.attributes?.proposal_name
                                            }
                                            answerTestId='proposal-name'
                                        />

                                        <DetailRow
                                            question={'Proposal Description'}
                                            answer={
                                                proposal?.attributes
                                                    ?.bd_proposal_detail?.data
                                                    ?.attributes
                                                    ?.proposal_description
                                            }
                                            answerTestId='proposal-description'
                                        />

                                        <DetailRow
                                            question={
                                                'Please list any key dependencies (if any) for this proposal?'
                                            }
                                            answer={
                                                proposal?.attributes
                                                    ?.bd_proposal_detail?.data
                                                    ?.attributes
                                                    ?.key_dependencies
                                            }
                                            answerTestId={`proposal-key-dependencies`}
                                        />

                                        <DetailRow
                                            question={
                                                'How will this proposal be maintained and supported after initial development?'
                                            }
                                            answer={
                                                proposal?.attributes
                                                    ?.bd_proposal_detail?.data
                                                    ?.attributes
                                                    ?.maintain_and_support
                                            }
                                            answerTestId={`proposal-maintain-and-support`}
                                        />
                                        <DetailRow
                                            question={
                                                'Key Proposal Deliverable(s) and Definition of Done: What tangible milestones or outcomes are to be delivered and what will the community ultimately receive?'
                                            }
                                            answer={
                                                proposal?.attributes
                                                    ?.bd_proposal_detail?.data
                                                    ?.attributes
                                                    ?.key_proposal_deliverables
                                            }
                                            answerTestId={`proposal-milestone`}
                                        />

                                        <DetailRow
                                            question={
                                                'Resourcing & Duration Estimates: Please provide estimates of team size and duration to achieve the Key Proposal Deliverables outlined above.'
                                            }
                                            answer={
                                                proposal?.attributes
                                                    ?.bd_proposal_detail?.data
                                                    ?.attributes
                                                    ?.resourcing_duration_estimates
                                            }
                                            answerTestId={`proposal-resources-&-duration-estimates`}
                                        />

                                        <DetailRow
                                            question={
                                                'Experience: Please provide previous experience relevant to complete this project.'
                                            }
                                            answer={
                                                proposal?.attributes
                                                    ?.bd_proposal_detail?.data
                                                    ?.attributes?.experience
                                            }
                                            answerTestId={`project-experience`}
                                        />

                                        <DetailRow
                                            question={
                                                'Contracting: Please describe how you expect to be contracted.'
                                            }
                                            answer={
                                                proposal?.attributes
                                                    ?.bd_proposal_detail?.data
                                                    ?.attributes
                                                    ?.contract_type_name?.data
                                                    ?.attributes
                                                    ?.contract_type_name
                                            }
                                            answerTestId={`proposal-contracting`}
                                        />
                                        {proposal?.attributes
                                            ?.bd_proposal_detail?.data
                                            ?.attributes?.contract_type_name
                                            ?.data?.attributes
                                            ?.contract_type_name ===
                                            'Other' && (
                                            <DetailRow
                                                question='Please describe what you have in mind.'
                                                answer={
                                                    proposal?.attributes
                                                        ?.bd_proposal_detail
                                                        ?.data?.attributes
                                                        ?.other_contract_type
                                                }
                                                answerTestId={`other-contract-description`}
                                            />
                                        )}
                                    </Box>
                                )}

                                {showFullText && (
                                    <Box sx={sectionSx}>
                                        {renderSectionTitle(
                                            'costing',
                                            'Costing'
                                        )}

                                        <DetailRow
                                            question={'ADA Amount'}
                                            answer={`₳ ${correctVoteAdaFormat(
                                                proposal?.attributes
                                                    ?.bd_costing?.data
                                                    ?.attributes?.ada_amount ||
                                                    0
                                            )}`}
                                            answerTestId={`costing-amount`}
                                        />

                                        <DetailRow
                                            question={
                                                'USD to ADA Conversion Rate'
                                            }
                                            answer={
                                                proposal?.attributes
                                                    ?.bd_costing?.data
                                                    ?.attributes
                                                    ?.usd_to_ada_conversion_rate
                                            }
                                            answerTestId={`costing-conversion-rate`}
                                        />

                                        <DetailRow
                                            question={'Preferred currency'}
                                            answer={
                                                proposal?.attributes
                                                    ?.bd_costing?.data
                                                    ?.attributes
                                                    ?.preferred_currency?.data
                                                    ?.attributes?.currency_name
                                            }
                                            answerTestId={`costing-preferred-currency`}
                                        />

                                        <DetailRow
                                            question={
                                                'Amount in preferred currency'
                                            }
                                            answer={correctVoteAdaFormat(
                                                proposal?.attributes
                                                    ?.bd_costing?.data
                                                    ?.attributes
                                                    ?.amount_in_preferred_currency ||
                                                    0
                                            )}
                                            answerTestId={`costing-preferred-currency-amount`}
                                        />

                                        <DetailRow
                                            question={'Cost breakdown'}
                                            answer={
                                                proposal?.attributes
                                                    ?.bd_costing?.data
                                                    ?.attributes?.cost_breakdown
                                            }
                                            answerTestId={`cost-breakdown`}
                                        />
                                    </Box>
                                )}

                                {showFullText && (
                                    <Box sx={sectionSx}>
                                        {renderSectionTitle(
                                            'further-information',
                                            'Further information'
                                        )}
                                        {proposal?.attributes
                                            ?.bd_further_information?.data
                                            ?.attributes?.proposal_links
                                            ?.length > 0 && (
                                            <Box>
                                                {/* GovernanceActionDetailsCardLinks */}
                                                <Typography
                                                    component='span'
                                                    sx={{
                                                        ...detailLabelSx,
                                                        display: 'block',
                                                        mb: 2,
                                                    }}
                                                >
                                                    Supporting links
                                                </Typography>

                                                <Box
                                                    display='flex'
                                                    flexDirection='column'
                                                    alignItems='flex-start'
                                                    gap={1}
                                                    mb='32px'
                                                >
                                                    {proposal?.attributes?.bd_further_information?.data?.attributes?.proposal_links?.map(
                                                        (item, index) =>
                                                            item?.prop_link && (
                                                                <Button
                                                                    variant='text'
                                                                    size='medium'
                                                                    key={index}
                                                                    sx={{
                                                                        px: 0,
                                                                        maxWidth:
                                                                            '100%',
                                                                        justifyContent:
                                                                            'flex-start',
                                                                        '&:hover':
                                                                            {
                                                                                backgroundColor:
                                                                                    'transparent',
                                                                            },
                                                                    }}
                                                                    startIcon={
                                                                        <img
                                                                            src={
                                                                                ICONS.link
                                                                            }
                                                                            alt=''
                                                                            width={
                                                                                18
                                                                            }
                                                                            height={
                                                                                18
                                                                            }
                                                                        />
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
                                                                        variant='body2'
                                                                        fontWeight={
                                                                            400
                                                                        }
                                                                        component={
                                                                            'p'
                                                                        }
                                                                        style={{
                                                                            margin: 0,
                                                                            textOverflow:
                                                                                'ellipsis',
                                                                            overflow:
                                                                                'hidden',
                                                                            maxWidth:
                                                                                '800px',
                                                                        }}
                                                                        sx={{
                                                                            fontSize: 16,
                                                                            lineHeight:
                                                                                '24px',
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
                                )}
                                {showFullText && (
                                    <Box sx={sectionSx}>
                                        {renderSectionTitle(
                                            'administrating-and-auditing',
                                            'Administration and Auditing'
                                        )}

                                        <DetailRow
                                            question={
                                                'Would you like Intersect to be your named Administrator, including acting as the auditor, as per the Cardano Constitution?*'
                                            }
                                            answer={
                                                proposal?.attributes
                                                    ?.intersect_named_administrator
                                                    ? 'Yes'
                                                    : 'No'
                                            }
                                            answerTestId={`include-as-auditor`}
                                        />
                                        {proposal?.attributes
                                            ?.intersect_named_administrator ? (
                                            ''
                                        ) : (
                                            <DetailRow
                                                question='Please provide further information to help inform DReps. Who is the vendor and what services are they providing?'
                                                answer={
                                                    proposal?.attributes
                                                        ?.intersect_admin_further_text ||
                                                    ''
                                                }
                                                answerTestId={
                                                    'intersect-admin-further-text'
                                                }
                                            />
                                        )}
                                    </Box>
                                )}
                                <Button
                                    size='medium'
                                    variant='text'
                                    onClick={() => {
                                        setShowFullText(!showFullText),
                                            setShouldExpand(!showFullText);
                                    }}
                                    sx={{
                                        textTransform: 'none',
                                        padding: '0',
                                        marginTop: '8px',
                                        color: 'primaryBlue',
                                        fontWeight: 500,
                                        '&:hover': {
                                            backgroundColor: 'transparent',
                                            textDecoration: 'underline',
                                        },
                                    }}
                                    data-testid={
                                        showFullText
                                            ? 'show-less-button'
                                            : 'read-more-button'
                                    }
                                >
                                    {showFullText ? 'Show less' : 'Read more'}
                                </Button>
                                <Box
                                    mt={4}
                                    pt={3}
                                    display={'flex'}
                                    flexDirection={'row'}
                                    justifyContent={'space-between'}
                                    sx={{
                                        borderTop: '1px solid',
                                        borderColor: 'lightBlue',
                                    }}
                                >
                                    <Tooltip paragraphOne='Total comments number'>
                                        <span>
                                            <Box
                                                display={'flex'}
                                                alignItems={'center'}
                                            >
                                                <IconButton disabled>
                                                    <Badge
                                                        slotProps={{
                                                            badge: {
                                                                'data-testid':
                                                                    'total-comments',
                                                            },
                                                        }}
                                                        badgeContent={
                                                            proposal?.attributes
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
                                                                            theme
                                                                                .palette
                                                                                .primary
                                                                                .main,
                                                                },
                                                        }}
                                                    ></Badge>
                                                    <IconChatAlt />
                                                </IconButton>
                                            </Box>
                                        </span>
                                    </Tooltip>
                                </Box>
                            </Box>
                        </Box>
                        {activePoll &&
                            proposal?.attributes?.submitted_for_vote ===
                                null && (
                                <Box
                                    mt={5}
                                    display='flex'
                                    alignItems='center'
                                    justifyContent='space-between'
                                >
                                    <Typography
                                        variant='title1'
                                        component='h3'
                                    >
                                        Poll of DRep sentiment
                                    </Typography>
                                </Box>
                            )}

                        {activePoll &&
                            proposal?.attributes?.submitted_for_vote ===
                                null && (
                                <Box mt={3}>
                                    <BudgetDiscussionPoll
                                        proposalUserId={
                                            proposal?.attributes?.creator?.data
                                                ?.id
                                        }
                                        proposalAuthorUsername={
                                            proposal?.attributes
                                                ?.user_govtool_username
                                        }
                                        poll={activePoll}
                                        fetchActivePoll={fetchActivePoll}
                                    />
                                </Box>
                            )}
                        <Box
                            mt={5}
                            display='flex'
                            alignItems='center'
                            justifyContent='space-between'
                        >
                            <Typography variant='title1' component='h3'>
                                Comments
                            </Typography>

                            <IconButton
                                sx={roundIconButtonSx(false)}
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
                                {/* Comment composer, as VoteActionForm */}
                                <Box sx={composerCardSx}>
                                    <Typography
                                        variant='body1'
                                        fontWeight={400}
                                        component='h6'
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
                                        layoutStyles={{ mt: 2, mb: 1 }}
                                    />

                                    <Box
                                        mt={3}
                                        display='flex'
                                        justifyContent={
                                            !checkShowValidation(
                                                true,
                                                walletAPI,
                                                user
                                            )
                                                ? 'flex-end'
                                                : 'space-between'
                                        }
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
                                            size='extraLarge'
                                            onClick={() =>
                                                user?.user?.govtool_username
                                                    ? handleCreateComment()
                                                    : setOpenUsernameModal({
                                                          open: true,
                                                          callBackFn: () => {},
                                                      })
                                            }
                                            disabled={
                                                !newCommentText ||
                                                checkShowValidation(
                                                    true,
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
                                                            : theme.palette
                                                                  .neutralWhite
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
                            <Box
                                sx={{
                                    my: 3,
                                    py: 4,
                                    px: 3,
                                    borderRadius: '20px',
                                    border: '1px solid',
                                    borderColor: 'lightBlue',
                                    bgcolor: 'rgba(255, 255, 255, 0.30)',
                                }}
                            >
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
                                    setRefetchProposal={setRefetchProposal}
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
                                marginY={2}
                                display={'flex'}
                                justifyContent={'flex-end'}
                            >
                                <Button
                                    variant='text'
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

                        <ProposalOwnModal
                            open={ownProposalModal}
                            onClose={() => setOwnProposalModal(false)}
                        />
                    </Box>
                )}
            </Typography>
        </>
    );
};

export default SingleBudgetDiscussion;
