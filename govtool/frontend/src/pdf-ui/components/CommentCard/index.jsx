import { useEffect, useRef, useState } from 'react';
import {
    getComments,
    createComment,
    addCommentReport,
    removeCommentReport,
} from '../../lib/api';
import { formatDateWithOffset } from '../../lib/utils';
import { useAppContext } from '../../context/context';
import { useTheme } from '@emotion/react';
import {
    IconMinusCircle,
    IconPlusCircle,
    IconChat,
    IconPlus,
    IconReply,
    IconMinus,
} from '@intersect.mbo/intersectmbo.org-icons-set';
import { Box, Card, Link } from '@mui/material';
import { Button, Tooltip, Typography } from '@atoms';
import { PdfTextArea } from '../PdfFields';
import Subcomponent from './Subcomponent';
import {
    checkIfDrepIsSignedIn,
    checkShowValidation,
    isCommentRestricted,
} from '../../lib/helpers';
import UsernameSection from './UsernameSection';
import MarkdownTypography from '../../lib/markdownRenderer';
import UserValidation from '../UserValidation/UserValidation';
import { gray } from '@/consts/colors';

import { readMoreLinkSx } from './commentStyles';

const CommentCard = ({
    comment,
    proposal,
    fetchComments,
    setRefetchProposal,
    checkShowComments,
    drepCheck,
    // Read-only: the comment's replies, given rather than fetched, and no
    // reply form (the 2025 budget proposals archive).
    archivedReplies,
}) => {
    const readOnly = archivedReplies !== undefined;
    const {
        setLoading,
        walletAPI,
        user,
        setOpenUsernameModal,
        fetchDRepVotingPowerList,
        addSuccessAlert,
        addErrorAlert,
    } = useAppContext();
    const theme = useTheme();
    const maxLength = 128;
    const subcommentMaxLength = 15000;
    const sliceString = (str) => {
        if (!str) return '';
        if (str.length > maxLength) {
            return str.slice(0, maxLength - 3) + '...';
        }
        return str;
    };

    const showMoreRef = useRef(null);
    const commentCardRef = useRef(null);
    const [isExpanded, setIsExpanded] = useState(false);
    const [showSubcomments, setShowSubcomments] = useState(false);
    const [showMoreTopPosition, setShowMoreTopPosition] = useState(0);
    const [commentCardTopPosition, setCommentCardTopPosition] = useState(0);
    const [windowWidth, setWindowWidth] = useState(0);
    const [subcommnetsList, setSubcommnetsList] = useState([]);
    const [showReply, setShowReply] = useState(false);
    const [totalSubcomments, setTotalSubcomments] = useState(0);
    const [subCommentsPageCount, setSubCommentsPageCount] = useState(0);
    const [currentPageSubcomments, setCurrentPageSubcomments] = useState(1);
    const [subcommentText, setSubcommentText] = useState('');
    const [commentHasReplays, setCommentHasReplays] = useState(false);
    const [showReportCommentPopup, setShowReportCommentPopup] = useState(false);
    const [drepData, setDrepData] = useState(null);

    const handleGetDrepData = async () => {
        try {
            const data = await fetchDRepVotingPowerList([
                comment?.attributes?.drep_id,
            ]);

            if (!data) return;

            if (data?.length === 0) return;
            setDrepData(data[0]);
        } catch (error) {
            console.error(error);
        }
    };

    const loadSubComments = async (page = 1) => {
        try {
            let query = `filters[comment_parent_id]=${comment?.id}&pagination[page]=${page}&pagination[pageSize]=3&sort[createdAt]=desc&populate[comments_reports][populate][reporter][fields][0]=username&populate[comments_reports][populate][maintainer][fields][0]=username`;
            const { comments, pgCount, total } = await getComments(query);
            if (!comments) return;

            if (page > currentPageSubcomments) {
                setSubcommnetsList((prev) => [...prev, ...comments]);
            } else {
                if (page === 1) {
                    setCurrentPageSubcomments(1);
                }
                setSubcommnetsList(comments);
            }
            setSubCommentsPageCount(pgCount);
            setTotalSubcomments(total);
        } catch (error) {
            console.error(error);
        }
    };
    const handleCreateComment = async () => {
        setLoading(true);
        try {
            const newComment = await createComment({
                proposal_id: comment?.attributes?.proposal_id?.toString(),
                comment_parent_id: comment?.id?.toString(),
                comment_text: subcommentText,
            });

            if (!newComment) return;
            setSubcommentText('');
            loadSubComments(1);
            setCommentHasReplays(true);
            setShowReply(false);
            if (!newComment?.data?.attributes?.comment_parent_id) {
                setRefetchProposal(true);
            }
            addSuccessAlert('Commented successfully');
        } catch (error) {
            addErrorAlert('Failed to comment');
            console.error(error);
        } finally {
            setLoading(false);
        }
    };
    const handleChange = (event) => {
        let value = event.target.value || '';
        if (value.length <= subcommentMaxLength) {
            setSubcommentText(value);
        }
    };
    const handleBlur = (event) => {
        const cleanedValue = event.target.value
            .replace(/[^\S\n]+/g, ' ')
            .trim();
        setSubcommentText(cleanedValue);
    };
    const isUserReporter = (curComment) => {
        try {
            return curComment.attributes.comments_reports.data.some(
                (report) => {
                    return (
                        report.attributes.moderation_status !== false &&
                        report.attributes.reporter.data.attributes.username ===
                            user.user.username
                    );
                }
            );
        } catch (e) {
            return false;
        }
    };
    const handleReportComment = async (curComment) => {
        //   setLoading(true);
        try {
            if (isUserReporter(curComment)) {
                let x = curComment.attributes.comments_reports.data.filter(
                    (report) => {
                        return (
                            report.attributes.moderation_status !== false &&
                            report.attributes.reporter.data.attributes
                                .username === user.user.username
                        );
                    }
                );
                let d = await removeCommentReport(x[0].id);
            } else {
                setShowReportCommentPopup(true);
            }
            if (curComment.id == comment.id) {
                fetchComments(1);
            } else loadSubComments(currentPageSubcomments);
        } catch (error) {
            console.error(error);
        } finally {
            // setLoading(false);
        }
    };
    const handleCloseReportPopup = () => {
        setShowReportCommentPopup(false);
    };
    const handleCancelReporting = () => {
        setShowReportCommentPopup(false);
    };
    const handleProceedReport = async (com) => {
        let x = await addCommentReport(com, user.user.id);
        if (curComment.id == comment.id) {
            fetchComments(1);
        } else loadSubComments(currentPageSubcomments);
        setShowReportCommentPopup(false);
    };

    useEffect(() => {
        if (readOnly) {
            setSubcommnetsList(showSubcomments ? archivedReplies : []);
            return;
        }
        if (showSubcomments) {
            loadSubComments(1);
            setCommentHasReplays(false);
        }
    }, [showSubcomments, comment, archivedReplies]);
    useEffect(() => {
        if (window) {
            setWindowWidth(window?.innerWidth);
        }
        const handleResize = () => {
            setWindowWidth(window?.innerWidth);
        };
        window?.addEventListener('resize', handleResize);
        return () => window?.removeEventListener('resize', handleResize);
    }, [window]);

    useEffect(() => {
        if (showMoreRef.current) {
            const showMore = showMoreRef.current.getBoundingClientRect();
            setShowMoreTopPosition(showMore.top);
            const commentCard = commentCardRef.current.getBoundingClientRect();
            setCommentCardTopPosition(commentCard.top);
        }
    }, [showMoreRef, windowWidth, isExpanded]);

    useEffect(() => {
        if (comment?.attributes?.drep_id) {
            handleGetDrepData();
        } else {
            setDrepData(null);
        }
    }, [comment]);

    const [clickedHash, setClickedHash] = useState(null);

    const handleMarkdownLinkClick = (href, e) => {
        const isSameHash = window?.location?.href === href;
        if (isSameHash) {
            e?.preventDefault(); // Do not reload
            setClickedHash(null); // pre reset
            setTimeout(() => {
                setClickedHash(window?.location?.hash.slice(1));
            }, 50);
        }
    };

    useEffect(() => {
        if (clickedHash) {
            const section = document?.querySelector(
                `[data-section='${clickedHash}']`
            );

            if (section) {
                const rect = section?.getBoundingClientRect();
                const offsetTop = rect.top + window.scrollY - 20;

                const header = document?.querySelector('header');
                const headerHeight = header ? header?.offsetHeight : 80;

                window.scrollTo({
                    top: offsetTop - headerHeight,
                    behavior: 'smooth',
                });
            }
        }
    }, [clickedHash]);

    return (
        <Card
            sx={{
                position: 'relative',
                overflow: 'visible',
                borderRadius: '12px',
                boxShadow: (theme) => theme.shadows[3],
                backgroundColor: 'rgba(255, 255, 255, 0.3)',
            }}
            data-testid={`comment-${comment?.id}-card`}
        >
            <Box
                data-testid={`comment-${comment?.id}-content-card`}
                sx={{ p: 3, pl: { xxs: 1, md: 2 } }}
            >
                <Box
                    display='flex'
                    ref={commentCardRef}
                    sx={{
                        position: 'relative',
                    }}
                >
                    <Box
                        width={{ xxs: '40px', md: '56px' }}
                        minWidth={{ xxs: '40px', md: '56px' }}
                        display='flex'
                        flexDirection='column'
                        alignItems='center'
                    >
                        <Box
                            sx={{
                                minWidth: '24px',
                                width: '24px',
                                minHeight: '24px',
                                height: '24px',
                                backgroundColor: 'lightBlue',
                                borderRadius: '50%',
                                visibility:
                                    commentHasReplays ||
                                    comment?.attributes?.subcommens_number
                                        ? 'visible'
                                        : 'hidden',
                            }}
                        ></Box>

                        <Box
                            sx={{
                                width: '2px',
                                height: '100%',
                                backgroundColor: 'lightBlue',
                                marginTop: '4px',
                                visibility:
                                    commentHasReplays ||
                                    comment?.attributes?.subcommens_number
                                        ? 'visible'
                                        : 'hidden',
                            }}
                        ></Box>

                        {commentHasReplays ||
                        comment?.attributes?.subcommens_number ? (
                            <Box
                                sx={{
                                    py: '4px',
                                    position: 'absolute',
                                    maxHeight: '32px',
                                    top: `${
                                        showMoreTopPosition -
                                        commentCardTopPosition +
                                        20
                                    }px`,
                                    backgroundColor: 'neutralWhite',
                                }}
                            >
                                {showSubcomments ? (
                                    <IconMinusCircle
                                        width='24'
                                        height='24'
                                        fill={theme.palette.primary.main}
                                        onClick={() =>
                                            setShowSubcomments((prev) => !prev)
                                        }
                                        cursor={'pointer'}
                                    />
                                ) : (
                                    <IconPlusCircle
                                        width='24'
                                        height='24'
                                        fill={theme.palette.primary.main}
                                        onClick={() =>
                                            setShowSubcomments((prev) => !prev)
                                        }
                                        cursor={'pointer'}
                                    />
                                )}
                            </Box>
                        ) : null}
                    </Box>
                    <Box
                        sx={{
                            width: '100%',
                            minWidth: 0,
                        }}
                    >
                        <UsernameSection
                            drepData={drepData}
                            comment={comment}
                        />

                        <Box
                            width={'100%'}
                            display={'flex'}
                            justifyContent={'space-between'}
                            mt={0.5}
                            mb={1.5}
                        >
                            <Typography
                                variant='caption'
                                component='span'
                                sx={{
                                    textTransform: 'uppercase',
                                    color: 'neutralGray',
                                }}
                            >
                                {formatDateWithOffset(
                                    new Date(comment?.attributes?.createdAt),
                                    0,
                                    'dd/MM/yyyy - p',
                                    'UTC'
                                )}
                            </Typography>
                            <Tooltip
                                paragraphOne='Report inappropriate comment'
                                arrow
                                placement='top'
                            >
                                <Box>
                                    <Typography
                                        variant='body2'
                                        fontWeight={400}
                                        float='right'
                                    >
                                        {/* <Link onClick={() =>handleReportComment(comment)} >{isUserReporter(comment)?"Comment reported" : "Report comment"}</Link> */}
                                    </Typography>
                                    {/* <IconFlag
                                        id={"report-flag-"+comment?.id}
                                        width={24}
                                        height={24}
                                        fill={isUserReporter(comment) ? 'red' : 'none'}
                                        stroke={isUserReporter(comment) ? 'red' : theme.palette.neutralGray}
                                        onClick={() => handleReportComment(comment)}
                                        sx={{
                                            cursor: 'pointer',
                                            marginLeft: 1,
                                        }}
                                    /> */}
                                </Box>
                            </Tooltip>
                        </Box>
                        {isCommentRestricted(comment) === true ? (
                            <Typography
                                variant='body2'
                                fontWeight={400}
                                sx={{
                                    maxWidth: '100%',
                                    wordWrap: 'break-word',
                                    whiteSpace: 'pre-line',
                                }}
                                data-testid={`comment-${comment?.id}-content`}
                                // ref={showMoreRef}
                            >
                                Restricted comment due to reports
                            </Typography>
                        ) : null}
                        {isCommentRestricted(comment) === false ? (
                            // <Typography
                            //     variant='body2'
                            //     sx={{
                            //         maxWidth: '100%',
                            //         wordWrap: 'break-word',
                            //         whiteSpace: 'pre-line',
                            //     }}
                            //     data-testid={`comment-${comment?.id}-content`}
                            //     ref={showMoreRef}
                            // >
                            //     {sliceString(
                            //               comment?.attributes?.comment_text
                            //           ) || ''}
                            // </Typography>

                            <Box ref={showMoreRef}>
                                <MarkdownTypography
                                    content={
                                        isExpanded
                                            ? comment?.attributes?.comment_text
                                            : sliceString(
                                                  comment?.attributes
                                                      ?.comment_text
                                              ) || ''
                                    }
                                    testId={`comment-${comment?.id}-content`}
                                    onLinkClick={(href, e) =>
                                        handleMarkdownLinkClick(href, e)
                                    }
                                />
                            </Box>
                        ) : null}

                        {comment?.attributes?.comment_text?.length >
                            maxLength && (
                            <Box mt={1.5}>
                                <Link
                                    onClick={() => setIsExpanded(!isExpanded)}
                                    underline='none'
                                    sx={readMoreLinkSx}
                                >
                                    {isExpanded
                                        ? 'Show less'
                                        : 'Read full comment'}
                                </Link>
                            </Box>
                        )}

                        <Box
                            display={'flex'}
                            justifyContent={'space-between'}
                            alignItems={'center'}
                            flexDirection={'row'}
                            flexWrap={'wrap'}
                            gap={1}
                            mt={2}
                            pt={2}
                            sx={{
                                borderTop: 1,
                                borderColor: 'lightBlue',
                            }}
                        >
                            <Box display={'flex'} alignItems={'center'}>
                                <IconChat width={20} height={20} />
                                <Typography
                                    variant='body2'
                                    fontWeight={500}
                                    marginLeft={1}
                                >
                                    {totalSubcomments > 0
                                        ? totalSubcomments
                                        : comment?.attributes
                                              ?.subcommens_number || 0}
                                </Typography>
                            </Box>
                            {readOnly ||
                            proposal?.attributes?.content?.attributes
                                ?.prop_submitted ? null : (
                                <Box display='flex' gap={2}>
                                    {checkShowValidation(
                                        true,
                                        walletAPI,
                                        user
                                    ) && (
                                        <UserValidation
                                            type='comment'
                                            drepCheck={checkIfDrepIsSignedIn(
                                                walletAPI
                                            )}
                                            drepRequired={true}
                                        />
                                    )}
                                    <Button
                                        variant='outlined'
                                        size='medium'
                                        startIcon={
                                            showReply ? (
                                                <IconMinus
                                                    fill={
                                                        theme.palette.primary
                                                            .main
                                                    }
                                                />
                                            ) : (
                                                <IconPlus
                                                    fill={
                                                        theme.palette.primary
                                                            .main
                                                    }
                                                />
                                            )
                                        }
                                        onClick={() =>
                                            setShowReply((prev) => !prev)
                                        }
                                        data-testid='reply-button'
                                        disabled={
                                            checkShowValidation(
                                                true,
                                                walletAPI,
                                                user
                                            )
                                        }
                                    >
                                        {showReply ? 'Cancel' : 'Reply'}
                                    </Button>
                                </Box>
                            )}
                        </Box>

                        {showReply && !readOnly ? (
                            <Box>
                                <PdfTextArea
                                    layoutStyles={{
                                        mt: 2,
                                    }}
                                    name='subcomment'
                                    placeholder='Add comment'
                                    value={subcommentText || ''}
                                    onChange={(e) => handleChange(e)}
                                    onBlur={handleBlur}
                                    maxLength={subcommentMaxLength}
                                    spellCheck='false'
                                    autoCorrect='off'
                                    autoCapitalize='none'
                                    autoComplete='off'
                                    dataTestId='reply-input'
                                />

                                <Box
                                    display={'flex'}
                                    justifyContent={'flex-end'}
                                    mt={1.5}
                                >
                                    <Button
                                        variant='contained'
                                        size='large'
                                        sx={{ minWidth: 140 }}
                                        onClick={
                                            user?.user?.govtool_username
                                                ? () => {
                                                      handleCreateComment();
                                                      setShowSubcomments(true);
                                                  }
                                                : () =>
                                                      setOpenUsernameModal({
                                                          open: true,
                                                          callBackFn: () => {},
                                                      })
                                        }
                                        disabled={
                                            !subcommentText ||
                                            !walletAPI?.address
                                        }
                                        endIcon={
                                            <IconReply
                                                height={18}
                                                width={18}
                                                fill={
                                                    !subcommentText ||
                                                    !walletAPI?.address
                                                        ? gray.c300
                                                        : theme.palette.neutralWhite
                                                }
                                            />
                                        }
                                        data-testid='reply-comment-button'
                                    >
                                        Comment
                                    </Button>
                                </Box>
                            </Box>
                        ) : null}

                        {showSubcomments &&
                            subcommnetsList?.map((subcomment, index) => (
                                <Subcomponent
                                    key={index}
                                    comment={subcomment}
                                    handleMarkdownLinkClick={
                                        handleMarkdownLinkClick
                                    }
                                />
                            ))}

                        {showSubcomments &&
                            currentPageSubcomments < subCommentsPageCount && (
                                <Box
                                    marginY={2}
                                    display={'flex'}
                                    justifyContent={'flex-end'}
                                >
                                    <Button
                                        variant='text'
                                        size='medium'
                                        onClick={() => {
                                            loadSubComments(
                                                currentPageSubcomments + 1
                                            );
                                            setCurrentPageSubcomments(
                                                (prev) => prev + 1
                                            );
                                        }}
                                    >
                                        Load more
                                    </Button>
                                </Box>
                            )}
                    </Box>
                </Box>
            </Box>
        </Card>
    );
};
export default CommentCard;
