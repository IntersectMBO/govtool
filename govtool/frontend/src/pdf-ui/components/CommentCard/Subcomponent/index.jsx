import React, { useState, useEffect } from 'react';
import { Box, Modal, Link } from '@mui/material';
import { Typography } from '@atoms';
import { useAppContext } from '../../../context/context';
import CommentReportPopup from '../../CommentReportPopup';
import { isCommentRestricted } from '../../../lib/helpers';
import { formatDateWithOffset } from '../../../lib/utils';
import UsernameSection from '../UsernameSection';
import MarkdownTypography from '../../../lib/markdownRenderer';
import { readMoreLinkSx } from '../commentStyles';
const Subcomponent = ({ comment, handleMarkdownLinkClick }) => {
    const { fetchDRepVotingPowerList } = useAppContext();
    const [drepData, setDrepData] = useState({});
    const [showReportCommentPopup, setShowReportCommentPopup] = useState(false);
    const [isExpanded, setIsExpanded] = useState(false);
    const maxLength = 128;
    const sliceString = (str) => {
        if (!str) return '';
        if (str.length > maxLength) {
            return str.slice(0, maxLength - 3) + '...';
        }
        return str;
    };

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

    useEffect(() => {
        if (comment?.attributes?.drep_id) {
            handleGetDrepData();
        } else {
            setDrepData(null);
        }
    }, [comment]);

    return (
        <Box
            display='flex'
            mt={2}
            sx={{
                position: 'relative',
                p: 2,
                borderRadius: 3,
                backgroundColor: 'rgba(214, 226, 255, 0.2)',
            }}
            // data-testid={`subcomment-${comment?.id}-content`}
        >
            <Box
                sx={{
                    width: '100%',
                }}
            >
                <UsernameSection drepData={drepData} comment={comment} />
                <Box
                    display='flex'
                    alignItems='center'
                    sx={{
                        marginRight: 2,
                    }}
                >
                    <Box
                        display='flex'
                        width='100%'
                        justifyContent={'space-between'}
                    >
                        <Typography
                            variant='caption'
                            component='span'
                            float='left'
                            sx={{
                                textTransform: 'uppercase',
                                color: 'neutralGray',
                                mt: 0.5,
                                mb: 1.5,
                            }}
                        >
                            {formatDateWithOffset(
                                new Date(comment?.attributes?.createdAt),
                                0,
                                'dd/MM/yyyy - p',
                                'UTC'
                            )}
                        </Typography>
                        {/* <Tooltip 
                                title="Report inappropriate comment" 
                                arrow 
                                placement="top" 
                            > */}
                        <Box>
                            <Typography
                                variant='body2'
                                fontWeight={400}
                                float='right'
                            >
                                {/* <Link onClick={() =>handleReportComment(comment)} >{isUserReporter(comment)?"Comment reported" : "Report comment"}</Link> */}
                                {/* <IconFlag
                                    key={comment?.id}
                                    id={"report-flag-"+comment?.id}
                                    width={24}
                                    height={24}
                                    fill={isUserReporter(comment) ? 'red' : 'none'}
                                    stroke={isUserReporter(comment) ? 'red' : theme.palette.neutralGray}
                                    onClick={() => handleReportComment(comment)}                                    
                                    sx={{
                                        cursor: 'pointer',
                                        marginLeft: 1,
                                        float: "right",
                                    }}
                                /> */}
                            </Typography>
                            {showReportCommentPopup && (
                                <Box>
                                    <Modal
                                        open={showReportCommentPopup}
                                        sx={{
                                            display: 'flex',
                                            justifyContent: 'center',
                                            alignItems: 'center',
                                        }}
                                    >
                                        <Box>
                                            <CommentReportPopup
                                                commentId={comment?.id}
                                                onReport={handleProceedReport}
                                                onCancel={() =>
                                                    setShowReportCommentPopup(
                                                        false
                                                    )
                                                }
                                            />
                                        </Box>
                                    </Modal>
                                </Box>
                                // <CommentReportPopup onReport={() => handleProceedReport()} onCancel={() => handleCancelReporting()} />
                                // <Backdrop
                                //     sx={{ color: '#fff', zIndex: (theme) => theme.zIndex.drawer + 100 }}
                                //     open={showReportCommentPopup}
                                //     onClick={() => handleCloseReportPopup()}
                                //     >

                                // </Backdrop>
                            )}
                        </Box>
                    </Box>
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
                    >
                        Restricted comment due to reports
                    </Typography>
                ) : null}

                {isCommentRestricted(comment) === false ? (
                    <MarkdownTypography
                        content={
                            isExpanded
                                ? comment?.attributes?.comment_text
                                : sliceString(
                                      comment?.attributes?.comment_text
                                  ) || ''
                        }
                        testId={`subcomment-${comment?.id}-content`}
                        onLinkClick={(href, e) =>
                            handleMarkdownLinkClick &&
                            handleMarkdownLinkClick(href, e)
                        }
                    />
                ) : null}
                {/* <Typography
                    variant='body2'
                    data-testid={`reply-${comment?.id}-content`}
                    sx={{
                        maxWidth: '100%',
                        wordWrap: 'break-word',
                        whiteSpace: 'pre-line',
                    }}
                >
                    {!isCommentRestricted(comment)
                        ? isExpanded
                            ? comment?.attributes?.comment_text
                            : sliceString(comment?.attributes?.comment_text) ||
                              ''
                        : 'Restricted comment due to reports'}
                </Typography> */}

                {comment?.attributes?.comment_text?.length > maxLength && (
                    <Box mt={1.5}>
                        <Link
                            onClick={() => setIsExpanded(!isExpanded)}
                            underline='none'
                            sx={readMoreLinkSx}
                        >
                            {isExpanded ? 'Show less' : 'Read full comment'}
                        </Link>
                    </Box>
                )}
            </Box>
        </Box>
    );
};

export default Subcomponent;
