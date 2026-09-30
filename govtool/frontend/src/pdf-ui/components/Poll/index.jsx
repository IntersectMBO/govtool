import {
    Box,
    Card,
    CardContent,
    Divider,
    LinearProgress,
} from '@mui/material';
import { Button, Typography } from '@atoms';
import { PdfStatusModal } from '../PdfModal';
import { useEffect, useState } from 'react';
import { useAppContext } from '../../context/context';
import {
    closePoll,
    createPollVote,
    getUserPollVote,
    updatePollVote,
} from '../../lib/api';
import { formatDateWithOffset } from '../../lib/utils';
import {
    pollBarSx,
    pollCardContentSx,
    pollCardSx,
    pollCountSx,
    pollDividerSx,
    pollTitleSx,
} from './pollStyles';

const Poll = ({
    fetchActivePoll = false,
    fetchUnactivePolls = false,
    proposalUserId,
    proposalAuthorUsername,
    proposalSubmitted,
    poll,
}) => {
    const { user, setLoading, setOpenUsernameModal } = useAppContext();
    const [userPollVote, setUserPollVote] = useState(null);
    const [showChangeVoteModal, setShowChangeVoteModal] = useState(false);
    const [showClosePollModal, setShowClosePollModal] = useState(false);

    const fetchUserPollVote = async (id) => {
        try {
            const response = await getUserPollVote({ pollID: id });
            if (!response) return;
            setUserPollVote(response);
        } catch (error) {
            console.error(error);
        }
    };

    const totalVotesGreaterThanZero = (pollData) => {
        const yes = +pollData?.attributes?.poll_yes;
        const no = +pollData?.attributes?.poll_no;

        if (yes + no > 0) {
            return true;
        } else {
            return false;
        }
    };

    const calculatePercentage = (pollData, yes = true) => {
        return Math.round(
            (+pollData?.attributes?.[yes ? 'poll_yes' : 'poll_no'] /
                (+pollData?.attributes?.poll_yes +
                    +pollData?.attributes?.poll_no)) *
                100
        );
    };

    const handlePollVote = async ({ vote }) => {
        try {
            const response =
                // userProposalVote
                // ? await updateProposalLikesOrDislikes({
                // 		proposalVoteID: userProposalVote?.id,
                // 		updateData: data,
                //   })
                // :
                await createPollVote({
                    createData: { poll_id: `${poll?.id}`, vote_result: vote },
                });

            if (!response) return;

            setUserPollVote(response);
            if (fetchActivePoll) {
                fetchActivePoll();
            }
        } catch (error) {
            console.error(error);
        }
    };

    const toggleChangeVoteModal = () => {
        setShowChangeVoteModal((prev) => !prev);
    };
    const toggleClosePollModal = () => {
        setShowClosePollModal((prev) => !prev);
    };

    const handlePollVoteChange = async () => {
        setLoading(true);
        try {
            const response = await updatePollVote({
                pollVoteID: userPollVote?.id,
                updateData: {
                    vote_result: !userPollVote?.attributes?.vote_result,
                },
            });

            if (!response) return;
            setUserPollVote(response);
            if (fetchActivePoll) {
                fetchActivePoll();
            }
            toggleChangeVoteModal();
        } catch (error) {
            console.error(error);
        } finally {
            setLoading(false);
        }
    };

    const closeProposalPoll = async () => {
        try {
            const response = await closePoll({ pollID: poll?.id });

            if (!response) return;

            if (fetchActivePoll) {
                fetchActivePoll();
            }
            if (fetchUnactivePolls) {
                fetchUnactivePolls();
            }
            toggleClosePollModal();
        } catch (error) {
            console.error(error);
        }
    };

    useEffect(() => {
        if (user) {
            if (poll) {
                if (!userPollVote) {
                    fetchUserPollVote(poll?.id);
                }
            }
        }
    }, [user, poll]);

    if (poll) {
        return (
            <>
                {proposalSubmitted ? null : user &&
                  !userPollVote &&
                  user?.user?.id !== +proposalUserId &&
                  poll?.attributes?.is_poll_active ? (
                    <Card
                        sx={{ ...pollCardSx, mb: 3 }}
                        data-testid='poll-vote-card'
                    >
                        <CardContent sx={pollCardContentSx}>
                            <Typography variant='body2' fontWeight={400}>
                                @{proposalAuthorUsername}
                            </Typography>
                            <Typography
                                variant='caption'
                                component='span'
                                sx={{ color: 'neutralGray' }}
                                mt={0.5}
                            >
                                {formatDateWithOffset(new Date(
                                    poll?.attributes?.poll_start_dt),0,"dd/MM/yyyy - p","UTC"
                                )}
                            </Typography>
                            <Typography sx={pollTitleSx} my={2}>
                                Is this proposal ready to be submitted on chain?
                            </Typography>

                            <Button
                                variant='outlined'
                                size='large'
                                sx={{ mb: 1.5 }}
                                onClick={
                                    user?.user?.govtool_username
                                        ? () => handlePollVote({ vote: true })
                                        : () =>
                                              setOpenUsernameModal({
                                                  open: true,
                                                  callBackFn: () => {},
                                              })
                                }
                                data-testid='poll-yes-button'
                            >
                                Yes
                            </Button>
                            <Button
                                variant='outlined'
                                size='large'
                                onClick={
                                    user?.user?.govtool_username
                                        ? () => handlePollVote({ vote: false })
                                        : () =>
                                              setOpenUsernameModal({
                                                  open: true,
                                                  callBackFn: () => {},
                                              })
                                }
                                data-testid='poll-no-button'
                            >
                                No
                            </Button>
                        </CardContent>
                    </Card>
                ) : null}
                <Card sx={pollCardSx} data-testid='poll-result-card'>
                    <CardContent sx={pollCardContentSx}>
                        <Typography variant='body2' fontWeight={400}>
                            @{proposalAuthorUsername}
                        </Typography>
                        <Typography
                            variant='caption'
                            component='span'
                            sx={{ color: 'neutralGray' }}
                            mt={0.5}
                        >
                            {formatDateWithOffset(new Date(
                                poll?.attributes?.poll_start_dt),0,"dd/MM/yyyy - p","UTC"
                            )}
                        </Typography>
                        <Typography sx={pollTitleSx} mt={2}>
                            Poll Results
                        </Typography>
                        <Typography variant='body2' fontWeight={400} mt={1}>
                            Is this proposal ready to be submitted on chain?
                        </Typography>
                        <Divider variant='fullWidth' sx={pollDividerSx} />
                        <Typography
                            variant='caption'
                            component='span'
                            sx={{ color: 'neutralGray', fontWeight: 500 }}
                        >
                            Total votes:{' '}
                            {+poll?.attributes?.poll_yes +
                                +poll?.attributes?.poll_no}
                        </Typography>
                        <Box
                            display={'flex'}
                            width={'100%'}
                            justifyContent={'flex-start'}
                            alignItems={'center'}
                            mt={1.5}
                            gap={1.5}
                        >
                            <Typography
                                variant='caption'
                                component='span'
                                sx={pollCountSx(
                                    'yes',
                                    userPollVote?.attributes?.vote_result ===
                                        true
                                )}
                                data-testid='poll-yes-count'
                            >
                                {`Yes: (${
                                    totalVotesGreaterThanZero(poll)
                                        ? calculatePercentage(poll, true)
                                        : 0
                                }%)`}
                            </Typography>
                            {user?.user?.id !== +proposalUserId && (
                                <LinearProgress
                                    variant='determinate'
                                    color='primary'
                                    value={
                                        totalVotesGreaterThanZero(poll)
                                            ? calculatePercentage(poll, true)
                                            : 0
                                    }
                                    sx={pollBarSx('yes')}
                                />
                            )}
                        </Box>
                        <Box
                            display={'flex'}
                            width={'100%'}
                            justifyContent={'flex-start'}
                            alignItems={'center'}
                            mt={1.5}
                            gap={1.5}
                        >
                            <Typography
                                variant='caption'
                                component='span'
                                sx={pollCountSx(
                                    'no',
                                    userPollVote?.attributes?.vote_result ===
                                        false
                                )}
                                data-testid='poll-no-count'
                            >
                                {`No: (${
                                    totalVotesGreaterThanZero(poll)
                                        ? calculatePercentage(poll, false)
                                        : 0
                                }%)`}
                            </Typography>
                            {user?.user?.id !== +proposalUserId && (
                                <LinearProgress
                                    variant='determinate'
                                    color='primary'
                                    value={
                                        totalVotesGreaterThanZero(poll)
                                            ? calculatePercentage(poll, false)
                                            : 0
                                    }
                                    sx={pollBarSx('no')}
                                />
                            )}
                        </Box>
                        {user?.user?.id === +proposalUserId &&
                            poll?.attributes?.is_poll_active && (
                                <Box
                                    mt={3}
                                    display={'flex'}
                                    justifyContent={'flex-end'}
                                >
                                    <Button
                                        variant='outlined'
                                        size='large'
                                        onClick={toggleClosePollModal}
                                        data-testid='close-poll-button'
                                    >
                                        Close Poll
                                    </Button>
                                </Box>
                            )}
                        {user &&
                            userPollVote &&
                            user?.user?.id !== +proposalUserId &&
                            poll?.attributes?.is_poll_active && (
                                <Box
                                    mt={3}
                                    display={'flex'}
                                    justifyContent={'flex-end'}
                                >
                                    <Button
                                        variant='outlined'
                                        size='large'
                                        onClick={toggleChangeVoteModal}
                                        data-testid='change-vote-button'
                                    >
                                        Change Vote
                                    </Button>
                                </Box>
                            )}
                    </CardContent>
                </Card>
                <PdfStatusModal
                    open={showChangeVoteModal}
                    onClose={toggleChangeVoteModal}
                    dataTestId='change-poll-vote-modal'
                    title='Do you really want to change your Poll Vote?'
                    titleId='modal-modal-title'
                    titleComponent='h2'
                    message={`Currently your Poll Vote is ${
                        userPollVote?.attributes?.vote_result ? 'Yes' : 'No'
                    }. After changing your vote, it will be ${
                        userPollVote?.attributes?.vote_result ? 'No' : 'Yes'
                    }.`}
                    messageId='modal-modal-description'
                    primaryButton={{
                        label: "I don't want to change",
                        onClick: toggleChangeVoteModal,
                        dataTestId: 'change-poll-vote-no-button',
                    }}
                    secondaryButton={{
                        label: 'Yes, change my Poll Vote',
                        onClick: handlePollVoteChange,
                        dataTestId: 'change-poll-vote-yes-button',
                    }}
                />
                <PdfStatusModal
                    open={showClosePollModal}
                    onClose={toggleClosePollModal}
                    title='Do you really want to close the Poll?'
                    titleId='modal-modal-title'
                    titleComponent='h2'
                    message='Closing the poll will prevent users from submitting additional votes.'
                    messageId='modal-modal-description'
                    primaryButton={{
                        label: 'Close the Poll',
                        onClick: () => closeProposalPoll(),
                        dataTestId: 'close-the-poll-button',
                    }}
                    secondaryButton={{
                        label: 'Cancel',
                        onClick: toggleClosePollModal,
                        dataTestId: 'cancel-the-poll-button',
                    }}
                />
            </>
        );
    }
};

export default Poll;
