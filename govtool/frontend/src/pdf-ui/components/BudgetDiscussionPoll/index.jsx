import { Box, Card, CardContent, Divider, alpha } from '@mui/material';
import { Button, Typography } from '@atoms';
import { PdfStatusModal } from '../PdfModal';
import { useEffect, useState } from 'react';
import { useAppContext } from '../../context/context';
import {
    createBudgetDiscussionPollVote,
    getUserBudgetDiscussionPollVote,
    updateBudgetDiscussionPollVote,
} from '../../lib/api';
import { decodeJWT, formatDateWithOffset } from '../../lib/utils';
import DrepVotersDialog from '../DrepVotersDialog';
import { add, max } from 'date-fns';
import { checkIfDrepIsSignedIn, checkShowValidation } from '../../lib/helpers';
import UserValidation from '../UserValidation/UserValidation';
import {
    pollCardContentSx,
    pollCardSx,
    pollCountSx,
    pollDividerSx,
    pollTitleSx,
} from '../Poll/pollStyles';

const BudgetDiscussionPoll = ({
    fetchActivePoll = false,
    proposalUserId,
    proposalAuthorUsername,
    poll,
}) => {
    const {
        user,
        setLoading,
        setOpenUsernameModal,
        walletAPI,
        addSuccessAlert,
        addErrorAlert,
    } = useAppContext();
    const [userPollVote, setUserPollVote] = useState(null);
    const [showChangeVoteModal, setShowChangeVoteModal] = useState(false);
    const [showDrepVotersDialog, setShowDrepVotersDialog] = useState(false);

    const fetchUserPollVote = async (id) => {
        try {
            const response = await getUserBudgetDiscussionPollVote({
                pollID: id,
                userID: user?.user?.id,
            });

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

    const jwtData = decodeJWT();

    const handlePollVote = async ({ vote }) => {
        try {
            if (jwtData) {
                if (jwtData?.dRepID) {
                    const response = await createBudgetDiscussionPollVote({
                        createData: {
                            bd_poll_id: `${poll?.id}`,
                            vote_result: vote,
                            drep_voting_power:
                                walletAPI?.voter?.votingPower || '',
                        },
                    });

                    if (!response) return;

                    setUserPollVote(response);
                    if (fetchActivePoll) {
                        fetchActivePoll();
                    }

                    addSuccessAlert(
                        `Voted ${vote ? 'yes' : 'no'} successfully.`
                    );
                } else {
                    addErrorAlert(
                        'dRepID is not available in authorization token.'
                    );
                    throw new Error(
                        'dRepID is not available in authorization token.'
                    );
                }
            } else {
                addErrorAlert('Authorization token not available.');
                throw new Error('Authorization token not available.');
            }
        } catch (error) {
            addErrorAlert('Failed to submit vote.');
            console.error(error);
        }
    };

    const toggleChangeVoteModal = () => {
        setShowChangeVoteModal((prev) => !prev);
    };

    const handlePollVoteChange = async () => {
        setLoading(true);
        try {
            const response = await updateBudgetDiscussionPollVote({
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
            addSuccessAlert(
                `Voted ${userPollVote?.attributes?.vote_result ? 'no' : 'yes'} successfully.`
            );
        } catch (error) {
            addErrorAlert('Failed to submit vote.');
            console.error(error);
        } finally {
            setLoading(false);
        }
    };

    useEffect(() => {
        if (user) {
            fetchUserPollVote(poll?.id);
        }
    }, [user, poll]);

    // user &&
    //     userPollVote &&
    //     poll?.attributes?.is_poll_active &&
    //     (walletAPI?.voter?.isRegisteredAsDRep ||
    //         walletAPI?.voter?.isRegisteredAsSoleVoter) &&
    //     jwtData?.dRepID;

    // console.log('user', user);
    // console.log('userPollVote', userPollVote);
    // console.log('poll', poll?.attributes?.is_poll_active);
    // console.log('walletAPI', walletAPI?.voter?.isRegisteredAsDRep);
    // console.log('walletAPI', walletAPI?.voter?.isRegisteredAsSoleVoter);
    // console.log('jwtData', jwtData);
    // console.log('jwtData.dRepID', jwtData?.dRepID);

    // console.log(
    //     'log',
    //     user &&
    //         userPollVote &&
    //         poll?.attributes?.is_poll_active &&
    //         (walletAPI?.voter?.isRegisteredAsDRep ||
    //             walletAPI?.voter?.isRegisteredAsSoleVoter) &&
    //         jwtData?.dRepID
    // );

    if (poll) {
        return (
            <>
                {/* {user &&
                !userPollVote &&
                (walletAPI?.voter?.isRegisteredAsDRep ||
                    walletAPI?.voter?.isRegisteredAsSoleVoter) &&
                jwtData?.dRepID ? (
                    <Card
                        sx={{
                            mb: 3,
                            backgroundColor: (theme) => alpha(theme.palette.neutralWhite, 0.3),
                        }}
                        data-testid='poll-vote-card'
                    >
                        <CardContent
                            sx={{ display: 'flex', flexDirection: 'column' }}
                        >
                            <Typography variant='body1' fontWeight={600} my={2}>
                                Do you support this proposal to be included in
                                the next Cardano Budget?
                            </Typography>

                            <Button
                                variant='outlined'
                                sx={{ mb: 1 }}
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
                ) : null} */}

                <Card sx={pollCardSx} data-testid='poll-result-card'>
                    <CardContent sx={pollCardContentSx}>
                        <Box sx={{ display: 'flex', flexDirection: 'column' }}>
                            <Typography sx={pollTitleSx} mb={2}>
                                Should this proposal be included in the next
                                Cardano Budget?
                            </Typography>
                            {!(user &&
                                userPollVote &&
                                poll?.attributes?.is_poll_active &&
                                (walletAPI?.voter?.isRegisteredAsDRep ||
                                    walletAPI?.voter
                                        ?.isRegisteredAsSoleVoter) &&
                                jwtData?.dRepID) && (
                                    <Box>
                                        {checkShowValidation(
                                            true,
                                            walletAPI,
                                            user
                                        ) ? (
                                            <UserValidation
                                                type='drep-poll'
                                                drepCheck={checkIfDrepIsSignedIn(
                                                    walletAPI
                                                )}
                                                drepRequired={true}
                                            />
                                        ) : 

                                        (walletAPI?.voter?.isRegisteredAsDRep ||
                                            walletAPI?.voter
                                        ?.isRegisteredAsSoleVoter)&&(
                                            <Box
                                                sx={{
                                                    display: 'flex',
                                                    gap: 1.5,
                                                    flexDirection: 'row',
                                                    alignItems: 'center',
                                                    width: '100%',
                                                    maxWidth: { xxs: '100%', md: '360px' },
                                                }}
                                            >
                                                <Button
                                                    variant='outlined'
                                                    size='large'
                                                    sx={{ flex: 1 }}
                                                    onClick={
                                                        user?.user
                                                            ?.govtool_username
                                                            ? () =>
                                                                  handlePollVote(
                                                                      {
                                                                          vote: true,
                                                                      }
                                                                  )
                                                            : () =>
                                                                  setOpenUsernameModal(
                                                                      {
                                                                          open: true,
                                                                          callBackFn:
                                                                              () => {},
                                                                      }
                                                                  )
                                                    }
                                                    data-testid='poll-yes-button'
                                                >
                                                    Yes
                                                </Button>
                                                <Button
                                                    sx={{ flex: 1 }}
                                                    variant='outlined'
                                                    size='large'
                                                    onClick={
                                                        user?.user
                                                            ?.govtool_username
                                                            ? () =>
                                                                  handlePollVote(
                                                                      {
                                                                          vote: false,
                                                                      }
                                                                  )
                                                            : () =>
                                                                  setOpenUsernameModal(
                                                                      {
                                                                          open: true,
                                                                          callBackFn:
                                                                              () => {},
                                                                      }
                                                                  )
                                                    }
                                                    data-testid='poll-no-button'
                                                >
                                                    No
                                                </Button>
                                            </Box>
                                        )}
                                    </Box>
                                )}
                        </Box>
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
                            alignItems={'center'}
                            justifyContent={'space-between'}
                            mt={2}
                            gap={1}
                        >
                            <Typography
                                variant='body1'
                                sx={{
                                    ...pollCountSx('yes', false),
                                    fontSize: 14,
                                    lineHeight: '20px',
                                }}
                                data-testid='poll-yes-count'
                            >
                                {`Yes: (${
                                    totalVotesGreaterThanZero(poll)
                                        ? calculatePercentage(poll, true)
                                        : 0
                                }%)`}
                            </Typography>

                            <Button
                                variant='text'
                                size='medium'
                                onClick={() => setShowDrepVotersDialog('YES')}
                            >
                                See details
                            </Button>
                        </Box>
                        <Box
                            display={'flex'}
                            width={'100%'}
                            justifyContent={'space-between'}
                            alignItems={'center'}
                            mt={2}
                            gap={1}
                        >
                            <Typography
                                variant='body1'
                                sx={{
                                    ...pollCountSx('no', false),
                                    fontSize: 14,
                                    lineHeight: '20px',
                                }}
                                data-testid='poll-no-count'
                            >
                                {`No: (${
                                    totalVotesGreaterThanZero(poll)
                                        ? calculatePercentage(poll, false)
                                        : 0
                                }%)`}
                            </Typography>
                            <Button
                                variant='text'
                                size='medium'
                                onClick={() => setShowDrepVotersDialog('NO')}
                            >
                                See details
                            </Button>
                        </Box>

                        {user &&
                            userPollVote &&
                            poll?.attributes?.is_poll_active &&
                            (walletAPI?.voter?.isRegisteredAsDRep ||
                                walletAPI?.voter?.isRegisteredAsSoleVoter) &&
                            jwtData?.dRepID && (
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

                <DrepVotersDialog
                    open={showDrepVotersDialog}
                    handleClose={() => setShowDrepVotersDialog(false)}
                    pollID={poll?.id}
                />
            </>
        );
    }
};

export default BudgetDiscussionPoll;
