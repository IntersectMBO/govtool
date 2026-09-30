import React, { useEffect, useState } from 'react';
import { Box, Link } from '@mui/material';
import { Typography } from '@atoms';
import { PdfModal } from '../PdfModal';
import { getBudgetDiscussionPollVotes } from '../../lib/api';
import { useAppContext } from '../../context/context';
import {
    correctVoteAdaFormat,
    formatDateWithOffset,
    LOVELACE,
} from '../../lib/utils';

const DrepVotersDialog = ({ open, handleClose, pollID }) => {
    const { fetchDRepVotingPowerList } = useAppContext();
    const [drepList, setDrepList] = useState([]);
    const [totalVotingPower, setTotalVotingPower] = useState(0);

    const fetchPollVotes = async () => {
        const votes = await getBudgetDiscussionPollVotes({
            pollID: pollID,
            vote: open === 'YES' ? true : false,
        });
        if (votes?.length === 0) return;
        const drepIds = votes?.map((vote) => vote?.attributes?.drep_id);
        const drepWhoVoted = await fetchDRepVotingPowerList(drepIds);

        drepWhoVoted.sort((a, b) => {
            return b?.votingPower - a?.votingPower;
        });

        let totalPower = 0;
        for (const drep of drepWhoVoted) {
            totalPower += +drep?.votingPower;
            const findVote = votes?.find(
                (vote) => vote?.attributes?.drep_id === drep?.hashRaw
            );

            drep.voted_at = findVote?.attributes?.createdAt;
        }

        setTotalVotingPower(totalPower);
        setDrepList(drepWhoVoted);
    };

    useEffect(() => {
        if (!open) return;
        setDrepList([]);
        setTotalVotingPower(0);
        fetchPollVotes();
    }, [open]);

    return (
        <PdfModal
            open={open ? true : false}
            onClose={handleClose}
            aria-labelledby='alert-dialog-title'
            aria-describedby='alert-dialog-description'
            closeButtonDataTestId='close-modal-button'
            sx={{ maxWidth: '600px' }}
        >
            <Box>
                <Typography
                    variant='headline5'
                    component='h5'
                    sx={{
                        mb: 4,
                    }}
                >
                    DReps who voted '{open}'
                </Typography>

                <Typography
                    variant='body1'
                    fontWeight={400}
                    sx={{
                        mb: 3,
                        color: 'textBlack',
                    }}
                >
                    List of DReps who voted '{open}' for this proposal to be
                    included in the next Cardano Budget
                </Typography>

                <Box
                    sx={{
                        mb: 3,
                        px: 2.25,
                        py: 1.5,
                        borderRadius: 3,
                        bgcolor: 'rgba(214, 226, 255, 0.5)',
                    }}
                >
                <Typography
                    variant='caption'
                    component='span'
                    sx={{ color: 'neutralGray', fontWeight: 500 }}
                >
                    Total DRep Stake for this Proposal
                </Typography>
                <Typography
                    variant='title2'
                    component='h6'
                    sx={{ fontWeight: 600 }}
                >
                    ₳ {correctVoteAdaFormat(totalVotingPower / LOVELACE)}
                </Typography>
                </Box>

                {drepList?.length === 0 && (
                    <Typography variant='body1' fontWeight={400}>
                        No DReps voted '{open}' on this proposal
                    </Typography>
                )}
                {drepList?.length > 0 && (
                    <>
                        <Typography
                            variant='body1'
                            fontWeight={400}
                            sx={{ mb: 2 }}
                        >
                            DReps who voted '{open}' on this proposal:
                        </Typography>

                        {drepList?.map((drep, index) => (
                            <Box
                                key={index}
                                sx={{
                                    display: 'flex',
                                    alignItems: 'flex-start',
                                    gap: 1.5,
                                    mb: 1.5,
                                    p: 2,
                                    border: 1,
                                    borderColor: 'lightBlue',
                                    borderRadius: 3,
                                    bgcolor: 'rgba(255, 255, 255, 0.3)',
                                }}
                            >
                                <Box>
                                    <Box
                                        sx={{
                                            lineHeight: 0,
                                            fontSize: 40,
                                            color: 'primaryBlue',
                                        }}
                                    >
                                        .
                                    </Box>
                                </Box>
                                <Box
                                    sx={{
                                        display: 'flex',
                                        flexDirection: 'column',
                                        minWidth: 0,
                                    }}
                                >
                                    <Link
                                        href={
                                            '/connected/drep_directory/' +
                                            drep?.view
                                        }
                                        target='_blank'
                                        rel='noopener noreferrer'
                                        underline='none'
                                        sx={{ color: 'primaryBlue' }}
                                    >
                                        <Typography
                                            variant='body1'
                                            sx={{ wordBreak: 'break-word' }}
                                        >
                                            {drep?.givenName}
                                        </Typography>
                                    </Link>
                                    <Typography
                                        variant='caption'
                                        component='span'
                                        sx={{ color: 'neutralGray' }}
                                    >
                                        {drep?.view
                                            ? drep?.view?.slice(0, 20) + '...'
                                            : '-'}
                                    </Typography>
                                    <Typography
                                        variant='title2'
                                        component='h6'
                                        fontSize={16}
                                        fontWeight={600}
                                        lineHeight='24px'
                                        mt={0.5}
                                    >
                                        ₳{' '}
                                        {correctVoteAdaFormat(
                                            drep?.votingPower / LOVELACE
                                        )}
                                    </Typography>
                                    {drep?.voted_at && (
                                        <Typography
                                            variant='caption'
                                            component='span'
                                            sx={{ color: 'neutralGray' }}
                                        >
                                            Vote submitted on{' '}
                                            {formatDateWithOffset(
                                                new Date(drep?.voted_at),
                                                0,
                                                'dd/MM/yyyy - p',
                                                'UTC'
                                            )}
                                        </Typography>
                                    )}
                                </Box>
                            </Box>
                        ))}
                    </>
                )}
            </Box>
        </PdfModal>
    );
};

export default DrepVotersDialog;
