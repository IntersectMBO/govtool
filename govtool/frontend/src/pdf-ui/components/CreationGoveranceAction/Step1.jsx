'use client';

import { Box, List, ListItem } from '@mui/material';
import { Button, Typography } from '@atoms';
import { ProposalsList } from '..';
import { getProposals } from '../../lib/api';
import { useEffect, useState } from 'react';
import { useAppContext } from '../../context/context';
import { StepBox, StepButtons, StepHeading, stepButtonSx } from './StepLayout';

const Step1 = ({ setStep, setProposalData, onClose, setSelectedDraftId }) => {
    const [draftsEnabled, setDraftsEnabled] = useState(false);
    const [mounted, setMounted] = useState(false);

    const fetchProposals = async () => {
        try {
            const query = `filters[$and][0][is_draft]=true&pagination[page]=1&pagination[pageSize]=1`;

            const { total } = await getProposals(query);
            if (total === 0) return;
            setDraftsEnabled(true);
        } catch (error) {
            console.error(error);
        }
    };
    const {walletAPI} = useAppContext();
    useEffect(() => {
        if (!mounted) {
            setMounted(true);
        } else {
            fetchProposals();
        }
    }, [mounted]);

    const listItemSx = {
        display: 'list-item',
        paddingX: 0,
        paddingY: 0.5,
    };

    return (
        <Box display='flex' flexDirection='column'>
            <Box>
                {draftsEnabled ? (
                    <StepBox>
                        <StepHeading
                            title='Decide if you want to use Existing Draft or Create new proposal'
                        />
                        <Typography
                            variant='body1'
                            fontWeight={400}
                            sx={{
                                color: 'textBlack',
                                pt: 4,
                                pb: { xxs: 6, md: 4 },
                                textAlign: 'center',
                            }}
                        >
                            Existing Drafts can save you some time and
                            effort or simply start from stratch.
                        </Typography>
                        <Box sx={{ display: 'flex', justifyContent: 'center' }}>
                            <Button
                                variant='contained'
                                size='extraLarge'
                                sx={stepButtonSx}
                                onClick={() => setStep(2)}
                                data-testid='create-new-proposal-button'
                            >
                                Create new Proposal
                            </Button>
                        </Box>
                    </StepBox>
                ) : (
                    <StepBox>
                        <StepHeading title='Step to submit a Governance action' />
                        <List
                            sx={{
                                color: 'textBlack',
                                listStyleType: 'disc',
                                pl: 2.5,
                                pt: 4,
                                pb: { xxs: 6, md: 4 },
                            }}
                        >
                            <ListItem sx={listItemSx}>
                                <Typography variant='body1' fontWeight={400}>
                                    Before submitting a Governance
                                    Action on chain you need to submit a
                                    Proposal.
                                </Typography>

                                <Typography
                                    variant='body1'
                                    sx={{ fontWeight: 'bold', mt: 0.5 }}
                                >
                                    This allows you to get feedback from
                                    the community to refine and improve
                                    your proposal, increasing the
                                    chances of your Governance Action
                                    getting approved, and also building
                                    up supporting context in the form of
                                    metadata.
                                </Typography>
                            </ListItem>
                            <ListItem sx={listItemSx}>
                                <Typography variant='body1' fontWeight={400}>
                                    Once you are happy with your
                                    proposal you can open a poll to
                                    check ‘Is this proposal ready to be
                                    submitted on chain?’
                                </Typography>
                            </ListItem>
                            <ListItem sx={listItemSx}>
                                <Typography variant='body1' fontWeight={400}>
                                    If you get support on the poll you
                                    are ready to submit your proposal on
                                    chain as a Governance Action to get
                                    voted on.
                                </Typography>
                            </ListItem>
                            <ListItem sx={{ ...listItemSx, color: 'errorRed' }}>
                                <Typography
                                    variant='body1'
                                    fontWeight={400}
                                    sx={{ color: 'errorRed' }}
                                >
                                    Please be aware that Ledger and Trezor hardware wallet do not support submission of governance actions, but you'll still be able to create the proposal.
                                </Typography>
                            </ListItem>
                        </List>
                        <StepButtons
                            sx={{ mt: 0 }}
                            start={
                                <Button
                                    variant='outlined'
                                    size='extraLarge'
                                    sx={stepButtonSx}
                                    onClick={onClose}
                                    data-testid='cancel-button'
                                >
                                    Cancel
                                </Button>
                            }
                            end={
                                <Button
                                    variant='contained'
                                    size='extraLarge'
                                    sx={stepButtonSx}
                                    onClick={() => setStep(2)}
                                    data-testid='continue-button'
                                >
                                    Continue
                                </Button>
                            }
                        />
                    </StepBox>
                )}
            </Box>

            <Box mt={4}>
                <ProposalsList
                    isDraft={true}
                    startEdittinButtonClick={(proposal) => {
                        setStep(2);
                        setProposalData(
                            proposal?.attributes?.content?.attributes
                        );
                        setSelectedDraftId(proposal?.id);
                    }}
                    statusList={[]}
                />
            </Box>
        </Box>
    );
};

export default Step1;
