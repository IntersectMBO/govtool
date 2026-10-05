'use client';

import { Box, List, ListItem } from '@mui/material';
import { Button, Typography } from '@atoms';
import { getBudgetDiscussionDrafts } from '../../lib/api';
import { useEffect, useState } from 'react';
import { BudgetDiscussionsList } from '../';
import { StepperActionButtons } from '../BudgetDiscussionParts'
import StepBox from './StepBox';

const BudgetDiscussionInfo = ({setStep, step, onClose, setBudgetDiscussionData, currentBudgetDiscussionData, selectedDraftId, setSelectedDraftId, handleSaveDraft}) => {
    const [draftsEnabled, setDraftsEnabled] = useState(false);
    const [mounted, setMounted] = useState(false);

    const fetchBudgetDiscussionDrafts = async () => {
        try {
            const res = await getBudgetDiscussionDrafts();
            if (res?.meta?.pagination?.total === 0) return;
            setDraftsEnabled(true);
        } catch (error) {
            console.error(error);
        }
    };

    useEffect(() => {
        if (!mounted) {
            setMounted(true);
        } else {
            fetchBudgetDiscussionDrafts();
        }
    }, [mounted]);

    return (
        <Box display='flex' flexDirection='column'>
            <Box>
                {draftsEnabled ? (
                    <StepBox>
                            <Box
                                sx={{
                                    mt: 2,
                                }}
                            >
                                <Typography variant='headline4' component='h4' gutterBottom>
                                    Decide if you want to use Existing Draft or
                                    Create new budget proposal
                                </Typography>
                            </Box>

                            <Box color={(theme) => theme.palette.textBlack}>
                                <Typography variant='body1' fontWeight={400} gutterBottom>
                                    Existing Drafts can save you some time and
                                    effort or simply start from stratch.
                                </Typography>
                            </Box>
                            <Box
                                sx={{
                                    mt: 2,
                                }}
                            >
                                <Button
                                    variant='contained'
                                    sx={{
                                        whiteSpace: 'normal',
                                        height: 'auto',
                                        minHeight: 40,
                                    }}
                                    onClick={() => setStep(2)}
                                    data-testid='create-new-budget-discussion-button'
                                >
                                    Create new Budget Proposal
                                </Button>
                            </Box>
                        </StepBox>
                ) : (
                    <StepBox>
                            <Box
                                sx={{
                                    align: 'center',
                                    textAlign: 'center',
                                }}
                            >
                                <Typography variant='headline4' component='h4' gutterBottom>
                                    Budget Proposal
                                </Typography>
                            </Box>

                            <Box color={(theme) => theme.palette.textBlack}>
                                <Typography variant='body1' fontWeight={400} gutterBottom>
                                    This process is open to any individual or
                                    organization within the Cardano ecosystem
                                    that wishes to submit a proposal related to
                                    the Cardano blockchain ecosystem for
                                    inclusion in a Cardano Budget facilitated by
                                    Intersect.
                                </Typography>
                                <List
                                    sx={{
                                        listStyleType: 'disc',
                                        marginLeft: 2,
                                        textAlign: 'justify',
                                        marginBottom: 0,
                                    }}
                                >
                                    <ListItem
                                        sx={{
                                            textAlign: 'justify',
                                            display: 'list-item',
                                            paddingY: 0,
                                            marginY: 0,
                                        }}
                                    >
                                        <Typography
                                            variant='body1'
                                            fontWeight={400}
                                            gutterBottom
                                        >
                                            All proposals, except private
                                            contact details, will be made public
                                            to facilitate community and DRep
                                            review and decision-making.
                                        </Typography>
                                    </ListItem>
                                    <ListItem
                                        sx={{
                                            textAlign: 'justify',
                                            display: 'list-item',
                                            paddingY: 0,
                                            marginY: 0,
                                        }}
                                    >
                                        <Typography
                                            variant='body1'
                                            fontWeight={400}
                                            gutterBottom
                                        >
                                            If you have submitted or are already
                                            receiving funding for your proposal
                                            from Project Catalyst, you are not
                                            eligible to apply for funding via
                                            this process.
                                        </Typography>
                                    </ListItem>
                                </List>
                                <Typography variant='body1' fontWeight={400} gutterBottom>
                                    If you have made a prior proposal submission
                                    through an Intersect Committee, you will
                                    need to either:
                                </Typography>
                                <List
                                    sx={{
                                        listStyleType: 'disc',
                                        marginLeft: 2,
                                        textAlign: 'justify',
                                        marginBottom: 0,
                                    }}
                                >
                                    <ListItem
                                        sx={{
                                            textAlign: 'justify',
                                            display: 'list-item',
                                            paddingY: 0,
                                            marginY: 0,
                                        }}
                                    >
                                        <Typography
                                            variant='body1'
                                            fontWeight={400}
                                            gutterBottom
                                        >
                                            Resubmit your proposal through this
                                            new process, this ensures all
                                            proposals will have the same
                                            information, look and feel for DRep
                                            reconciliation;
                                        </Typography>
                                    </ListItem>
                                    <ListItem
                                        sx={{
                                            textAlign: 'justify',
                                            display: 'list-item',
                                            paddingY: 0,
                                            marginY: 0,
                                        }}
                                    >
                                        <Typography
                                            variant='body1'
                                            fontWeight={400}
                                            gutterBottom
                                        >
                                            Or confirm if you would like
                                            us to repurpose the
                                            information previously provided.
                                            Please note that we may not have
                                            enough details to complete the
                                            submission on your behalf, and any
                                            missing information will be shown as
                                            ‘not provided’. You can confirm this
                                            by contacting
                                            operational-services@gov.tools
                                            with the following information:
                                        </Typography>
                                    </ListItem>
                                    <List
                                        sx={{
                                            listStyleType: 'disc',
                                            marginLeft: 2,
                                            textAlign: 'justify',
                                            marginBottom: 0,
                                        }}
                                    >
                                        <ListItem
                                            sx={{
                                                textAlign: 'justify',
                                                display: 'list-item',
                                                paddingY: 0,
                                                marginY: 0,
                                            }}
                                        >
                                            <Typography
                                                variant='body1'
                                                fontWeight={400}
                                                gutterBottom
                                            >
                                                Proposal Name
                                            </Typography>
                                        </ListItem>
                                        <ListItem
                                            sx={{
                                                textAlign: 'justify',
                                                display: 'list-item',
                                                paddingY: 0,
                                                marginY: 0,
                                            }}
                                        >
                                            <Typography
                                                variant='body1'
                                                fontWeight={400}
                                                gutterBottom
                                            >
                                                Proposer Name
                                            </Typography>
                                        </ListItem>
                                        <ListItem
                                            sx={{
                                                textAlign: 'justify',
                                                display: 'list-item',
                                                paddingY: 0,
                                                marginY: 0,
                                            }}
                                        >
                                            <Typography
                                                variant='body1'
                                                fontWeight={400}
                                                gutterBottom
                                            >
                                                Intersect Committee that it has
                                                been aligned / submitted to
                                            </Typography>
                                        </ListItem>
                                        <ListItem
                                            sx={{
                                                textAlign: 'justify',
                                                display: 'list-item',
                                                paddingY: 0,
                                                marginY: 0,
                                            }}
                                        >
                                            <Typography
                                                variant='body1'
                                                fontWeight={400}
                                                gutterBottom
                                            >
                                                Submission Date (approximate)
                                            </Typography>
                                        </ListItem>
                                        <ListItem
                                            sx={{
                                                textAlign: 'justify',
                                                display: 'list-item',
                                                paddingY: 0,
                                                marginY: 0,
                                            }}
                                        >
                                            <Typography
                                                variant='body1'
                                                fontWeight={400}
                                                gutterBottom
                                            >
                                                Intersect POC (if known)
                                            </Typography>
                                        </ListItem>
                                        <ListItem
                                            sx={{
                                                textAlign: 'justify',
                                                display: 'list-item',
                                                paddingY: 0,
                                                marginY: 0,
                                            }}
                                        >
                                            <Typography
                                                variant='body1'
                                                fontWeight={400}
                                                gutterBottom
                                            >
                                                Brief Proposal Description
                                            </Typography>
                                        </ListItem>
                                    </List>
                                </List>
                                <Typography variant='body1' fontWeight={400} gutterBottom>
                                    The more information you provide, the better
                                    we will be able to help you.
                                </Typography>
                                <Typography variant='body1' fontWeight={400} gutterBottom>
                                    All proposals will be made public (barring
                                    any private contact information). Intersect
                                    committees may provide advice or
                                    recommendations to help refine or improve
                                    your proposal. Committees can support DReps
                                    with advice, guidance, and recommendations
                                    to aid in their decision-making wherever
                                    helpful.
                                </Typography>
                            </Box>
                            <StepperActionButtons
                                onClose={onClose}
                                onSaveDraft={handleSaveDraft}
                                showSaveDraft={false}
                                showBack={false}
                                onContinue={setStep}
                                selectedDraftId={selectedDraftId}
                                nextStep={step + 1}
                            />
                        </StepBox>
                )}
            </Box>
            <Box mt={4}>
                {
                    <BudgetDiscussionsList
                        isDraft={true}
                        startEdittingDraft={(budgetDiscussion) => {
                            setStep(2);
                            setBudgetDiscussionData(
                                budgetDiscussion?.attributes.draft_data
                            );
                            setSelectedDraftId(budgetDiscussion?.id);
                        }}
                        statusList={[]}
                    />
                }
            </Box>
        </Box>
    );
};

export default BudgetDiscussionInfo;
