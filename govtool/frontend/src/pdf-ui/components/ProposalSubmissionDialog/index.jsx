'use client';

import { useState } from 'react';
import { Box, Dialog } from '@mui/material';
import {
    StoreDataStep,
    InformationStorageStep,
} from '../../components/SubmissionGovernanceAction';
import { useNavigate } from 'react-router';
import {
    FlowBackLink,
    FlowHeader,
    FlowPage,
} from '../CreationGoveranceAction/StepLayout';
import CreateGA2 from '../../assets/svg/CreateGA2';
const ProposalSubmissionDialog = ({
    proposal,
    openEditDialog,
    handleCloseSubmissionDialog,
}) => {
    const navigate = useNavigate();
    const [step, setStep] = useState(1);
    return (
        <Dialog
            fullScreen
            open={openEditDialog}
            onClose={handleCloseSubmissionDialog}
            data-testid='proposal-submission-dialog'
            PaperProps={{
                sx: { borderRadius: 0 },
            }}
        >
            <Box
                sx={{
                    width: '100%',
                    display: 'flex',
                    flexDirection: 'column',
                    flexGrow: 1,
                    overflow: 'auto',
                    minHeight: 0,
                    position: 'relative',
                }}
            >
                <FlowHeader title='Create a Governance Action' />
                <FlowPage sx={{ pb: 4 }}>
                    <FlowBackLink
                        onClick={() => navigate(`/proposal_discussion`)}
                        data-testid='back-button'
                    >
                        Show all
                    </FlowBackLink>
                    <Box sx={{ pt: { xxs: 3, md: 1.5 } }}>
                        {step === 1 && <StoreDataStep setStep={setStep} />}
                        {step === 2 && (
                            <InformationStorageStep
                                proposal={proposal}
                                handleCloseSubmissionDialog={
                                    handleCloseSubmissionDialog
                                }
                            />
                        )}
                    </Box>
                </FlowPage>

                <Box
                    sx={{
                        position: 'absolute',
                        top: 0,
                        left: 0,
                        zIndex: 1,
                    }}
                >
                    <CreateGA2 />
                </Box>
            </Box>
        </Dialog>
    );
};

export default ProposalSubmissionDialog;
