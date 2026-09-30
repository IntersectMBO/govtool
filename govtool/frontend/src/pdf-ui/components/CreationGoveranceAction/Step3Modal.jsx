import { ICONS } from '@/consts/icons';
import { Box } from '@mui/material';
import { Button, Typography } from '@atoms';
import { useState } from 'react';
import { useNavigate } from 'react-router';
import { PdfModal } from '../PdfModal';

// StatusModal's buttons: full width, extraLarge, and able to wrap on narrow
// screens.
const modalButtonSx = {
    whiteSpace: 'normal',
    height: 'auto',
    minHeight: 48,
};

const titleSx = { textAlign: 'center', wordBreak: 'break-word' };
const messageSx = {
    color: 'textBlack',
    textAlign: 'center',
    wordBreak: 'break-word',
};

const AddPollModal = ({ handleSaveDraft }) => {
    const navigation = useNavigate();
    const [openChildModal, setOpenChildModal] = useState(false);
    const [isSaving, setIsSaving] = useState(false);
    const [proposalId, setProposalId] = useState(null);

    const handleOpenChildModal = () => {
        setOpenChildModal(true);
    };
    const handleCloseChildModal = () => {
        setOpenChildModal(false);
    };

    const saveDraft = async () => {
        setIsSaving(true);
        try {
            const newProposalId = await handleSaveDraft(true, false);
            setProposalId(newProposalId);
            handleOpenChildModal();
        } catch (error) {
            console.error(error);
        } finally {
            setIsSaving(false);
        }
    };

    return (
        <Box>
            <Box display='flex' flexDirection='column' gap={3} mt='38px'>
                <Button
                    variant='contained'
                    size='extraLarge'
                    fullWidth
                    sx={modalButtonSx}
                    onClick={() => saveDraft()}
                >
                    Add poll
                </Button>
                <Button
                    variant='outlined'
                    size='extraLarge'
                    fullWidth
                    sx={modalButtonSx}
                    onClick={() => {
                        handleSaveDraft(false, true);
                    }}
                >
                    Submit without Poll
                </Button>
            </Box>
            <PdfModal
                open={openChildModal}
                onClose={handleCloseChildModal}
                hideCloseButton
            >
                <Box display='flex' flexDirection='column' gap='38px'>
                    <Box
                        display='flex'
                        flexDirection='column'
                        justifyContent='space-between'
                    >
                        <Typography
                            id='modal-modal-title'
                            variant='headline5'
                            component='h2'
                            sx={titleSx}
                        >
                            Proposal submitted!
                        </Typography>
                        <Typography
                            id='modal-modal-description'
                            mt={1}
                            variant='body1'
                            fontWeight={400}
                            sx={messageSx}
                        >
                            Now you can check your proposal
                        </Typography>
                    </Box>
                    <Button
                        variant='contained'
                        size='extraLarge'
                        fullWidth
                        sx={modalButtonSx}
                        disabled={isSaving}
                        onClick={() => {
                            if (proposalId) {
                                navigation(
                                    `/proposal_discussion/${proposalId}`
                                );
                            } else {
                                console.error(
                                    'No proposal ID available for navigation'
                                );
                            }
                        }}
                    >
                        {isSaving ? <img src={ICONS.timerIcon} alt='' style={{ width: '1em', height: '1em' }} /> : null} Go to my proposal
                    </Button>
                </Box>
            </PdfModal>
        </Box>
    );
};

const Step3Modal = ({ open, handleClose, handleSaveDraft }) => {
    return (
        <PdfModal open={open} onClose={handleClose}>
            <Box>
                <Typography
                    id='modal-modal-title'
                    variant='headline5'
                    component='h2'
                    sx={titleSx}
                >
                    We recommend to add poll first
                </Typography>
                <Typography
                    id='modal-modal-description'
                    mt={1}
                    variant='body1'
                    fontWeight={400}
                    sx={messageSx}
                >
                    Is this proposal ready to be submitted on chain?
                    Community can help the proposer making the proposal
                    better.
                </Typography>
            </Box>
            <AddPollModal handleSaveDraft={handleSaveDraft} />
        </PdfModal>
    );
};

export default Step3Modal;
