import React from 'react';
import { useNavigate } from 'react-router';
import { PdfModalCloseButton, PdfStatusModal } from '../../PdfModal';

const GovernanceActionSubmittedModal = ({ open, onClose }) => {
    const navigate = useNavigate();
    return (
        <PdfStatusModal
            open={open}
            onClose={onClose}
            dataTestId='ga-submitted-modal'
            // The close control navigates away rather than calling onClose.
            hideCloseButton
            status='success'
            title='Governance Action submitted!'
            titleId='ga-submitted-modal-title'
            titleDataTestId='ga-submitted-modal-title'
            titleComponent='h5'
            message='Your Governance Action may take a little time to submit to the chain.'
            messageId='ga-submitted-modal-description-1'
            messageDataTestId='ga-submitted-modal-description-1'
            primaryButton={{
                label: 'Go to Dashboard',
                variant: 'outlined',
                sx: { borderRadius: '20px' },
                onClick: () => navigate('/proposal_discussion'),
                dataTestId: 'ga-submitted-modal-dashboard-button',
            }}
        >
            <PdfModalCloseButton
                onClick={() => navigate('/proposal_discussion')}
                dataTestId='ga-submitted-modal-close-button'
            />
        </PdfStatusModal>
    );
};

export default GovernanceActionSubmittedModal;
