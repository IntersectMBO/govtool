import React from 'react';
import { useNavigate } from 'react-router';
import { PdfStatusModal } from '../../PdfModal';

const CancelRegistrationModal = ({ open, onClose }) => {
    const navigate = useNavigate();
    return (
        <PdfStatusModal
            open={open}
            onClose={onClose}
            dataTestId='cancel-registration-modal'
            closeButtonDataTestId='cancel-registration-modal-close-button'
            status='warning'
            title='Do You Want to Cancel Registration?'
            titleId='cancel-registration-modal-title'
            titleDataTestId='cancel-registration-modal-title'
            titleComponent='h5'
            message='If you return to the Dashboard, your information will not be saved.'
            messageId='cancel-registration-modal-description-1'
            messageDataTestId='cancel-registration-modal-description-1'
            primaryButton={{
                label: 'Back to Dashboard',
                sx: { borderRadius: '20px' },
                onClick: () => navigate(`/proposal_discussion`),
                dataTestId: 'cancel-registration-modal-back-button',
            }}
        />
    );
};

export default CancelRegistrationModal;
