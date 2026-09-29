'use client';

import React from 'react';
import { useNavigate } from 'react-router';
import { PdfStatusModal } from '../../PdfModal';

const CancelGovActionSubmissionModal = ({ open, onClose }) => {
    const navigate = useNavigate();

    return (
        <PdfStatusModal
            open={open}
            onClose={onClose}
            dataTestId='cancel-ga-submission-modal'
            closeButtonDataTestId='cancel-ga-submission-modal-close-button'
            status='warning'
            title='Do you want to Cancel your Governance Action submission?'
            titleId='cancel-ga-submission-modal-title'
            titleDataTestId='cancel-ga-submission-modal-title'
            titleComponent='h5'
            message='Returning to the proposal list will cancel your submission and your proposed Governance Action will not be submitted.'
            messageId='cancel-ga-submission-modal-description'
            messageDataTestId='cancel-ga-submission-modal-description'
            primaryButton={{
                label: 'I don’t want to cancel',
                sx: { borderRadius: '20px' },
                onClick: onClose,
                dataTestId: 'cancel-ga-submission-modal-no-button',
            }}
            secondaryButton={{
                label: 'Yes, cancel my proposal submission and take me to the to proposal list',
                variant: 'text',
                sx: { borderRadius: '20px' },
                onClick: () => navigate('/proposal_discussion'),
                dataTestId: 'cancel-ga-submission-modal-yes-button',
            }}
        />
    );
};

export default CancelGovActionSubmissionModal;
