import React from 'react';
import { PdfStatusModal } from '../PdfModal';

const ProposalOwnModal = ({ open, onClose }) => {
    return (
        <PdfStatusModal
            open={open}
            onClose={onClose}
            dataTestId='own-proposal-modal'
            title='Action not allowed'
            titleId='own-proposal-modal-title'
            titleComponent='h2'
            message='You can not like or dislike your own proposal'
            messageId='own-proposal-modal-description'
            primaryButton={{
                label: 'Go back to Proposal',
                sx: { borderRadius: '20px' },
                onClick: onClose,
                dataTestId: 'own-proposal-back-button',
            }}
        />
    );
};

export default ProposalOwnModal;
