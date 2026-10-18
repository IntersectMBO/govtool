import React from 'react';
import { PdfStatusModal } from '../PdfModal';
const DeleteProposalModal = ({ open, onClose, handleDeleteProposal }) => {
    return (
        <PdfStatusModal
            open={open}
            onClose={onClose}
            dataTestId='delete-proposal-modal'
            title='Do you want to delete your proposal?'
            titleId='modal-modal-title'
            titleComponent='h2'
            message='Clicking "Delete Proposal" will permanently remove your proposal from the system. This action cannot be undone.'
            messageId='modal-modal-description'
            primaryButton={{
                label: 'Delete Proposal',
                dataTestId: 'delete-proposal-yes-button',
                onClick: handleDeleteProposal,
            }}
            secondaryButton={{
                label: 'Cancel',
                onClick: onClose,
                dataTestId: 'delete-proposal-no-button',
            }}
        />
    );
};

export default DeleteProposalModal;
