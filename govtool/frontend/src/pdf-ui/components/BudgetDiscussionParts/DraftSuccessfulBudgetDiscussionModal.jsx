import React from 'react';

import { PdfStatusModal } from '../PdfModal';

const DraftSuccessfulBudgetDiscussionModal = ({ open, onClose, closeCreateBDDialog }) => {
    return (
        <PdfStatusModal
            open={open}
            onClose={onClose}
            dataTestId='draft-successful-modal'
            hideCloseButton
            title='Draft successfully saved'
            titleId='draft-successful-modal-title'
            titleComponent='h2'
            primaryButton={{
                label: 'Close and go to Budget Discussion List',
                sx: { borderRadius: '20px' },
                onClick: () => {
                    onClose();
                    closeCreateBDDialog();
                },
                dataTestId: 'close-button',
            }}
        />
    );
};

export default DraftSuccessfulBudgetDiscussionModal;
