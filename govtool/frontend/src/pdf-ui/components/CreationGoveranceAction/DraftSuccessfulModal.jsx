import React from 'react';

import { PdfStatusModal } from '../PdfModal';

const DraftSuccessfulModal = ({ open, onClose, closeCreateGADialog }) => {
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
                label: 'Close and go to Proposal List',
                onClick: () => {
                    onClose();
                    closeCreateGADialog();
                },
                dataTestId: 'close-button',
            }}
        />
    );
};

export default DraftSuccessfulModal;
