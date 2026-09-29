import { Typography } from '@atoms';
import React from 'react';
import { PdfStatusModal } from '../../PdfModal';

const ExternalDataNotMatchModal = ({
    open,
    onClose,
    buttonOneClick,
    buttonTwoClick,
}) => {
    return (
        <PdfStatusModal
            open={open}
            onClose={onClose}
            dataTestId='data-not-match-modal'
            closeButtonDataTestId='data-not-match-modal-close-button'
            status='warning'
            title='Your External Data Does Not Match the Original File.'
            titleId='data-not-match-modal-title'
            titleDataTestId='data-not-match-modal-title'
            titleComponent='h5'
            message='GovTool checks the URL you entered to see if the JSON file that you self-host matches the one that was generated in GovTool. To complete registration, this match must be exact.'
            messageId='data-not-match-modal-description-1'
            messageDataTestId='data-not-match-modal-description-1'
            primaryButton={{
                label: 'Go to Data Edit Screen',
                sx: { borderRadius: '20px' },
                onClick: buttonOneClick,
                dataTestId: 'data-not-match-modal-go-to-data-button',
            }}
            secondaryButton={{
                label: 'Cancel registration',
                sx: { borderRadius: '20px' },
                onClick: buttonTwoClick,
                dataTestId: 'data-not-match-modal-cancel-button',
            }}
        >
            <Typography
                id='data-not-match-modal-description-2'
                data-testid='data-not-match-modal-description-2'
                component='p'
                fontWeight={400}
                mt={2}
                sx={{
                    textAlign: 'center',
                    wordBreak: 'break-word',
                }}
            >
                In this case, there is a mismatch. You can go back to the data
                edit screen and try the process again.
            </Typography>
        </PdfStatusModal>
    );
};

export default ExternalDataNotMatchModal;
