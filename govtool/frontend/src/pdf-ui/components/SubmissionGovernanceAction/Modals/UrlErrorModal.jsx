import { Link } from '@mui/material';
import React from 'react';
import { openInNewTab } from '../../../lib/utils';
import { PdfStatusModal } from '../../PdfModal';

const UrlErrorModal = ({
    open,
    onClose,
    buttonOneClick,
    buttonTwoClick,
    children,
}) => {
    const openLink = () =>
        openInNewTab(
            'https://docs.gov.tools/using-govtool/govtool-functions/storing-information-offline'
        );

    return (
        <PdfStatusModal
            open={open}
            onClose={onClose}
            dataTestId='url-error-modal'
            closeButtonDataTestId='url-error-modal-close-button'
            status='warning'
            title='The URL You Entered Cannot be Found'
            titleId='url-error-modal-title'
            titleDataTestId='url-error-modal-title'
            titleComponent='h5'
            message='GovTool cannot find the URL that you entered. Please check it and re-enter.'
            messageId='url-error-modal-description'
            messageDataTestId='url-error-modal-description'
            primaryButton={{
                label: 'Go to Data Edit Screen',
                sx: { borderRadius: '20px' },
                onClick: buttonOneClick,
                dataTestId: 'url-error-modal-go-to-data-button',
            }}
            secondaryButton={{
                label: 'Cancel registration',
                sx: { borderRadius: '20px' },
                onClick: buttonTwoClick,
                dataTestId: 'url-error-modal-cancel-button',
            }}
        >
            <Link
                sx={{
                    color: (theme) => theme?.palette?.primary?.main,
                    mt: 2,
                }}
                component={'button'}
                variant='body1'
                onClick={openLink}
                id='url-error-modal-learn-more-link'
                data-testid='url-error-modal-learn-more-link'
            >
                Learn More about self-hosting
            </Link>
            {children}
        </PdfStatusModal>
    );
};

export default UrlErrorModal;
