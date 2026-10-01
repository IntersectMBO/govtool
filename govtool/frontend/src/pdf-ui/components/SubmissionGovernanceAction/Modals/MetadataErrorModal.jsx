import React from 'react';
import { PdfStatusModal } from '../../PdfModal';

// The validation statuses that have neither the "cannot be found" nor the
// "does not match" modal, with what each one tells the author.
const TEXTS = {
    INCORRECT_FORMAT: {
        title: 'Your External Data Is Not in the Expected Format',
        message:
            'GovTool checked the file at the URL you entered, and it does not follow the metadata standard for a governance action. Make sure the URL serves the exact JSON file generated in GovTool, then try again.',
    },
    EXCEEDS_LIMIT: {
        title: 'Your External Data Is Too Large',
        message:
            'The file at the URL you entered is larger than the 2 MB GovTool accepts. Make sure the URL serves the JSON file generated in GovTool, then try again.',
    },
    URL_BLOCKED: {
        title: 'The URL You Entered Cannot Be Used',
        message:
            'The URL you entered points to a private or local network address. GovTool only fetches data from public internet addresses, so host the file somewhere public and enter that URL.',
    },
    INTERNAL_ERROR: {
        title: 'GovTool Could Not Check Your Data',
        message:
            "Something went wrong on GovTool's side while checking the file at your URL. This is not a problem with your data. Please try again in a moment.",
    },
};
TEXTS.INVALID_JSONLD = TEXTS.INCORRECT_FORMAT;

/** `true` when `status` has its own texts here. */
export const hasMetadataErrorModal = (status) =>
    Object.prototype.hasOwnProperty.call(TEXTS, status);

const MetadataErrorModal = ({
    status,
    open,
    onClose,
    buttonOneClick,
    buttonTwoClick,
}) => {
    const texts = TEXTS[status] ?? TEXTS.INTERNAL_ERROR;

    return (
        <PdfStatusModal
            open={open}
            onClose={onClose}
            dataTestId='metadata-error-modal'
            closeButtonDataTestId='metadata-error-modal-close-button'
            status='warning'
            title={texts.title}
            titleId='metadata-error-modal-title'
            titleDataTestId='metadata-error-modal-title'
            titleComponent='h5'
            message={texts.message}
            messageId='metadata-error-modal-description'
            messageDataTestId='metadata-error-modal-description'
            primaryButton={{
                label: 'Go to Data Edit Screen',
                sx: { borderRadius: '20px' },
                onClick: buttonOneClick,
                dataTestId: 'metadata-error-modal-go-to-data-button',
            }}
            secondaryButton={{
                label: 'Cancel registration',
                sx: { borderRadius: '20px' },
                onClick: buttonTwoClick,
                dataTestId: 'metadata-error-modal-cancel-button',
            }}
        />
    );
};

export default MetadataErrorModal;
