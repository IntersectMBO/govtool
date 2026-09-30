import React from 'react';
import { getItemFromLocalStorage, correctAdaFormat } from '../../../lib/utils';
import { PdfStatusModal } from '../../PdfModal';
const PROTOCOL_PARAMS_KEY = 'protocol_params';

const InsufficientBallanceModal = ({ open, onClose, buttonOneClick }) => {
    const protocolParams = getItemFromLocalStorage(PROTOCOL_PARAMS_KEY);

    return (
        <PdfStatusModal
            open={open}
            onClose={onClose}
            dataTestId='insufficient-ballance-error-modal'
            closeButtonDataTestId='insufficient-ballance-error-modal-close-button'
            status='warning'
            title='Insufficient Balance'
            titleId='insufficient-ballance-error-modal-title'
            titleDataTestId='insufficient-ballance-error-modal-title'
            titleComponent='h5'
            message={
                <>
                    To submit a Governance Action, you will be required to
                    post a refundable balance of ₳
                    {correctAdaFormat(protocolParams?.gov_action_deposit)}.
                    You do not currently have enough ADA in your wallet to
                    continue.
                </>
            }
            messageId='insufficient-ballance-error-modal-description'
            messageDataTestId='insufficient-ballance-error-modal-description'
            primaryButton={{
                label: 'Cancel',
                variant: 'outlined',
                sx: { borderRadius: '20px' },
                onClick: buttonOneClick,
                dataTestId: 'cancel-button',
            }}
        />
    );
};

export default InsufficientBallanceModal;
