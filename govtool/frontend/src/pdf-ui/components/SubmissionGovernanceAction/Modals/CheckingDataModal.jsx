import { CircularProgress } from '@mui/material';
import React from 'react';
import { PdfStatusModal } from '../../PdfModal';

const CheckingDataModal = ({ open }) => {
    return (
        <PdfStatusModal
            open={open}
            dataTestId='data-checking-modal'
            hideCloseButton
            icon={
                <CircularProgress
                    id='data-checking-modal-loader'
                    data-testid='data-checking-modal-loader'
                    size={60}
                    color='inherit'
                    sx={{ margin: '0 auto' }}
                />
            }
            title='GovTool Is Checking Your Data'
            titleId='data-checking-modal-title'
            titleDataTestId='data-checking-modal-title'
            titleComponent='h5'
            message='GovTool will read the URL that you supplied and make a check to see if it’s identical with the information that you entered on the form.'
            messageId='data-checking-modal-description'
            messageDataTestId='data-checking-modal-description'
        />
    );
};

export default CheckingDataModal;
