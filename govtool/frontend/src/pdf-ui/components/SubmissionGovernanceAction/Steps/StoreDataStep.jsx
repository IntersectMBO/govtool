'use client';

import React, { useState } from 'react';

import { Box } from '@mui/material';
import { Button } from '@atoms';
import { PdfCheckbox } from '../../PdfFields';
import { useNavigate } from 'react-router';
import { openInNewTab } from '../../../lib/utils';
import CancelGovActionSubmissionModal from '../Modals/CancelGovActionSubmissionModal';
import {
    StepBox,
    StepButtons,
    StepHeading,
    stepButtonSx,
} from '../../CreationGoveranceAction/StepLayout';

const StoreDataStep = ({ setStep }) => {
    const navigate = useNavigate();
    const [checked, setChecked] = useState(false);
    const [openCancelGASubmissionModal, setOpenCancelGASubmissionModal] =
        useState(false);

    const openLink = () =>
        openInNewTab(
            'https://docs.gov.tools/using-govtool/govtool-functions/storing-information-offline'
        );

    return (
        <>
            <Box
                display='flex'
                flexDirection='column'
                data-testid='store-data-step'
            >
                <StepBox>
                    <StepHeading
                        title='Store and Maintain the Data Yourself'
                        titleComponent='h2'
                    />
                    <Box
                        sx={{ display: 'flex', justifyContent: 'center', my: 4 }}
                    >
                        <Button
                            variant='text'
                            size='extraLarge'
                            sx={{
                                fontWeight: 500,
                                whiteSpace: 'normal',
                                height: 'auto',
                                minHeight: 48,
                            }}
                            onClick={openLink}
                            data-testid='storing-information-link'
                        >
                            Learn more about storing information
                        </Button>
                    </Box>
                    <PdfCheckbox
                        id='submission-checkbox'
                        name='agreeTerms'
                        color='primary'
                        checked={checked}
                        onChange={(value) => setChecked(value)}
                        dataTestId='agree-checkbox'
                        layoutStyles={{
                            mx: 0,
                            width: '100%',
                        }}
                        labelStyles={{ sx: { ml: 0.5 } }}
                        label='I agree to store correctly this information and to maintain them over the years'
                    />
                    <StepButtons
                        sx={{ mt: { xxs: 4, md: 12.5 } }}
                        start={
                            <Button
                                variant='outlined'
                                size='extraLarge'
                                sx={stepButtonSx}
                                onClick={() =>
                                    setOpenCancelGASubmissionModal(true)
                                }
                                data-testid='cancel-button'
                            >
                                Cancel
                            </Button>
                        }
                        end={
                            <Button
                                variant='contained'
                                size='extraLarge'
                                sx={stepButtonSx}
                                onClick={() => setStep(2)}
                                disabled={!checked}
                                data-testid='continue-button'
                            >
                                Continue
                            </Button>
                        }
                    />
                </StepBox>
            </Box>
            <CancelGovActionSubmissionModal
                open={openCancelGASubmissionModal}
                onClose={() => setOpenCancelGASubmissionModal(false)}
            />
        </>
    );
};

export default StoreDataStep;
