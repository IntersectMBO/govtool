import {
    Box,
    List,
    ListItem,
    MenuItem,
    Grid,
    Link,
} from '@mui/material';
import { useEffect, useState } from 'react';
import { Typography } from '@atoms';
import {
    BudgetDiscussionLinkManager,
    StepperActionButtons,
} from '../../BudgetDiscussionParts';
import { isValidURLFormat } from '../../../lib/utils';
import { useTheme } from '@mui/material/styles';
import StepBox from '../StepBox';

const FurtherInformation = ({
    setStep,
    step,
    currentBudgetDiscussionData,
    setBudgetDiscussionData,
    onClose,
    setSelectedDraftId,
    selectedDraftId,
    handleSaveDraft,
    errors,
    setErrors,
}) => {
    const costBreakdownMaxLength = 256;

    const handleDataChange = (e, dataName) => {
        setBudgetDiscussionData({
            ...currentBudgetDiscussionData,
            bd_further_information: {
                ...currentBudgetDiscussionData?.bd_further_information,
                [dataName]: e.target.value,
            },
        });
    };
    return (
        <Box display='flex' flexDirection='column'>
            <Box>
                <StepBox>
                        <Box
                            sx={{
                                align: 'center',
                                textAlign: 'center',
                            }}
                        >
                            <Typography
                                variant='headline4'
                                component='h4'
                                gutterBottom
                                mb={2}
                            >
                                Section 5: Further information
                            </Typography>
                            <Box
                                sx={{ mt: 1, mb: 4 }}
                                display={'flex'}
                                alignItems={'center'}
                                justifyContent={'center'}
                                gap={0.5}
                            >
                                <Typography
                                    variant='body1'
                                    fontWeight={500}
                                    color={'textBlack'}
                                >
                                    5
                                </Typography>
                                <Typography
                                    variant='body1'
                                    fontWeight={500}
                                    color={'textBlack'}
                                >
                                    /
                                </Typography>
                                <Typography
                                    variant='body1'
                                    fontWeight={300}
                                    color={'textBlack'}
                                >
                                    6
                                </Typography>
                            </Box>
                            <Box color={(theme) => theme.palette.textBlack}>
                                <Typography
                                    variant='body1'
                                    fontWeight={400}
                                    gutterBottom
                                    mb={2}
                                >
                                    Please link your full proposal and any
                                    supplementary information on this proposal
                                    to help aid knowledge sharing. (E.g.,
                                    Specifications, Videos, Initiation, or
                                    Proposal Documents.)
                                </Typography>
                            </Box>
                        </Box>
                        <Box sx={{ align: 'center', mt: 2 }}>
                            <BudgetDiscussionLinkManager
                                budgetDiscussionData={
                                    currentBudgetDiscussionData
                                }
                                setBudgetDiscussionData={
                                    setBudgetDiscussionData
                                }
                                setLinksData={(links) =>
                                    handleDataChange(links, 'proposal_links')
                                }
                                errors={errors}
                                setErrors={setErrors}
                            />
                        </Box>
                        <StepperActionButtons
                            onClose={onClose}
                            onSaveDraft={handleSaveDraft}
                            onContinue={setStep}
                            onBack={setStep}
                            selectedDraftId={selectedDraftId}
                            nextStep={step + 1}
                            backStep={step - 1}
                            errors={errors}
                            showSaveDraft={
                                !currentBudgetDiscussionData?.master_id
                            }
                        />
                    </StepBox>
            </Box>
        </Box>
    );
};
export default FurtherInformation;
