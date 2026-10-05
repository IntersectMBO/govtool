import { Box } from '@mui/material';
import { Typography } from '@atoms';
import { LINKS } from '@/consts/links';
import { PdfCheckbox } from '../../PdfFields';

import { StepperActionButtons } from '../../BudgetDiscussionParts';
import StepBox from '../StepBox';

const BudgetDiscussionSubmit = ({
    setStep,
    step,
    currentBudgetDiscussionData,
    setBudgetDiscussionData,
    onClose,
    selectedDraftId,
    handleSaveDraft,
    errors,
}) => {
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
                                Privacy Policy & Terms of Use
                            </Typography>
                            <Box color={(theme) => theme.palette.textBlack}>
                                <PdfCheckbox
                                    checked={
                                        currentBudgetDiscussionData?.privacy_policy ===
                                        true
                                            ? true
                                            : false
                                    }
                                    onChange={(checked) =>
                                        setBudgetDiscussionData({
                                            ...currentBudgetDiscussionData,
                                            privacy_policy: checked,
                                        })
                                    }
                                    dataTestId='submit-checkbox'
                                    labelStyles={{
                                        variant: 'body2',
                                        fontWeight: 400,
                                        textAlign: 'left',
                                    }}
                                    label={
                                        <>
                                            I consent to the public sharing of
                                            all information provided in this
                                            form in accordance with the{' '}
                                            <span>
                                                <a
                                                    href={LINKS.PRIVACY_POLICY}
                                                    target='_blank'
                                                    rel='noopener noreferrer'
                                                >
                                                    Privacy Policy
                                                </a>
                                            </span>{' '}
                                            and{' '}
                                            <span>
                                                <a
                                                    href={LINKS.TERMS_OF_USE}
                                                    target='_blank'
                                                    rel='noopener noreferrer'
                                                >
                                                    Terms of Use
                                                </a>
                                            </span>
                                            .
                                        </>
                                    }
                                />
                            </Box>
                        </Box>
                        <StepperActionButtons
                            onClose={onClose}
                            onSaveDraft={handleSaveDraft}
                            onContinue={setStep}
                            onBack={setStep}
                            selectedDraftId={selectedDraftId}
                            nextStep={step + 1}
                            backStep={step - 1}
                            errors={
                                currentBudgetDiscussionData?.privacy_policy
                                    ? {}
                                    : { privacy_policy: 'Required' }
                            }
                            showSaveDraft={
                                !currentBudgetDiscussionData?.master_id
                            }
                        />
                    </StepBox>
            </Box>
        </Box>
    );
};
export default BudgetDiscussionSubmit;
