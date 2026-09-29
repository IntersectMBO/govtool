import React, { useEffect, useState } from 'react';
import { Box } from '@mui/material';
import { Button } from '@atoms';
import PropTypes from 'prop-types';
const StepperActionButtons = ({
    onClose,
    onSaveDraft = () => {},
    onContinue,
    onBack = () => {},
    selectedDraftId = null,
    nextStep = 0,
    backStep,
    showCancel = true,
    showSaveDraft = true,
    showContinue = true,
    showBack = true,
    cancelText = 'Cancel',
    saveDraftText = 'Save Draft',
    continueText = 'Continue',
    backText = 'Back',
    errors,
}) => {
    // Calculate backStep if not provided
    const calculatedBackStep = backStep !== undefined ? backStep : nextStep - 2;
    const [continueDisabled, setContinueDisabled] = useState(false);
    const [draftDisabled, setDraftDisabled] = useState(false);

    const hasAnyNonEmptyString = (obj) => {
        if (typeof obj === 'string') {
            return obj.trim() !== '';
        }
        if (typeof obj !== 'object' || obj === null) {
            return false;
        }
        if (Array.isArray(obj)) {
            return obj.some((item) => hasAnyNonEmptyString(item));
        }
        return Object.values(obj).some((value) => hasAnyNonEmptyString(value));
    };

    useEffect(() => {
        setContinueDisabled(hasAnyNonEmptyString(errors));

        setDraftDisabled(
            !!(
                errors &&
                errors.linkErrors &&
                (errors.linkErrors[0]?.url || errors.linkErrors[0]?.text)
            )
        );
    }, [errors, continueDisabled]);

    // GovTool CenteredBoxBottomButtons layout: secondary actions on the left,
    // the primary one on the right; stacked with the primary on top on mobile.
    const groupSx = {
        display: 'flex',
        flexDirection: { xxs: 'column-reverse', md: 'row' },
        gap: { xxs: 2, md: 1 },
    };
    const buttonSx = { width: { xxs: '100%', md: 'auto' } };

    return (
        <Box
            sx={{
                display: 'flex',
                flexDirection: { xxs: 'column-reverse', md: 'row' },
                flexWrap: 'wrap',
                gap: { xxs: 2, md: 1 },
                justifyContent: 'space-between',
                mt: { xxs: 5, md: 6 },
            }}
        >
            <Box sx={groupSx}>
                {continueDisabled}
                {showCancel && (
                    <Button
                        size='extraLarge'
                        variant='outlined'
                        sx={buttonSx}
                        onClick={onClose}
                        data-testid='cancel-button'
                    >
                        {cancelText}
                    </Button>
                )}
                {showBack && (
                    <Button
                        size='extraLarge'
                        variant='outlined'
                        sx={buttonSx}
                        onClick={() => onBack(calculatedBackStep)}
                        data-testid='back-button'
                    >
                        {backText}
                    </Button>
                )}
            </Box>

            <Box sx={groupSx}>
                {showSaveDraft && (
                    <Button
                        size='extraLarge'
                        variant='outlined'
                        sx={buttonSx}
                        disabled={draftDisabled}
                        onClick={() => onSaveDraft(selectedDraftId)}
                        data-testid='draft-button'
                    >
                        {saveDraftText}
                    </Button>
                )}
                {showContinue && (
                    <Button
                        size='extraLarge'
                        variant='contained'
                        sx={buttonSx}
                        disabled={continueDisabled}
                        onClick={() => onContinue(nextStep)}
                        data-testid={
                            continueText === 'Continue'
                                ? 'continue-button'
                                : 'submit-button'
                        }
                    >
                        {continueText}
                    </Button>
                )}
            </Box>
        </Box>
    );
};

StepperActionButtons.propTypes = {
    onClose: PropTypes.func.isRequired,
    onSaveDraft: PropTypes.func,
    onContinue: PropTypes.func.isRequired,
    onBack: PropTypes.func,
    selectedDraftId: PropTypes.any,
    nextStep: PropTypes.number,
    backStep: PropTypes.number,
    showCancel: PropTypes.bool,
    showSaveDraft: PropTypes.bool,
    showContinue: PropTypes.bool,
    showBack: PropTypes.bool,
    cancelText: PropTypes.string,
    saveDraftText: PropTypes.string,
    continueText: PropTypes.string,
    backText: PropTypes.string,
    errors: PropTypes.object,
};

export default StepperActionButtons;
