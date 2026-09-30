import { useState, useEffect } from 'react';

import {
    Box,
    List,
    ListItem,
    TextField,
    MenuItem,
} from '@mui/material';
import { Typography } from '@atoms';
import { StepperActionButtons } from '../../BudgetDiscussionParts';
import StepBox from '../StepBox';

const AdministrationAndAuditing = ({
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
    validateSection,
}) => {
    const [errorObj, setErrorObj] = useState({
        intersect_named_administrator: 'Error',
    });
    const handleDataChange = (e, dataName) => {
        let value =
            e.target.type === 'checkbox' ? e.target.checked : e.target.value;

        if (dataName === 'intersect_named_administrator') {
            value = value === 'true';
        }

        setBudgetDiscussionData((prev) => ({
            ...prev,
            [dataName]: value,
            ...(dataName === 'intersect_named_administrator'
                ? { intersect_admin_further_text: '' }
                : {}),
        }));
    };

    const handleDisableNext = () => {
        if (
            currentBudgetDiscussionData?.intersect_named_administrator === true
        ) {
            return {};
        } else {
            if (
                currentBudgetDiscussionData?.intersect_named_administrator ===
                false
            ) {
                if (
                    currentBudgetDiscussionData?.intersect_admin_further_text ===
                    ''
                ) {
                    return { intersect_admin_further_text: 'Required' };
                } else {
                    return {};
                }
            } else {
                return {
                    intersect_named_administrator: 'Error',
                };
            }
        }
    };

    useEffect(() => {
        setErrorObj(handleDisableNext);
    }, [currentBudgetDiscussionData]);

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
                            <Typography variant='headline4' component='h4' gutterBottom mb={2}>
                                Section 6: Administration and Auditing
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
                                    6
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
                                <Typography variant='body1' fontWeight={400} gutterBottom mb={2}>
                                    A successful proposal requires an
                                    Administrator. To ensure transparency and
                                    accuracy, audits may also be undertaken.
                                </Typography>
                                <Typography variant='body1' fontWeight={400} gutterBottom mb={2}>
                                    Intersect's role as an administrator,
                                    through our committees and internal
                                    operational function, would consist of the
                                    following:
                                    <List
                                        sx={{
                                            listStyleType: 'disc',
                                            marginLeft: 2,
                                            textAlign: 'justify',
                                            marginBottom: 0,
                                        }}
                                    >
                                        <ListItem
                                            sx={{
                                                textAlign: 'justify',
                                                display: 'list-item',
                                                paddingY: 0,
                                                marginY: 0,
                                            }}
                                        >
                                            <Typography
                                                variant='body1'
                                                fontWeight={400}
                                                gutterBottom
                                            >
                                                Contract management
                                            </Typography>
                                        </ListItem>
                                        <ListItem
                                            sx={{
                                                textAlign: 'justify',
                                                display: 'list-item',
                                                paddingY: 0,
                                                marginY: 0,
                                            }}
                                        >
                                            <Typography
                                                variant='body1'
                                                fontWeight={400}
                                                gutterBottom
                                            >
                                                Delivery assurance and
                                                Communications
                                            </Typography>
                                        </ListItem>
                                        <ListItem
                                            sx={{
                                                textAlign: 'justify',
                                                display: 'list-item',
                                                paddingY: 0,
                                                marginY: 0,
                                            }}
                                        >
                                            <Typography
                                                variant='body1'
                                                fontWeight={400}
                                                gutterBottom
                                            >
                                                Fund management
                                            </Typography>
                                        </ListItem>
                                        <ListItem
                                            sx={{
                                                textAlign: 'justify',
                                                display: 'list-item',
                                                paddingY: 0,
                                                marginY: 0,
                                            }}
                                        >
                                            <Typography
                                                variant='body1'
                                                fontWeight={400}
                                                gutterBottom
                                            >
                                                Fiat conversion
                                            </Typography>
                                        </ListItem>
                                        <ListItem
                                            sx={{
                                                textAlign: 'justify',
                                                display: 'list-item',
                                                paddingY: 0,
                                                marginY: 0,
                                            }}
                                        >
                                            <Typography
                                                variant='body1'
                                                fontWeight={400}
                                                gutterBottom
                                            >
                                                Legal
                                            </Typography>
                                        </ListItem>
                                        <ListItem
                                            sx={{
                                                textAlign: 'justify',
                                                display: 'list-item',
                                                paddingY: 0,
                                                marginY: 0,
                                            }}
                                        >
                                            <Typography
                                                variant='body1'
                                                fontWeight={400}
                                                gutterBottom
                                            >
                                                KYC / KYB
                                            </Typography>
                                        </ListItem>
                                        <ListItem
                                            sx={{
                                                textAlign: 'justify',
                                                display: 'list-item',
                                                paddingY: 0,
                                                marginY: 0,
                                            }}
                                        >
                                            <Typography
                                                variant='body1'
                                                fontWeight={400}
                                                gutterBottom
                                            >
                                                Dispute resolution
                                            </Typography>
                                        </ListItem>
                                        <ListItem
                                            sx={{
                                                textAlign: 'justify',
                                                display: 'list-item',
                                                paddingY: 0,
                                                marginY: 0,
                                            }}
                                        >
                                            <Typography
                                                variant='body1'
                                                fontWeight={400}
                                                gutterBottom
                                            >
                                                Technical and Financial auditing
                                            </Typography>
                                        </ListItem>
                                    </List>
                                </Typography>

                                <TextField
                                    select
                                    name='Intersect named Administrator'
                                    label='Would you like Intersect to be your named Administrator, including acting as the auditor, as per the Cardano Constitution?*'
                                    value={
                                        currentBudgetDiscussionData?.intersect_named_administrator ===
                                        undefined
                                            ? ''
                                            : String(
                                                  currentBudgetDiscussionData.intersect_named_administrator
                                              )
                                    }
                                    required
                                    fullWidth
                                    onChange={(e) =>
                                        handleDataChange(
                                            e,
                                            'intersect_named_administrator'
                                        )
                                    }
                                    helperText={errors[
                                        'intersect_named_administrator'
                                    ]?.trim()}
                                    error={
                                        !!errors[
                                            'intersect_named_administrator'
                                        ]?.trim()
                                    }
                                    SelectProps={{
                                        SelectDisplayProps: {
                                            'data-testid':
                                                'intersect-named-administrator',
                                        },
                                    }}
                                    sx={{ mb: 4 }}
                                >
                                    <MenuItem
                                        value=''
                                        disabled
                                        data-testid='please-select-option'
                                    >
                                        Please select
                                    </MenuItem>
                                    <MenuItem
                                        key={1}
                                        value='true'
                                        data-testid='true-button'
                                    >
                                        Yes
                                    </MenuItem>
                                    <MenuItem
                                        key={2}
                                        value='false'
                                        data-testid='false-button'
                                    >
                                        No
                                    </MenuItem>
                                </TextField>
                                {currentBudgetDiscussionData?.intersect_named_administrator ===
                                undefined ? (
                                    ''
                                ) : currentBudgetDiscussionData?.intersect_named_administrator ? (
                                    ''
                                ) : (
                                    <TextField
                                        name='Additional reason'
                                        label='Please provide further information to help inform DReps. Who is the vendor and what services are they providing?'
                                        value={
                                            currentBudgetDiscussionData?.intersect_admin_further_text ||
                                            ''
                                        }
                                        required
                                        multiline
                                        rows={4}
                                        fullWidth
                                        onChange={(e) =>
                                            handleDataChange(
                                                e,
                                                'intersect_admin_further_text'
                                            )
                                        }
                                        // helperText={errors['intersect_admin_further_text']?.trim()}
                                        // error={!!errors['intersect_admin_further_text']?.trim()}
                                        sx={{ mb: 3 }}
                                        InputProps={{
                                            'data-testid':
                                                'intersect-admin-further-text',
                                        }}
                                    />
                                )}
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
                            errors={{ ...errorObj }}
                            showSaveDraft={
                                !currentBudgetDiscussionData?.master_id
                            }
                        />
                    </StepBox>
            </Box>
        </Box>
    );
};
export default AdministrationAndAuditing;
