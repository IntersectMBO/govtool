import {
    Box,
    List,
    ListItem,
    TextField,
    MenuItem,
    Grid,
    Link,
} from '@mui/material';
import { useEffect, useState } from 'react';
import { Typography } from '@atoms';
import { PdfInput, PdfTextArea } from '../../PdfFields';
import { getAllCurrencies } from '../../../lib/api';
import { StepperActionButtons } from '../../BudgetDiscussionParts';
import StepBox from '../StepBox';

const Costing = ({
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
    const [allCurrencyList, setAllCurrencyList] = useState([]);
    const costBreakdownMaxLength = 15000;
    const [touched, setTouched] = useState({
        ada_amount: false,
        usd_to_ada_conversion_rate: false,
        amount_in_preferred_currency: false,
    });
    const handleDataChange = (e, dataName) => {
        setBudgetDiscussionData({
            ...currentBudgetDiscussionData,
            bd_costing: {
                ...currentBudgetDiscussionData?.bd_costing,
                [dataName]: e.target.value,
            },
        });
        setTouched({
            ...touched,
            dataName: false,
        });
    };
    useEffect(() => {
        const fetchData = async () => {
            try {
                if (!allCurrencyList.length) {
                    const allCurrenciesResponse = await getAllCurrencies();
                    setAllCurrencyList(allCurrenciesResponse?.data || []);
                }
            } catch (error) {
                console.error('Error fetching data:', error);
            }
        };
        fetchData();
    }, []);

    useEffect(() => {
        validateSection('bd_costing');
    }, [currentBudgetDiscussionData?.bd_costing]);
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
                                Section 4: Costing
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
                                    4
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
                                    Please provide requested cost of this
                                    proposal
                                </Typography>
                            </Box>
                        </Box>
                        <Box sx={{ display: 'flex', gap: 2 }}>
                            <Grid container spacing={2}>
                                <Grid item xxs={6}>
                                    <PdfInput
                                        name='ADA Amount'
                                        label='ADA Amount'
                                        value={
                                            currentBudgetDiscussionData
                                                ?.bd_costing?.ada_amount || ''
                                        }
                                        required
                                        //changed to string from number to allow entering letters for error notifications
                                        type='string'
                                        onChange={(e) =>
                                            handleDataChange(e, 'ada_amount')
                                        }
                                        layoutStyles={{ mb: 3 }}
                                        onBlur={() => {
                                            if (!touched.ada_amount) {
                                                setTouched({
                                                    ...touched,
                                                    ada_amount: true,
                                                });
                                            }
                                        }}
                                        dataTestId='ada-amount-input'
                                        errorMessage={
                                            (touched.ada_amount &&
                                                errors[
                                                    'bd_costing.ada_amount'
                                                ]?.trim()) ||
                                            undefined
                                        }
                                        errorDataTestId='ada-amount-error'
                                    />
                                </Grid>
                                <Grid item xxs={6}>
                                    <PdfInput
                                        name='USD to ADA Conversion Rate'
                                        label='USD to ADA Conversion Rate'
                                        value={
                                            currentBudgetDiscussionData
                                                ?.bd_costing
                                                ?.usd_to_ada_conversion_rate ||
                                            ''
                                        }
                                        required
                                        type='string'
                                        onBlur={() => {
                                            setTouched({
                                                ...touched,
                                                usd_to_ada_conversion_rate: true,
                                            });
                                        }}
                                        errorMessage={
                                            (touched.usd_to_ada_conversion_rate &&
                                                errors[
                                                    'bd_costing.usd_to_ada_conversion_rate'
                                                ]?.trim()) ||
                                            undefined
                                        }
                                        helpfulText='The rate you used to budget for this proposal'
                                        // The testid follows the ada amount
                                        // error, as before, on whichever text
                                        // is shown.
                                        errorDataTestId={
                                            errors[
                                                'bd_costing.ada_amount'
                                            ]?.trim()
                                                ? 'usd-to-ada-converson-error'
                                                : undefined
                                        }
                                        helpfulTextDataTestId={
                                            errors[
                                                'bd_costing.ada_amount'
                                            ]?.trim()
                                                ? 'usd-to-ada-converson-error'
                                                : undefined
                                        }
                                        onChange={(e) =>
                                            handleDataChange(
                                                e,
                                                'usd_to_ada_conversion_rate'
                                            )
                                        }
                                        layoutStyles={{ mb: 3 }}
                                        dataTestId='usd-ada-conversion-input'
                                    />
                                </Grid>
                                <Grid item xxs={6}>
                                    <TextField
                                        select
                                        name='Preferred currency'
                                        label='Preferred currency'
                                        value={
                                            currentBudgetDiscussionData
                                                ?.bd_costing
                                                ?.preferred_currency || ''
                                        }
                                        required
                                        fullWidth
                                        // helperText={errors[
                                        //     'bd_costing.preferred_currency'
                                        // ]?.trim()}
                                        // error={
                                        //     !!errors[
                                        //         'bd_costing.preferred_currency'
                                        //     ]?.trim()
                                        // }
                                        onChange={(e) =>
                                            handleDataChange(
                                                e,
                                                'preferred_currency'
                                            )
                                        }
                                        SelectProps={{
                                            SelectDisplayProps: {
                                                'data-testid':
                                                    'preferred-currency',
                                            },
                                        }}
                                        sx={{ mb: 4 }}
                                    >
                                        {allCurrencyList?.map((option) => (
                                            <MenuItem
                                                key={option?.id}
                                                value={option?.id}
                                                data-testid={`${option?.attributes.currency_name?.toLowerCase()}-button`}
                                            >
                                                {
                                                    option?.attributes
                                                        ?.currency_name
                                                }{' '}
                                                (
                                                {
                                                    option?.attributes
                                                        ?.currency_letter_code
                                                }
                                                )
                                            </MenuItem>
                                        ))}
                                    </TextField>
                                </Grid>
                                <Grid item xxs={6}>
                                    <PdfInput
                                        name='Amount in preferred currency'
                                        label='Amount in preferred currency'
                                        value={
                                            currentBudgetDiscussionData
                                                ?.bd_costing
                                                ?.amount_in_preferred_currency ||
                                            ''
                                        }
                                        required
                                        type='string'
                                        errorMessage={
                                            (touched.amount_in_preferred_currency &&
                                                errors[
                                                    'bd_costing.amount_in_preferred_currency'
                                                ]?.trim()) ||
                                            undefined
                                        }
                                        errorDataTestId='preferred-currency-error'
                                        onBlur={(e) => {
                                            setTouched({
                                                ...touched,
                                                amount_in_preferred_currency: true,
                                            });
                                        }}
                                        onChange={(e) =>
                                            handleDataChange(
                                                e,
                                                'amount_in_preferred_currency'
                                            )
                                        }
                                        layoutStyles={{ mb: 3 }}
                                        dataTestId='preferred-currency-amount-input'
                                    />
                                </Grid>
                            </Grid>
                        </Box>
                        <PdfTextArea
                            name='Cost breakdown'
                            label='Cost breakdown'
                            required
                            value={
                                currentBudgetDiscussionData?.bd_costing
                                    ?.cost_breakdown || ''
                            }
                            onChange={(e) =>
                                handleDataChange(e, 'cost_breakdown')
                            }
                            layoutStyles={{ mb: 4 }}
                            maxLength={costBreakdownMaxLength}
                            dataTestId='cost-breakdown-input'
                            helperText='Based on your preferred contract type and cost estimate, please provide a cost breakdown in ada and in USD.'
                            helperTextDataTestId='cost-breakdown-helper-text'
                            counterDataTestId='cost-breakdown-helper-character-count'
                            errorMessage={errors?.bd_costing?.cost_breakdown || undefined}
                            errorDataTestId='cost-breakdown-helper-error'
                        />
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
export default Costing;
