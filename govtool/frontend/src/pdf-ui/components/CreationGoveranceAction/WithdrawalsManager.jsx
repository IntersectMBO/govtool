import { useTheme } from '@emotion/react';
import {
    IconPlus,
} from '@intersect.mbo/intersectmbo.org-icons-set';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import { Box, IconButton } from '@mui/material';
import { Button } from '@atoms';
import { PdfInput } from '../PdfFields';
import { isRewardAddress, numberValidation } from '../../lib/utils';

const WithdrawalsManager = ({
    maxWithdrawals = 10,
    proposalData,
    setProposalData,
    withdrawalsErrors,
    setWithdrawalsErrors,
}) => {
    const theme = useTheme();

    const handleWithdrawalChange = async (index, field, value) => {
        const newWithdrawal = proposalData?.proposal_withdrawals?.map(
            (proposal_withdrawal, i) => {
                if (i === index) {
                    return { ...proposal_withdrawal, [field]: value };
                }
                return proposal_withdrawal;
            }
        );
        setProposalData({
            ...proposalData,
            proposal_withdrawals: newWithdrawal,
        });
        // If the prop_receiving_address is empty, remove the error
        if (field === 'prop_' && value === '') {
            return setWithdrawalsErrors((prev) => {
                const { [index]: removed, ...rest } = prev;
                return rest;
            });
        }
        // Validate prop_receiving_address

        if (field === 'prop_receiving_address') {
            const validationResult = await isRewardAddress(value);
            setWithdrawalsErrors((prev) => ({
                ...prev,
                [index]: {
                    ...prev[index],
                    prop_receiving_address:
                        validationResult === true ? '' : validationResult,
                },
            }));
        }
        // To DO add validation for registered stake address

        if (field === 'prop_amount') {
            const validationResult = numberValidation(value);
            setWithdrawalsErrors((prev) => ({
                ...prev,
                [index]: {
                    ...prev[index],
                    prop_amount:
                        validationResult === true ? '' : validationResult,
                },
            }));
        }
    };

    const handleAddWithdrawal = () => {
        if (proposalData?.proposal_withdrawals?.length < maxWithdrawals) {
            setProposalData({
                ...proposalData,
                proposal_withdrawals: [
                    ...proposalData?.proposal_withdrawals,
                    { prop_receiving_address: null, prop_amount: null },
                ],
            });
        }
    };

    const handleRemoveWithdrawal = (index) => {
        let newWithdrawal = proposalData?.proposal_withdrawals?.filter(
            (_, i) => i !== index
        );
        setProposalData({
            ...proposalData,
            proposal_withdrawals: newWithdrawal,
        });

        // Remove errors for removed link
        setWithdrawalsErrors((prev) => {
            const { [index]: removed, ...rest } = prev;
            return rest;
        });
    };
    // Laid out like GovTool's link fields: stacked inputs, the delete icon
    // inside the address field of every extra withdrawal, and a text add
    // button below.
    return (
        <Box sx={{ display: 'flex', flexDirection: 'column', gap: 3 }}>
            {proposalData?.proposal_withdrawals?.map((withdrawal, index) => (
                <Box
                    key={index}
                    sx={{
                        display: 'flex',
                        flexDirection: 'column',
                        gap: 3,
                        textAlign: 'left',
                    }}
                >
                    <PdfInput
                        label={`Receiving stake address #${index + 1}`}
                        placeholder='e.g. stake1...'
                        value={withdrawal.prop_receiving_address || ''}
                        onChange={(e) => {
                            handleWithdrawalChange(
                                index,
                                'prop_receiving_address',
                                e.target.value
                            );
                        }}
                        required
                        dataTestId={`receiving-address-${index}-text-input`}
                        errorMessage={
                            withdrawalsErrors[index]?.prop_receiving_address
                        }
                        errorDataTestId={`receiving-address-${index}-text-error`}
                        endAdornment={
                            index === 0 ? null : (
                                <IconButton
                                    onClick={() =>
                                        handleRemoveWithdrawal(index)
                                    }
                                    data-testid='withdrawal-wrapper-remove-address-button'
                                    size='small'
                                    sx={{ p: 0.5, mr: -0.5 }}
                                >
                                    <DeleteOutlineIcon
                                        color='primary'
                                        sx={{ height: 24, width: 24 }}
                                    />
                                </IconButton>
                            )
                        }
                    />
                    <PdfInput
                        label={`Amount (in ada) #${index + 1}`}
                        type='tel'
                        placeholder='e.g. 2000 ada'
                        value={withdrawal?.prop_amount || ''}
                        onChange={(e) =>
                            handleWithdrawalChange(
                                index,
                                'prop_amount',
                                e.target.value
                            )
                        }
                        required
                        dataTestId={`amount-${index}-text-input`}
                        errorMessage={withdrawalsErrors[index]?.prop_amount}
                        errorDataTestId={`amount-${index}-text-error`}
                    />
                </Box>
            ))}
            {proposalData?.proposal_withdrawals?.length < maxWithdrawals && (
                <Box sx={{ display: 'flex', justifyContent: 'center' }}>
                    <Button
                        variant='text'
                        size='extraLarge'
                        startIcon={
                            <IconPlus fill={theme.palette.primary.main} />
                        }
                        onClick={handleAddWithdrawal}
                        data-testid='add-withdrawal-link-button'
                    >
                        Add withdrawal address
                    </Button>
                </Box>
            )}
        </Box>
    );
};

export default WithdrawalsManager;
