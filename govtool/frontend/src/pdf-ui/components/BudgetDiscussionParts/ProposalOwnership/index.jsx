import { Box, TextField, MenuItem } from '@mui/material';
import { Typography } from '@atoms';
import { PdfInput, PdfCheckbox } from '../../PdfFields';
import { useEffect, useState } from 'react';
import { StepperActionButtons } from '../../BudgetDiscussionParts';
import { getCountryList } from '../../../lib/api';
import StepBox from '../StepBox';

const ProposalOwnership = ({
    setStep,
    step,
    currentBudgetDiscussionData,
    setBudgetDiscussionData,
    onClose,
    errors,
    setErrors,
    setSelectedDraftId,
    selectedDraftId,
    handleSaveDraft,
    validateSection,
}) => {
    const [allCountries, setAllCountries] = useState([]);
    useEffect(() => {
        const fetchData = async () => {
            try {
                if (!allCountries.length) {
                    const countriesResponse = await getCountryList();
                    setAllCountries(countriesResponse?.data || []);
                }
            } catch (error) {
                console.error('Error fetching data:', error);
            }
        };

        fetchData();
    }, []);
    useEffect(() => {
        validateSection('bd_proposal_ownership');
    }, [currentBudgetDiscussionData?.bd_proposal_ownership]);

    const handleDataChange = (e, dataName) => {
        const value =
            e.target.type === 'checkbox' ? e.target.checked : e.target.value;

        setBudgetDiscussionData({
            ...currentBudgetDiscussionData,
            bd_proposal_ownership: {
                ...currentBudgetDiscussionData?.bd_proposal_ownership,
                [dataName]: value,
            },
        });
    };
    const handleSubmitedOnBehalfChange = (e) => {
        setBudgetDiscussionData({
            ...currentBudgetDiscussionData,
            bd_proposal_ownership: {
                ...currentBudgetDiscussionData?.bd_proposal_ownership,
                submited_on_behalf: e.target.value,
                company_name: '',
                company_domain_name: '',
                be_country: null,
                group_name: '',
                type_of_group: '',
                key_info_to_identify_group: '',
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
                                sx={{ mb: 3 }}
                            >
                                Section 1: Proposal Ownership
                            </Typography>
                        </Box>
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
                                1
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
                        <Box>
                            <TextField
                                select
                                label='Is this proposal being submitted on behalf of an individual (the beneficiary), company, or some other group?'
                                value={
                                    currentBudgetDiscussionData
                                        ?.bd_proposal_ownership
                                        ?.submited_on_behalf || 'Please Choose'
                                }
                                required
                                fullWidth
                                onChange={(e) => {
                                    handleSubmitedOnBehalfChange(e);
                                }}
                                SelectProps={{
                                    SelectDisplayProps: {
                                        'data-testid': 'proposal-committee',
                                    },
                                }}
                                helperText='If you are submitting on behalf of an Intersect Committee, please select Group. The Group Name would be the “Name of the Committee (e.g. MCC, TSC)”. The Type of Group would be “Intersect Committee”. The Key Information to Identify the Group would be the names of the Voting members of the Committee.'
                                sx={{ mb: 3 }}
                            >
                                <MenuItem
                                    key={'1'}
                                    value={'Individual'}
                                    data-testid='individual-submission'
                                >
                                    Individual
                                </MenuItem>
                                <MenuItem
                                    key={'2'}
                                    value={'Company'}
                                    data-testid='company-submission'
                                >
                                    Company
                                </MenuItem>
                                <MenuItem
                                    key={'3'}
                                    value={'Group'}
                                    data-testid='group-submission'
                                >
                                    Group
                                </MenuItem>
                            </TextField>
                            {currentBudgetDiscussionData.bd_proposal_ownership
                                ?.submited_on_behalf === 'Company' ? (
                                <Box>
                                    <PdfInput
                                        name='Company Name*'
                                        label='Company Name'
                                        value={
                                            currentBudgetDiscussionData
                                                ?.bd_proposal_ownership
                                                ?.company_name || ''
                                        }
                                        required
                                        onChange={(e) =>
                                            handleDataChange(e, 'company_name')
                                        }
                                        // helperText={errors[
                                        //     'bd_proposal_ownership.company_name'
                                        // ]?.trim()}
                                        // error={
                                        //     !!errors[
                                        //         'bd_proposal_ownership.company_name'
                                        //     ]?.trim()
                                        // }
                                        layoutStyles={{ mb: 3 }}
                                        dataTestId='company-name-input'
                                    />
                                    <PdfInput
                                        name='Company Domain Name'
                                        label='Company Domain Name'
                                        value={
                                            currentBudgetDiscussionData
                                                ?.bd_proposal_ownership
                                                ?.company_domain_name || ''
                                        }
                                        required
                                        helpfulText={
                                            //     errors[
                                            //         'bd_proposal_ownership.company_domain_name'
                                            //     ]?.trim() ||
                                            'Example of domain format to input: intersectmbo.org'
                                        }
                                        // error={
                                        //     !!errors[
                                        //         'bd_proposal_ownership.company_domain_name'
                                        //     ]?.trim()
                                        // }
                                        onChange={(e) =>
                                            handleDataChange(
                                                e,
                                                'company_domain_name'
                                            )
                                        }
                                        layoutStyles={{ mb: 3 }}
                                        dataTestId='company-domain-input'
                                    />
                                    <TextField
                                        select
                                        label='Country of Incorporation'
                                        value={
                                            currentBudgetDiscussionData
                                                ?.bd_proposal_ownership
                                                ?.be_country || ''
                                        }
                                        required
                                        fullWidth
                                        // helperText={errors[
                                        //     'bd_proposal_ownership.be_country'
                                        // ]?.trim()}
                                        // error={
                                        //     !!errors[
                                        //         'bd_proposal_ownership.be_country'
                                        //     ]?.trim()
                                        // }
                                        onChange={(e) =>
                                            handleDataChange(e, 'be_country')
                                        }
                                        SelectProps={{
                                            SelectDisplayProps: {
                                                'data-testid':
                                                    'country-of-incorporation',
                                            },
                                        }}
                                        sx={{ mb: 3 }}
                                    >
                                        {allCountries.map((option) => (
                                            <MenuItem
                                                key={option?.id}
                                                value={option?.id}
                                                data-testid={`${option?.attributes.country_name?.replace(/\s+/g, '-').toLowerCase()}-country-of-incorporation-button`}
                                            >
                                                {
                                                    option?.attributes
                                                        .country_name
                                                }
                                            </MenuItem>
                                        ))}
                                    </TextField>
                                </Box>
                            ) : (
                                ''
                            )}
                            {currentBudgetDiscussionData.bd_proposal_ownership
                                ?.submited_on_behalf === 'Group' ? (
                                <Box>
                                    <PdfInput
                                        name='Group Name*'
                                        label='Group Name'
                                        value={
                                            currentBudgetDiscussionData
                                                ?.bd_proposal_ownership
                                                ?.group_name || ''
                                        }
                                        required
                                        onChange={(e) =>
                                            handleDataChange(e, 'group_name')
                                        }
                                        // helperText={errors[
                                        //     'bd_proposal_ownership.group_name'
                                        // ]?.trim()}
                                        // error={
                                        //     !!errors[
                                        //         'bd_proposal_ownership.group_name'
                                        //     ]?.trim()
                                        // }
                                        layoutStyles={{ mb: 3 }}
                                        dataTestId='group-name-input'
                                    />
                                    <PdfInput
                                        name='Type of Group'
                                        label='Type of Group'
                                        value={
                                            currentBudgetDiscussionData
                                                ?.bd_proposal_ownership
                                                ?.type_of_group || ''
                                        }
                                        required
                                        onChange={(e) =>
                                            handleDataChange(e, 'type_of_group')
                                        }
                                        // helperText={errors[
                                        //     'bd_proposal_ownership.type_of_group'
                                        // ]?.trim()}
                                        // error={
                                        //     !!errors[
                                        //         'bd_proposal_ownership.type_of_group'
                                        //     ]?.trim()
                                        // }
                                        layoutStyles={{ mb: 3 }}
                                        dataTestId='group-type-input'
                                    />
                                    <PdfInput
                                        name='Key Information to Identify Group'
                                        label='Key Information to Identify Group'
                                        value={
                                            currentBudgetDiscussionData
                                                ?.bd_proposal_ownership
                                                ?.key_info_to_identify_group ||
                                            ''
                                        }
                                        required
                                        onChange={(e) =>
                                            handleDataChange(
                                                e,
                                                'key_info_to_identify_group'
                                            )
                                        }
                                        // helperText={errors[
                                        //     'bd_proposal_ownership.key_info_to_identify_group'
                                        // ]?.trim()}
                                        // error={
                                        //     !!errors[
                                        //         'bd_proposal_ownership.key_info_to_identify_group'
                                        //     ]?.trim()
                                        // }
                                        layoutStyles={{ mb: 4 }}
                                        dataTestId='group-identity-information-input'
                                    />
                                </Box>
                            ) : (
                                ''
                            )}
                            {
                                /* <TextField
                                select
                                label='Proposal Public Champion: Who would you like to be the public proposal champion?'
                                value={
                                    currentBudgetDiscussionData
                                        ?.bd_proposal_ownership
                                        ?.proposal_public_champion || ''
                                }
                                required
                                fullWidth
                                onChange={(e) =>
                                    handleDataChange(
                                        e,
                                        'proposal_public_champion'
                                    )
                                }
                                helperText={
                                    //  errors[
                                    //      'bd_proposal_ownership.proposal_public_champion'
                                    //  ]?.trim() ||
                                    'A Proposal Champion is a formal advocate for the proposal. They are responsible for providing information on the proposal, answering queries and generally building support amongst DReps. To facilitate this, the preferred contact details will be shared publicly.'
                                }
                                SelectProps={{
                                    SelectDisplayProps: {
                                        'data-testid':
                                            'proposal-public-champion',
                                    },
                                }}
                                sx={{ mb: 3 }}
                            >
                                <MenuItem
                                    key={'1'}
                                    value={'Beneficiary listed above'}
                                    data-testid='beneficiary-listed-above'
                                >
                                    Beneficiary listed above

                                </MenuItem>
                                <MenuItem
                                    key={'2'}
                                    value={'Submission lead listed above'}
                                    data-testid='submission-lead-listed-above'
                                >
                                    Submission lead listed above
                                </MenuItem>
                            </TextField> */
                                <PdfInput
                                    label='Please provide your preferred contact details that will be shared publicly (e.g. email address, X handle, Discord handle, Github) ?'
                                    value={
                                        currentBudgetDiscussionData
                                            ?.bd_proposal_ownership
                                            ?.social_handles || ''
                                    }
                                    required
                                    onChange={(e) =>
                                        handleDataChange(e, 'social_handles')
                                    }
                                    //   helperText={errors[
                                    //       'bd_proposal_ownership.social_handles'
                                    //   ]?.trim()}
                                    //   error={
                                    //       !!errors[
                                    //           'bd_proposal_ownership.social_handles'
                                    //       ]?.trim()
                                    //   }
                                    layoutStyles={{ mb: 3 }}
                                    dataTestId='provide-preferred-input'
                                />
                            }
                            <PdfCheckbox
                                checked={Boolean(
                                    currentBudgetDiscussionData
                                        ?.bd_proposal_ownership?.agreed
                                )}
                                onChange={(checked) =>
                                    setBudgetDiscussionData({
                                        ...currentBudgetDiscussionData,
                                        bd_proposal_ownership: {
                                            ...currentBudgetDiscussionData?.bd_proposal_ownership,
                                            agreed: checked,
                                        },
                                    })
                                }
                                dataTestId='agree-checkbox'
                                labelStyles={{
                                    variant: 'body2',
                                    fontWeight: 400,
                                }}
                                label='I agree to the information in section 1 to be shared publicly'
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
                            showBack={!currentBudgetDiscussionData?.master_id}
                            showSaveDraft={
                                !currentBudgetDiscussionData?.master_id
                            }
                        />
                    </StepBox>
            </Box>
        </Box>
    );
};
export default ProposalOwnership;
