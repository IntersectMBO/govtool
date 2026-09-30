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
import { getContractTypeList } from '../../../lib/api';
import { StepperActionButtons } from '../../BudgetDiscussionParts';
import StepBox from '../StepBox';

const ProposalDetails = ({
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
    const [allContractTypes, setAllContractTypes] = useState([]);

    const proposalDescriptionMaxLength = 15000;
    const keyDependenciesMaxLength = 15000;
    const resourcingDurationEstimatesMaxLength = 15000;
    const keyProposalDeliverablesMaxLength = 15000;

    const supplementaryEndorsementMaxLength = 15000;
    useEffect(() => {
        const fetchData = async () => {
            try {
                if (!allContractTypes.length) {
                    const allContractTypesResponse =
                        await getContractTypeList();
                    setAllContractTypes(allContractTypesResponse?.data || []);
                }
            } catch (error) {
                console.error('Error fetching data:', error);
            }
        };
        fetchData();
    }, []);
    useEffect(() => {
        validateSection('bd_proposal_detail');
    }, [currentBudgetDiscussionData?.bd_proposal_detail]);
    const handleDataChange = (e, dataName) => {
        setBudgetDiscussionData({
            ...currentBudgetDiscussionData,
            bd_proposal_detail: {
                ...currentBudgetDiscussionData?.bd_proposal_detail,
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
                            <Typography variant='headline4' component='h4' gutterBottom mb={2}>
                                Section 3: Proposal Details
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
                                    3
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
                                    This section looks to gather key details of
                                    the proposal.
                                </Typography>
                            </Box>
                        </Box>
                        <PdfInput
                            name='Name of proposal'
                            label='What is your proposed name to be used to reference this proposal publicly?'
                            required
                            value={
                                currentBudgetDiscussionData?.bd_proposal_detail
                                    ?.proposal_name || ''
                            }
                            onChange={(e) =>
                                handleDataChange(e, 'proposal_name')
                            }
                            layoutStyles={{ mb: 3 }}
                            dataTestId='proposal-name-input'
                        />
                        <PdfTextArea
                            name='Proposal Description'
                            label='Proposal Description'
                            required
                            value={
                                currentBudgetDiscussionData?.bd_proposal_detail
                                    ?.proposal_description || ''
                            }
                            onChange={(e) =>
                                handleDataChange(e, 'proposal_description')
                            }
                            layoutStyles={{ mb: 4 }}
                            maxLength={proposalDescriptionMaxLength}
                            dataTestId='proposal-description-input'
                            helperText='* Please provide a high-level description / abstract of the proposal (2500 words max).'
                            helperTextDataTestId='proposal-description-helper-text'
                            counterDataTestId='proposal-description-helper-character-count'
                            errorMessage={errors?.bd_proposal_detail?.proposal_description || undefined}
                            errorDataTestId='proposal-description-helper-error'
                        />
                        <PdfTextArea
                            name='Key dependencies'
                            label='Please list any key dependencies (if any) for this proposal?'
                            required
                            value={
                                currentBudgetDiscussionData?.bd_proposal_detail
                                    ?.key_dependencies || ''
                            }
                            onChange={(e) =>
                                handleDataChange(e, 'key_dependencies')
                            }
                            layoutStyles={{ mb: 4 }}
                            maxLength={keyDependenciesMaxLength}
                            dataTestId='key-dependencies-input'
                            helperText='* These can be internal or external to the proposal. What else needs to be done for this proposal to begin or be completed.'
                            helperTextDataTestId='key-dependencies-helper-text'
                            counterDataTestId='key-dependencies-helper-character-count'
                            errorMessage={errors?.bd_proposal_detail?.key_dependencies || undefined}
                            errorDataTestId='key-dependencies-helper-error'
                        />
                        <PdfTextArea
                            name='Maintain and support'
                            label='How will this proposal be maintained and supported after initial development?'
                            required
                            value={
                                currentBudgetDiscussionData?.bd_proposal_detail
                                    ?.maintain_and_support || ''
                            }
                            onChange={(e) =>
                                handleDataChange(e, 'maintain_and_support')
                            }
                            layoutStyles={{ mb: 3 }}
                            maxLength={15000}
                            dataTestId='proposal-maintain-and-support-input'
                        />
                        <PdfTextArea
                            name='Key Proposal Deliverable(s) and Definition of Done:'
                            label='What tangible milestones or outcomes are to be delivered and what will the community ultimately receive?'
                            required
                            value={
                                currentBudgetDiscussionData?.bd_proposal_detail
                                    ?.key_proposal_deliverables || ''
                            }
                            onChange={(e) =>
                                handleDataChange(e, 'key_proposal_deliverables')
                            }
                            layoutStyles={{ mb: 4 }}
                            maxLength={keyProposalDeliverablesMaxLength}
                            dataTestId='key-proposal-deliverables-input'
                            helperText='* Keeping in mind sometimes proposals are multi-phased, what would be the target state of this tranche of the proposal or body of work.'
                            helperTextDataTestId='key-proposal-deliverables-helper-text'
                            counterDataTestId='key-proposal-deliverables-helper-character-count'
                            errorMessage={errors?.bd_proposal_detail?.key_proposal_deliverables || undefined}
                            errorDataTestId='key-proposal-deliverables-helper-error'
                        />
                        <PdfTextArea
                            name='Resourcing & Duration Estimates'
                            label='Please provide estimates of team size and duration to achieve the Key Proposal Deliverables outlined above.'
                            required
                            value={
                                currentBudgetDiscussionData?.bd_proposal_detail
                                    ?.resourcing_duration_estimates || ''
                            }
                            onChange={(e) =>
                                handleDataChange(
                                    e,
                                    'resourcing_duration_estimates'
                                )
                            }
                            layoutStyles={{ mb: 4 }}
                            maxLength={resourcingDurationEstimatesMaxLength}
                            dataTestId='resourcing-duration-estimates-input'
                            helperText='* If not known estimates can be provided.'
                            helperTextDataTestId='resourcing-duration-estimates-helper-text'
                            counterDataTestId='resourcing-duration-estimates-helper-character-count'
                            errorMessage={errors?.bd_proposal_detail?.resourcing_duration_estimates || undefined}
                            errorDataTestId='resourcing-duration-estimates-helper-error'
                        />
                        <PdfTextArea
                            name='Experience'
                            label='Please provide previous experience relevant to complete this project.'
                            required
                            value={
                                currentBudgetDiscussionData?.bd_proposal_detail
                                    ?.experience || ''
                            }
                            onChange={(e) => handleDataChange(e, 'experience')}
                            layoutStyles={{ mb: 3 }}
                            maxLength={15000}
                            dataTestId='proposal-previous-experience-input'
                        />
                        <TextField
                            select
                            name='Contract Types'
                            label='Contracting: Please describe how you expect to be contracted.'
                            //helperText={(<> {'Please click'} <Link href="">here</Link> {'to see details of Intersect Committees.'}</>)}
                            value={
                                currentBudgetDiscussionData?.bd_proposal_detail
                                    ?.contract_type_name || ''
                            }
                            required
                            fullWidth
                            onChange={(e) =>
                                handleDataChange(e, 'contract_type_name')
                            }
                            // helperText={errors[
                            //     'bd_proposal_detail.contract_type_name'
                            // ]?.trim()}
                            // error={
                            //     !!errors[
                            //         'bd_proposal_detail.contract_type_name'
                            //     ]?.trim()
                            // }
                            SelectProps={{
                                SelectDisplayProps: {
                                    'data-testid': 'contract-type-name',
                                },
                            }}
                            sx={{ mb: 4 }}
                        >
                            {allContractTypes?.map((option) => (
                                <MenuItem
                                    key={option?.id}
                                    value={option?.id}
                                    data-testid={`${option?.attributes.contract_type_name?.toLowerCase()}-button`}
                                >
                                    {option?.attributes.contract_type_name}
                                </MenuItem>
                            ))}
                        </TextField>
                        {allContractTypes?.find(
                            (type) =>
                                type?.id ==
                                currentBudgetDiscussionData?.bd_proposal_detail
                                    ?.contract_type_name
                        )?.attributes?.contract_type_name === 'Other' ? (
                            <PdfTextArea
                                name='Other contract type'
                                label='Please describe what you have in mind.'
                                required
                                value={
                                    currentBudgetDiscussionData
                                        ?.bd_proposal_detail
                                        ?.other_contract_type || ''
                                }
                                onChange={(e) =>
                                    handleDataChange(e, 'other_contract_type')
                                }
                                layoutStyles={{ mb: 3 }}
                                maxLength={15000}
                                dataTestId='other-contract-description'
                                errorMessage={errors[
                                    'bd_proposal_detail.other_contract_type'
                                ]?.trim() || undefined
                                }
                            />
                        ) : (
                            ''
                        )}
                        <StepperActionButtons
                            onClose={onClose}
                            onSaveDraft={handleSaveDraft}
                            onContinue={setStep}
                            onBack={setStep}
                            selectedDraftId={selectedDraftId}
                            nextStep={step + 1}
                            backStep={step - 1}
                            errors={
                                allContractTypes?.find(
                                    (type) =>
                                        type?.id ==
                                        currentBudgetDiscussionData
                                            ?.bd_proposal_detail
                                            ?.contract_type_name
                                )?.attributes?.contract_type_name === 'Other'
                                    ? currentBudgetDiscussionData
                                          ?.bd_proposal_detail
                                          ?.other_contract_type
                                        ? {
                                              ...errors,
                                          }
                                        : {
                                              ...errors,
                                              other_contract_type:
                                                  'Field should be a valid string',
                                          }
                                    : { ...errors }
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
export default ProposalDetails;
