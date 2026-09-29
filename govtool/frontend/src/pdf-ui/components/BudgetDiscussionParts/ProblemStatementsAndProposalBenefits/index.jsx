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
import { PdfTextArea } from '../../PdfFields';
import {
    getBudgetDiscussionRoadMapList,
    getBudgetDiscussionTypes,
    getBudgetDiscussionIntersectCommittee,
} from '../../../lib/api';
import { StepperActionButtons } from '../../BudgetDiscussionParts';
import StepBox from '../StepBox';

const ProblemStatementsAndProposalBenefits = ({
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
    const problemStatementMaxLength = 15000;
    const proposalBenefitMaxLength = 15000;
    const supplementaryEndorsementMaxLength = 15000;
    const [allRoadMaps, setAllRoadMaps] = useState([]);
    const [allBDTypes, setAllBDTypes] = useState([]);
    const [allCommittees, setAllCommittees] = useState([]);
    useEffect(() => {
        const fetchData = async () => {
            try {
                if (!allRoadMaps.length) {
                    const allRoadMapsResponse =
                        await getBudgetDiscussionRoadMapList();
                    setAllRoadMaps(allRoadMapsResponse?.data || []);
                }
                if (!allBDTypes.length) {
                    const allBDTypesResponse = await getBudgetDiscussionTypes();
                    setAllBDTypes(allBDTypesResponse?.data || []);
                }
                if (!allCommittees.length) {
                    const allCommitteesResponse =
                        await getBudgetDiscussionIntersectCommittee();
                    setAllCommittees(allCommitteesResponse?.data || []);
                }
            } catch (error) {
                console.error('Error fetching data:', error);
            }
        };
        fetchData();
    }, []);
    useEffect(() => {
        validateSection('bd_psapb');
    }, [currentBudgetDiscussionData?.bd_psapb]);
    const handleDataChange = (e, dataName) => {
        setBudgetDiscussionData((prev) => {
            const updatedBdPsapb = {
                ...prev?.bd_psapb,
                [dataName]: e.target.value,
            };

            if (dataName === 'roadmap_name') {
                updatedBdPsapb.explain_proposal_roadmap = '';
            }

            return {
                ...prev,
                bd_psapb: updatedBdPsapb,
            };
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
                                Section 2: Problem Statements and Proposal
                                Benefits
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
                                    2
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
                                    This section focuses on understanding the
                                    drivers behind the proposed project.
                                    E.g., Why should this project be undertaken?
                                </Typography>
                            </Box>
                        </Box>
                        <PdfTextArea
                            name='Problem Statement'
                            label='Problem Statement'
                            required
                            value={
                                currentBudgetDiscussionData?.bd_psapb
                                    ?.problem_statement || ''
                            }
                            onChange={(e) =>
                                handleDataChange(e, 'problem_statement')
                            }
                            layoutStyles={{ mb: 4 }}
                            maxLength={problemStatementMaxLength}
                            dataTestId='problem-statement-input'
                            helperText='What problem does this proposal seek to address?'
                            helperTextDataTestId='problem-statement-helper-text'
                            counterDataTestId='problem-statement-helper-character-count'
                            errorMessage={errors?.bd_psapb?.problem_statement || undefined}
                            errorDataTestId='problem-statement-helper-error'
                        />
                        <PdfTextArea
                            name='Proposal Benefit'
                            label='Proposal Benefit'
                            required
                            value={
                                currentBudgetDiscussionData?.bd_psapb
                                    ?.proposal_benefit || ''
                            }
                            onChange={(e) =>
                                handleDataChange(e, 'proposal_benefit')
                            }
                            layoutStyles={{ mb: 4 }}
                            maxLength={proposalBenefitMaxLength}
                            dataTestId='proposal-benefit-input'
                            helperText='If implemented, what would be the benefit and to which parts of the community? Please include the demonstrated value or return on investment to the Cardano Community.'
                            helperTextDataTestId='proposal-benefit-helper-text'
                            counterDataTestId='proposal-benefit-helper-character-count'
                            errorMessage={errors?.bd_psapb?.proposal_benefit || undefined}
                            errorDataTestId='proposal-benefit-helper-error'
                        />
                        <TextField
                            select
                            name='Product Roadmap'
                            label='Does this proposal align to the Product Roadmap and Roadmap Goals?'
                            helperText={
                                <>
                                    {' Please click '}{' '}
                                    <Link
                                        href='https://productcommittee.docs.intersectmbo.org/committee-outcomes/2025-cardanos-roadmap/2025-proposed-cardano-roadmap#scaling-the-l1-engine'
                                        target='_blank'
                                        rel='noreferrer noopener'
                                    >
                                        here
                                    </Link>{' '}
                                    {' to see details of the Product Roadmap'}
                                </>
                            }
                            value={
                                currentBudgetDiscussionData?.bd_psapb
                                    ?.roadmap_name || ''
                            }
                            // error={!!errors['bd_psapb.roadmap_name']?.trim()}
                            required
                            fullWidth
                            onChange={(e) =>
                                handleDataChange(e, 'roadmap_name')
                            }
                            SelectProps={{
                                SelectDisplayProps: {
                                    'data-testid': 'roadmap-name',
                                },
                            }}
                            sx={{ mb: 4 }}
                        >
                            {allRoadMaps?.map((option) => (
                                <MenuItem
                                    key={option?.id}
                                    value={option?.id}
                                    data-testid={`${option?.attributes.roadmap_name?.toLowerCase()}-button`}
                                >
                                    {option?.attributes.roadmap_name}
                                </MenuItem>
                            ))}
                        </TextField>
                        {allRoadMaps?.find(
                            (roadmap) =>
                                roadmap?.id ==
                                currentBudgetDiscussionData?.bd_psapb
                                    ?.roadmap_name
                        )?.attributes?.roadmap_name ===
                        'It supports the product roadmap' ? (
                            <PdfTextArea
                                name='Proposal explanation'
                                label='Please explain how your proposal supports the Product Roadmap.'
                                required
                                value={
                                    currentBudgetDiscussionData?.bd_psapb
                                        ?.explain_proposal_roadmap || ''
                                }
                                onChange={(e) =>
                                    handleDataChange(
                                        e,
                                        'explain_proposal_roadmap'
                                    )
                                }
                                layoutStyles={{ mb: 3 }}
                                maxLength={15000}
                                dataTestId='proposal-roadmap-description-input'
                                errorMessage={errors[
                                    'bd_psapb.explain_proposal_roadmap'
                                ]?.trim() || undefined
                                }
                            />
                        ) : null}
                        <TextField
                            select
                            name='bd_type'
                            label='Does your proposal align to any of the budget categories?'
                            // helperText={
                            //     errors['bd_psapb.type_name']?.trim() || ''
                            // }
                            // error={!!errors['bd_psapb.type_name']?.trim()}
                            value={
                                currentBudgetDiscussionData?.bd_psapb
                                    ?.type_name || ''
                            }
                            required
                            fullWidth
                            onChange={(e) => handleDataChange(e, 'type_name')}
                            SelectProps={{
                                SelectDisplayProps: {
                                    'data-testid':
                                        'budget-discussion-type-name',
                                },
                            }}
                            sx={{ mb: 4 }}
                        >
                            {allBDTypes?.map((option) => (
                                <MenuItem
                                    key={option?.id}
                                    value={option?.id}
                                    data-testid={`${option?.attributes.type_name?.toLowerCase()}-button`}
                                >
                                    {option?.attributes.type_name}
                                </MenuItem>
                            ))}
                        </TextField>
                        <TextField
                            select
                            name='Committee Alignment'
                            label='Does your proposal align with any of the Intersect Committees?'
                            helperText={
                                <>
                                    {'Please click'}{' '}
                                    <Link
                                        href='https://www.intersectmbo.org/committees'
                                        target='_blank'
                                        rel='noreferrer noopener'
                                    >
                                        here
                                    </Link>{' '}
                                    {'to see details of Intersect Committees.'}
                                </>
                            }
                            value={
                                currentBudgetDiscussionData?.bd_psapb
                                    ?.committee_name || ''
                            }
                            // error={!!errors['bd_psapb.committee_name']?.trim()}
                            required
                            fullWidth
                            onChange={(e) =>
                                handleDataChange(e, 'committee_name')
                            }
                            SelectProps={{
                                SelectDisplayProps: {
                                    'data-testid': 'committee-alignment-type',
                                },
                            }}
                            sx={{ mb: 4 }}
                        >
                            {allCommittees?.map((option) => (
                                <MenuItem
                                    key={option?.id}
                                    value={option?.id}
                                    data-testid={`${option?.attributes.committee_name?.toLowerCase()}-button`}
                                >
                                    {option?.attributes.committee_name}
                                </MenuItem>
                            ))}
                        </TextField>
                        <PdfTextArea
                            name='Supplementary Endorsement'
                            label='If possible provide evidence of wider community endorsement for this proposal?'
                            value={
                                currentBudgetDiscussionData?.bd_psapb
                                    ?.supplementary_endorsement || ''
                            }
                            onChange={(e) =>
                                handleDataChange(e, 'supplementary_endorsement')
                            }
                            layoutStyles={{ float: 'right' }}  /*CHECK*/
                            maxLength={supplementaryEndorsementMaxLength}
                            dataTestId='supplementary-endorsement-input'
                            helperText='E.g., CIP/CPS discussion, Technical Working Group, Special Interest Group, draft committee budget code, or other forum where comments and consensus have been made to date. Please share any links you have available.'
                            helperTextDataTestId='supplementary-endorsement-helper-text'
                            counterDataTestId='supplementary-endorsement-helper-character-count'
                            errorMessage={errors?.bd_psapb?.supplementary_endorsement || undefined}
                            errorDataTestId='supplementary-endorsement-helper-error'
                        />
                        <StepperActionButtons
                            onClose={onClose}
                            onSaveDraft={handleSaveDraft}
                            onContinue={setStep}
                            onBack={setStep}
                            selectedDraftId={selectedDraftId}
                            nextStep={step + 1}
                            backStep={step - 1}
                            errors={
                                allRoadMaps?.find(
                                    (roadmap) =>
                                        roadmap?.id ==
                                        currentBudgetDiscussionData?.bd_psapb
                                            ?.roadmap_name
                                )?.attributes?.roadmap_name ===
                                'It supports the product roadmap'
                                    ? currentBudgetDiscussionData?.bd_psapb
                                          ?.explain_proposal_roadmap
                                        ? {
                                              ...errors,
                                          }
                                        : {
                                              ...errors,
                                              explain_proposal_roadmap:
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
export default ProblemStatementsAndProposalBenefits;
