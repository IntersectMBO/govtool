import { Box, MenuItem, TextField } from '@mui/material';
import { Button, Typography } from '@atoms';
import { PdfInput, PdfTextArea } from '../PdfFields';
import { useEffect, useState } from 'react';
import { LinkManager, WithdrawalsManager, ConstitutionManager } from '.';
import { useAppContext } from '../../context/context';
import { getGovernanceActionTypes } from '../../lib/api';
import { containsString, maxLengthCheck } from '../../lib/utils';
import { set } from 'date-fns';
import HardForkManager from './HardForkManager';
import CommitteeManager from './CommitteeManager';
import { StepBox, StepButtons, StepHeading, stepButtonSx } from './StepLayout';
const Step2 = ({
    setStep,
    proposalData,
    setProposalData,
    handleSaveDraft,
    governanceActionTypes,
    setGovernanceActionTypes,
    isSmallScreen,
    isContinueDisabled,
    errors,
    setErrors,
    helperText,
    setHelperText,
    linksErrors,
    setLinksErrors,
    withdrawalsErrors,
    setWithdrawalsErrors,
    constitutionErrors,
    setConstitutionErrors,
    hardForkErrors,
    setHardForkErrors,
    committeeErrors,
    setCommitteeErrors,
}) => {
    const titleMaxLength = 80;
    const abstractMaxLength = 2500;
    const motivationRationaleMaxLength = 12000;
    const { setLoading } = useAppContext();
    const [selectedGovActionName, setSelectedGovActionName] = useState(
        governanceActionTypes.find(
            (option) => option?.value === +proposalData?.gov_action_type_id
        )?.label || ''
    );
    const [selectedGovActionId, setSelectedGovActionId] = useState(
        proposalData?.attributes?.content?.attributes?.gov_action_type?.id ||
            null
    );
    const [isDraftDisabled, setIsDraftDisabled] = useState(true);
    const handleChange = (e) => {
        const selectedValue = e.target.value;
        const selectedLabel = governanceActionTypes.find(
            (option) => option?.value === selectedValue
        )?.label;

        setProposalData((prev) => ({
            ...prev,
            gov_action_type_id: selectedValue,
            proposal_withdrawals: [
                { prop_receiving_address: null, prop_amount: null },
            ],
        }));
        if (selectedValue != 3) {
            //cleanup fields co
            setProposalData((prev) => ({
                ...prev,
                proposal_constitution_content: {},
            }));
        }
        setSelectedGovActionId(selectedValue);
        setSelectedGovActionName(selectedLabel);
    };

    const fetchGovernanceActionTypes = async () => {
        setLoading(true);
        try {
            const governanceActionTypeList = await getGovernanceActionTypes();
            const mappedData = governanceActionTypeList?.data?.map((item) => ({
                value: item?.id,
                label: item?.attributes?.gov_action_type_name,
            }));
            setGovernanceActionTypes(mappedData);
        } catch (error) {
            console.error(error);
        } finally {
            setLoading(false);
        }
    };

    const handleTextAreaChange = (event, field, errorField) => {
        const value = event?.target?.value;

        setProposalData((prev) => ({
            ...prev,
            [field]: value,
        }));

        if (value === '') {
            setHelperText((prev) => ({
                ...prev,
                [errorField]: '',
            }));
            setErrors((prev) => ({
                ...prev,
                [errorField]: false,
            }));
            return;
        }

        let errorMessage = '';
        errorMessage = containsString(value);

        if (errorMessage === true && field === 'prop_name') {
            errorMessage = maxLengthCheck(value, titleMaxLength);
        }

        setHelperText((prev) => ({
            ...prev,
            [errorField]: errorMessage === true ? '' : errorMessage,
        }));

        setErrors((prev) => ({
            ...prev,
            [errorField]: errorMessage === true ? false : true,
        }));
    };

    useEffect(() => {
        fetchGovernanceActionTypes();
    }, []);

    useEffect(() => {
        setSelectedGovActionName(
            governanceActionTypes.find(
                (option) => option?.value === +proposalData?.gov_action_type_id
            )?.label || ''
        );
        setSelectedGovActionId(+proposalData?.gov_action_type_id);
    }, [governanceActionTypes]);

    useEffect(() => {
        if (linksErrors && typeof linksErrors === 'object') {
            const hasLinkError = Object.values(linksErrors).some(
                (err) =>
                    (typeof err?.url === 'string' && err.url.trim() !== '') ||
                    (typeof err?.text === 'string' && err.text.trim() !== '')
            );
            if (hasLinkError) {
                setIsDraftDisabled(true);
                return;
            }
        }
        setIsDraftDisabled(false);
        if (
            proposalData?.gov_action_type_id &&
            proposalData?.prop_name?.length !== 0
        ) {
            setIsDraftDisabled(false);
        } else {
            setIsDraftDisabled(true);
        }
        if (
            proposalData?.proposal_constitution_content
                ?.prop_have_guardrails_script
        ) {
            if (
                proposalData.proposal_constitution_content
                    .prop_guardrails_script_url &&
                proposalData.proposal_constitution_content
                    .prop_guardrails_script_hash
            ) {
                setIsDraftDisabled(false);
            } else setIsDraftDisabled(true);
        }
    }, [proposalData, linksErrors]);

    return (
        <StepBox>
            <Box
                sx={{
                    display: 'flex',
                    flexDirection: 'column',
                    gap: 3,
                }}
            >
                <StepHeading
                    info='REQUIRED'
                    title='Proposal Details'
                    sx={{ mb: 1.25 }}
                />
                    <TextField
                        select
                        label='Governance Action Type'
                        value={proposalData?.gov_action_type_id || ''}
                        required
                        fullWidth
                        onChange={handleChange}
                        SelectProps={{
                            SelectDisplayProps: {
                                'data-testid': 'governance-action-type',
                            },
                        }}
                    >
                        {governanceActionTypes?.map((option, index) => (
                            <MenuItem
                                key={option?.value}
                                value={option?.value}
                                data-testid={`${option?.label?.toLowerCase()}-button`}
                            >
                                {option?.label}
                            </MenuItem>
                        ))}
                    </TextField>
                    <PdfInput
                        label='Title'
                        value={proposalData?.prop_name || ''}
                        onChange={(e) =>
                            handleTextAreaChange(e, 'prop_name', 'name')
                        }
                        required
                        dataTestId='title-input'
                        errorMessage={
                            errors?.name ? helperText?.name : undefined
                        }
                        errorDataTestId='title-input-error'
                    />
                    <PdfTextArea
                        name='Abstract'
                        label='Abstract'
                        value={proposalData?.prop_abstract || ''}
                        onChange={(e) =>
                            handleTextAreaChange(e, 'prop_abstract', 'abstract')
                        }
                        required
                        maxLength={abstractMaxLength}
                        dataTestId='abstract-input'
                        helperText='* A short summary of your proposal'
                        helperTextDataTestId='abstract-helper-text'
                        counterDataTestId='abstract-helper-character-count'
                        errorMessage={
                            errors?.abstract ? helperText?.abstract : undefined
                        }
                        errorDataTestId='abstract-helper-error'
                    />
                    <PdfTextArea
                        name='Motivation'
                        label='Motivation'
                        placeholder='This is a problem because...'
                        value={proposalData?.prop_motivation || ''}
                        onChange={(e) =>
                            handleTextAreaChange(
                                e,
                                'prop_motivation',
                                'motivation'
                            )
                        }
                        required
                        maxLength={motivationRationaleMaxLength}
                        dataTestId='motivation-input'
                        helperText='* What problem is your proposal solving?'
                        helperTextDataTestId='motivation-helper-text'
                        counterDataTestId='motivation-helper-character-count'
                        errorMessage={
                            errors?.motivation ? helperText?.motivation : undefined
                        }
                        errorDataTestId='motivation-helper-error'
                    />
                    <PdfTextArea
                        name='Rationale'
                        label='Rationale'
                        placeholder='This problem is solved by...'
                        value={proposalData?.prop_rationale || ''}
                        onChange={(e) =>
                            handleTextAreaChange(
                                e,
                                'prop_rationale',
                                'rationale'
                            )
                        }
                        required
                        maxLength={motivationRationaleMaxLength}
                        dataTestId='rationale-input'
                        helperText='* How does the on-chain change solve the problem?'
                        helperTextDataTestId='rationale-helper-text'
                        counterDataTestId='rationale-helper-character-count'
                        errorMessage={
                            errors?.rationale ? helperText?.rationale : undefined
                        }
                        errorDataTestId='rationale-helper-error'
                    />
                    {
                        /// Treasury
                        selectedGovActionId == 2 ? (
                            <WithdrawalsManager
                                proposalData={proposalData}
                                setProposalData={setProposalData}
                                withdrawalsErrors={withdrawalsErrors}
                                setWithdrawalsErrors={setWithdrawalsErrors}
                            />
                        ) : null
                    }
                    {
                        /// 'Constitution'
                        selectedGovActionId === 3 ? (
                            <ConstitutionManager
                                proposalData={proposalData}
                                setProposalData={setProposalData}
                                constitutionManagerErrors={constitutionErrors}
                                setConstitutionManagerErrors={
                                    setConstitutionErrors
                                }
                            ></ConstitutionManager>
                        ) : null
                    }
                    {
                        /// 'Committee'
                        selectedGovActionId === 5 ? (
                            <CommitteeManager
                                proposalData={proposalData}
                                setProposalData={setProposalData}
                                committeeManagerErrors={committeeErrors}
                                setCommitteeManagerErrors={setCommitteeErrors}
                            />
                        ) : null
                    }
                    {
                        /// 'Hard Fork'
                        selectedGovActionId === 6 ? (
                            <HardForkManager
                                proposalData={proposalData}
                                setProposalData={setProposalData}
                                hardForkErrors={hardForkErrors}
                                setHardForkErrors={setHardForkErrors}
                                isEdit={false}
                            ></HardForkManager>
                        ) : null
                    }
                    <StepHeading
                        info='OPTIONAL'
                        title='References and Supporting Information'
                        titleComponent='h5'
                        sx={{ mt: 3 }}
                    >
                        <Typography
                            variant='body2'
                            component='h6'
                            fontWeight={400}
                            sx={{ color: 'neutralGray', mt: 1 }}
                        >
                            Links to additional content or social media contacts
                        </Typography>
                    </StepHeading>
                    <LinkManager
                        proposalData={proposalData}
                        setProposalData={setProposalData}
                        linksErrors={linksErrors}
                        setLinksErrors={setLinksErrors}
                    />
                </Box>
                <StepButtons
                    start={
                        <Button
                            variant='outlined'
                            size='extraLarge'
                            sx={stepButtonSx}
                            onClick={() => setStep(1)}
                            data-testid='back-button'
                        >
                            Back
                        </Button>
                    }
                    end={
                        <>
                            <Button
                                variant='text'
                                size='extraLarge'
                                sx={stepButtonSx}
                                disabled={isDraftDisabled}
                                onClick={() => {
                                    handleSaveDraft(true);
                                }}
                                data-testid='save-draft-button'
                            >
                                Save Draft
                            </Button>
                            <Button
                                variant='contained'
                                size='extraLarge'
                                sx={stepButtonSx}
                                disabled={isContinueDisabled}
                                onClick={() => setStep(3)}
                                data-testid='continue-button'
                            >
                                Continue
                            </Button>
                        </>
                    }
                />
        </StepBox>
    );
};

export default Step2;
