'use client';

import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import InfoOutlinedIcon from '@mui/icons-material/InfoOutlined';
import { ICONS } from '@/consts/icons';
import {
    Box,
    Dialog,
    MenuItem,
    TextField,
    useMediaQuery,
} from '@mui/material';
import { Button, Typography } from '@atoms';
import { useEffect, useState } from 'react';
import { useNavigate } from 'react-router';
import { useAppContext } from '../../context/context';
import {
    createProposalContent,
    deleteProposal,
    getGovernanceActionTypes,
} from '../../lib/api';
import {
    containsString,
    formatIsoDate,
    isRewardAddress,
    maxLengthCheck,
    numberValidation,
} from '../../lib/utils';
import { HardForkManager, LinkManager } from '../CreationGoveranceAction';
import { WithdrawalsManager } from '../CreationGoveranceAction';
import { ConstitutionManager } from '../CreationGoveranceAction';
import DeleteProposalModal from '../DeleteProposalModal';
import CreateGA2 from '../../assets/svg/CreateGA2.jsx';
import { primaryBlue } from '@/consts/colors';
import { PdfModalCloseButton, PdfStatusModal } from '../PdfModal';
import {
    FlowBackLink,
    FlowHeader,
    FlowPage,
    StepBox,
    StepButtons,
    StepHeading,
    stepButtonSx,
} from '../CreationGoveranceAction/StepLayout';
import { PdfInput, PdfTextArea } from '../PdfFields';
const EditProposalDialog = ({
    proposal,
    openEditDialog,
    handleCloseEditDialog,
    setMounted,
    abstractMaxLength = 2500,
    motivationRationaleMaxLength = 12000,
    titleMaxLength = 80,
    onUpdate = false,
    setShouldRefresh = false,
}) => {
    const navigate = useNavigate();
    const { setLoading } = useAppContext();
    const [draft, setDraft] = useState({});
    const [isSaveDisabled, setIsSaveDisabled] = useState(true);
    const [openSaveDraftModal, setOpenSaveDraftModal] = useState(false);
    const [openPublishModal, setOpenPublishModal] = useState(false);
    const [governanceActionTypes, setGovernanceActionTypes] = useState([]);
    const [openDeleteConfirmationModal, setOpenDeleteConfirmationModal] =
        useState(false);
    const [selectedGovActionName, setSelectedGovActionName] = useState(
        proposal?.attributes?.content?.attributes?.gov_action_type?.attributes
            ?.gov_action_type_name
    );
    const [selectedGovActionId, setSelectedGovActionId] = useState(
        proposal?.attributes?.content?.attributes?.gov_action_type?.id
    );
    const [showProposalDeleteModal, setShowProposalDeleteModal] =
        useState(false);
    const [errors, setErrors] = useState({
        name: false,
        abstract: false,
        motivation: false,
        rationale: false,
    });
    const [helperText, setHelperText] = useState({
        name: '',
        abstract: '',
        motivation: '',
        rationale: '',
    });
    const [linksErrors, setLinksErrors] = useState({});
    const [withdrawalsErrors, setWithdrawalsErrors] = useState({});
    const [constitutionErrors, setConstitutionErrors] = useState({});
    const [hardForkErrors, setHardForkErrors] = useState({});
    const isSmallScreen = useMediaQuery((theme) =>
        theme.breakpoints.down('md')
    );
    const handleIsSaveDisabled = () => {
        if (
            draft?.gov_action_type_id &&
            draft?.prop_name &&
            !errors?.name &&
            draft?.prop_abstract &&
            !errors?.abstract &&
            draft?.prop_motivation &&
            !errors?.motivation &&
            draft?.prop_rationale &&
            !errors?.rationale
        ) {
            if (draft?.proposal_links.length > 0) {
                if (
                    draft?.proposal_links?.some(
                        (link) => !link.prop_link || !link.prop_link_text
                    ) ||
                    Object.values(linksErrors).some((error) => error.url)
                ) {
                    return setIsSaveDisabled(true);
                } else {
                    setIsSaveDisabled(false);
                }
            }
            if (draft?.gov_action_type_id == 2) {
                if (
                    draft?.proposal_withdrawals?.some(
                        (proposal_withdrawal) =>
                            !proposal_withdrawal ||
                            !proposal_withdrawal.prop_receiving_address
                    ) ||
                    Object.values(withdrawalsErrors).some(
                        (error) => error.prop_receiving_address
                    )
                ) {
                    return setIsSaveDisabled(true);
                } else {
                    setIsSaveDisabled(false);
                }
            }
            if (draft?.gov_action_type_id == 3) {
                if (
                    draft?.proposal_constitution_content
                        ?.prop_constitution_url == '' ||
                    constitutionErrors.prop_constitution_url ||
                    constitutionErrors.prop_guardrails_script_url
                ) {
                    return setIsSaveDisabled(true);
                } else {
                    setIsSaveDisabled(false);
                }
            }
            if (draft?.gov_action_type_id == 6) {
                if (
                    draft?.proposal_hard_fork_content?.major == '' ||
                    draft?.proposal_hard_fork_content?.minor == '' ||
                    isNaN(Number(draft?.proposal_hard_fork_content?.major)) ||
                    isNaN(Number(draft?.proposal_hard_fork_content?.minor))
                ) {
                    return setIsSaveDisabled(true);
                } else {
                    setIsSaveDisabled(false);
                }
            }
            // if(draft?.gov_action_type_id == 6){
            //     console.log(draft,"drafttttt")
            //     draft.proposal_hard_fork_content.id = draft.proposal_hard_fork_content.data.id
            //     draft.proposal_hard_fork_content.previous_ga_hash = draft.proposal_hard_fork_content.data.attributes.previous_ga_hash
            //     draft.proposal_hard_fork_content.previous_ga_id = draft.proposal_hard_fork_content.data.attributes.previous_ga_id
            //     draft.proposal_hard_fork_content.major = draft.proposal_hard_fork_content.data.attributes.major
            //     draft.proposal_hard_fork_content.minor = draft.proposal_hard_fork_content.data.attributes.minor
            //  //   delete draft.proposal_hard_fork_content.data
            //     setDraftData(draft)
            // }

            const selectedLabel = governanceActionTypes.find(
                (option) => option?.value === draft?.gov_action_type_id
            )?.label;
            const selectedType = governanceActionTypes.find(
                (option) => option?.value === draft?.gov_action_type_id
            )?.value;

            if (selectedType === 2) {
                if (
                    draft?.prop_receiving_address &&
                    !errors?.address &&
                    draft?.prop_amount &&
                    !errors?.amount
                ) {
                    setIsSaveDisabled(false);
                } else {
                    setIsSaveDisabled(true);
                }
            } else {
                setIsSaveDisabled(false);
            }
            if (selectedType === 3) {
                if (
                    draft?.proposal_constitution_content
                        .prop_constitution_url &&
                    !errors?.prop_constitution_url &&
                    draft?.proposal_constitution_content
                        .prop_guardrails_script_url &&
                    !errors?.prop_guardrails_script_url
                ) {
                    setIsSaveDisabled(false);
                } else {
                    setIsSaveDisabled(true);
                }
            } else {
                setIsSaveDisabled(false);
            }
        } else {
            setIsSaveDisabled(true);
        }
    };
    const setDraftData = (proposalData) => {
        const draft = {
            proposal_id: proposalData?.id,
            gov_action_type_id:
                proposalData?.attributes?.content?.attributes
                    ?.gov_action_type_id,
            prop_abstract:
                proposalData?.attributes?.content?.attributes?.prop_abstract,
            prop_motivation:
                proposalData?.attributes?.content?.attributes?.prop_motivation,
            prop_name: proposalData?.attributes?.content?.attributes?.prop_name,
            prop_rationale:
                proposalData?.attributes?.content?.attributes?.prop_rationale,
            proposal_withdrawals:
                proposalData?.attributes?.content?.attributes
                    ?.proposal_withdrawals,
            proposal_links:
                proposalData?.attributes?.content?.attributes?.proposal_links,
            proposal_constitution_content:
                proposalData?.attributes?.content?.attributes
                    ?.proposal_constitution_content,
            proposal_hard_fork_content:
                proposalData?.attributes?.content?.attributes
                    ?.proposal_hard_fork_content || {},
        };
        return draft;
    };
    const handleDeleteProposal = async () => {
        setLoading(true);
        try {
            const response = await deleteProposal(proposal?.id);
            if (!response) return;
            setShowProposalDeleteModal(false);
            setOpenDeleteConfirmationModal(true);
        } catch (error) {
            console.error('Failed to delete proposal:', error);
        } finally {
            setLoading(false);
        }
    };
    const handleTextAreaChange = (event, field, errorField) => {
        const value = event?.target?.value;
        setDraft((prev) => ({
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
    const handleUpdatePorposal = async (isDraft = false) => {
        setLoading(true);

        let proposalConentObj = {};

        if (isDraft === true) {
            proposalConentObj.is_draft = true;
            proposalConentObj.prop_rev_active = true;
        } else {
            proposalConentObj.prop_rev_active = true;
        }

        try {
            let payload = {
                ...draft,
                proposal_hard_fork_content: {
                    previous_ga_hash:
                        draft.proposal_hard_fork_content.previous_ga_hash,
                    previous_ga_id:
                        draft.proposal_hard_fork_content.previous_ga_id,
                    major: draft.proposal_hard_fork_content.major,
                    minor: draft.proposal_hard_fork_content.minor,
                },
            };
            const response = await createProposalContent({
                ...payload,
                ...proposalConentObj,
            });
            if (!response) return;

            if (onUpdate && !isDraft) {
                onUpdate();
            }
        } catch (error) {
            console.error('Failed to delete proposal:', error);
        } finally {
            setLoading(false);
        }
    };
    const handleOpenSaveDraftModal = () => {
        setOpenSaveDraftModal(true);
    };
    const handleCloseSaveDraftModal = () => {
        setOpenSaveDraftModal(false);
    };
    const handleOpenPublishModal = () => {
        setOpenPublishModal(true);
    };
    const handleClosePublishModal = () => {
        setOpenPublishModal(false);
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
    const handleChange = (e) => {
        const selectedValue = e.target.value;
        const selectedLabel = governanceActionTypes.find(
            (option) => option?.value === selectedValue
        )?.label;

        setDraft((prev) => ({
            ...prev,
            gov_action_type_id: selectedValue,
            prop_receiving_address: null,
            prop_amount: null,
        }));

        if (selectedValue != 3) {
            //cleanup fields co
            setDraft((prev) => ({
                ...prev,
                proposal_constitution_content: {},
            }));
        }

        if (selectedValue !== 6) {
            setDraft((prev) => ({
                ...prev,
                proposal_hard_fork_content: {},
            }));
        }
        setSelectedGovActionId(selectedValue);
        setSelectedGovActionName(selectedLabel);
    };
    useEffect(() => {
        fetchGovernanceActionTypes();
    }, []);
    useEffect(() => {
        setDraft(setDraftData(proposal));
        handleIsSaveDisabled();
    }, [proposal]);
    useEffect(() => {
        handleIsSaveDisabled();
    }, [draft, errors, linksErrors, withdrawalsErrors, constitutionErrors]);

    return (
        <>
            <Dialog
                fullScreen
                open={openEditDialog}
                onClose={handleCloseEditDialog}
                data-testid='edit-proposal-dialog'
                PaperProps={{
                    sx: { borderRadius: 0 },
                }}
            >
                <Box
                    sx={{
                        display: 'flex',
                        flexDirection: 'column',
                        minHeight: '100%',
                        position: 'relative',
                    }}
                >
                    <FlowHeader title='Edit Proposal' />
                    <FlowPage sx={{ pb: 4 }}>
                        <FlowBackLink
                            onClick={() => {
                                handleCloseEditDialog();
                            }}
                        >
                            Back
                        </FlowBackLink>
                        <Box sx={{ pt: { xxs: 3, md: 1.5 } }}>
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
                                    >
                                        <Button
                                            variant='outlined'
                                            size='medium'
                                            sx={{ mt: 3 }}
                                            startIcon={
                                                <DeleteOutlineIcon sx={{ fontSize: 18 }} />
                                            }
                                            onClick={() =>
                                                setShowProposalDeleteModal(
                                                    true
                                                )
                                            }
                                            data-testid='delete-proposal-button'
                                        >
                                            Delete Proposal
                                        </Button>
                                    </StepHeading>

                                    <Typography
                                        variant='body2'
                                        component='p'
                                        fontWeight={400}
                                        sx={{
                                            display: 'flex',
                                            justifyContent: 'center',
                                            alignItems: 'center',
                                            alignSelf: 'center',
                                            borderRadius: 50,
                                            gap: 1,
                                            mb: 1.25,
                                            px: 2,
                                            py: 0.5,
                                            backgroundColor: primaryBlue.c50,
                                            color: 'textBlack',
                                        }}
                                    >
                                        <InfoOutlinedIcon fontSize='inherit' />
                                        {`Last Edit: ${
                                            proposal?.attributes?.updatedAt
                                                ? formatIsoDate(
                                                      proposal?.attributes
                                                          ?.updatedAt
                                                  )
                                                : '--'
                                        }`}
                                    </Typography>

                                    <TextField
                                        select
                                        label='Governance Action Type'
                                        fullwidth
                                        required
                                        value={
                                            draft?.gov_action_type_id ||
                                            ''
                                        }
                                        onChange={handleChange}
                                        SelectProps={{
                                            SelectDisplayProps: {
                                                'data-testid':
                                                    'governance-action-type',
                                            },
                                        }}
                                    >
                                        {governanceActionTypes?.map(
                                            (option) => (
                                                <MenuItem
                                                    key={option?.value}
                                                    value={
                                                        option?.value
                                                    }
                                                    data-testid={`${option?.label?.toLowerCase()}-button`}
                                                >
                                                    {option?.label}
                                                </MenuItem>
                                            )
                                        )}
                                    </TextField>

                                    <PdfInput
                                        label='Title'
                                        value={draft?.prop_name || ''}
                                        onChange={(e) =>
                                            handleTextAreaChange(
                                                e,
                                                'prop_name',
                                                'name'
                                            )
                                        }
                                        required
                                        dataTestId='title-input'
                                        errorMessage={
                                            errors?.name
                                                ? helperText?.name
                                                : undefined
                                        }
                                        errorDataTestId='title-input-error'
                                    />

                                    <PdfTextArea
                                        name='Abstract'
                                        label='Abstract'
                                        placeholder='Summary...'
                                        value={
                                            draft?.prop_abstract || ''
                                        }
                                        onChange={(e) =>
                                            handleTextAreaChange(
                                                e,
                                                'prop_abstract',
                                                'abstract'
                                            )
                                        }
                                        required
                                        maxLength={abstractMaxLength}
                                        dataTestId='abstract-input'
                                        helperText='* A short summary of your proposal'
                                        helperTextDataTestId='abstract-helper-text'
                                        counterDataTestId='abstract-helper-character-count'
                                        errorMessage={
                                            errors?.abstract
                                                ? helperText?.abstract
                                                : undefined
                                        }
                                        errorDataTestId='abstract-helper-error'
                                    />

                                    <PdfTextArea
                                        name='Motivation'
                                        label='Motivation'
                                        placeholder='This is a problem because...'
                                        value={
                                            draft?.prop_motivation || ''
                                        }
                                        onChange={(e) =>
                                            handleTextAreaChange(
                                                e,
                                                'prop_motivation',
                                                'motivation'
                                            )
                                        }
                                        required
                                        maxLength={
                                            motivationRationaleMaxLength
                                        }
                                        dataTestId='motivation-input'
                                        helperText='* What problem is your proposal solving?'
                                        helperTextDataTestId='motivation-helper-text'
                                        counterDataTestId='motivation-helper-character-count'
                                        errorMessage={
                                            errors?.motivation
                                                ? helperText?.motivation
                                                : undefined
                                        }
                                        errorDataTestId='motivation-helper-error'
                                    />

                                    <PdfTextArea
                                        name='Rationale'
                                        label='Rationale'
                                        placeholder='This problem is solved by...'
                                        value={
                                            draft?.prop_rationale || ''
                                        }
                                        onChange={(e) =>
                                            handleTextAreaChange(
                                                e,
                                                'prop_rationale',
                                                'rationale'
                                            )
                                        }
                                        required
                                        maxLength={
                                            motivationRationaleMaxLength
                                        }
                                        dataTestId='rationale-input'
                                        helperText='* How does the on-chain change solve the problem?'
                                        helperTextDataTestId='rationale-helper-text'
                                        counterDataTestId='rationale-helper-character-count'
                                        errorMessage={
                                            errors?.rationale
                                                ? helperText?.rationale
                                                : undefined
                                        }
                                        errorDataTestId='rationale-helper-error'
                                    />

                                    {
                                        /// 'Treasury'
                                        selectedGovActionId === 2 ? (
                                            <>
                                                <WithdrawalsManager
                                                    proposalData={draft}
                                                    setProposalData={
                                                        setDraft
                                                    }
                                                    withdrawalsErrors={
                                                        withdrawalsErrors
                                                    }
                                                    setWithdrawalsErrors={
                                                        setWithdrawalsErrors
                                                    }
                                                />
                                            </>
                                        ) : null
                                    }
                                    {
                                        /// 'Constitution'
                                        selectedGovActionId === 3 ? (
                                            <ConstitutionManager
                                                proposalData={draft}
                                                setProposalData={
                                                    setDraft
                                                }
                                                constitutionManagerErrors={
                                                    constitutionErrors
                                                }
                                                setConstitutionManagerErrors={
                                                    setConstitutionErrors
                                                }
                                            ></ConstitutionManager>
                                        ) : null
                                    }
                                    {
                                        /// HardFork
                                        selectedGovActionId === 6 ? (
                                            <HardForkManager
                                                proposalData={draft}
                                                setProposalData={
                                                    setDraft
                                                }
                                                hardForkErrors={
                                                    hardForkErrors
                                                }
                                                setHardForkErrors={
                                                    setHardForkErrors
                                                }
                                                isEdit={true}
                                            />
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
                                            Links to additional content or
                                            social media contacts
                                        </Typography>
                                    </StepHeading>
                                    <LinkManager
                                        proposalData={draft}
                                        setProposalData={setDraft}
                                        linksErrors={linksErrors}
                                        setLinksErrors={setLinksErrors}
                                    />
                                </Box>
                                <StepButtons
                                    start={
                                        <Button
                                            variant='outlined'
                                            size='extraLarge'
                                            startIcon={
                                                <img src={ICONS.editIcon} alt='' width={20} height={20} />
                                            }
                                            sx={stepButtonSx}
                                            onClick={() => {
                                                handleCloseEditDialog();
                                            }}
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
                                                disabled={isSaveDisabled}
                                                onClick={async () => {
                                                    await handleUpdatePorposal(
                                                        true
                                                    );
                                                    handleOpenSaveDraftModal();
                                                    setMounted(false);
                                                }}
                                                data-testid='save-draft-button'
                                            >
                                                Save Draft
                                            </Button>
                                            <Button
                                                variant='contained'
                                                size='extraLarge'
                                                sx={stepButtonSx}
                                                disabled={isSaveDisabled}
                                                onClick={() => {
                                                    handleOpenPublishModal();
                                                }}
                                                data-testid='publish-with-new-edits-button'
                                            >
                                                Publish with new edits
                                            </Button>
                                        </>
                                    }
                                />
                            </StepBox>
                        </Box>
                    </FlowPage>
                    <PdfStatusModal
                        open={openSaveDraftModal}
                        onClose={handleCloseSaveDraftModal}
                        hideCloseButton
                        title='Proposal saved to drafts'
                        titleComponent='h2'
                        primaryButton={{
                            label: 'Close',
                            onClick: () => {
                                handleCloseSaveDraftModal();
                                handleCloseEditDialog();
                            },
                        }}
                    >
                        <PdfModalCloseButton
                            onClick={() => {
                                handleCloseSaveDraftModal();
                                handleCloseEditDialog();

                                setMounted(false);
                            }}
                        />
                    </PdfStatusModal>
                    <PdfStatusModal
                        open={openPublishModal}
                        onClose={handleClosePublishModal}
                        title='Please confirm applied changes'
                        titleComponent='h2'
                        primaryButton={{
                            label: 'Confirm',
                            dataTestId: 'confirm-button',
                            onClick: async () => {
                                await handleUpdatePorposal(false);
                                handleCloseSaveDraftModal();
                                handleCloseEditDialog();
                                setMounted(false);
                            },
                        }}
                        secondaryButton={{
                            label: 'Cancel',
                            dataTestId: 'cancel-button',
                            onClick: () => {
                                handleClosePublishModal();
                            },
                        }}
                    />
                    <PdfStatusModal
                        open={openDeleteConfirmationModal}
                        onClose={() => {
                            handleCloseEditDialog();
                            navigate('/proposal_discussion');
                            if (setShouldRefresh) {
                                setShouldRefresh(true);
                            }
                        }}
                        hideCloseButton
                        title='Proposal Deleted'
                        titleComponent='h2'
                        message='The proposal has been deleted successfully.'
                        primaryButton={{
                            label: 'Go to Proposal Discussion',
                            onClick: () => {
                                setOpenDeleteConfirmationModal(false);
                                handleCloseEditDialog();
                                navigate('/proposal_discussion');
                                if (setShouldRefresh) {
                                    setShouldRefresh(true);
                                }
                            },
                        }}
                    >
                        <PdfModalCloseButton
                            onClick={() => {
                                setOpenDeleteConfirmationModal(false);
                                handleCloseEditDialog();
                                navigate('/proposal_discussion');
                                if (setShouldRefresh) {
                                    setShouldRefresh(true);
                                }
                            }}
                        />
                    </PdfStatusModal>
                    <DeleteProposalModal
                        open={showProposalDeleteModal}
                        onClose={() => setShowProposalDeleteModal(false)}
                        handleDeleteProposal={handleDeleteProposal}
                    />
                    <Box
                        sx={{
                            position: 'absolute',
                            top: 0,
                            left: 0,
                            zIndex: 1,
                        }}
                    >
                        <CreateGA2 />
                    </Box>
                </Box>
            </Dialog>
        </>
    );
};

export default EditProposalDialog;
