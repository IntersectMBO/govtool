import { useTheme } from '@emotion/react';
import ArrowBackIosIcon from '@mui/icons-material/ArrowBackIos';
import { Dialog, Box } from '@mui/material';
import { Button, Typography } from '@atoms';
import React, { useEffect, useRef, useState } from 'react';
import { useNavigate } from 'react-router';
import {
    DraftSuccessfulBudgetDiscussionModal,
    ProblemStatementsAndProposalBenefits,
    ProposalOwnership,
    ProposalDetails,
    Costing,
    FurtherInformation,
    AdministrationAndAuditing,
    BudgetDiscussionSubmit,
    BudgetDiscussionReview,
} from '../BudgetDiscussionParts';
import {
    createBudgetDiscussionDraft,
    updateBudgetDiscussionDraft,
    createBudgetDiscussion,
    getBudgetDiscussion,
    deleteBudgetDiscussionDraft,
} from '../../lib/api';
import { cleanObject } from '../../lib/helpers';
import { PdfModal } from '../PdfModal';
import { useAppContext } from '../../context/context';
import { useLocation } from 'react-router';
import BudgetDiscussionInfo from '../BudgetDiscussionParts/BudgetDiscussionInfo';
import { add } from 'date-fns';

const CreateBudgetDiscussionDialog = ({
    open = false,
    onClose = false,
    current_bd_id = null,
}) => {
    const location = useLocation();
    const navigate = useNavigate();
    const theme = useTheme();
    const { setLoading, addChangesSavedAlert, addSuccessAlert } =
        useAppContext();
    const [step, setStep] = useState(1);
    const [budgetDiscussionData, setBudgetDiscussionData] = useState({
        // bd_contact_information: {},
        bd_proposal_ownership: {},
        bd_psapb: {},
        bd_proposal_detail: {},
        bd_costing: {},
        bd_further_information: {},
    });

    const [
        showDraftSuccessfulBudgetDiscussionModal,
        setShowDraftSuccessfulBudgetDiscussionModal,
    ] = useState(false);
    const [selectedDraftId, setSelectedDraftId] = useState(null);

    const [openDraftDialog, setOpenDraftDialog] = useState(false);
    const [draftDialogMessage, setDraftDialogMessage] = useState(false);
    const handleCloseDraftDialog = () => {
        setOpenDraftDialog(false);
    };

    const [errors, setErrors] = useState({});

    const fetchBudgetDiscussion = async (id) => {
        setLoading(true);
        //let query = `populate[0]=creator&populate[1]=bd_costing.preferred_currency&populate[2]=bd_proposal_detail.contract_type_name&populate[3]=bd_further_information.proposal_links&populate[4]=bd_psapb.type_name&populate[5]=bd_psapb.roadmap_name&populate[6]=bd_psapb.committee_name&populate[7]=bd_proposal_ownership.be_country&populate[8]=bd_contact_information.be_nationality&populate[9]=bd_contact_information.be_country_of_res`;
        let query = `populate[0]=creator&populate[1]=bd_costing.preferred_currency&populate[2]=bd_proposal_detail.contract_type_name&populate[3]=bd_further_information.proposal_links&populate[4]=bd_psapb.type_name&populate[5]=bd_psapb.roadmap_name&populate[6]=bd_psapb.committee_name&populate[7]=bd_proposal_ownership.be_country`;
        try {
            const response = await getBudgetDiscussion({
                id: id,
                query: query,
            });
            let newData = cleanObject(response);

            newData.master_id = id;
            //newData.bd_contact_information.be_country_of_res =
            //    response?.attributes?.bd_contact_information?.data.attributes?.be_country_of_res?.data?.id;
            //newData.bd_contact_information.be_nationality =
            //    response?.attributes?.bd_contact_information?.data?.attributes?.be_nationality?.data?.id;
            newData.bd_proposal_ownership.be_country =
                response?.attributes?.bd_proposal_ownership?.data?.attributes?.be_country?.data?.id;
            newData.bd_psapb.roadmap_name =
                response?.attributes?.bd_psapb?.data?.attributes?.roadmap_name?.data?.id;
            newData.bd_psapb.type_name =
                response?.attributes?.bd_psapb?.data?.attributes?.type_name?.data?.id;
            newData.bd_psapb.committee_name =
                response?.attributes?.bd_psapb.data?.attributes?.committee_name?.data?.id;
            newData.bd_proposal_detail.contract_type_name =
                response?.attributes?.bd_proposal_detail?.data?.attributes?.contract_type_name?.data?.id;
            newData.bd_costing.preferred_currency =
                response?.attributes?.bd_costing?.data.attributes?.preferred_currency?.data?.id;

            setBudgetDiscussionData({ ...newData });
            if (!response) return response;
        } catch (error) {
            console.error(error);
        } finally {
            setLoading(false);
        }
    };

    const closeCreateBDDialog = () => {
        onClose();
        if (location.pathname.includes('propose'))
            navigate('/budget_discussion');
    };

    const handleSaveDraft = async () => {
        try {
            let draftId;
            if (selectedDraftId == null) {
                draftId =
                    await createBudgetDiscussionDraft(budgetDiscussionData);
                setSelectedDraftId(draftId.data.id);
                setDraftDialogMessage('Draft Budget Discussion is saved');
            } else {
                draftId = await updateBudgetDiscussionDraft(
                    budgetDiscussionData,
                    selectedDraftId
                );
                setDraftDialogMessage('Draft Budget Discussion is updated');
            }
            setOpenDraftDialog(true);
        } catch (error) {
            console.error('Error while saving draft:', error);
        }
    };

    const handleDeleteProposal = async () => {
        setLoading(true);
        try {
            // await deleteProposal(selectedDraftId);
        } catch (error) {
            console.error('Failed to delete proposal:', error);
        } finally {
            setLoading(false);
        }
    };
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

    const handleCreateBudgetDiscussion = async (isDraft = false) => {
        setLoading(true);
        try {
            budgetDiscussionData.bd_costing.ada_amount =
                budgetDiscussionData?.bd_costing?.ada_amount?.toString() || '';
            budgetDiscussionData.bd_costing.usd_to_ada_conversion_rate =
                budgetDiscussionData?.bd_costing?.usd_to_ada_conversion_rate?.toString() ||
                '';
            budgetDiscussionData.bd_costing.amount_in_preferred_currency =
                budgetDiscussionData?.bd_costing?.amount_in_preferred_currency?.toString() ||
                '';

            const newBD = await createBudgetDiscussion(budgetDiscussionData);

            //Moved deletion down and added check for newBD so draft is deleted only if newBD is created
            if (selectedDraftId && newBD) {
                await deleteBudgetDiscussionDraft(selectedDraftId);
            }

            onClose();
            navigate(
                `/budget_discussion/${current_bd_id ? current_bd_id : newBD?.master_id}`
            );
            current_bd_id
                ? addChangesSavedAlert()
                : addSuccessAlert('Budget proposal created successfully');
            // if (
            //     !(
            //         budgetDiscussionData?.proposal_id &&
            //         budgetDiscussionData?.proposal_content_id
            //     )
            // ) {
            //     const { data } = await createProposal({
            //         ...budgetDiscussionData,
            //         is_draft: isDraft,
            //     });
            //     if (data && data?.attributes && data?.attributes?.proposal_id) {
            //         if (isDraft) {
            //             setShowDraftSuccessfulModal(true);
            //         } else {
            //             navigate(
            //                 `/proposal_discussion/${data?.attributes?.proposal_id}`
            //             );
            //         }

            //         if (selectedDraftId) {
            //             handleDeleteProposal();
            //         }
            //     }

            //     return data?.attributes?.proposal_id;
            //}
        } catch (error) {
            throw error;
        } finally {
            setLoading(false);
        }
    };

    const validateSection = (section) => {
        const validationResult = validateProposal(
            { [section]: budgetDiscussionData[section] },
            section
        );
        setErrors(validationResult || {});
    };

    function validateProposal(proposal, sectionsToValidate = null) {
        const validationRules = {
            bd_psapb: {
                type_name: { required: true, type: 'number' },
                roadmap_name: { required: true, type: 'number' },
                committee_name: { required: true, type: 'number' },
                proposal_benefit: { required: true, type: 'string' },
                problem_statement: { required: true, type: 'string' },
                supplementary_endorsement: { required: false, type: 'string' },
                explain_proposal_roadmap: {
                    required: false,
                    type: 'string',
                },
            },
            bd_costing: {
                ada_amount: {
                    required: true,
                    type: 'string',
                    //added isSpecial: true, for value check in validation
                    isSpecial: true,
                },
                cost_breakdown: { required: true, type: 'string' },
                preferred_currency: { required: true, type: 'number' },
                usd_to_ada_conversion_rate: {
                    required: true,
                    type: 'string',
                    isSpecial: true,
                },
                amount_in_preferred_currency: {
                    required: true,
                    type: 'string',
                    isSpecial: true,
                },
            },
            privacy_policy: { required: true, type: 'boolean' },
            bd_proposal_detail: {
                experience: { required: true, type: 'string' },
                proposal_name: { required: true, type: 'string' },
                key_dependencies: { required: true, type: 'string' },
                contract_type_name: { required: true, type: 'number' },
                maintain_and_support: { required: true, type: 'string' },
                proposal_description: { required: true, type: 'string' },
                key_proposal_deliverables: { required: true, type: 'string' },
                resourcing_duration_estimates: {
                    required: true,
                    type: 'string',
                },
                other_contract_type: {
                    required: false,
                    type: 'string',
                },
            },
            bd_proposal_ownership: {
                agreed: { required: true, type: 'boolean' },
                be_country: {
                    required:
                        budgetDiscussionData.bd_proposal_ownership
                            .submited_on_behalf === 'Company',
                    type: 'number',
                },
                group_name: {
                    required:
                        budgetDiscussionData.bd_proposal_ownership
                            .submited_on_behalf === 'Group',
                    type: 'string',
                },
                company_name: {
                    required:
                        budgetDiscussionData.bd_proposal_ownership
                            .submited_on_behalf === 'Company',
                    type: 'string',
                },
                type_of_group: {
                    required:
                        budgetDiscussionData.bd_proposal_ownership
                            .submited_on_behalf === 'Group',
                    type: 'string',
                },
                social_handles: { required: true, type: 'string' },
                submited_on_behalf: { required: true, type: 'string' },
                company_domain_name: {
                    required:
                        budgetDiscussionData.bd_proposal_ownership
                            .submited_on_behalf === 'Company',
                    type: 'string',
                },
                // proposal_public_champion: { required: true, type: 'string' },
                key_info_to_identify_group: {
                    required:
                        budgetDiscussionData.bd_proposal_ownership
                            .submited_on_behalf === 'Group',
                    type: 'string',
                },
            },
            // bd_contact_information: {
            //     be_email: { required: true, type: 'email' },
            //     be_full_name: { required: true, type: 'string' },
            //     be_nationality: { required: true, type: 'number' },
            //     be_country_of_res: { required: true, type: 'number' },
            //     submission_lead_email: { required: true, type: 'email' },
            //     submission_lead_full_name: { required: true, type: 'string' },
            // },
            // "bd_further_information": {
            //     "proposal_links": {
            //         required: true,
            //         type: 'array',
            //         itemValidation: {
            //             "prop_link": { required: true, type: 'url' },
            //             "prop_link_text": { required: true, type: 'string' }
            //         }
            //     }
            // },
            intersect_named_administrator: { required: true, type: 'boolean' },
            intersect_admin_further_text: {
                required:
                    budgetDiscussionData.intersect_named_administrator ===
                    false,
                type: 'string',
            },
        };

        function isEmail(email) {
            const re = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
            return re.test(String(email).toLowerCase());
        }

        function isURL(str) {
            try {
                new URL(str);
                return true;
            } catch (_) {
                return false;
            }
        }

        function isNumberString(str) {
            return !isNaN(str) && !isNaN(parseFloat(str));
        }

        function validateSection(section, rules, path = '') {
            const errors = {};

            for (const [field, rule] of Object.entries(rules)) {
                const fullFieldPath = path ? `${path}.${field}` : field;

                if (
                    rule.required &&
                    rule.type === 'boolean' &&
                    (section[field] === undefined ||
                        section[field] === null ||
                        section[field] === false)
                ) {
                    errors[fullFieldPath] = 'Required field is missing';

                    continue;
                }

                if (
                    rule.required &&
                    (section[field] === undefined || section[field] === null)
                ) {
                    errors[fullFieldPath] = 'Required field is missing';
                    continue;
                }

                if (
                    !rule.required &&
                    (section[field] === undefined ||
                        section[field] === null ||
                        section[field] === '')
                ) {
                    continue;
                }
                let isValid = true;
                switch (rule.type) {
                    case 'string':
                        isValid =
                            typeof section[field] === 'string' &&
                            section[field].trim() !== '';
                        if (rule.isSpecial && isValid) {
                            const value = Number(section[field]);
                            if (isNaN(value)) {
                                isValid = false;
                                errors[fullFieldPath] =
                                    'Only number is allowed';
                            } else if (value < 0) {
                                isValid = false;
                                errors[fullFieldPath] =
                                    'Only positive number is allowed';
                            }
                        } else if (!isValid && rule.isSpecial) {
                            errors[fullFieldPath] =
                                `Field should be a valid number`;
                        }
                        break;
                    case 'number':
                        isValid = typeof section[field] === 'number';

                        break;
                    case 'numberString':
                        isValid = isNumberString(section[field]);
                        break;
                    case 'boolean':
                        isValid = typeof section[field] === 'boolean';
                        break;
                    case 'email':
                        isValid =
                            typeof section[field] === 'string' &&
                            isEmail(section[field]);
                        break;
                    case 'url':
                        isValid =
                            typeof section[field] === 'string' &&
                            isURL(section[field]);
                        break;
                    case 'array':
                        if (!Array.isArray(section[field])) {
                            isValid = false;
                        } else if (rule.itemValidation) {
                            // Validacija svakog elementa niza
                            for (let i = 0; i < section[field].length; i++) {
                                const itemErrors = validateSection(
                                    section[field][i],
                                    rule.itemValidation,
                                    `${fullFieldPath}[${i}]`
                                );
                                Object.assign(errors, itemErrors);
                            }
                            continue;
                        }
                        break;
                    default:
                        isValid = true;
                }
                if (!isValid && !rule.isSpecial) {
                    errors[fullFieldPath] =
                        `Field should be a valid ${rule.type}`;
                }
            }

            return errors;
        }
        const sections = sectionsToValidate
            ? Array.isArray(sectionsToValidate)
                ? sectionsToValidate
                : [sectionsToValidate]
            : Object.keys(validationRules);

        const allErrors = {};

        for (const sectionName of sections) {
            const rules = validationRules[sectionName];
            if (!rules) continue;
            if (!proposal[sectionName]) {
                if (rules.required) {
                    allErrors[sectionName] = 'Required section is missingaaa';
                }
                continue;
            }

            const sectionErrors = validateSection(
                proposal[sectionName],
                rules,
                sectionName
            );
            Object.assign(allErrors, sectionErrors);
        }

        return Object.keys(allErrors).length === 0 ? null : allErrors;
    }

    const contentRef = useRef(null);

    useEffect(() => {
        if (contentRef?.current) {
            contentRef?.current.scrollIntoView({
                behavior: 'smooth',
                block: 'start',
            });
        }
    }, [step]);

    useEffect(() => {
        if (current_bd_id !== null) {
            fetchBudgetDiscussion(current_bd_id);
            setStep(2);
        }
    }, [current_bd_id]);

    return (
        <Dialog
            fullScreen
            open={open}
            onClose={onClose}
            data-testid='create-budget-discussion-dialog'
            PaperProps={{
                sx: { borderRadius: 0 },
            }}
        >
            <Box
                sx={{
                    position: 'relative',
                    minHeight: '100%',
                    display: 'flex',
                    flexDirection: 'column',
                }}
                ref={contentRef}
            >
                {/* Title band in the DashboardTopNav style. */}
                <Box
                    sx={{
                        borderBottom: `1px solid ${theme.palette.lightBlue}`,
                        px: { xxs: 2, md: 5 },
                        py: 3,
                    }}
                >
                    <Typography
                        variant='headline4'
                        component='h1'
                        sx={{ fontSize: { xxs: 22, md: 28 } }}
                    >
                        Submit a Budget Proposal
                    </Typography>
                </Box>
                {/* Page body in the CenteredBoxPageWrapper style. */}
                <Box
                    sx={{
                        flex: 1,
                        px: { xxs: 2, md: 5 },
                        py: { xxs: 3, md: 1.5 },
                        pb: { xxs: 4, md: 6 },
                        display: 'flex',
                        flexDirection: 'column',
                        gap: { xxs: 2, md: 1.5 },
                    }}
                >
                    <Box zIndex={10}>
                        <Button
                            variant='text'
                            size='small'
                            startIcon={
                                <ArrowBackIosIcon
                                    color='primary'
                                    sx={{ fontSize: 14 }}
                                />
                            }
                            onClick={() => {
                                onClose();
                                if (location.pathname.includes('propose')) {
                                    navigate('/budget_discussion');
                                }
                            }}
                            data-testid='show-all-button'
                            sx={{
                                px: 0,
                                fontSize: 14,
                                fontWeight: 400,
                                '&:hover': { backgroundColor: 'transparent' },
                            }}
                        >
                            Show all
                        </Button>
                    </Box>
                    <Box
                        sx={{
                            width: '100%',
                            display: 'flex',
                            flexDirection: 'column',
                            zIndex: 10,
                        }}
                    >
                            {step === 1 && (
                                <BudgetDiscussionInfo
                                    setStep={setStep}
                                    step={step}
                                    onClose={() => closeCreateBDDialog()}
                                    setBudgetDiscussionData={
                                        setBudgetDiscussionData
                                    }
                                    currentBudgetDiscussionData={
                                        budgetDiscussionData
                                    }
                                    selectedDraftId={selectedDraftId}
                                    setSelectedDraftId={setSelectedDraftId}
                                    handleSaveDraft={handleSaveDraft}
                                />
                            )}
                            {/* {step === 2 && (
                                <ContractInformation
                                    setStep={setStep}
                                    step={step}
                                    onClose={() => closeCreateBDDialog()}
                                    setBudgetDiscussionData={
                                        setBudgetDiscussionData
                                    }
                                    currentBudgetDiscussionData={
                                        budgetDiscussionData
                                    }
                                    errors={errors}
                                    setErrors={setErrors}
                                    setSelectedDraftId={setSelectedDraftId}
                                    handleSaveDraft={handleSaveDraft}
                                    validateSection={validateSection}
                                />
                            )} */}
                            {step === 2 && (
                                <ProposalOwnership
                                    setStep={setStep}
                                    step={step}
                                    onClose={() => closeCreateBDDialog()}
                                    setBudgetDiscussionData={
                                        setBudgetDiscussionData
                                    }
                                    currentBudgetDiscussionData={
                                        budgetDiscussionData
                                    }
                                    errors={errors}
                                    setErrors={setErrors}
                                    setSelectedDraftId={setSelectedDraftId}
                                    handleSaveDraft={handleSaveDraft}
                                    validateSection={validateSection}
                                />
                            )}
                            {step === 3 && (
                                <ProblemStatementsAndProposalBenefits
                                    setStep={setStep}
                                    step={step}
                                    onClose={() => closeCreateBDDialog()}
                                    setBudgetDiscussionData={
                                        setBudgetDiscussionData
                                    }
                                    currentBudgetDiscussionData={
                                        budgetDiscussionData
                                    }
                                    errors={errors}
                                    setErrors={setErrors}
                                    setSelectedDraftId={setSelectedDraftId}
                                    handleSaveDraft={handleSaveDraft}
                                    validateSection={validateSection}
                                />
                            )}
                            {step === 4 && (
                                <ProposalDetails
                                    setStep={setStep}
                                    step={step}
                                    onClose={() => closeCreateBDDialog()}
                                    setBudgetDiscussionData={
                                        setBudgetDiscussionData
                                    }
                                    currentBudgetDiscussionData={
                                        budgetDiscussionData
                                    }
                                    errors={errors}
                                    setErrors={setErrors}
                                    setSelectedDraftId={setSelectedDraftId}
                                    handleSaveDraft={handleSaveDraft}
                                    validateSection={validateSection}
                                />
                            )}
                            {step === 5 && (
                                <Costing
                                    setStep={setStep}
                                    step={step}
                                    onClose={() => closeCreateBDDialog()}
                                    setBudgetDiscussionData={
                                        setBudgetDiscussionData
                                    }
                                    currentBudgetDiscussionData={
                                        budgetDiscussionData
                                    }
                                    errors={errors}
                                    setErrors={setErrors}
                                    setSelectedDraftId={setSelectedDraftId}
                                    handleSaveDraft={handleSaveDraft}
                                    validateSection={validateSection}
                                />
                            )}
                            {step === 6 && (
                                <FurtherInformation
                                    setStep={setStep}
                                    step={step}
                                    onClose={() => closeCreateBDDialog()}
                                    setBudgetDiscussionData={
                                        setBudgetDiscussionData
                                    }
                                    currentBudgetDiscussionData={
                                        budgetDiscussionData
                                    }
                                    errors={errors}
                                    setErrors={setErrors}
                                    setSelectedDraftId={setSelectedDraftId}
                                    handleSaveDraft={handleSaveDraft}
                                    validateSection={validateSection}
                                />
                            )}
                            {step === 7 && (
                                <AdministrationAndAuditing
                                    setStep={setStep}
                                    step={step}
                                    onClose={() => closeCreateBDDialog()}
                                    setBudgetDiscussionData={
                                        setBudgetDiscussionData
                                    }
                                    currentBudgetDiscussionData={
                                        budgetDiscussionData
                                    }
                                    errors={errors}
                                    setErrors={setErrors}
                                    setSelectedDraftId={setSelectedDraftId}
                                    handleSaveDraft={handleSaveDraft}
                                    validateSection={validateSection}
                                />
                            )}
                            {step === 8 && (
                                <BudgetDiscussionSubmit
                                    setStep={setStep}
                                    step={step}
                                    onClose={() => closeCreateBDDialog()}
                                    setBudgetDiscussionData={
                                        setBudgetDiscussionData
                                    }
                                    currentBudgetDiscussionData={
                                        budgetDiscussionData
                                    }
                                    errors={errors}
                                    setErrors={setErrors}
                                    setSelectedDraftId={setSelectedDraftId}
                                    handleSaveDraft={handleSaveDraft}
                                    validateSection={validateSection}
                                />
                            )}
                            {step === 9 && (
                                <BudgetDiscussionReview
                                    setStep={setStep}
                                    step={step}
                                    onClose={() => closeCreateBDDialog()}
                                    setBudgetDiscussionData={
                                        setBudgetDiscussionData
                                    }
                                    currentBudgetDiscussionData={
                                        budgetDiscussionData
                                    }
                                    submitBudgetDiscussion={
                                        handleCreateBudgetDiscussion
                                    }
                                    errors={errors}
                                    setErrors={setErrors}
                                    setSelectedDraftId={setSelectedDraftId}
                                    handleSaveDraft={handleSaveDraft}
                                    validateSection={validateSection}
                                />
                            )}
                    </Box>
                </Box>
            </Box>

            <DraftSuccessfulBudgetDiscussionModal
                open={showDraftSuccessfulBudgetDiscussionModal}
                onClose={() =>
                    setShowDraftSuccessfulBudgetDiscussionModal(false)
                }
                closeCreateBDDialog={onClose}
            />
            <PdfModal
                open={openDraftDialog}
                keepMounted
                onClose={handleCloseDraftDialog}
                wrapperDataTestId='draft-saved-dialog-wrapper'
                hideCloseButton
            >
                <Typography
                    id='alert-dialog-slide-description'
                    variant='body1'
                    fontWeight={400}
                >
                    {draftDialogMessage}
                </Typography>
                <Box
                    sx={{ display: 'flex', justifyContent: 'flex-end', mt: 3 }}
                >
                    <Button
                        variant='text'
                        size='medium'
                        onClick={handleCloseDraftDialog}
                        data-testid={'close-button'}
                    >
                        Close
                    </Button>
                </Box>
            </PdfModal>
        </Dialog>
    );
};

export default CreateBudgetDiscussionDialog;
