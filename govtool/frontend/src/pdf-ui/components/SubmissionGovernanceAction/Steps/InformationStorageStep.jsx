'use client';

import React, { useState, useEffect } from 'react';

import { Box } from '@mui/material';
import { Button, Spacer, Typography } from '@atoms';
import { Step } from '@molecules';
import { PdfInput } from '../../PdfFields';
import { useNavigate } from 'react-router';
import { useAppContext } from '../../../context/context';
import {
    CheckingDataModal,
    ExternalDataNotMatchModal,
    UrlErrorModal,
    CancelRegistrationModal,
    GovernanceActionSubmittedModal,
    InsufficientBallanceModal,
} from '../../../components/SubmissionGovernanceAction';
import { getViaProxy, updateProposalContent } from '../../../lib/api';
import {
    isValidURLFormat,
    isValidURLLength,
    openInNewTab,
} from '../../../lib/utils';
import { ICONS } from '@/consts/icons';
import {
    StepBox,
    StepButtons,
    StepHeading,
    stepButtonSx,
} from '../../CreationGoveranceAction/StepLayout';

const InformationStorageStep = ({ proposal, handleCloseSubmissionDialog }) => {
    const navigate = useNavigate();
    const { walletAPI, validateMetadata } = useAppContext();
    const [jsonLdData, setJsonLdData] = useState({});
    const [hashData, setHashData] = useState('');
    const [fileURL, setFileURL] = useState('');

    const [showCheckingDataModal, setCheckingDataModal] = useState(false);
    const [showExternalDataNotMatchModal, setShowExternalDataNotMatchModal] =
        useState(false);
    const [showUrlErrorModal, setShowUrlErrorModal] = useState(false);
    const [showCancelRegistrationModal, setShowCancelRegistrationModal] =
        useState(false);
    const [
        showGovernanceActionSubmittedModal,
        setShowGovernanceActionSubmittedModal,
    ] = useState(false);

    const [showInsufficientBallanceModal, setShowInsufficientBallanceModal] =
        useState(false);

    const [urlError, setUrlError] = useState('');

    const handleURLChange = (url) => {
        setFileURL(url);

        if (!url?.length) {
            return setUrlError('');
        }

        let errorMessage = '';

        if (!isValidURLFormat(url)) {
            errorMessage = 'Invalid URL';
        } else {
            const lengthValidation = isValidURLLength(url);
            if (lengthValidation !== true) {
                errorMessage = lengthValidation;
            }
        }

        setUrlError(errorMessage);
    };
    const openGuideAboutStoringInformation = () =>
        openInNewTab(
            'https://docs.gov.tools/using-govtool/govtool-functions/storing-information-offline'
        );

    const handleCreateGAJsonLD = async () => {
        const referencesList = [];

        if (
            proposal?.attributes?.content?.attributes?.proposal_links?.length >
            0
        ) {
            proposal?.attributes?.content?.attributes?.proposal_links?.map(
                (reference) => {
                    referencesList.push({
                        label: reference?.prop_link_text || 'Label',
                        uri: reference?.prop_link,
                    });
                }
            );
        }

        const jsonLd = await walletAPI.createGovernanceActionJsonLD({
            title: proposal?.attributes?.content?.attributes?.prop_name,
            abstract: proposal?.attributes?.content?.attributes?.prop_abstract,
            motivation:
                proposal?.attributes?.content?.attributes?.prop_motivation,
            rationale:
                proposal?.attributes?.content?.attributes?.prop_rationale,
            references: referencesList,
        });

        if (!jsonLd) return;
        setJsonLdData(jsonLd);
        const hash = await walletAPI.createHash(jsonLd);
        setHashData(hash);
    };
    const proposalGATypeId =
        proposal?.attributes?.content?.attributes.gov_action_type_id;
    const handleGASubmission = async () => {
        try {
            let url = fileURL;
            setCheckingDataModal(true);
            if (fileURL.startsWith('ipfs://')) {
                url = `https://ipfs.io/ipfs/${fileURL.replace('ipfs://', '')}`;
            }
            const response = await validateMetadata({
                url: url,
                hash: hashData,
                standard: 'CIP108',
            });

            if (response?.valid) {
                let govActionBuilder = null;
                if (parseInt(proposalGATypeId) === 1) {
                    govActionBuilder =
                        await walletAPI.buildNewInfoGovernanceAction({
                            hash: hashData,
                            url: fileURL,
                        });
                    console.log(
                        '🚀 ~ handleGASubmission ~ walletAPI:',
                        walletAPI
                    );
                } else if (parseInt(proposalGATypeId) === 2) {
                    govActionBuilder =
                        await walletAPI.buildTreasuryGovernanceAction({
                            hash: hashData,
                            url: fileURL,
                            withdrawals: getWithdrawalsArray(),
                        });
                    console.log(
                        '🚀 ~ handleGASubmission ~ govActionBuilder:',
                        govActionBuilder
                    );
                } else if (parseInt(proposalGATypeId) === 3) {
                    const constitUrl =
                        proposal?.attributes?.content?.attributes
                            .proposal_constitution_content.data.attributes
                            .prop_constitution_url;
                    const constiUrlHash = await getHashFromUrl(constitUrl);
                    govActionBuilder =
                        await walletAPI.buildNewConstitutionGovernanceAction({
                            hash: hashData,
                            url: fileURL,
                            constitutionUrl: constitUrl,
                            constitutionHash: constiUrlHash,
                            //prevGovernanceActionHash: string;
                            //prevGovernanceActionIndex: number;
                            //scriptHash: string;
                        });
                } else if (parseInt(proposalGATypeId) === 4) {
                    ///Motion of No Confidence
                    govActionBuilder =
                        await walletAPI.buildNoConfidenceGovernanceAction({
                            hash: hashData,
                            url: fileURL,
                        });
                } else if (parseInt(proposalGATypeId) === 6) {
                    ///Hard Fork Initiation
                    govActionBuilder =
                        await walletAPI.buildHardForkInitiationGovernanceActions(
                            {
                                prevGovernanceActionHash:
                                    proposal?.attributes?.content?.attributes
                                        ?.proposal_hard_fork_content
                                        .previous_ga_hash,
                                prevGovernanceActionIndex:
                                    proposal?.attributes?.content?.attributes
                                        ?.proposal_hard_fork_content
                                        .previous_ga_id,
                                major: proposal?.attributes?.content?.attributes
                                    ?.proposal_hard_fork_content.major,
                                minor: proposal?.attributes?.content?.attributes
                                    ?.proposal_hard_fork_content.minor,
                                hash: hashData,
                                url: fileURL,
                            }
                        );
                }

                if (govActionBuilder) {
                    const tx = await walletAPI.buildSignSubmitConwayCertTx({
                        govActionBuilder: govActionBuilder,
                        type: 'createGovAction',
                    });

                    if (tx) {
                        await updateProposalContent(
                            proposal?.attributes?.content?.id,
                            {
                                prop_submitted: true,
                                prop_submission_date: new Date(),
                                prop_submission_tx_hash: tx,
                            }
                        );
                        setShowGovernanceActionSubmittedModal(true); 
                    }
                }
            } else {
                console.error(response);
                if (response?.status === 'URL_NOT_FOUND') {
                    setShowUrlErrorModal(true);
                } else {
                    setShowExternalDataNotMatchModal(true);
                }
            }
        } catch (error) {
            console.error(error);
            if (error?.includes('Insufficient')) {
                setShowInsufficientBallanceModal(true);
            }
        } finally {
            setCheckingDataModal(false);
        }
    };

    const getWithdrawalsArray = () => {
        let withdrawalsArray = [];
        let x =
            proposal?.attributes?.content?.attributes?.proposal_withdrawals.forEach(
                (withdrawal) => {
                    withdrawalsArray.push({
                        receivingAddress: withdrawal.prop_receiving_address,
                        amount: (withdrawal.prop_amount * 1000000).toString(),
                    });
                }
            );
        return withdrawalsArray;
    };

    const handleDownloadJsonLD = () => {
        const blob = new Blob([JSON.stringify(jsonLdData, null, 2)], {
            type: 'application/ld+json',
        });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = 'data.jsonld';
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(url);
    };

    async function getHashFromUrl(url) {
        try {
            if (!url) {
                throw new Error('url is not defined or null');
            }
            const response = await getViaProxy('', { url: url, method: 'GET' });
            if (response.status !== 200) {
                throw new Error(`HTTP error! Status: ${response.status}`);
            }
            const content =
                typeof response.data === 'string'
                    ? response.data
                    : JSON.stringify(response.data);
            const urlHash = await walletAPI.createHash(content);
            return urlHash;
        } catch (error) {
            alert(
                `Error fetching data from URL: Please verify that the URL is publicly accessible and try again.`
            );
            throw error;
        }
    }
    useEffect(() => {
        if (proposal && walletAPI) {
            handleCreateGAJsonLD();
        }
    }, [!!walletAPI, proposal]);

    return (
        <Box
            display='flex'
            flexDirection='column'
            data-testid='information-storage-step'
        >
            <StepBox>
                <StepHeading
                    title='Information Storage Steps'
                    titleComponent='h2'
                />
                <Box sx={{ display: 'flex', justifyContent: 'center' }}>
                    <Button
                        variant='text'
                        size='extraLarge'
                        endIcon={
                            <img
                                src={ICONS.externalLinkIcon}
                                alt=''
                                width={17}
                                height={17}
                            />
                        }
                        onClick={openGuideAboutStoringInformation}
                    >
                        <Typography
                            variant='body1'
                            component='span'
                            fontWeight={500}
                            color='primary'
                        >
                            Read full guide
                        </Typography>
                    </Button>
                </Box>

                <Typography
                    variant='body1'
                    fontWeight={400}
                    textAlign={'center'}
                >
                    Download your file, save it to your chosen location, and
                    enter the URL of that location in step 3
                </Typography>

                <Box sx={{ my: 4 }}>
                    <Step
                        stepNumber={1}
                        label='Download this file'
                        componentsLayoutStyles={{
                            alignItems: { xxs: 'flex-start', lg: 'center' },
                            flexDirection: { xxs: 'column', lg: 'row' },
                        }}
                        component={
                            <Button
                                variant='outlined'
                                size='extraLarge'
                                startIcon={
                                    <img alt='' src={ICONS.download} />
                                }
                                sx={{
                                    width: 'fit-content',
                                    ml: { xxs: 0, lg: 1.75 },
                                    mt: { xxs: 1.5, lg: 0 },
                                }}
                                onClick={() => handleDownloadJsonLD()}
                                data-testid='download-button'
                            >
                                data.jsonld
                            </Button>
                        }
                    />
                    <Spacer y={6} />
                    <Step
                        stepNumber={2}
                        label='Save this file in a location that provides a public URL (ex. github)'
                    />
                    <Spacer y={6} />
                    <Step
                        stepNumber={3}
                        label='Paste the URL here'
                        component={
                            <PdfInput
                                label='URL'
                                placeholder='URL'
                                value={fileURL || ''}
                                dataTestId='url-input'
                                onChange={(e) =>
                                    handleURLChange(e.target.value)
                                }
                                errorMessage={urlError || undefined}
                                errorDataTestId='url-input-error-text'
                                helpfulText='Required'
                                helpfulTextDataTestId='required-url-input-text'
                                required
                                layoutStyles={{ mt: 1.5 }}
                            />
                        }
                    />
                </Box>
                <StepButtons
                    sx={{ mt: 0 }}
                    start={
                        <Button
                            variant='outlined'
                            size='extraLarge'
                            sx={stepButtonSx}
                            onClick={() => navigate(-1)}
                            data-testid='cancel-button'
                        >
                            Cancel
                        </Button>
                    }
                    end={
                        <Button
                            variant='contained'
                            size='extraLarge'
                            sx={stepButtonSx}
                            onClick={handleGASubmission}
                            disabled={!fileURL || urlError?.length > 0}
                            data-testid='submit-button'
                        >
                            Submit
                        </Button>
                    }
                />
            </StepBox>

            <CheckingDataModal open={showCheckingDataModal} />
            <ExternalDataNotMatchModal
                open={showExternalDataNotMatchModal}
                onClose={() => setShowExternalDataNotMatchModal(false)}
                buttonOneClick={handleCloseSubmissionDialog}
                buttonTwoClick={() => {
                    setShowExternalDataNotMatchModal(false);
                    setShowCancelRegistrationModal(true);
                }}
            />
            <UrlErrorModal
                open={showUrlErrorModal}
                onClose={() => setShowUrlErrorModal(false)}
                buttonOneClick={handleCloseSubmissionDialog}
                buttonTwoClick={() => {
                    setShowUrlErrorModal(false);
                    setShowCancelRegistrationModal(true);
                }}
            />

            <CancelRegistrationModal
                open={showCancelRegistrationModal}
                onClose={() => setShowCancelRegistrationModal(false)}
            />

            <GovernanceActionSubmittedModal
                open={showGovernanceActionSubmittedModal}
                onClose={() => setShowGovernanceActionSubmittedModal(false)}
            />

            <InsufficientBallanceModal
                open={showInsufficientBallanceModal}
                onClose={() => setShowInsufficientBallanceModal(false)}
                buttonOneClick={handleCloseSubmissionDialog}
            />
        </Box>
    );
};

export default InformationStorageStep;
