import { useEffect } from 'react';
import { Box } from '@mui/material';
import { PdfInput, PdfCheckbox } from '../PdfFields';
import { isValidURLFormat, isValidURLLength } from '../../lib/utils';

const ConstitutionManager = ({
    proposalData,
    setProposalData,
    constitutionManagerErrors,
    setConstitutionManagerErrors,
}) => {
    console.log(proposalData);

    useEffect(() => {
        if (proposalData?.proposal_constitution_content?.data?.attributes) {
            setProposalData({
                ...proposalData,
                proposal_constitution_content:
                    proposalData.proposal_constitution_content.data.attributes,
            });
        } else {
            return;
        }
    }, [proposalData]);

    const togglePropHhaveGuScript = (checked) => {
        let pk = proposalData.proposal_constitution_content;
        pk.prop_have_guardrails_script = checked;
        if (!checked) {
            pk.prop_guardrails_script_url = '';
            pk.prop_guardrails_script_hash = '';
            setConstitutionManagerErrors((prev) => ({
                ...prev,
                ['prop_guardrails_script_url']: null,
            }));
            setConstitutionManagerErrors((prev) => ({
                ...prev,
                ['prop_guardrails_script_hash']: null,
            }));
        } else {
            constcheckLinkValue('', 'prop_guardrails_script_url');
        }
        setProposalData({ ...proposalData, proposal_constitution_content: pk });
        setConstitutionManagerErrors((prev) => ({
            ...prev,
            ['prop_guardrails_script_url']: null,
        }));
        setConstitutionManagerErrors((prev) => ({
            ...prev,
            ['prop_guardrails_script_hash']: null,
        }));
    };
    const handleUrlChange = (url_text) => {
        constcheckLinkValue(url_text, 'prop_constitution_url');
        let pk = proposalData.proposal_constitution_content || {};
        pk.prop_constitution_url = url_text;
        setProposalData({ ...proposalData, proposal_constitution_content: pk });
    };
    const handleGLinkChange = (url_text) => {
        constcheckLinkValue(url_text, 'prop_guardrails_script_url');
        let pk = proposalData.proposal_constitution_content;
        pk.prop_guardrails_script_url = url_text;
        setProposalData({ ...proposalData, proposal_constitution_content: pk });
    };

    const handleHashChange = (hash_text) => {
        constcheckHashValue(hash_text, 'prop_guardrails_script_hash');
        let pk = proposalData.proposal_constitution_content;
        pk.prop_guardrails_script_hash = hash_text;
        setProposalData({ ...proposalData, proposal_constitution_content: pk });
    };
    const constcheckLinkValue = (prop_value, prop_name) => {
        if (prop_value === '') {
            setConstitutionManagerErrors((prev) => ({
                ...prev,
                [prop_name]: 'Url is mandatory',
            }));
        } else {
            const isValid = isValidURLFormat(prop_value);
            const isValid1 = isValidURLLength(prop_value);
            setConstitutionManagerErrors((prev) => ({
                ...prev,
                [prop_name]: isValid
                    ? isValid1 === true
                        ? null
                        : 'Url longer than 128 char'
                    : 'Invalid URL format',
            }));
        }
    };
    const constcheckHashValue = (prop_value, prop_name) => {
        let isValid = false;
        if (prop_value) isValid = prop_value?.length > 0 ? true : false;
        setConstitutionManagerErrors((prev) => ({
            ...prev,
            [prop_name]: isValid ? null : 'Invalid HASH value',
        }));
    };

    useEffect(() => {
        let pk =
            proposalData.proposal_constitution_content ||
            proposalData.data?.attributes?.proposal_constitution_content;
        if (pk != undefined) {
            if (Boolean(pk.prop_constitution_url))
                constcheckLinkValue(
                    pk.prop_constitution_url,
                    'prop_constitution_url'
                );
            if (Boolean(pk.prop_guardrails_script_url)) {
                constcheckLinkValue(
                    pk.prop_guardrails_script_url,
                    'prop_guardrails_script_url'
                );
                constcheckHashValue(
                    pk.prop_guardrails_script_hash,
                    'prop_guardrails_script_hash'
                );
            }
        }
    }, [proposalData]);
    return (
        <Box sx={{ display: 'flex', flexDirection: 'column', gap: 3 }}>
            <PdfInput
                label={`New constitution URL`}
                placeholder='e.g. https://website.com/file.txt'
                value={
                    proposalData?.proposal_constitution_content
                        ?.prop_constitution_url || ''
                }
                onChange={(e) => handleUrlChange(e.target.value)}
                required
                dataTestId={`prop_constitution_url`}
                errorMessage={constitutionManagerErrors?.prop_constitution_url}
                errorDataTestId={`prop-constitution-url-text-error`}
            />
            <PdfCheckbox
                label={`Do you want to provide new guardrails script data?`}
                onChange={(checked) => togglePropHhaveGuScript(checked)}
                checked={
                    proposalData?.proposal_constitution_content
                        ?.prop_have_guardrails_script == 1 || false
                }
                id={`prop-have-guardrails-script`}
                dataTestId={`chb-prop-have-guardrails-script`}
            />
            {proposalData?.proposal_constitution_content
                ?.prop_have_guardrails_script == 1 ? (
                <>
                    <PdfInput
                        label={`Guardrails script URL`}
                        value={
                            proposalData?.proposal_constitution_content
                                ?.prop_guardrails_script_url || ''
                        }
                        onChange={(e) => handleGLinkChange(e.target.value)}
                        placeholder='ipfs://somesite.com/idsdads'
                        dataTestId={`prop-guardrails-script-url-input`}
                        errorMessage={
                            constitutionManagerErrors?.prop_guardrails_script_url
                        }
                        errorDataTestId={`prop-guardrails-script-url-input-error`}
                    />
                    <PdfInput
                        label={`Guardrails script hash`}
                        value={
                            proposalData?.proposal_constitution_content
                                ?.prop_guardrails_script_hash || ''
                        }
                        onChange={(e) => handleHashChange(e.target.value)}
                        placeholder='Guardrails script hash'
                        dataTestId={`prop-guardrails-script-hash-input`}
                        errorMessage={
                            constitutionManagerErrors?.prop_guardrails_script_hash
                        }
                        errorDataTestId={`prop-guardrails-script-hash-input-error`}
                    />
                </>
            ) : null}
        </Box>
    );
};

export default ConstitutionManager;
