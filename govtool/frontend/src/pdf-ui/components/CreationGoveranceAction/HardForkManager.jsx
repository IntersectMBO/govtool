import { Box } from '@mui/material';
import { PdfInput } from '../PdfFields';
import React, { useEffect } from 'react';
import { getHardForkData } from '../../lib/api';
import { getHardForkContent, numberValidation } from '../../lib/utils';

const HardForkManager = ({
    proposalData,
    setProposalData,
    hardForkErrors,
    setHardForkErrors,
    isEdit = null, // Flag to indicate if the component is in edit mode
}) => {
        const fetchAndSetHardForkData = async () => {
        try {
            const resp = await getHardForkData();
            const data = resp.data;

            if (data) {
                setProposalData((prevData) => ({
                    ...prevData,
                    proposal_hard_fork_content: {
                        ...prevData.proposal_hard_fork_content,
                        previous_ga_hash: data.hash,
                        previous_ga_id: data.index,
                    },
                }));
            }
        } catch (error) {
            console.error('Error fetching hard fork data:', error);
        }
    };

  useEffect(() => {    
        if (isEdit === false) 
        fetchAndSetHardForkData();
        
    }, []);
    // A draft opened from the list, or edited, carries the API's relation
    // ({ data: { attributes } }); the form works on flat fields.
    useEffect(() => {
        const content = proposalData?.proposal_hard_fork_content;
        if (content && typeof content === 'object' && 'data' in content) {
            const stored = getHardForkContent(content);
            setProposalData((prevData) => ({
                ...prevData,
                proposal_hard_fork_content: {
                    previous_ga_hash: stored?.previous_ga_hash,
                    previous_ga_id: stored?.previous_ga_id,
                    major: stored?.major,
                    minor: stored?.minor,
                },
            }));
        }
    }, [proposalData?.proposal_hard_fork_content]);
        const handleHardForkDataChange = (field, value) => {
        setProposalData((prevData) => ({
            ...prevData,
            proposal_hard_fork_content: {
                ...prevData.proposal_hard_fork_content,
                [field]: value,
            },
        }));
        const validationError = validateNumberInput(value);
        if (!validationError.value) {
            setHardForkErrors((prevErrors) => {
                const { [field]: removed, ...rest } = prevErrors;
                return rest;
            });
        } else {
            setHardForkErrors((prevErrors) => ({
                ...prevErrors,
                [field]: validationError.text,
            }));
        }
    };

    const validateNumberInput = (value) => {
        if (isNaN(value) || value === '') {
            return { value: true, text: 'Please enter a valid number' };
        } else if (value < 0) {
            return { value: true, text: 'Number cannot be negative' };
        } else {
            return { value: false, text: '' };
        }
    };
    return (
        <Box sx={{ display: 'flex', flexDirection: 'column', gap: 3 }}>
            <Box display='flex' flexDirection='column' flexGrow={1}>
                <PdfInput
                    label={`Previous Gov Action Hash`}
                    placeholder='txHash#index'
                    value={
                        proposalData?.proposal_hard_fork_content
                            ?.previous_ga_hash || ''
                    }
                    onChange={() => {}}
                    // required
                    disabled
                    dataTestId={`previous-ga-hash-input`}
                    errorMessage={hardForkErrors?.previous_ga_hash}
                    errorDataTestId={`previous-ga-hash-error`}
                />
            </Box>
            <Box display='flex' flexDirection='column' flexGrow={1}>
                <PdfInput
                    label={`Previous Gov Action ID`}
                    placeholder='txHash#index'
                    disabled
                    value={
                        proposalData?.proposal_hard_fork_content
                            ?.previous_ga_id ?? ''
                    }
                    onChange={() => {}}
                    // required
                    dataTestId={`previous-ga-id-input`}
                    errorMessage={hardForkErrors?.previous_ga_id}
                    errorDataTestId={`previous-ga-id-error`}
                />
            </Box>

            <Box display='flex' flexDirection='column' flexGrow={1}>
                <PdfInput
                    label={`Major version`}
                    placeholder=''
                    value={
                        proposalData?.proposal_hard_fork_content?.major ?? ''
                    }
                    onChange={(e) => {
                        handleHardForkDataChange('major', e.target.value);
                    }}
                    required
                    dataTestId={`major-input`}
                    errorMessage={hardForkErrors?.major}
                    errorDataTestId={`major-error`}
                />
            </Box>
            <Box display='flex' flexDirection='column' flexGrow={1}>
                <PdfInput
                    label={`Minor version`}
                    placeholder=''
                    value={
                        proposalData?.proposal_hard_fork_content?.minor ?? ''
                    }
                    onChange={(e) => {
                        handleHardForkDataChange('minor', e.target.value);
                    }}
                    required
                    dataTestId={`minor-input`}
                    errorMessage={hardForkErrors?.minor}
                    errorDataTestId={`minor-error`}
                />
            </Box>
        </Box>
    );
};
export default HardForkManager;
