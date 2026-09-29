import { Box } from '@mui/material';
import { PdfInput } from '../PdfFields';
import React from 'react';

const CommitteeManager = ({
    proposalData,
    setProposalData,
    committeeManagerErrors,
    setCommitteeManagerErrors,
}) => {
    return (
        <Box sx={{ display: 'flex', flexDirection: 'column', gap: 3 }}>
            <Box display='flex' flexDirection='column' flexGrow={1}>
                <PdfInput
                    label={`Numerator`}
                    placeholder='Numerator'
                    value={
                        proposalData?.proposal_committee_content?.numerator ||
                        ''
                    }
                    onChange={() => {}}
                    required
                    dataTestId={`numerator-input`}
                    errorMessage={committeeManagerErrors?.numerator}
                    errorDataTestId={`numerator-error`}
                />
            </Box>
            <Box display='flex' flexDirection='column' flexGrow={1}>
                <PdfInput
                    label={`Denominator`}
                    placeholder='Denominator'
                    value={
                        proposalData?.proposal_committee_content?.denominator ||
                        ''
                    }
                    onChange={() => {}}
                    required
                    dataTestId={`denominator-input`}
                    errorMessage={committeeManagerErrors?.denominator}
                    errorDataTestId={`denominator-error`}
                />
            </Box>
        </Box>
    );
};

export default CommitteeManager;
