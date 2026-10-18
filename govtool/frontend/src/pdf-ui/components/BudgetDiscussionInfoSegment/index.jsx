import React from 'react';

import { Box } from '@mui/material';
import { Typography } from '@atoms';
import MarkdownTypography from '../../lib/markdownRenderer';

// A label/value row in the style of GovTool's GovernanceActionCardElement:
// a 14/600 grey label, 4px gap, then the markdown value, 32px apart.
const BudgetDiscussionInfoSegment = ({
    question,
    answer,
    show = true,
    answerTestId,
}) => {
    if (!show) {
        return null;
    }
    return (
        <Box mb={4}>
            <Typography
                variant='body2'
                component='span'
                sx={{
                    display: 'block',
                    fontSize: 14,
                    fontWeight: 600,
                    lineHeight: '20px',
                    mb: '4px',
                    color: (theme) => theme?.palette?.neutralGray,
                }}
            >
                {question}
            </Typography>
            <MarkdownTypography testId={`${answerTestId}`} content={answer} />
        </Box>
    );
};

export default BudgetDiscussionInfoSegment;
