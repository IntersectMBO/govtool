'use client';

import React from 'react';
import { CommentReview } from '../../components'
import { Box } from '@mui/material';
const CommentReviewPage = ({ reportHash }) => {
    return (
        <Box sx={{ px: { xxs: 2, md: 5 }, py: 3 }}>
           <CommentReview reportHash={reportHash}></CommentReview>
        </Box>
    );
};
export default CommentReviewPage;
