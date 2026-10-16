import React from 'react';
import { Card, Box, Link, Grid } from '@mui/material';
import { Button, Typography } from '@atoms';

const CommentReportPopup = ({commentId, onReport, onCancel }) => {
    const handleCancel = () => {
        onCancel();
    };

    const handleReport = (commentId) => {
        onReport(commentId);
    };

    return (
        
        <Card
            display={'flex'}
           // flexDirection={'column'}
         //   justifyContent={'center'}
           // alignItems={'center'}
            // GovTool ModalWrapper look: radius 24, the modal shadow and padding.
            sx={{
                p: { xxs: 3, md: 4 },
                width: { xxs: 'calc(100vw - 32px)', md: 600 },
                maxWidth: 600,
                boxSizing: 'border-box',
                borderRadius: '24px',
                boxShadow: '1px 2px 11px 0px rgba(0, 18, 61, 0.37)',
                bgcolor: 'neutralWhite',
            }}
        >
            <Box>
                <Typography variant="headline5" component="h5">
                    Report Comment
                </Typography>
            </Box>
            <Box textAlign={'left'} width={'100%'} sx={{ mt: 2 }}>
                <Typography variant="body1" fontWeight={400} sx={{ whiteSpace: 'pre-line' }}>
                    If you feel this comment is contrary to our content policies, or is in {'\n'}
                    your view inappropriate, you can report it, and it will be reviewed. {'\n'}
                    See our <Link href="#">comment policy</Link> for more details.
                </Typography>
            </Box>
            <Box width={'100%'} sx={{ mt: 3 }}>
                <Grid container justifyContent="space-between" gap={2}>
                    <Grid item>
                        <Button
                            variant="outlined"
                            size="large"
                            onClick={() =>handleCancel()}
                            data-testid="cancel-button"
                        >
                            Cancel
                        </Button>
                    </Grid>
                    <Grid item>
                        <Button
                            variant="contained"
                            size="large"
                            onClick={() =>handleReport(commentId)}
                            data-testid="report-button"
                        >
                            Report comment
                        </Button>
                    </Grid>
                </Grid>
            </Box>
        </Card>
    );
};

export default CommentReportPopup;