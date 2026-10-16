import { Box, Card, CardContent, Divider, LinearProgress } from '@mui/material';
import { Typography } from '@atoms';
import {
    pollBarSx,
    pollCardContentSx,
    pollCardSx,
    pollCountSx,
    pollDividerSx,
    pollTitleSx,
} from '../Poll/pollStyles';

// The final totals of an archived budget proposal's DRep poll. The archive
// holds totals only, not who voted.
const BudgetDiscussionPoll = ({ poll }) => {
    if (!poll) return null;

    const yes = +poll?.attributes?.poll_yes || 0;
    const no = +poll?.attributes?.poll_no || 0;
    const total = yes + no;
    const percentage = (count) =>
        total > 0 ? Math.round((count / total) * 100) : 0;

    const row = (vote, label, count) => (
        <Box mt={2}>
            <Box
                display={'flex'}
                alignItems={'center'}
                justifyContent={'space-between'}
                gap={1}
                mb={1}
            >
                <Typography
                    variant='body1'
                    sx={{
                        ...pollCountSx(vote, false),
                        fontSize: 14,
                        lineHeight: '20px',
                    }}
                    data-testid={`poll-${vote}-count`}
                >
                    {`${label}: ${count} (${percentage(count)}%)`}
                </Typography>
            </Box>
            <LinearProgress
                variant='determinate'
                value={percentage(count)}
                sx={pollBarSx(vote)}
            />
        </Box>
    );

    return (
        <Card sx={pollCardSx} data-testid='poll-result-card'>
            <CardContent sx={pollCardContentSx}>
                <Typography sx={pollTitleSx}>
                    Should this proposal be included in the next Cardano
                    Budget?
                </Typography>
                <Divider variant='fullWidth' sx={pollDividerSx} />
                <Typography
                    variant='caption'
                    component='span'
                    sx={{ color: 'neutralGray', fontWeight: 500 }}
                    data-testid='poll-total-votes'
                >
                    Total votes: {total}
                </Typography>
                {row('yes', 'Yes', yes)}
                {row('no', 'No', no)}
            </CardContent>
        </Card>
    );
};

export default BudgetDiscussionPoll;
