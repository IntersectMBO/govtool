// GovTool look for the poll cards. The shell is GovernanceActionCard's
// (radius 20, the #DDE3F5 shadow, translucent white); the result counts use
// VotePill's yes/no colours (as in VotesSubmitted).

export const pollCardSx = {
    borderRadius: '20px',
    boxShadow: '0px 4px 15px 0px #DDE3F5',
    backgroundColor: 'rgba(255, 255, 255, 0.3)',
};

export const pollCardContentSx = {
    display: 'flex',
    flexDirection: 'column',
    p: 3,
    '&:last-child': { pb: 3 },
};

export const pollTitleSx = {
    fontSize: 18,
    fontWeight: 600,
    lineHeight: '24px',
};

export const pollDividerSx = { my: 2, borderColor: 'lightBlue' };

const votePillColors = {
    yes: { bgcolor: '#F0F9EE', borderColor: '#C0E4BA', bar: '#62BC52' },
    no: { bgcolor: '#FBEBEB', borderColor: '#EDACAC', bar: '#D32F2F' },
};

// The count text sits in a VotePill-shaped chip; the user's own choice is
// shown in primary blue and bold, as before.
export const pollCountSx = (vote, isMine) => ({
    display: 'inline-block',
    py: 0.75,
    px: 2.25,
    border: 1,
    borderRadius: 100,
    bgcolor: votePillColors[vote].bgcolor,
    borderColor: isMine ? 'primaryBlue' : votePillColors[vote].borderColor,
    color: isMine ? 'primaryBlue' : 'textBlack',
    fontSize: 12,
    lineHeight: '16px',
    fontWeight: isMine ? 600 : 400,
    textAlign: 'center',
    whiteSpace: 'nowrap',
    minWidth: '100px',
});

export const pollBarSx = (vote) => ({
    height: '6px',
    width: '100%',
    borderRadius: 100,
    backgroundColor: 'lightBlue',
    '& .MuiLinearProgress-bar': {
        borderRadius: 100,
        backgroundColor: votePillColors[vote].bar,
    },
});
