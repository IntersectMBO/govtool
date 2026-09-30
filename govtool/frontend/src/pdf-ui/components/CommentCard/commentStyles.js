// GovTool look for the comment thread. The card follows the DRep directory
// card (Card molecule: shadows[3], radius 12, translucent white); the reply
// box follows VoteActionForm (the text area, then the action on its own row).

export const readMoreLinkSx = {
    cursor: 'pointer',
    fontSize: 14,
    fontWeight: 500,
    lineHeight: '20px',
    color: 'primaryBlue',
};

// UsernameSection: the DRep tag looks like StatusPill's "Yourself" chip.
export const dRepTagSx = {
    height: 24,
    px: 0.75,
    py: 0.5,
    fontSize: '0.75rem',
    fontWeight: 500,
    bgcolor: 'lightBlue',
    color: 'textBlack',
};

export const dRepLinkSx = {
    color: 'primaryBlue',
    '&:hover': { textDecoration: 'underline' },
};
