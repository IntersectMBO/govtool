// Shared look for the pdf-ui list cards (ProposalCard, BudgetDiscussionCard),
// copied from GovTool's GovernanceActionCard and its elements
// (GovernanceActionCardHeader, GovernanceActionCardElement,
// GovernanceActionsDatesBox) and the Share molecule.

import { primaryBlue, successGreen } from '@/consts/colors';

// GovernanceActionCard shell.
export const cardShellSx = {
    position: 'relative',
    display: 'flex',
    flexDirection: 'column',
    justifyContent: 'space-between',
    width: '100%',
    height: '100%',
    minHeight: '400px',
    boxShadow: '0px 4px 15px 0px #DDE3F5',
    borderRadius: '20px',
    backgroundColor: 'rgba(255, 255, 255, 0.3)',
};

// GovernanceActionCard body padding.
export const cardBodySx = {
    display: 'flex',
    flexDirection: 'column',
    flexGrow: 1,
    padding: '40px 24px 0',
};

// GovernanceActionCard white footer holding the full-width button.
export const cardFooterSx = {
    boxShadow: '0px 4px 15px 0px #DDE3F5',
    borderBottomLeftRadius: 20,
    borderBottomRightRadius: 20,
    padding: 3,
    bgcolor: 'neutralWhite',
};

// GovernanceActionCardHeader title.
export const cardTitleSx = {
    fontSize: 18,
    fontWeight: 600,
    lineHeight: '24px',
    display: '-webkit-box',
    WebkitBoxOrient: 'vertical',
    WebkitLineClamp: 2,
    lineClamp: 2,
    overflow: 'hidden',
    textOverflow: 'ellipsis',
    wordBreak: 'break-word',
};

// GovernanceActionCardElement (slider card) label and value.
export const cardElementSx = { mb: '20px' };

export const cardLabelSx = {
    fontSize: 12,
    fontWeight: 500,
    lineHeight: '16px',
    color: 'neutralGray',
    mb: '4px',
    overflow: 'hidden',
    textOverflow: 'ellipsis',
    whiteSpace: 'nowrap',
};

export const cardValueSx = {
    fontSize: 14,
    fontWeight: 400,
    lineHeight: '20px',
    color: 'textBlack',
};

// GovernanceActionCardElement "pill" value (the type chip).
export const cardPillSx = {
    display: 'inline-flex',
    maxWidth: '100%',
    padding: '6px 18px',
    overflow: 'hidden',
    bgcolor: 'lightBlue',
    borderRadius: 100,
};

export const cardPillTextSx = {
    fontSize: 12,
    fontWeight: 400,
    lineHeight: '16px',
    overflow: 'hidden',
    textOverflow: 'ellipsis',
    whiteSpace: 'nowrap',
};

// GovernanceActionsDatesBox (one row).
export const cardDatesBoxSx = {
    border: 1,
    borderColor: 'lightBlue',
    borderRadius: 3,
    display: 'flex',
    overflow: 'hidden',
    mb: '20px',
};

export const cardDatesRowSx = {
    alignItems: 'center',
    bgcolor: `${primaryBlue.c100}80`,
    display: 'flex',
    flex: 1,
    justifyContent: 'center',
    gap: 0.5,
    py: '6px',
    px: 1,
    width: '100%',
};

export const cardDatesTextSx = {
    fontSize: 12,
    fontWeight: 300,
    lineHeight: '16px',
};

export const cardInfoIconSx = { fontSize: '19px', color: '#ADAEAD' };

// Status chip placed like the Card molecule label and the
// GovernanceActionCardStatePill: over the top-right edge of the card.
export const statusChipSx = {
    position: 'absolute',
    top: -14,
    right: 30,
    zIndex: 1,
    height: 28,
    px: 0.5,
    fontSize: 12,
    fontWeight: 500,
    border: 1,
};

export const statusChipColors = {
    draft: {
        bgcolor: 'lightBlue',
        color: 'textBlack',
        borderColor: primaryBlue.c200,
    },
    active: {
        bgcolor: successGreen.c100,
        color: successGreen.c700,
        borderColor: successGreen.c500,
    },
    submitted: {
        bgcolor: primaryBlue.c100,
        color: primaryBlue.c500,
        borderColor: primaryBlue.c500,
    },
};

// Share molecule trigger.
export const shareButtonSx = (open) => (theme) => ({
    width: 40,
    height: 40,
    padding: 1,
    borderRadius: 50,
    bgcolor: open ? '#F7F9FB' : 'transparent',
    boxShadow: open ? theme.shadows[1] : 'none',
    transition: 'all 0.3s',
    '&:hover': {
        boxShadow: theme.shadows[1],
        bgcolor: '#F7F9FB',
    },
});

// Share molecule popover content.
export const sharePaperSx = {
    mt: 1,
    borderRadius: '12px',
};

export const shareContentSx = {
    alignItems: 'center',
    boxSizing: 'border-box',
    display: 'flex',
    flexDirection: 'column',
    justifyContent: 'center',
    padding: '12px 24px',
    width: 148,
};

export const shareCopyButtonSx = (active) => ({
    width: 48,
    height: 48,
    mt: 1.5,
    mb: 1,
    borderRadius: 50,
    bgcolor: active ? 'lightBlue' : 'neutralWhite',
    boxShadow: (theme) => theme.shadows[1],
    '&:hover': { bgcolor: active ? 'lightBlue' : 'neutralWhite' },
    '&.Mui-disabled': { bgcolor: 'neutralWhite' },
});

// Comment counter next to the chat icon.
export const commentCountSx = {
    fontSize: 14,
    fontWeight: 500,
    lineHeight: '20px',
    color: 'textBlack',
    ml: 0.5,
};
