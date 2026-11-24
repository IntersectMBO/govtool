/**
 * Fixed wording of the SEO tags. The frontend's en.json cannot be read from
 * the backend, so the page titles here are kept in step with the frontend's
 * utils/getPageTitle.ts by hand.
 */

export const SITE_NAME = 'Cardano GovTool';
export const TITLE_SUFFIX = ` | ${SITE_NAME}`;

export const DEFAULT_TITLE = SITE_NAME;
export const HOME_TITLE = `${SITE_NAME}: vote, delegate and follow Cardano governance`;
export const SITE_DESCRIPTION =
  'Take part in Cardano on-chain governance: read and vote on governance actions, delegate to a DRep, or become one.';

export const HOME_CRUMB = 'Home';

export const LIST_TITLES = {
  actionList: 'Governance actions',
  actionHistoryList: 'Governance action history',
  drepList: 'DRep directory',
  forumList: 'Proposal discussion',
  budget: 'Archived: 2025 budget proposals',
  guides: 'Guides',
  faqs: 'FAQs',
} as const;

export const FIXED_DESCRIPTIONS = {
  actionHistoryList:
    'Every past Cardano governance action, with its outcome and how DReps, stake pools and the Constitutional Committee voted.',
  forumList:
    'Discuss Cardano governance action proposals with the community before they are submitted on chain.',
  budget:
    'The archive of the 2025 Cardano budget proposals and their community discussion.',
  guides:
    'Guides to Cardano governance: delegating your voting power, becoming a DRep and voting on governance actions.',
  faqs: 'Answers to common questions about Cardano governance and GovTool.',
} as const;

export const actionListDescription = (open: number | undefined): string => {
  if (open === undefined) return SITE_DESCRIPTION;
  if (open === 0) {
    return 'No governance actions open right now. See past actions and outcomes.';
  }
  if (open === 1) return '1 governance action open for voting on Cardano.';
  return `${open} governance actions open for voting on Cardano.`;
};

export const drepListDescription = (listed: number | undefined): string => {
  if (listed === undefined || listed === 0) {
    return 'Find and delegate to a DRep on Cardano.';
  }
  if (listed === 1) return 'Find and delegate to the 1 DRep on Cardano.';
  return `Find and delegate to one of ${listed} DReps on Cardano.`;
};

/** Legacy type names, as the frontend shows them. */
export const ACTION_TYPE_NAMES: Record<string, string> = {
  InfoAction: 'Info action',
  TreasuryWithdrawals: 'Treasury withdrawal',
  ParameterChange: 'Protocol parameter change',
  HardForkInitiation: 'Hard fork',
  NoConfidence: 'No confidence',
  NewCommittee: 'Committee update',
  NewConstitution: 'New constitution',
};

export const DREP_SUFFIX = ' · Cardano DRep';
export const FORUM_CARD_LABEL = 'Proposal discussion';
