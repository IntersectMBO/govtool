// URLs name resources (/dreps/:dRepId, /actions/:proposalId with a CIP-129
// gov_action1… id) and serve the same page with or without a wallet; App.tsx
// picks the layout. Intents are /new, or under /my when they act on the
// connected wallet's own role.
export const PATHS = {
  createGovernanceAction: "/actions/new",
  dashboard: "/dashboard",
  dRepDirectory: "/dreps",
  dRepDirectoryDRep: "/dreps/:dRepId",
  editDrepMetadata: "/my/drep/edit",
  error: "/error",
  faqs: "/faqs",
  governanceActions: "/actions",
  governanceActionsAction: "/actions/:proposalId",
  guides: "/guides",
  home: "/",
  registerAsdRep: "/my/drep/register",
  registerAsDirectVoter: "/my/direct-voter/register",
  retireAsDrep: "/my/drep/retire",
  retireAsDirectVoter: "/my/direct-voter/retire",
  stakeKeys: "/my/stake-key",
};

// Filters the governance actions list by type: /actions?type=InfoAction.
export const GOVERNANCE_ACTION_TYPE_PARAM = "type";

export const PDF_PATHS = {
  proposalDiscussion: "/proposals",
  proposalDiscussionProposal: "/proposals/:id",
  proposalDiscussionPropose: "/proposals/new",
};

// The read-only 2025 budget proposals archive; /propose redirects to the
// list.
export const BUDGET_DISCUSSION_PATHS = {
  budgetDiscussion: "/budget",
  budgetDiscussionProposal: "/budget/:id",
  budgetDiscussionPropose: "/budget/propose",
  budgetDiscussionCategory: "/budget/category/:category",
  budgetDiscussionCategoryAction: "/budget/category/:category/:proposalId",
};

export const USER_PATHS = {
  governanceActionsVotedByMe: "/my/votes",
};

export const GOV_ACTION_HISTORY_PATHS = {
  governanceActionHistory: "/actions/history",
  governanceActionHistoryDetail: "/actions/history/:id",
};

// Path segments GovTool used before its URLs named resources. Old links and
// bookmarks are redirected by replacing each segment that matches; a value
// may stand for several segments.
export const LEGACY_PATH_SEGMENTS: Record<string, string> = {
  budget_discussion: "budget",
  create_governance_action: "actions/new",
  drep_directory: "dreps",
  edit_drep: "my/drep/edit",
  governance_actions: "actions",
  proposal_comment_review: "comment-review",
  proposal_discussion: "proposals",
  register_direct_voter: "my/direct-voter/register",
  register_drep: "my/drep/register",
  retire_direct_voter: "my/direct-voter/retire",
  retire_drep: "my/drep/retire",
  stake_keys: "my/stake-key",
  votes_and_favorites: "votes",
};
