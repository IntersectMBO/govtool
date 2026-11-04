export const PATHS = {
  createGovernanceAction: "/create-action",
  dashboard: "/dashboard",
  dashboardGovernanceActions: "/connected/actions",
  dashboardGovernanceActionsAction: "/connected/actions/:proposalId",
  dashboardGovernanceActionsCategory: "/connected/actions/category/:category",
  dashboardDRepDirectory: "/connected/dreps",
  dashboardDRepDirectoryDRep: "/connected/dreps/:dRepId",
  dRepDirectory: "/dreps",
  dRepDirectoryDRep: "/dreps/:dRepId",
  editDrepMetadata: "/edit-drep",
  error: "/error",
  faqs: "/faqs",
  governanceActions: "/actions",
  governanceActionsAction: "/actions/:proposalId",
  governanceActionsCategory: "/actions/category/:category",
  governanceActionsCategoryAction: "/actions/category/:category/:proposalId",
  guides: "/guides",
  home: "/",
  registerAsdRep: "/register-drep",
  registerAsDirectVoter: "/register-direct-voter",
  retireAsDrep: "/retire-drep",
  retireAsDirectVoter: "/retire-direct-voter",
  stakeKeys: "/stake-keys",
};

export const PDF_PATHS = {
  proposalDiscussion: "/proposals",
  proposalDiscussionProposal: "/proposals/:id",
  proposalDiscussionPropose: "/proposals/propose",
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

// Path segments GovTool used before it moved to short, dashed URLs. Old links
// and bookmarks are redirected by replacing each segment that matches.
export const LEGACY_PATH_SEGMENTS: Record<string, string> = {
  budget_discussion: "budget",
  create_governance_action: "create-action",
  drep_directory: "dreps",
  edit_drep: "edit-drep",
  governance_actions: "actions",
  proposal_comment_review: "comment-review",
  proposal_discussion: "proposals",
  register_direct_voter: "register-direct-voter",
  register_drep: "register-drep",
  retire_direct_voter: "retire-direct-voter",
  retire_drep: "retire-drep",
  stake_keys: "stake-keys",
  votes_and_favorites: "votes",
};
