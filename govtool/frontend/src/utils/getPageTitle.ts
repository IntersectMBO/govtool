import { matchPath } from "react-router";

import {
  BUDGET_DISCUSSION_PATHS,
  GOVERNANCE_ACTION_TYPE_PARAM,
  GOV_ACTION_HISTORY_PATHS,
  PATHS,
  PDF_PATHS,
  USER_PATHS,
} from "@/consts/paths";

const PAGE_TITLES = [
  { path: PATHS.home, title: "Home" },
  { path: PATHS.dashboard, title: "Dashboard" },

  // Before the action paths: "/actions/:proposalId" also matches the
  // history list and the new action form, taking "history" or "new" for an
  // action id.
  {
    path: `${GOV_ACTION_HISTORY_PATHS.governanceActionHistory}/*`,
    title: "Historical Actions",
  },
  {
    path: PATHS.createGovernanceAction,
    title: "Create Governance Action",
  },
  { path: PATHS.governanceActions, title: "Governance Actions" },
  {
    path: PATHS.governanceActionsAction,
    title: "Governance Action Details",
  },

  { path: PATHS.dRepDirectory, title: "DRep Directory" },
  { path: PATHS.dRepDirectoryDRep, title: "DRep Details" },

  { path: PATHS.registerAsdRep, title: "Become a DRep" },
  { path: PATHS.retireAsDrep, title: "Retire as a DRep" },
  {
    path: PATHS.registerAsDirectVoter,
    title: "Become a Direct Voter",
  },
  {
    path: PATHS.retireAsDirectVoter,
    title: "Retire as a Direct Voter",
  },
  { path: PATHS.stakeKeys, title: "Select Stake Key" },
  { path: PATHS.editDrepMetadata, title: "Edit DRep Info" },

  {
    path: `${PDF_PATHS.proposalDiscussion}/*`,
    title: "Proposal Discussion",
  },
  {
    path: `${BUDGET_DISCUSSION_PATHS.budgetDiscussion}/*`,
    title: "Archived: 2025 Budget Proposals",
  },
  {
    path: USER_PATHS.governanceActionsVotedByMe,
    title: "My Votes and Favorites",
  },

  { path: PATHS.error, title: "Page Not Found" },
];

export const getPageTitle = (pathname: string, search = ""): string => {
  if (
    pathname === PATHS.governanceActions &&
    new URLSearchParams(search).has(GOVERNANCE_ACTION_TYPE_PARAM)
  ) {
    return "Governance Action Category";
  }
  return (
    PAGE_TITLES.find(({ path }) => matchPath({ path, end: true }, pathname))
      ?.title ?? "Page Not Found"
  );
};
