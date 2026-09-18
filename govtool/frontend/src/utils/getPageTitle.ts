import { matchPath } from "react-router";

import {
  BUDGET_DISCUSSION_PATHS,
  OUTCOMES_PATHS,
  PATHS,
  PDF_PATHS,
  USER_PATHS,
} from "@/consts/paths";

const PAGE_TITLES = [
  { path: PATHS.home, title: "Home" },
  { path: PATHS.dashboard, title: "Dashboard" },

  { path: PATHS.governanceActions, title: "Governance Actions" },
  {
    path: PATHS.governanceActionsCategory,
    title: "Governance Action Category",
  },
  {
    path: PATHS.governanceActionsCategoryAction,
    title: "Governance Action Details",
  },
  {
    path: PATHS.governanceActionsAction,
    title: "Governance Action Details",
  },

  {
    path: PATHS.dashboardGovernanceActions,
    title: "Governance Actions",
  },
  {
    path: PATHS.dashboardGovernanceActionsCategory,
    title: "Governance Action Category",
  },
  {
    path: PATHS.dashboardGovernanceActionsAction,
    title: "Governance Action Details",
  },

  { path: PATHS.dRepDirectory, title: "DRep Directory" },
  { path: PATHS.dRepDirectoryDRep, title: "DRep Details" },
  { path: PATHS.dashboardDRepDirectory, title: "DRep Directory" },
  { path: PATHS.dashboardDRepDirectoryDRep, title: "DRep Details" },

  {
    path: PATHS.createGovernanceAction,
    title: "Create Governance Action",
  },
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
    title: "Budget Discussion",
  },
  {
    path: `${OUTCOMES_PATHS.governanceActionsOutcomes}/*`,
    title: "Governance Action Outcomes",
  },
  {
    path: USER_PATHS.governanceActionsVotedByMe,
    title: "My Votes and Favorites",
  },

  { path: PATHS.error, title: "Page Not Found" },
];

export const getPageTitle = (pathname: string): string =>
  PAGE_TITLES.find(({ path }) => matchPath({ path, end: true }, pathname))
    ?.title ?? "Page Not Found";
