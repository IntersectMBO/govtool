import {
  BUDGET_DISCUSSION_PATHS,
  PATHS,
  PDF_PATHS,
  USER_PATHS,
} from "@/consts/paths";

const WALLET_OPTIONAL_PATHS = [
  PATHS.governanceActions,
  PATHS.dRepDirectory,
  PDF_PATHS.proposalDiscussion,
  BUDGET_DISCUSSION_PATHS.budgetDiscussion,
  USER_PATHS.governanceActionsVotedByMe,
];

/**
 * Whether a page works without a wallet: the same URL then shows the public
 * layout instead of the dashboard's. Intents such as /actions/new and /my/…
 * need one.
 */
export const isWalletOptionalPath = (pathname: string) =>
  pathname === PATHS.home ||
  (pathname !== PATHS.createGovernanceAction &&
    !pathname.startsWith(PDF_PATHS.proposalDiscussionPropose) &&
    WALLET_OPTIONAL_PATHS.some(
      (path) => pathname === path || pathname.startsWith(`${path}/`),
    ));
