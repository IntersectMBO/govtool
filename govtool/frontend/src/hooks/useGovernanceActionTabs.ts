import { useLocation } from "react-router";
import { useTranslation } from "react-i18next";

import { CONNECTED_NAV_ITEMS, NAV_ITEMS } from "@/consts/navItems";
import { GOV_ACTION_HISTORY_PATHS, PATHS, PDF_PATHS } from "@/consts/paths";

export type PageTab = {
  label: string;
  to: string;
  dataTestId: string;
  isActive: boolean;
};

type NavEntry = { dataTestId: string; childNavItems?: { dataTestId: string }[] };

// The three Governance Actions tabs reuse the test ids of the nav dropdown
// they replace: the public nav's children without a wallet, the connected
// nav's with one.
const childIds = (items: NavEntry[], parentId: string) =>
  items
    .find((item) => item.dataTestId === parentId)
    ?.childNavItems?.map((child) => child.dataTestId) ?? [];

export const useGovernanceActionTabs = (isConnected: boolean): PageTab[] => {
  const { t } = useTranslation();
  const { pathname } = useLocation();

  const [proposedId, liveId, outcomesId] = isConnected
    ? childIds(CONNECTED_NAV_ITEMS as NavEntry[], "governance-actions-link")
    : childIds(NAV_ITEMS as NavEntry[], "governance-actions");

  const isOutcomes = pathname.startsWith(
    GOV_ACTION_HISTORY_PATHS.governanceActionHistory,
  );
  const isLive =
    !isOutcomes &&
    pathname.startsWith(PATHS.governanceActions);

  return [
    {
      label: t("govActions.proposedTab"),
      to: PDF_PATHS.proposalDiscussion,
      dataTestId: proposedId,
      isActive: pathname.startsWith(PDF_PATHS.proposalDiscussion),
    },
    {
      label: t("govActions.liveVotingTab"),
      to: PATHS.governanceActions,
      dataTestId: liveId,
      isActive: isLive,
    },
    {
      label: t("govActions.outcomesTab"),
      to: GOV_ACTION_HISTORY_PATHS.governanceActionHistory,
      dataTestId: outcomesId,
      isActive: isOutcomes,
    },
  ];
};
