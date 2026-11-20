import { useEffect, useRef } from "react";
import { useLocation, Outlet, useNavigate } from "react-router";
import { Box } from "@mui/material";

import { Background, ScrollToManage } from "@atoms";
import {
  BUDGET_DISCUSSION_PATHS,
  CONNECTED_NAV_ITEMS,
  DRAWER_WIDTH,
  GOV_ACTION_HISTORY_PATHS,
  PATHS,
  PDF_PATHS,
} from "@consts";
import { useCardano } from "@context";
import { useScreenDimension, useTranslation } from "@hooks";
import { DashboardTopNav, Drawer, Footer } from "@organisms";
import { checkIsWalletConnected } from "@utils";

export const Dashboard = () => {
  const { isEnabled, stakeKey } = useCardano();
  const { isMobile } = useScreenDimension();
  const { pathname } = useLocation();
  const divRef = useRef<HTMLDivElement>(null);
  const navigate = useNavigate();
  const { t } = useTranslation();

  const getPageTitle = (path: string) => {
    if (path === PATHS.dashboard) return t("dashboard.title");

    // Before /actions, which is a prefix of the history path.
    if (path.startsWith(GOV_ACTION_HISTORY_PATHS.governanceActionHistory)) {
      return t("govActions.history.title");
    }

    // Linked from Useful links, not the sidebar.
    if (path.startsWith(BUDGET_DISCUSSION_PATHS.budgetDiscussion)) {
      return t("budgetProposalsArchive.navLabel");
    }

    if (path.startsWith(PDF_PATHS.proposalDiscussion)) {
      const proposalDiscussionNavItem = findNavItem(
        CONNECTED_NAV_ITEMS,
        PDF_PATHS.proposalDiscussion,
      );

      return proposalDiscussionNavItem ?? "";
    }

    if (path.startsWith(PATHS.governanceActions)) {
      return t("govActions.title");
    }

    return findNavItem(CONNECTED_NAV_ITEMS, path) ?? "";
  };

  const findNavItem = (items: NavItem[], targetPath: string): string | null =>
    items.reduce<string | null>(
      (result, item) =>
        result ??
        (targetPath === item.navTo || targetPath.startsWith(`${item.navTo}/`)
          ? item.label
          : item.childNavItems
          ? findNavItem(item.childNavItems, targetPath)
          : null),
      null,
    );

  useEffect(() => {
    if (divRef.current && pathname !== PATHS.governanceActions) {
      divRef.current.scrollTo({ top: 0 });
    }
  }, [pathname, divRef]);

  // Pages other than /dashboard switch to their public layout on their own
  // when the wallet goes; the dashboard has none.
  useEffect(() => {
    if (
      !checkIsWalletConnected() &&
      window.location.pathname === PATHS.dashboard
    ) {
      navigate(PATHS.home);
    }
  }, [isEnabled, stakeKey]);

  return (
    <Background opacity={0.7}>
      <Box
        sx={{
          display: "flex",
          position: "relative",
        }}
      >
        {isMobile ? null : <Drawer />}
        <Box
          sx={{
            display: "flex",
            flex: 1,
            flexDirection: "column",
            minHeight: "100vh",
            width: `calc(100vw - ${DRAWER_WIDTH}px)`,
            overflow: "clip",
            position: "relative",
          }}
        >
          <DashboardTopNav title={getPageTitle(window.location.pathname)} />
          <ScrollToManage />
          <Outlet />
          {/* FIXME: Footer should be on top of the layout.
          Should not be rerendered across the pages */}
          <Footer />
        </Box>
      </Box>
    </Background>
  );
};
