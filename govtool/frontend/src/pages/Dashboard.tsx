import { useEffect, useRef } from "react";
import { useLocation, Outlet, useNavigate } from "react-router-dom";
import { Box } from "@mui/material";

import { Background, ScrollToManage } from "@atoms";
import {
  BUDGET_DISCUSSION_PATHS,
  CONNECTED_NAV_ITEMS,
  DRAWER_WIDTH,
  OUTCOMES_PATHS,
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
  const { pathname, hash } = useLocation();
  const divRef = useRef<HTMLDivElement>(null);
  const navigate = useNavigate();
  const { t } = useTranslation();

  const getPageTitle = (path: string) => {
    if (path === PATHS.dashboard) return t("dashboard.title");

    if (path.startsWith(OUTCOMES_PATHS.governanceActionsOutcomes)) {
      const outcomesNavItem = findNavItem(
        CONNECTED_NAV_ITEMS,
        OUTCOMES_PATHS.governanceActionsOutcomes,
      );

      return outcomesNavItem ?? "";
    }

    if (path.startsWith(BUDGET_DISCUSSION_PATHS.budgetDiscussion)) {
      const budgetNavItem = findNavItem(
        CONNECTED_NAV_ITEMS,
        BUDGET_DISCUSSION_PATHS.budgetDiscussion,
      );

      return budgetNavItem ?? "";
    }

    if (path.startsWith(PDF_PATHS.proposalDiscussion)) {
      const proposalDiscussionNavItem = findNavItem(
        CONNECTED_NAV_ITEMS,
        PDF_PATHS.proposalDiscussion,
      );

      return proposalDiscussionNavItem ?? "";
    }
    return findNavItem(CONNECTED_NAV_ITEMS, path) ?? "";
  };

  const findNavItem = (items: NavItem[], targetPath: string): string | null =>
    items.reduce<string | null>(
      (result, item) =>
        result ??
        (targetPath === item.navTo
          ? item.label
          : item.childNavItems
          ? findNavItem(item.childNavItems, targetPath)
          : null),
      null,
    );

  useEffect(() => {
    if (divRef.current && pathname !== PATHS.dashboardGovernanceActions) {
      divRef.current.scrollTo({ top: 0 });
    }
  }, [pathname, divRef]);

  useEffect(() => {
    if (!checkIsWalletConnected()) {
      if (window.location.pathname === PATHS.dashboard) {
        navigate(PATHS.home);
      } else {
        navigate(
          window.location.pathname.replace("connected/", "") + hash || "",
        );
      }
    }
  }, [isEnabled, stakeKey]);

  return (
    <Background opacity={0.7}>
      <Box sx={{ display: "flex", position: "relative" }}>
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
