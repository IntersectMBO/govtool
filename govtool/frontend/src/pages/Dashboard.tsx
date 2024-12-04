import { useEffect, useRef } from "react";
import { useLocation, Outlet, useNavigate } from "react-router-dom";
import { Box } from "@mui/material";

import { Background, ScrollToManage } from "@atoms";
import { CONNECTED_NAV_ITEMS, DRAWER_WIDTH, PATHS } from "@consts";
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
    return (
      Object.values(CONNECTED_NAV_ITEMS).find(({ navTo }) =>
        pathname.startsWith(navTo),
      )?.label ?? ""
    );
  };

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
