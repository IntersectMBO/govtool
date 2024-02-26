import { Box } from "@mui/material";
import { useEffect, useRef } from "react";
import { useLocation, Outlet, useNavigate } from "react-router-dom";

import { CONNECTED_NAV_ITEMS, ICONS, PATHS } from "@consts";
import { useCardano } from "@context";
import { Background, ScrollToManage } from "@atoms";
import { useScreenDimension } from "@hooks";
import { DashboardTopNav, Drawer, Footer } from "@organisms";
import { checkIsWalletConnected } from "@/utils";

const getPageTitle = (pathname: string) => {
  if (pathname === PATHS.dashboard) {
    return "My Dashboard";
  }
  return Object.values(CONNECTED_NAV_ITEMS).find(({ navTo }) => navTo === pathname)?.label ?? "";
};

export const Dashboard = () => {
  const { isEnabled, stakeKey } = useCardano();
  const { isMobile } = useScreenDimension();
  const { pathname, hash } = useLocation();
  const divRef = useRef<HTMLDivElement>(null);
  const navigate = useNavigate();

  useEffect(() => {
    if (divRef.current) {
      pathname !== PATHS.dashboard_governance_actions &&
        divRef.current.scrollTo({ top: 0 });
    }
  }, [pathname, divRef]);

  useEffect(() => {
    if (checkIsWalletConnected()) {
      if (window.location.pathname === PATHS.dashboard) {
        navigate(PATHS.home);
      } else {
        navigate(
          window.location.pathname.replace("connected/", "") + hash ?? ""
        );
      }
    }
  }, [isEnabled, stakeKey]);

  return (
    <Background opacity={0.7}>
      <Box display={"flex"} flexDirection={"row"} position={"relative"}>
        {isMobile ? null : <Drawer />}
        <Box
          display="flex"
          flexDirection="column"
          flex={1}
          minHeight="100vh"
          overflow="hidden"
        >
          <DashboardTopNav
            title={getPageTitle(location.pathname)}
            imageSRC={isMobile ? ICONS.appLogoIcon : undefined}
            imageHeight={24}
          />
          <Box
            sx={{
              display: "flex",
              flexDirection: "column",
              flex: "1",
              overflowX: "hidden",
              overflowY: "auto",
              position: "relative",
              marginTop: "97px",
            }}
            ref={divRef}
          >
            <ScrollToManage />
            <Outlet />
          </Box>
          {isMobile ? <Footer /> : null}
        </Box>
      </Box>
    </Background>
  );
};
