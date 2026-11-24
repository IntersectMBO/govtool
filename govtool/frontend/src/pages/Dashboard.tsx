import { useEffect } from "react";
import { Outlet, useNavigate } from "react-router";
import { Box } from "@mui/material";

import { Background, ScrollToManage } from "@atoms";
import { PATHS } from "@consts";
import { useCardano } from "@context";
import { Footer, TopNav } from "@organisms";
import { checkIsWalletConnected } from "@utils";

/**
 * The connected layout. Since the redesign it is the same frame as the public
 * pages: the top nav, the page, the footer. The desktop sidebar is gone.
 */
export const Dashboard = () => {
  const { isEnabled, stakeKey } = useCardano();
  const navigate = useNavigate();

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
    <Background>
      <Box
        sx={{
          display: "flex",
          flexDirection: "column",
          minHeight: "100vh",
          overflow: "clip",
          position: "relative",
        }}
      >
        <TopNav />
        <ScrollToManage />
        <Box component="main" sx={{ display: "flex", flex: 1, flexDirection: "column" }}>
          <Outlet />
        </Box>
        <Footer />
      </Box>
    </Background>
  );
};
