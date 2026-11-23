import { useEffect } from "react";
import { useNavigate } from "react-router";
import { Box } from "@mui/material";

import { PATHS } from "@consts";
import { useCardano } from "@context";
import {
  CircleDivider,
  DRepBanner,
  Footer,
  HomeDRepLeaderboard,
  HomeFaq,
  HomeHero,
  HomeNews,
  HomeOutcomes,
  HomePulse,
  TopNav,
} from "@organisms";
import { WALLET_LS_KEY, getItemFromLocalStorage } from "@utils";

export const Home = () => {
  const { isEnabled } = useCardano();
  const navigate = useNavigate();

  useEffect(() => {
    if (isEnabled && getItemFromLocalStorage(`${WALLET_LS_KEY}_stake_key`)) {
      navigate(PATHS.dashboard);
    }
  }, [isEnabled]);

  return (
    <Box sx={{ backgroundColor: "#FFFFFF", minHeight: "100vh", overflowX: "clip" }}>
      <TopNav />
      <Box component="main">
        <HomeHero />
        <CircleDivider />
        <HomePulse />
        <HomeDRepLeaderboard />
        <HomeOutcomes />
        <HomeNews />
        <HomeFaq />
        <Box sx={{ pb: { xxs: 6, md: 10 } }}>
          <DRepBanner />
        </Box>
      </Box>
      <Footer />
    </Box>
  );
};
