import { useEffect } from "react";
import { useNavigate } from "react-router-dom";
import { Box } from "@mui/material";

import { Background } from "@atoms";
import { PATHS } from "@consts";
import { useCardano } from "@context";
import {
  TopNav, Hero, Footer, HomeCards,
} from "@organisms";
import { WALLET_LS_KEY, getItemFromLocalStorage } from "@utils";

export function Home() {
  const { isEnabled } = useCardano();
  const navigate = useNavigate();

  useEffect(() => {
    if (isEnabled && getItemFromLocalStorage(`${WALLET_LS_KEY}_stake_key`)) {
      navigate(PATHS.dashboard);
    }
  }, [isEnabled]);

  return (
    <Background>
      <Box overflow="hidden">
        <TopNav />
        <Hero />
        <HomeCards />
        <Footer />
      </Box>
    </Background>
  );
}
