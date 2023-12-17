import { useEffect } from "react";
import { useNavigate } from "react-router-dom";

import { Background } from "@atoms";
import { Box } from "@mui/material";
import { TopNav, Hero, Footer, HomeCards } from "@organisms";
import { useCardano } from "@context";
import { PATHS } from "@/consts";
import { WALLET_LS_KEY, getItemFromLocalStorage } from "@/utils/localStorage";

export const Home = () => {
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
};
