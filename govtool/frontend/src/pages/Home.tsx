import { useEffect } from "react";
import { useNavigate } from "react-router-dom";
import { Box } from "@mui/material";

import { Background } from "@atoms";
import { PATHS } from "@consts";
import { useCardano } from "@context";
import { TopNav, Hero, Footer, HomeCards } from "@organisms";
import { WALLET_LS_KEY, getItemFromLocalStorage } from "@utils";
import { UsefulLinks } from "@/components/organisms/UsefulLinks";
import { useScreenDimension } from "@/hooks";

export const Home = () => {
  const { isEnabled } = useCardano();
  const navigate = useNavigate();
  const { screenWidth } = useScreenDimension();

  useEffect(() => {
    if (isEnabled && getItemFromLocalStorage(`${WALLET_LS_KEY}_stake_key`)) {
      navigate(PATHS.dashboard);
    }
  }, [isEnabled]);

  return (
    <Background>
      <TopNav />
      <Box overflow="hidden">
        <Hero />
        <Box
          maxWidth={screenWidth < 2560 ? 866 : 1770}
          mx="auto"
          mt={screenWidth < 640 ? 10 : 14.5}
          mb={screenWidth < 640 ? 4.5 : 6}
          px={screenWidth < 640 ? 2 : 5}
          display="flex"
          flexDirection="column"
          gap={10}
        >
          <HomeCards />
          <UsefulLinks align="center" />
        </Box>
        {/* FIXME: Footer should be on top of the layout.
        Should not be rerendered across the pages */}
        <Footer />
      </Box>
    </Background>
  );
};
