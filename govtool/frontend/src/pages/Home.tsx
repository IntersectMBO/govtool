import { useEffect } from "react";
import { useNavigate } from "react-router-dom";
import { Box, Link } from "@mui/material";
import { Trans } from "react-i18next";

import { Background, Typography } from "@atoms";
import { PATHS } from "@consts";
import { useCardano } from "@context";
import {
  TopNav,
  Footer,
  UsefulLinks,
  OpenToAny,
  ConnectWalletTo,
  Socials,
  HelpBuildGovTool,
} from "@organisms";
import { WALLET_LS_KEY, getItemFromLocalStorage } from "@utils";
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
        <Box
          maxWidth={1290}
          minHeight="100vh"
          mx="auto"
          mt={screenWidth < 640 ? 2 : 3.125}
          mb={screenWidth < 640 ? 4.5 : 6}
          px={screenWidth < 640 ? 2 : 5}
        >
          <Typography
            sx={{
              whiteSpace: "pre-line",
              fontSize: "18px",
              fontWeight: 500,
              lineHeight: "32px",
            }}
            component="h2"
          >
            <Trans
              i18nKey="hero.description"
              components={[
                <Link
                  key="link-to-docs"
                  data-testid="link-to-docs"
                  href="https://docs.cardano.org/about-cardano/governance-overview"
                  target="_blank"
                  rel="noopener noreferrer"
                  sx={{
                    cursor: "pointer",
                    fontSize: "15px",
                  }}
                />,
              ]}
            />
          </Typography>
          <OpenToAny />
          <ConnectWalletTo />
          <UsefulLinks align="left" />
          <Socials />
          <HelpBuildGovTool />
        </Box>
        {/* FIXME: Footer should be on top of the layout.
        Should not be rerendered across the pages */}
        <Footer />
      </Box>
    </Background>
  );
};
