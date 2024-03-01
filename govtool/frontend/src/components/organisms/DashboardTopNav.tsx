import { useState } from "react";
import { useNavigate } from "react-router-dom";
import {
  Box,
  BoxProps,
  Grid,
  IconButton,
  SwipeableDrawer,
} from "@mui/material";

import { Background, Link, VotingPowerChips, Typography } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";
import { ICONS, PATHS } from "@consts";
import { useCardano } from "@context";
import { DRepInfoCard, WalletInfoCard } from "@molecules";
import { openInNewTab } from "@utils";

export type DashboardTopNavProps = {
  imageSRC?: string;
  imageWidth?: number;
  imageHeight?: number;
  title: string;
  isDrawer?: boolean;
  isVotingPowerHidden?: boolean;
  sx?: BoxProps["sx"];
};

const DRAWER_PADDING = 2;
const CALCULATED_DRAWER_PADDING = DRAWER_PADDING * 8 * 2;

export const DashboardTopNav = ({
  title,
  imageSRC,
  imageWidth,
  imageHeight,
  isVotingPowerHidden,
  sx,
}: DashboardTopNavProps) => {
  const { isMobile, screenWidth } = useScreenDimension();
  const { voter } = useCardano();
  const navigate = useNavigate();
  const [isDrawerOpen, setIsDrawerOpen] = useState<boolean>(false);
  const { t } = useTranslation();

  return (
    <Box
      px={isMobile ? 2 : 5}
      py={3}
      display="flex"
      bgcolor={isMobile ? "#FBFBFF" : undefined}
      sx={{
        backdropFilter: "blur(10px)",
        ...sx,
      }}
      alignItems="center"
      justifyContent="space-between"
      borderBottom={1}
      borderColor="#D6E2FF"
      position="fixed"
      zIndex={100}
      flex={1}
      width="fill-available"
      height="48px"
    >
      <Box display="flex">
        {imageSRC ? (
          <Box
            display="flex"
            mr={6}
            alignItems="center"
            onClick={() => {
              navigate(PATHS.dashboard);
            }}
          >
            <img src={imageSRC} width={imageWidth} height={imageHeight} />
          </Box>
        ) : null}
        {!isMobile && title ? (
          <Typography variant="headline5">{title}</Typography>
        ) : null}
      </Box>
      <Box display="flex">
        {!isVotingPowerHidden && <VotingPowerChips />}
        {isMobile && (
          <IconButton
            data-testid="open-drawer-button"
            sx={{ padding: 0, marginLeft: 1 }}
            onClick={() => setIsDrawerOpen(true)}
          >
            <img src={ICONS.drawerIcon} />
          </IconButton>
        )}
      </Box>
      {isMobile && (
        <SwipeableDrawer
          anchor="right"
          open={isDrawerOpen}
          onClose={() => setIsDrawerOpen(false)}
          onOpen={() => setIsDrawerOpen(true)}
        >
          <Background>
            <Box
              flex={1}
              px={DRAWER_PADDING}
              pb={3}
              display="flex"
              flexDirection="column"
              height="100%"
            >
              <Box flex={1}>
                <Box
                  width={screenWidth - CALCULATED_DRAWER_PADDING}
                  display="flex"
                  flex={1}
                  py={3}
                  justifyContent="space-between"
                >
                  <img src={ICONS.appLogoIcon} height={25} />
                  <IconButton
                    data-testid="close-drawer-button"
                    sx={{ padding: 0 }}
                    onClick={() => setIsDrawerOpen(false)}
                  >
                    <img src={ICONS.closeDrawerIcon} />
                  </IconButton>
                </Box>
                <Grid container direction="column" rowGap={4} mt={6}>
                  <Grid item>
                    <Link
                      dataTestId="home-link"
                      navTo={PATHS.dashboard}
                      label={t("menu.myDashboard")}
                      size="big"
                      onClick={() => {
                        setIsDrawerOpen(false);
                      }}
                      isConnectWallet
                    />
                  </Grid>
                  <Grid item>
                    <Link
                      dataTestId="governance-actions-link"
                      navTo={PATHS.dashboard_governance_actions}
                      label={t("menu.viewGovActions")}
                      size="big"
                      onClick={() => {
                        setIsDrawerOpen(false);
                      }}
                      isConnectWallet
                    />
                  </Grid>
                  <Grid item>
                    <Link
                      dataTestId="guides-link"
                      navTo=""
                      label={t("menu.guides")}
                      size="big"
                      onClick={() => {
                        openInNewTab(
                          "https://docs.sanchogov.tools/about/what-is-sanchonet-govtool"
                        );
                        setIsDrawerOpen(false);
                      }}
                      isConnectWallet
                    />
                  </Grid>
                  <Grid item>
                    <Link
                      dataTestId="faqs-link"
                      navTo=""
                      label={t("menu.faqs")}
                      size="big"
                      onClick={() => {
                        openInNewTab("https://docs.sanchogov.tools/faqs");
                        setIsDrawerOpen(false);
                      }}
                      isConnectWallet
                    />
                  </Grid>
                </Grid>
              </Box>
              {(voter?.isRegisteredAsDRep ||
                voter?.isRegisteredAsSoleVoter) && <DRepInfoCard />}
              <Box py={2} />
              <WalletInfoCard />
            </Box>
          </Background>
        </SwipeableDrawer>
      )}
    </Box>
  );
};
