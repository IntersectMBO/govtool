import { Box, Grid, IconButton, SwipeableDrawer } from "@mui/material";

import { Background, Link } from "@atoms";
import { CONNECTED_NAV_ITEMS, ICONS } from "@consts";
import { useCardano } from "@context";
import { DRepInfoCard, WalletInfoCard } from "@molecules";
import { useScreenDimension } from "@hooks";
import { openInNewTab } from "@utils";

import { DashboardDrawerMobileProps } from "./types";

const DRAWER_PADDING = 2;
const CALCULATED_DRAWER_PADDING = DRAWER_PADDING * 8 * 2;

export const DashboardDrawerMobile = ({
  isDrawerOpen,
  setIsDrawerOpen,
}: DashboardDrawerMobileProps) => {
  const { screenWidth } = useScreenDimension();
  const { voter } = useCardano();

  const openDrawer = () => {
    setIsDrawerOpen(true);
  };

  const closeDrawer = () => {
    setIsDrawerOpen(false);
  };

  return (
    <SwipeableDrawer
      anchor="right"
      onClose={closeDrawer}
      onOpen={openDrawer}
      open={isDrawerOpen}
    >
      <Background>
        <Box
          sx={{
            flex: 1,
            px: DRAWER_PADDING,
            pb: 3,
            display: "flex",
            flexDirection: "column",
            height: "100%",
          }}
        >
          <Box sx={{ flex: 1 }}>
            <Box
              sx={{
                display: "flex",
                flex: 1,
                justifyContent: "space-between",
                py: 3,
                width: screenWidth - CALCULATED_DRAWER_PADDING,
              }}
            >
              <img src={ICONS.appLogoIcon} height={25} />
              <IconButton
                data-testid="close-drawer-button"
                onClick={closeDrawer}
                sx={{ padding: 0 }}
              >
                <img src={ICONS.closeDrawerIcon} />
              </IconButton>
            </Box>
            <Grid container direction="column" rowGap={4} mt={6}>
              {CONNECTED_NAV_ITEMS.map((navItem) => (
                <Grid item>
                  <Link
                    {...navItem}
                    size="big"
                    onClick={() => {
                      navItem.newTabLink && openInNewTab(navItem.newTabLink);
                      setIsDrawerOpen(false);
                    }}
                    isConnectWallet
                  />
                </Grid>
              ))}
            </Grid>
          </Box>
          {(voter?.isRegisteredAsDRep || voter?.isRegisteredAsSoleVoter) && (
            <DRepInfoCard />
          )}
          <Box py={2} />
          <WalletInfoCard />
        </Box>
      </Background>
    </SwipeableDrawer>
  );
};
