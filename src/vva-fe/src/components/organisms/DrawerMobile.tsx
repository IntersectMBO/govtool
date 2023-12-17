import { Dispatch, SetStateAction } from "react";
import { Box, Grid, IconButton, SwipeableDrawer } from "@mui/material";

import { Background, Button, Link } from "../atoms";
import { ICONS, IMAGES, NAV_ITEMS } from "@consts";
import { useScreenDimension } from "@hooks";
import { useModal } from "@context";
import { openInNewTab } from "@utils";

type DrawerMobileProps = {
  isConnectButton: boolean;
  isDrawerOpen: boolean;
  setIsDrawerOpen: Dispatch<SetStateAction<boolean>>;
};

const DRAWER_PADDING = 2;
const CALCULATED_DRAWER_PADDING = DRAWER_PADDING * 8 * 2;

export const DrawerMobile = ({
  isConnectButton,
  isDrawerOpen,
  setIsDrawerOpen,
}: DrawerMobileProps) => {
  const { screenWidth } = useScreenDimension();
  const { openModal } = useModal();

  return (
    <SwipeableDrawer
      anchor="right"
      onClose={() => setIsDrawerOpen(false)}
      onOpen={() => setIsDrawerOpen(true)}
      open={isDrawerOpen}
    >
      <Background>
        <Box px={DRAWER_PADDING}>
          <Box
            display="flex"
            flex={1}
            justifyContent="space-between"
            py={3}
            width={screenWidth - CALCULATED_DRAWER_PADDING}
          >
            <img height={25} src={IMAGES.appLogo} />
            <IconButton
              sx={{ padding: 0 }}
              onClick={() => setIsDrawerOpen(false)}
            >
              <img src={ICONS.closeDrawerIcon} />
            </IconButton>
          </Box>
          {isConnectButton ? (
            <Button
              data-testid="connect-wallet-button"
              onClick={() => {
                openModal({ type: "chooseWallet" });
              }}
              size="extraLarge"
              sx={{
                marginTop: 1,
                width: "100%",
              }}
              variant="contained"
            >
              Connect your wallet
            </Button>
          ) : null}
          <Grid container direction="column" mt={6} rowGap={4}>
            {NAV_ITEMS.map((navItem) => (
              <Grid item key={navItem.label}>
                <Link
                  {...navItem}
                  isConnectWallet={isConnectButton}
                  onClick={() => {
                    if (navItem.newTabLink) openInNewTab(navItem.newTabLink);
                    setIsDrawerOpen(false);
                  }}
                  size="big"
                />
              </Grid>
            ))}
          </Grid>
        </Box>
      </Background>
    </SwipeableDrawer>
  );
};
