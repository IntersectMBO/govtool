import { Box, Grid, IconButton, SwipeableDrawer } from "@mui/material";

import { Background, Button, Link, Typography } from "@atoms";
import { ICONS, IMAGES, NAV_ITEMS } from "@consts";
import { useScreenDimension, useTranslation } from "@hooks";
import { useFeatureFlag, useModal } from "@context";
import { openInNewTab } from "@utils";

import { DrawerMobileProps } from "./types";

const DRAWER_PADDING = 2;
const CALCULATED_DRAWER_PADDING = DRAWER_PADDING * 8 * 2;

export const DrawerMobile = ({
  isConnectButton,
  isDrawerOpen,
  setIsDrawerOpen,
}: DrawerMobileProps) => {
  const { isProposalDiscussionForumEnabled } = useFeatureFlag();
  const { screenWidth } = useScreenDimension();
  const { openModal } = useModal();
  const { t } = useTranslation();

  const onClickHelp = () =>
    openInNewTab("https://docs.gov.tools/support/");

  return (
    <SwipeableDrawer
      anchor="right"
      onClose={() => setIsDrawerOpen(false)}
      onOpen={() => setIsDrawerOpen(true)}
      open={isDrawerOpen}
    >
      <Background>
        <Box
          sx={{
            display: "flex",
            flex: 1,
            flexDirection: "column",
            px: DRAWER_PADDING,
          }}
        >
          <Box
            sx={{
              display: "flex",
              justifyContent: "space-between",
              py: 3,
              width: screenWidth - CALCULATED_DRAWER_PADDING,
            }}
          >
            <img alt="app-logo" height={25} src={IMAGES.appLogo} />
            <IconButton
              sx={{ padding: 0 }}
              onClick={() => setIsDrawerOpen(false)}
            >
              <img alt="close-drawer" src={ICONS.closeDrawerIcon} />
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
              {t("wallet.connectYourWalletButton")}
            </Button>
          ) : null}
          <Box sx={{ display: "flex", flex: 1, flexDirection: "column" }}>
            <Grid container direction="column" mt={6} rowGap={4}>
              {NAV_ITEMS.map((navItem) => {
                if (
                  !isProposalDiscussionForumEnabled &&
                  navItem.dataTestId === "proposed-governance-actions-link"
                ) {
                  return null;
                }
                return (
                  <Grid item key={navItem.label}>
                    <Link
                      {...navItem}
                      isConnectWallet={isConnectButton}
                      onClick={() => {
                        if (navItem.newTabLink)
                          openInNewTab(navItem.newTabLink);
                        setIsDrawerOpen(false);
                      }}
                      size="big"
                    />
                  </Grid>
                );
              })}
            </Grid>
          </Box>
        </Box>
        <Box
          onClick={onClickHelp}
          sx={{ alignItems: "center", display: "flex", p: DRAWER_PADDING }}
        >
          <img alt="help" height={20} src={ICONS.helpIcon} width={20} />
          <Typography fontWeight={500} sx={{ ml: 1.5 }}>
            {t("menu.help")}
          </Typography>
        </Box>
      </Background>
    </SwipeableDrawer>
  );
};
