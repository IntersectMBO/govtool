import { FC } from "react";
import { NavLink } from "react-router-dom";
import { Box, Grid, IconButton, SwipeableDrawer } from "@mui/material";

import { Background, Button, Link, Typography } from "@atoms";
import { ICONS, IMAGES, NAV_ITEMS, NavMenuItem, NavItem, PATHS } from "@consts";
import { useScreenDimension, useTranslation } from "@hooks";
import { useFeatureFlag, useModal } from "@context";
import { openInNewTab } from "@utils";

import { DrawerMobileProps } from "./types";
import { LINKS } from "@/consts/links";

const DRAWER_PADDING = 2;
const CALCULATED_DRAWER_PADDING = DRAWER_PADDING * 8 * 2;

const isNavMenuItem = (item: NavItem | NavMenuItem): item is NavMenuItem =>
  "childNavItems" in item;

export const DrawerMobile = ({
  isConnectButton,
  isDrawerOpen,
  setIsDrawerOpen,
}: DrawerMobileProps) => {
  const { screenWidth } = useScreenDimension();
  const { openModal } = useModal();
  const { t } = useTranslation();

  const onClickHelp = () => openInNewTab(LINKS.SUPPORT);

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
            <NavLink
              data-testid="logo-button"
              style={{ display: "flex", justifyContent: "center" }}
              to={PATHS.dashboard}
            >
              <img alt="app-logo" height={25} src={IMAGES.appLogo} />
            </NavLink>
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
                if (isNavMenuItem(navItem)) {
                  return (
                    <MenuNavItem
                      closeDrawer={() => setIsDrawerOpen(false)}
                      navItem={navItem}
                      key={navItem.label}
                    />
                  );
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

const MenuNavItem: FC<{
  navItem: NavMenuItem;
  closeDrawer: () => void;
}> = ({ closeDrawer, navItem }) => {
  const {
    isProposalDiscussionForumEnabled,
    isGovernanceOutcomesPillarEnabled,
  } = useFeatureFlag();

  const filterChildNavItems = () => {
    if (navItem.dataTestId === "governance-actions") {
      return (navItem.childNavItems || []).filter((item) => {
        if (
          !isProposalDiscussionForumEnabled &&
          item.dataTestId === "proposed-governance-actions-link"
        )
          return false;
        if (
          !isGovernanceOutcomesPillarEnabled &&
          item.dataTestId === "governance-actions-outcomes-link"
        )
          return false;
        return true;
      });
    }
    return navItem.childNavItems;
  };
  return (
    <>
      <Grid item key={navItem.label}>
        <Link
          navTo=""
          data-testid={navItem.dataTestId}
          label={navItem.label}
          size="big"
        />
      </Grid>
      {filterChildNavItems()?.map((childNavItem) => (
        <Grid item key={childNavItem.label} ml={3}>
          <Link
            {...childNavItem}
            data-testid={childNavItem.dataTestId}
            onClick={() => {
              if (childNavItem.newTabLink) {
                openInNewTab(childNavItem.newTabLink);
              }
              closeDrawer();
            }}
            label={childNavItem.label}
            size="big"
          />
        </Grid>
      ))}
    </>
  );
};
