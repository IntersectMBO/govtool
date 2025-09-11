import { Box, Grid, IconButton, SwipeableDrawer } from "@mui/material";
import { NavLink } from "react-router-dom";

import { Background, Link } from "@atoms";
import { CONNECTED_NAV_ITEMS, ICONS, PATHS } from "@consts";
import { DRepInfoCard, WalletInfoCard } from "@molecules";
import { useGetVoterInfo, useScreenDimension } from "@hooks";
import { openInNewTab } from "@utils";
import { useFeatureFlag } from "@context";

import { DashboardDrawerMobileProps } from "./types";

const DRAWER_PADDING = 2;
const CALCULATED_DRAWER_PADDING = DRAWER_PADDING * 8 * 2;

type DrawerLinkBase = {
  dataTestId?: string;
  label: string;
  navTo: string;
  newTabLink?: string;
};

type NavItem = DrawerLinkBase & {
  childNavItems?: DrawerLinkBase[];
};

export const DashboardDrawerMobile = ({
  isDrawerOpen,
  setIsDrawerOpen,
}: DashboardDrawerMobileProps) => {
  const {
    isProposalDiscussionForumEnabled,
    isGovernanceOutcomesPillarEnabled,
  } = useFeatureFlag();
  const { screenWidth } = useScreenDimension();
  const { voter } = useGetVoterInfo();

  const openDrawer = () => setIsDrawerOpen(true);
  const closeDrawer = () => setIsDrawerOpen(false);

  const navItems = CONNECTED_NAV_ITEMS as unknown as NavItem[];

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
              <NavLink
                data-testid="logo-button"
                style={{ display: "flex", justifyContent: "center" }}
                to={PATHS.dashboard}
              >
                <img alt="app-logo" src={ICONS.appLogoIcon} height={25} />
              </NavLink>
              <IconButton
                data-testid="close-drawer-button"
                onClick={closeDrawer}
                sx={{ padding: 0 }}
              >
                <img alt="drawer" src={ICONS.closeDrawerIcon} />
              </IconButton>
            </Box>

            <Grid container direction="column" rowGap={4} mt={6}>
              {navItems.map((navItem, i) => (
                <Grid
                  item
                  key={
                    navItem.dataTestId ??
                    navItem.label ??
                    navItem.navTo ??
                    `nav-${i}`
                  }
                >
                  <Link
                    {...navItem}
                    size="big"
                    onClick={() => {
                      if (navItem.newTabLink) openInNewTab(navItem.newTabLink);
                      setIsDrawerOpen(false);
                    }}
                    isConnectWallet
                  />

                  {!!navItem.childNavItems?.length && (
                    <Grid container direction="column" rowGap={4} mt={3} pl={3}>
                      {navItem.childNavItems.map((childItem, ci) => {
                        if (
                          !isProposalDiscussionForumEnabled &&
                          childItem.dataTestId === "proposal-discussion-link"
                        ) {
                          return null;
                        }
                        if (
                          !isGovernanceOutcomesPillarEnabled &&
                          (childItem.dataTestId ===
                            "governance-actions-voted-by-me-link" ||
                            childItem.dataTestId ===
                              "governance-actions-outcomes-link")
                        ) {
                          return null;
                        }
                        return (
                          <Link
                            key={
                              childItem.dataTestId ??
                              childItem.label ??
                              childItem.navTo ??
                              `child-${i}-${ci}`
                            }
                            {...childItem}
                            size="big"
                            onClick={() => {
                              if (childItem.newTabLink)
                                openInNewTab(childItem.newTabLink);
                              setIsDrawerOpen(false);
                            }}
                            isConnectWallet
                          />
                        );
                      })}
                    </Grid>
                  )}
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
