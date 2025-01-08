import { Box, Grid } from "@mui/material";
import { NavLink } from "react-router-dom";

import { DrawerLink, Spacer } from "@atoms";
import { CONNECTED_NAV_ITEMS, IMAGES, PATHS, DRAWER_WIDTH } from "@consts";
import { useFeatureFlag } from "@context";
import { useGetVoterInfo } from "@hooks";
import { WalletInfoCard, DRepInfoCard } from "@molecules";
import { openInNewTab } from "@utils";

export const Drawer = () => {
  const { isProposalDiscussionForumEnabled } = useFeatureFlag();
  const { voter } = useGetVoterInfo();

  return (
    <Box
      sx={{
        bgcolor: "#FBFBFF",
        display: "flex",
        flexDirection: "column",
        height: "100vh",
        position: "sticky",
        top: 0,
        width: `${DRAWER_WIDTH}px`,
      }}
    >
      <NavLink
        data-testid="logo-button"
        style={{ display: "flex", justifyContent: "center" }}
        to={PATHS.dashboard}
      >
        <img
          alt="app-logo"
          height={35}
          src={IMAGES.appLogo}
          style={{ marginTop: 24, objectFit: "contain" }}
        />
      </NavLink>
      <Grid
        columns={1}
        container
        display="flex"
        flex={1}
        flexDirection="column"
        mt={12}
        px={3}
        rowGap={2}
      >
        {CONNECTED_NAV_ITEMS.map((navItem) => {
          if (
            !isProposalDiscussionForumEnabled &&
            navItem.dataTestId === "proposal-discussion-link"
          ) {
            return null;
          }

          return (
            <Grid item key={navItem.label}>
              <DrawerLink
                {...navItem}
                onClick={
                  navItem.newTabLink
                    ? () => openInNewTab(navItem.newTabLink)
                    : undefined
                }
              />
              {navItem.childNavItems && (
                <Grid
                  columns={1}
                  container
                  display="flex"
                  flex={1}
                  flexDirection="column"
                  mt={2}
                  px={3}
                  rowGap={2}
                >
                  {navItem.childNavItems.map((childItem) => (
                    <DrawerLink
                      key={childItem.label}
                      {...childItem}
                      onClick={
                        childItem.newTabLink
                          ? () => openInNewTab(childItem.newTabLink!)
                          : undefined
                      }
                    />
                  ))}
                </Grid>
              )}
            </Grid>
          );
        })}
      </Grid>
      <Box p={2}>
        {voter?.isRegisteredAsDRep && <DRepInfoCard />}
        <Spacer y={2} />
        <WalletInfoCard />
      </Box>
    </Box>
  );
};
