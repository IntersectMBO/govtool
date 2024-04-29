import { Box, Grid } from "@mui/material";
import { NavLink } from "react-router-dom";

import { DrawerLink, Spacer } from "@atoms";
import { CONNECTED_NAV_ITEMS, IMAGES, PATHS } from "@consts";
import { useGetVoterInfo } from "@hooks";
import { WalletInfoCard, DRepInfoCard } from "@molecules";
import { openInNewTab } from "@utils";

export const Drawer = () => {
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
        width: "268px",
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
        {CONNECTED_NAV_ITEMS.map((navItem) => (
          <Grid item key={navItem.label}>
            <DrawerLink
              {...navItem}
              onClick={
                navItem.newTabLink
                  ? () => openInNewTab(navItem.newTabLink)
                  : undefined
              }
            />
          </Grid>
        ))}
      </Grid>
      <Box p={2}>
        {voter?.isRegisteredAsDRep && <DRepInfoCard />}
        <Spacer y={2} />
        <WalletInfoCard />
      </Box>
    </Box>
  );
};
