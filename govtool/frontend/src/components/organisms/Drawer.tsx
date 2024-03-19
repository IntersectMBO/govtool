import { Box, Grid } from "@mui/material";
import { NavLink } from "react-router-dom";

import { DrawerLink, Typography } from "@atoms";
import {
  CONNECTED_NAV_ITEMS, ICONS, IMAGES, PATHS,
} from "@consts";
import { useCardano } from "@context";
import { useTranslation } from "@hooks";
import { WalletInfoCard, DRepInfoCard } from "@molecules";
import { openInNewTab } from "@utils";

export function Drawer() {
  const { voter } = useCardano();
  const { t } = useTranslation();

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
      <Box px={2}>
        {voter?.isRegisteredAsDRep && <DRepInfoCard />}
        <Box py={2} />
        <WalletInfoCard />
        <Box my={1}>
          <DrawerLink
            dataTestId="helps-link"
            label={t("menu.help")}
            activeIcon={ICONS.helpIcon}
            icon={ICONS.helpIcon}
            navTo=""
            onClick={() => openInNewTab(
              "https://docs.sanchogov.tools/support/get-help-in-discord",
            )}
          />
        </Box>
        <Typography sx={{ mb: 2 }} variant="caption">
          {t("footer.copyright")}
        </Typography>
      </Box>
    </Box>
  );
}
