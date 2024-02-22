import { Box, Grid } from "@mui/material";
import { NavLink } from "react-router-dom";

import { DrawerLink, Typography } from "@atoms";
import { CONNECTED_NAV_ITEMS, ICONS, IMAGES, PATHS } from "@consts";
import { useCardano } from "@context";
import { WalletInfoCard, DRepInfoCard } from "@molecules";
import { openInNewTab } from "@/utils";
import { useTranslation } from "@hooks";

export const Drawer = () => {
  const { user } = useCardano();
  const { t } = useTranslation();

  return (
    <Box height="100vh" width="268px" zIndex={101}>
      <Box
        bgcolor="#FBFBFF"
        borderRight={1}
        borderColor="white"
        display="flex"
        flex={1}
        flexDirection="column"
        height="100%"
        justifyItems="center"
        position="fixed"
        width="268px"
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
          {user?.isRegisteredAsDRep && <DRepInfoCard />}
          <Box py={2} />
          <WalletInfoCard />
          <Box my={1}>
            <DrawerLink
              dataTestId="helps-link"
              label={t("menu.help")}
              activeIcon={ICONS.helpIcon}
              icon={ICONS.helpIcon}
              navTo=""
              onClick={() =>
                openInNewTab(
                  "https://docs.sanchogov.tools/support/get-help-in-discord"
                )
              }
            />
          </Box>
          <Typography sx={{ mb: 2 }} variant="caption">
            {t("footer.copyright")}
          </Typography>
        </Box>
      </Box>
    </Box>
  );
};
