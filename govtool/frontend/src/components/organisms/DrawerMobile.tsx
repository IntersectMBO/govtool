import { useLocation, useNavigate } from "react-router";
import { Box, IconButton, SwipeableDrawer } from "@mui/material";
import { LifeBuoy, X } from "lucide-react";

import { Button } from "@atoms";
import { IMAGES } from "@consts";
import { useTranslation } from "@hooks";
import { openInNewTab } from "@utils";
import { ds } from "@/consts/designTokens";
import { LINKS } from "@/consts/links";
import { PATHS } from "@/consts/paths";
import { requestProfileMenu } from "@/utils/profileMenu";

import { ConnectedWalletCard } from "./ConnectedWalletCard";

import { DrawerMobileProps } from "./types";
import { isNavEntryActive, NavEntryLink } from "./topNavItems";

export const DrawerMobile = ({
  isConnectButton,
  isConnected,
  isDrawerOpen,
  navEntries,
  onConnect,
  onNavigateAway,
  setIsDrawerOpen,
}: DrawerMobileProps) => {
  const { t } = useTranslation();
  const { pathname } = useLocation();
  const navigate = useNavigate();
  const close = () => setIsDrawerOpen(false);

  return (
    <SwipeableDrawer
      anchor="right"
      disableSwipeToOpen
      // Unmounted while closed, so its links and Connect button never sit in
      // the page as hidden duplicates of the bar's.
      ModalProps={{ keepMounted: false }}
      onClose={close}
      onOpen={() => setIsDrawerOpen(true)}
      open={isDrawerOpen}
      PaperProps={{ sx: { width: { xxs: "100%", sm: 375 } } }}
    >
      <Box
        sx={{
          backgroundColor: "#FFFFFF",
          display: "flex",
          flexDirection: "column",
          minHeight: "100%",
        }}
      >
        <Box
          sx={{
            alignItems: "center",
            borderBottom: `1px solid ${ds.gray["200"]}`,
            display: "flex",
            justifyContent: "space-between",
            px: 2,
            py: 1.5,
          }}
        >
          <img alt="Cardano GovTool" height={24} src={IMAGES.govToolLogo} width={118} />
          <IconButton
            aria-label={t("menu.close")}
            onClick={close}
            sx={{ border: `1px solid ${ds.gray["200"]}`, height: 36, width: 36 }}
          >
            <X aria-hidden color={ds.gray["600"]} size={18} />
          </IconButton>
        </Box>
        <Box sx={{ display: "flex", flex: 1, flexDirection: "column", gap: 2, p: 2 }}>
          {!isConnected && isConnectButton && (
            <Button
              data-testid="connect-wallet-button"
              onClick={() => {
                onConnect();
                close();
              }}
              size="extraLarge"
              sx={{ width: "100%" }}
            >
              {t("wallet.connectYourWalletButton")}
            </Button>
          )}
          <Box
            aria-label={t("menu.mainNav")}
            component="nav"
            sx={{ display: "flex", flexDirection: "column", gap: 0.5 }}
          >
            {navEntries.map((entry) => (
              <NavEntryLink
                key={entry.dataTestId}
                entry={entry}
                isActive={isNavEntryActive(entry, pathname)}
                onClick={(event) => {
                  close();
                  if (onNavigateAway && !entry.newTabLink) {
                    event.preventDefault();
                    onNavigateAway();
                  }
                }}
                variant="drawer"
              />
            ))}
          </Box>
          {isConnected && (
            <Box sx={{ border: `1px solid ${ds.gray["200"]}`, borderRadius: "12px", mt: 1, p: 2 }}>
              <ConnectedWalletCard
                onViewProfile={() => {
                  close();
                  // Phones show no avatar, so no profile menu to open.
                  if (!requestProfileMenu()) navigate(PATHS.dashboard);
                }}
              />
            </Box>
          )}
        </Box>
        <Box
          component="button"
          onClick={() => openInNewTab(LINKS.SUPPORT)}
          sx={{
            alignItems: "center",
            background: "none",
            border: "none",
            borderTop: `1px solid ${ds.gray["200"]}`,
            color: ds.gray["700"],
            cursor: "pointer",
            display: "flex",
            font: "inherit",
            fontSize: 14,
            fontWeight: 500,
            gap: 1.5,
            p: 2,
          }}
          type="button"
        >
          <LifeBuoy aria-hidden size={18} />
          {t("menu.help")}
        </Box>
      </Box>
    </SwipeableDrawer>
  );
};
