import { useEffect, useState } from "react";
import { NavLink, useNavigate } from "react-router-dom";
import { AppBar, Box, Grid, IconButton } from "@mui/material";

import { Button, Link, Modal } from "@atoms";
import { ICONS, IMAGES, PATHS, NAV_ITEMS } from "@consts";
import { useCardano, useModal } from "@context";
import { useScreenDimension } from "@hooks";
import { DrawerMobile } from "./DrawerMobile";
import { openInNewTab } from "@utils";

const POSITION_TO_BLUR = 50;

export const TopNav = ({ isConnectButton = true }) => {
  const [windowScroll, setWindowScroll] = useState<number>(0);
  const { openModal, closeModal, modal } = useModal();
  const [isDrawerOpen, setIsDrawerOpen] = useState<boolean>(false);
  const { screenWidth, isMobile } = useScreenDimension();
  const { isEnabled, disconnectWallet, stakeKey } = useCardano();
  const navigate = useNavigate();

  useEffect(() => {
    const onScroll = () => {
      setWindowScroll(window.scrollY);
    };

    window.addEventListener("scroll", onScroll, {
      passive: true,
    });
    return () => window.removeEventListener("scroll", onScroll);
  }, []);

  return (
    <Box position="relative" py={isMobile ? 5 : 6}>
      <AppBar
        color="transparent"
        sx={{
          alignItems: "center",
          backdropFilter:
            windowScroll > POSITION_TO_BLUR ? "blur(10px)" : "none",
          backgroundColor:
            windowScroll > POSITION_TO_BLUR
              ? "rgba(256, 256, 256, 0.7)"
              : isMobile
              ? "white"
              : "transparent",
          borderBottom: isMobile ? 1 : 0,
          borderColor: "lightblue",
          boxShadow: 0,
          flex: 1,
          flexDirection: "row",
          justifyContent: "space-between",
          position: "fixed",
          px: screenWidth >= 1920 ? 37 : isMobile ? 2 : 5,
          py: 3,
        }}
      >
        <NavLink
          data-testid="logo-button"
          onClick={() => (isConnectButton ? {} : disconnectWallet())}
          to={PATHS.home}
        >
          <img height={isMobile ? 25 : 35} src={IMAGES.appLogo} />
        </NavLink>
        {!isMobile ? (
          <nav
            style={{
              alignItems: "center",
              display: "flex",
              justifyContent: "end",
            }}
          >
            <Grid
              alignItems="center"
              columns={5}
              columnSpacing={screenWidth < 1024 ? 2 : 4}
              container
            >
              {NAV_ITEMS.map((navItem) => (
                <Grid item key={navItem.label}>
                  <Link
                    {...navItem}
                    isConnectWallet={isConnectButton}
                    onClick={() => {
                      if (navItem.newTabLink) openInNewTab(navItem.newTabLink);
                      setIsDrawerOpen(false);
                    }}
                  />
                </Grid>
              ))}
              {isConnectButton ? (
                <Grid item>
                  <Button
                    data-testid="connect-wallet-button"
                    onClick={() => {
                      if (isEnabled && stakeKey) {
                        navigate(PATHS.dashboard);
                      } else {
                        openModal({ type: "chooseWallet" });
                      }
                    }}
                    size="extraLarge"
                    variant="contained"
                  >
                    Connect wallet
                  </Button>
                </Grid>
              ) : null}
            </Grid>
          </nav>
        ) : (
          <>
            <Box display="flex">
              {isConnectButton ? (
                <Button
                  data-testid="connect-wallet-button"
                  onClick={() => {
                    openModal({ type: "chooseWallet" });
                  }}
                  size="small"
                  sx={{
                    marginRight: 1,
                    flex: 1,
                  }}
                  variant="contained"
                >
                  Connect
                </Button>
              ) : null}
              <IconButton
                data-testid="open-drawer-button"
                onClick={() => setIsDrawerOpen(true)}
                sx={{ padding: 0 }}
              >
                <img src={ICONS.drawerIcon} />
              </IconButton>
            </Box>
            <DrawerMobile
              isConnectButton={isConnectButton}
              isDrawerOpen={isDrawerOpen}
              setIsDrawerOpen={setIsDrawerOpen}
            />
          </>
        )}
      </AppBar>
      {modal?.component && (
        <Modal
          open={Boolean(modal.component)}
          handleClose={!modal.preventDismiss ? closeModal : undefined}
        >
          {modal.component}
        </Modal>
      )}
    </Box>
  );
};
