import { useEffect, useState } from "react";
import { NavLink, useNavigate } from "react-router-dom";
import { AppBar, Box, Grid, IconButton } from "@mui/material";
import MenuIcon from "@mui/icons-material/Menu";

import { Button, Link } from "@atoms";
import { ICONS, IMAGES, PATHS, NAV_ITEMS } from "@consts";
import { useCardano, useModal } from "@context";
import { useScreenDimension, useTranslation } from "@hooks";
import { openInNewTab } from "@utils";

import { DrawerMobile } from "./DrawerMobile";

const POSITION_TO_BLUR = 50;

export const TopNav = ({ isConnectButton = true }) => {
  const [windowScroll, setWindowScroll] = useState<number>(0);
  const { openModal } = useModal();
  const [isDrawerOpen, setIsDrawerOpen] = useState<boolean>(false);
  const { screenWidth, isMobile } = useScreenDimension();
  const { isEnabled, disconnectWallet, stakeKey } = useCardano();
  const navigate = useNavigate();
  const { t } = useTranslation();

  useEffect(() => {
    const onScroll = () => {
      setWindowScroll(window.scrollY);
    };

    window.addEventListener("scroll", onScroll, {
      passive: true,
    });

    return () => window.removeEventListener("scroll", onScroll);
  }, []);

  const openDrawer = () => {
    setIsDrawerOpen(true);
  };

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
          borderRadius: 0,
          boxShadow: 0,
          justifyContent: "center",
          flex: 1,
          flexDirection: "row",
          position: "fixed",
          px: isMobile ? 2 : 5,
          py: 3,
        }}
      >
        <Box
          sx={{
            alignItems: "center",
            display: "flex",
            flex: 1,
            justifyContent: "space-between",
            maxWidth: 1290,
          }}
        >
          <NavLink
            data-testid="logo-button"
            onClick={() => (isConnectButton ? {} : disconnectWallet())}
            to={PATHS.home}
          >
            <img
              alt="app-logo"
              height={isMobile ? 25 : 35}
              src={IMAGES.appLogo}
            />
          </NavLink>
          {screenWidth >= 1024 ? (
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
                        if (navItem.newTabLink) {
                          openInNewTab(navItem.newTabLink);
                        }
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
                      {t("wallet.connectWallet")}
                    </Button>
                  </Grid>
                ) : null}
              </Grid>
            </nav>
          ) : (
            <>
              <Box sx={{ alignItems: "center", display: "flex" }}>
                {isConnectButton ? (
                  <Button
                    data-testid="connect-wallet-button"
                    onClick={() => {
                      openModal({ type: "chooseWallet" });
                    }}
                    size="small"
                    sx={{
                      marginRight: screenWidth >= 768 ? 3 : 1,
                      flex: 1,
                    }}
                    variant="contained"
                  >
                    {t("wallet.connect")}
                  </Button>
                ) : null}
                {screenWidth >= 768 ? (
                  <IconButton
                    data-testid="open-drawer-button"
                    onClick={openDrawer}
                    sx={{
                      bgcolor: "#FBFBFF",
                    }}
                  >
                    <MenuIcon color="primary" />
                  </IconButton>
                ) : (
                  <img
                    alt="drawer-icon"
                    src={ICONS.drawerIcon}
                    onClick={openDrawer}
                  />
                )}
              </Box>
              <DrawerMobile
                isConnectButton={isConnectButton}
                isDrawerOpen={isDrawerOpen}
                setIsDrawerOpen={setIsDrawerOpen}
              />
            </>
          )}
        </Box>
      </AppBar>
    </Box>
  );
};
