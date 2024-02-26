import { useEffect, useRef, useState } from "react";
import { NavLink, useNavigate } from "react-router-dom";
import { AppBar, Box, Grid, IconButton } from "@mui/material";

import { Button, Link } from "@atoms";
import { ICONS, IMAGES, PATHS, NAV_ITEMS } from "@consts";
import { useCardano, useModal } from "@context";
import { useScreenDimension, useTranslation } from "@hooks";
import { openInNewTab } from "@utils";

import { DrawerMobile } from "./DrawerMobile";

const POSITION_TO_BLUR = 20;

interface TopNavProps {
  isConnectButton?: boolean;
}

export const NewTopNav = ({ isConnectButton = true }: TopNavProps) => {
  const { openModal } = useModal();
  const { screenWidth, isMobile } = useScreenDimension();
  const navigate = useNavigate();
  const { t } = useTranslation();

  const { isEnabled, disconnectWallet, stakeKey } = useCardano();

  const [isDrawerOpen, setIsDrawerOpen] = useState<boolean>(false);

  const containerRef = useRef<HTMLDivElement>(null);
  const [isOverlapping, setIsOverlapping] = useState<boolean>(false);
  useEffect(() => {
    const parent = containerRef.current?.parentElement;
    if (!parent || !containerRef.current) return;

    const onScroll = () => {
      setIsOverlapping(parent.scrollTop > POSITION_TO_BLUR);
    };

    parent.addEventListener("scroll", onScroll, {
      passive: true,
    });

    return () => parent.removeEventListener("scroll", onScroll);
  }, []);

  return (
    <AppBar
      ref={containerRef}
      color="transparent"
      sx={{
        alignItems: "center",
        backdropFilter:
          isOverlapping ? "blur(10px)" : "none",
        backgroundColor:
          isOverlapping
            ? "rgba(256, 256, 256, 0.7)"
            : isMobile
              ? "white"
              : "transparent",
        borderBottom: isMobile ? 1 : 0,
        borderColor: "lightblue",
        boxShadow: 0,
        flexDirection: "row",
        justifyContent: "space-between",
        position: "sticky",
        px: screenWidth >= 1920 ? 37 : isMobile ? 2 : 5,
        py: 3,
        top: 0,
        transition: "background-color 0.3s",
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
                  {t("wallet.connectWallet")}
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
                {t("wallet.connect")}
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
  );
};
