import { useEffect, useRef, useState } from "react";
import { NavLink, useNavigate } from "react-router-dom";
import { AppBar, Box, Grid, IconButton } from "@mui/material";
import MenuIcon from "@mui/icons-material/Menu";
import { Button, Link } from "@atoms";
import { ICONS, IMAGES, PATHS, NAV_ITEMS } from "@consts";
import { useCardano, useFeatureFlag, useModal } from "@context";
import { useScreenDimension, useTranslation } from "@hooks";
import { openInNewTab } from "@utils";
import { DrawerMobile } from "./DrawerMobile";

const POSITION_TO_BLUR = 50;

export const TopNav = ({ isConnectButton = true }) => {
  const {
    isProposalDiscussionForumEnabled,
    isGovernanceOutcomesPillarEnabled,
  } = useFeatureFlag();
  const containerRef = useRef<HTMLDivElement>(null);
  const [shouldBlur, setShouldBlur] = useState(false);
  const { openModal } = useModal();
  const [isDrawerOpen, setIsDrawerOpen] = useState(false);
  const { screenWidth, isMobile } = useScreenDimension();
  const { isEnabled, disconnectWallet, stakeKey } = useCardano();
  const navigate = useNavigate();
  const { t } = useTranslation();

  useEffect(() => {
    const onScroll = () => {
      if (!containerRef.current?.nextElementSibling) return;
      setShouldBlur(
        containerRef.current.nextElementSibling.getBoundingClientRect().top <
          POSITION_TO_BLUR,
      );
    };

    window.addEventListener("scroll", onScroll, { passive: true });
    return () => window.removeEventListener("scroll", onScroll);
  }, []);

  const onClickConnectButton = () => {
    if (isEnabled && stakeKey) {
      navigate(PATHS.dashboard);
    }
    openModal({ type: "chooseWallet" });
  };

  const filteredNavItems = NAV_ITEMS.filter((navItem) => {
    if (
      !isProposalDiscussionForumEnabled &&
      navItem.dataTestId === "proposed-governance-actions-link"
    )
      return false;
    if (
      !isGovernanceOutcomesPillarEnabled &&
      navItem.dataTestId === "governance-actions-outcomes-link"
    )
      return false;
    return true;
  });

  const renderDesktopNav = () => (
    <nav
      style={{ display: "flex", alignItems: "center", justifyContent: "end" }}
    >
      <Grid
        container
        alignItems="center"
        columns={5}
        columnSpacing={screenWidth < 1024 ? 2 : 4}
      >
        {filteredNavItems.map((navItem) => (
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
        {isConnectButton && (
          <Grid item>
            <Button
              data-testid="connect-wallet-button"
              onClick={onClickConnectButton}
              size="extraLarge"
              variant="contained"
            >
              {t("wallet.connectWallet")}
            </Button>
          </Grid>
        )}
      </Grid>
    </nav>
  );

  const renderMobileNav = () => (
    <Box sx={{ display: "flex", alignItems: "center" }}>
      {isConnectButton && (
        <Button
          data-testid="connect-wallet-button"
          onClick={onClickConnectButton}
          size="small"
          sx={{ marginRight: screenWidth >= 768 ? 3 : 1, flex: 1 }}
          variant="contained"
        >
          {t("wallet.connect")}
        </Button>
      )}
      {screenWidth >= 768 ? (
        <IconButton
          data-testid="open-drawer-button"
          onClick={() => setIsDrawerOpen(true)}
          sx={{ bgcolor: "#FBFBFF" }}
        >
          <MenuIcon color="primary" />
        </IconButton>
      ) : (
        <img
          alt="drawer-icon"
          src={ICONS.drawerIcon}
          onClick={() => setIsDrawerOpen(true)}
          onKeyDown={(e) => e.key === "Enter" && setIsDrawerOpen(true)}
          data-testid="open-drawer-button"
        />
      )}
      <DrawerMobile
        isConnectButton={isConnectButton}
        isDrawerOpen={isDrawerOpen}
        setIsDrawerOpen={setIsDrawerOpen}
      />
    </Box>
  );

  return (
    <AppBar
      ref={containerRef}
      color="transparent"
      sx={{
        alignItems: "center",
        backdropFilter: shouldBlur ? "blur(10px)" : "none",
        backgroundColor: shouldBlur
          ? "rgba(256, 256, 256, 0.7)"
          : isMobile
          ? "white"
          : "transparent",
        borderBottom: isMobile ? 1 : 0,
        borderColor: "lightblue",
        boxShadow: 0,
        justifyContent: "center",
        flexDirection: "row",
        position: "sticky",
        top: 0,
        px: isMobile ? 2 : 5,
        py: 3,
        zIndex: 100,
      }}
    >
      <Box
        sx={{
          display: "flex",
          alignItems: "center",
          flex: 1,
          justifyContent: "space-between",
          maxWidth: 1290,
        }}
      >
        <NavLink
          data-testid="logo-button"
          to={PATHS.home}
          onClick={() => isConnectButton || disconnectWallet()}
        >
          <img
            alt="app-logo"
            height={isMobile ? 25 : 35}
            src={IMAGES.appLogo}
          />
        </NavLink>
        {screenWidth >= 1145 ? renderDesktopNav() : renderMobileNav()}
      </Box>
    </AppBar>
  );
};
