import { useEffect, useRef, useState, FC } from "react";
import { NavLink, useNavigate } from "react-router-dom";
import { AppBar, Box, Grid, IconButton, Menu, MenuItem } from "@mui/material";
import MenuIcon from "@mui/icons-material/Menu";
import { Button, Link } from "@atoms";
import { ICONS, IMAGES, PATHS, NAV_ITEMS, NavMenuItem } from "@consts";
import { useCardano, useFeatureFlag, useModal } from "@context";
import { useScreenDimension, useTranslation } from "@hooks";
import { openInNewTab } from "@utils";
import { DrawerMobile } from "./DrawerMobile";
import { useMaintenanceEndingBannerContext } from "./MaintenanceEndingBanner";

const POSITION_TO_BLUR = 80;

const isNavMenuItem = (item: NavItem | NavMenuItem): item is NavMenuItem =>
  "childNavItems" in item;

export const TopNav = ({ isConnectButton = true }) => {
  const containerRef = useRef<HTMLDivElement>(null);
  const [shouldBlur, setShouldBlur] = useState(false);
  const { openModal } = useModal();
  const [isDrawerOpen, setIsDrawerOpen] = useState(false);
  const { screenWidth, isMobile } = useScreenDimension();
  const { isEnabled, disconnectWallet, stakeKey } = useCardano();
  const navigate = useNavigate();
  const { t } = useTranslation();

  const { height: maintenanceEndingBannerHeight } =
    useMaintenanceEndingBannerContext();

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
        {NAV_ITEMS.map((navItem) => {
          if (isNavMenuItem(navItem)) {
            return (
              <Grid item key={navItem.label}>
                <MenuNavItem
                  navItem={navItem}
                  closeDrawer={() => setIsDrawerOpen(false)}
                />
              </Grid>
            );
          }
          return (
            <Grid item key={navItem.label}>
              <Link
                {...navItem}
                isConnectWallet={isConnectButton}
                onClick={() => {
                  setIsDrawerOpen(false);
                }}
              />
            </Grid>
          );
        })}
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
    <Box
      sx={{
        position: "sticky",
        top: maintenanceEndingBannerHeight,
        zIndex: 100,
        width: "100%",
      }}
    >
      <AppBar
        ref={containerRef}
        position="static"
        elevation={0}
        sx={{
          alignItems: "center",
          // TODO: Fix shouldBlur to work with sticky
          backdropFilter: shouldBlur ? "blur(10px)" : "none",
          backgroundColor: shouldBlur
            ? "rgba(256, 256, 256, 0.7)"
            : isMobile
            ? "white"
            : "rgba(256, 256, 256, 0.7)",
          borderBottom: isMobile ? 1 : 0,
          borderColor: "lightblue",
          boxShadow: 0,
          justifyContent: "center",
          flexDirection: "row",
          px: isMobile ? 2 : 5,
          py: 3,
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
    </Box>
  );
};

const MenuNavItem: FC<{
  navItem: NavMenuItem;
  closeDrawer: () => void;
}> = ({ closeDrawer, navItem }) => {
  const [anchorEl, setAnchorEl] = useState<HTMLAnchorElement | null>(null);
  const {
    isProposalDiscussionForumEnabled,
    isGovernanceOutcomesPillarEnabled,
  } = useFeatureFlag();

  // Create a ref array for child links to manage cliking within MenuItem but outside of Link
  const linkRefs = useRef<Array<HTMLAnchorElement | null>>([]);

  const open = Boolean(anchorEl);
  const handleClick = (event: React.MouseEvent<HTMLAnchorElement>) => {
    setAnchorEl(event.currentTarget);
  };
  const handleClose = () => {
    setAnchorEl(null);
  };

  const filterChildNavItems = () => {
    if (navItem.dataTestId === "governance-actions") {
      return (navItem.childNavItems || []).filter((item) => {
        if (
          !isProposalDiscussionForumEnabled &&
          item.dataTestId === "proposed-governance-actions-link"
        )
          return false;
        if (
          !isGovernanceOutcomesPillarEnabled &&
          item.dataTestId === "governance-actions-outcomes-link"
        )
          return false;
        return true;
      });
    }
    return navItem.childNavItems;
  };
  return (
    <>
      <Link
        navTo=""
        data-testid={navItem.dataTestId}
        aria-controls={open ? "basic-menu" : undefined}
        aria-haspopup="true"
        aria-expanded={open ? "true" : undefined}
        onClick={handleClick}
        label={
          <span
            style={{
              display: "inline-flex",
              alignItems: "center",
            }}
          >
            {navItem.label}
            <img
              alt="dropdown-icon"
              src={ICONS.arrowDownIcon}
              style={{
                marginLeft: "8px",
                width: "16px",
                height: "16px",
                transform: open ? "rotate(180deg)" : "rotate(0deg)",
                transition: "transform 0.2s ease-in-out",
              }}
            />
          </span>
        }
      />
      <Menu
        anchorEl={anchorEl}
        open={open}
        onClose={handleClose}
        anchorOrigin={{
          vertical: "bottom",
          horizontal: "right",
        }}
        transformOrigin={{
          vertical: "top",
          horizontal: "right",
        }}
        slotProps={{
          paper: {
            sx: {
              mt: 2,
            },
          },
        }}
      >
        {filterChildNavItems()?.map((childNavItem, idx) => (
          <MenuItem
            key={childNavItem.label}
            data-testid={childNavItem.dataTestId}
            sx={{ minWidth: 160 }}
            onClick={() => {
              linkRefs.current[idx]?.click();
            }}
          >
            <Link
              {...childNavItem}
              ref={(el) => {
                linkRefs.current[idx] = el;
              }}
              onClick={() => {
                if (childNavItem.newTabLink) {
                  openInNewTab(childNavItem.newTabLink);
                }
                closeDrawer();
                handleClose();
              }}
              label={childNavItem.label}
            />
          </MenuItem>
        ))}
      </Menu>
    </>
  );
};
