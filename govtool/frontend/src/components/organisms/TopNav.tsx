import { useState } from "react";
import { NavLink, useLocation, useNavigate } from "react-router";
import { Box, IconButton } from "@mui/material";
import { Menu, Wallet } from "lucide-react";

import { Button } from "@atoms";
import { CONNECTED_NAV_ITEMS, IMAGES, NAV_ITEMS, PATHS } from "@consts";
import { useCardano, useModal } from "@context";
import { useScreenDimension, useTranslation } from "@hooks";
import { WalletMenu } from "@molecules";
import { ds, SHADOWS } from "@/consts/designTokens";

import { DrawerMobile } from "./DrawerMobile";
import { GlobalSearchButton } from "./GlobalSearch/GlobalSearchButton";
import { NotificationBell } from "./Notifications/NotificationBell";
import { useOpenChooserAfterSwitch } from "@/hooks/useConnectedWallet";

import { ProfileMenu } from "./ProfileMenu";
import { isNavEntryActive, NavEntry, NavEntryLink } from "./topNavItems";

// Below this width the nav items and the wallet control no longer fit on one
// line, so the nav collapses into the drawer.
const DESKTOP_MIN_WIDTH = 1024;
// Below this the bar keeps only search, the bell and the menu: the wallet pill
// would push the menu button off the screen, and the drawer shows the wallet.
const PHONE_MAX_WIDTH = 480;

type TopNavProps = {
  isConnectButton?: boolean;
  /**
   * Set inside a wallet flow: in-app links (logo and nav) call it instead of
   * navigating, so the flow can confirm before its form is lost.
   */
  onNavigateAway?: () => void;
};

export const TopNav = ({ isConnectButton = true, onNavigateAway }: TopNavProps) => {
  const { openModal } = useModal();
  const [isDrawerOpen, setIsDrawerOpen] = useState(false);
  const { screenWidth } = useScreenDimension();
  const { isEnabled, disconnectWallet, stakeKey } = useCardano();
  const navigate = useNavigate();
  const { pathname } = useLocation();
  const { t } = useTranslation();

  const isConnected = Boolean(isEnabled && stakeKey);
  useOpenChooserAfterSwitch();
  const navEntries = (
    isConnected ? CONNECTED_NAV_ITEMS : NAV_ITEMS
  ) as NavEntry[];

  const guardNavigation = (event: React.MouseEvent<HTMLElement>) => {
    if (!onNavigateAway) return;
    event.preventDefault();
    onNavigateAway();
  };

  // The page's main landmark, or its heading when a page has none.
  const skipToMain = (event: React.MouseEvent<HTMLElement>) => {
    const target =
      document.querySelector<HTMLElement>("main") ??
      document.querySelector<HTMLElement>("h1");
    if (!target) return;
    event.preventDefault();
    if (!target.hasAttribute("tabindex")) {
      target.setAttribute("tabindex", "-1");
      // A focused region needs no ring; the skip already moved the reader.
      target.style.outline = "none";
    }
    target.focus();
    target.scrollIntoView?.({ block: "start" });
  };

  const onClickConnectButton = () => {
    if (isEnabled && stakeKey) {
      navigate(PATHS.dashboard);
    }
    openModal({ type: "chooseWallet" });
  };

  const renderWalletControl = (isCompact = false) => {
    // Wallet flows show neither search nor the bell, so the pill fits there.
    if (isConnected && isCompact && screenWidth < PHONE_MAX_WIDTH && !onNavigateAway) {
      return <NotificationBell />;
    }
    if (isConnected) {
      return (
        <>
          <WalletMenu />
          {!onNavigateAway && <NotificationBell />}
          {!onNavigateAway && <ProfileMenu />}
        </>
      );
    }
    if (!isConnectButton) return null;
    return (
      <Button
        data-testid="connect-wallet-button"
        endIcon={
          isCompact ? undefined : (
            <Wallet
              aria-hidden
              color={ds.primary["600"]}
              size={16}
              strokeWidth={1.5}
            />
          )
        }
        onClick={onClickConnectButton}
        size={isCompact ? "small" : "medium"}
        variant="soft"
      >
        {t(isCompact ? "wallet.connect" : "wallet.connectWallet")}
      </Button>
    );
  };

  const renderDesktopNav = () => (
    <>
      <Box
        aria-label={t("menu.mainNav")}
        component="nav"
        sx={{
          alignItems: "center",
          display: "flex",
          flex: 1,
          gap: 0.5,
          justifyContent: "center",
          px: 3,
        }}
      >
        {navEntries.map((entry) => (
          <NavEntryLink
            key={entry.dataTestId}
            entry={entry}
            isActive={isNavEntryActive(entry, pathname)}
            onClick={entry.newTabLink ? undefined : guardNavigation}
          />
        ))}
      </Box>
      <Box sx={{ alignItems: "center", display: "flex", gap: 1 }}>
        {/* Not inside a wallet flow, where leaving would drop the form. */}
        {!onNavigateAway && <GlobalSearchButton />}
        {renderWalletControl()}
      </Box>
    </>
  );

  const renderMobileNav = () => (
    <Box sx={{ alignItems: "center", display: "flex", gap: 1 }}>
      {!onNavigateAway && <GlobalSearchButton />}
      {renderWalletControl(true)}
      <IconButton
        aria-label={t("menu.open")}
        data-testid="open-drawer-button"
        onClick={() => setIsDrawerOpen(true)}
        sx={{
          border: `1px solid ${ds.gray["200"]}`,
          borderRadius: "50%",
          height: 36,
          width: 36,
        }}
      >
        <Menu aria-hidden color={ds.gray["700"]} size={18} />
      </IconButton>
      <DrawerMobile
        isConnectButton={isConnectButton}
        isConnected={isConnected}
        isDrawerOpen={isDrawerOpen}
        navEntries={navEntries}
        onConnect={onClickConnectButton}
        onNavigateAway={onNavigateAway}
        setIsDrawerOpen={setIsDrawerOpen}
      />
    </Box>
  );

  return (
    <Box
      component="header"
      sx={{
        backdropFilter: "blur(24px)",
        backgroundColor: "rgba(255, 255, 255, 0.8)",
        borderBottom: `1px solid ${ds.gray["200"]}`,
        boxShadow: SHADOWS.header,
        position: "sticky",
        px: { xxs: 2, md: 3 },
        top: 0,
        width: "100%",
        boxSizing: "border-box",
        zIndex: 100,
      }}
    >
      <Box
        component="a"
        data-testid="skip-to-content-link"
        href="#main-content"
        onClick={skipToMain}
        sx={{
          backgroundColor: "#FFFFFF",
          border: `2px solid ${ds.primary["600"]}`,
          borderRadius: "8px",
          color: ds.primary["700"],
          fontSize: 14,
          fontWeight: 600,
          left: 16,
          p: "8px 12px",
          position: "absolute",
          textDecoration: "none",
          top: 8,
          transform: "translateY(-200%)",
          zIndex: 101,
          "&:focus": { transform: "none" },
        }}
      >
        {t("menu.skipToContent")}
      </Box>
      <Box
        sx={{
          alignItems: "center",
          display: "flex",
          justifyContent: "space-between",
          maxWidth: 1180,
          mx: "auto",
          py: 2,
        }}
      >
        <NavLink
          data-testid="logo-button"
          // Only leaving wallet setup (ChooseStakeKey, no stake key chosen
          // yet) disconnects; a connected wallet goes to its dashboard.
          onClick={(event) => {
            if (onNavigateAway) {
              guardNavigation(event);
              return;
            }
            if (!isConnectButton && !isConnected) disconnectWallet();
          }}
          style={{ display: "flex" }}
          to={isConnected ? PATHS.dashboard : PATHS.home}
        >
          <img
            alt="Cardano GovTool"
            height={24}
            src={IMAGES.govToolLogo}
            width={118}
          />
        </NavLink>
        {screenWidth >= DESKTOP_MIN_WIDTH
          ? renderDesktopNav()
          : renderMobileNav()}
      </Box>
    </Box>
  );
};
