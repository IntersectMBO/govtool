import { useEffect, useId, useState } from "react";
import { useLocation, useNavigate } from "react-router";
import { Box, ButtonBase, Popover } from "@mui/material";
import { ChevronDown } from "lucide-react";

import { useCardano } from "@context";
import { useScreenDimension } from "@hooks";
import { useTranslation } from "react-i18next";
import { ConnectedWalletCard } from "@/components/organisms/ConnectedWalletCard";
import { ds, RADIUS, SHADOWS } from "@/consts/designTokens";
import { PATHS } from "@/consts/paths";
import { stakeHexToBech32 } from "@/utils/drepDashboard";
import { requestProfileMenu } from "@/utils/profileMenu";

// stake1u9…xyz: the first 7 and last 3 characters of the stake key.
const shortenStakeKey = (stakeKey?: string) =>
  (stakeKey && stakeKey.length > 12
    ? `${stakeKey.slice(0, 7)}…${stakeKey.slice(-3)}`
    : stakeKey ?? "");

export const WalletMenu = () => {
  const { stakeKey } = useCardano();
  const { isMobile } = useScreenDimension();
  const navigate = useNavigate();
  const { t } = useTranslation();
  const [anchorEl, setAnchorEl] = useState<HTMLElement | null>(null);
  const { pathname } = useLocation();
  // A link inside the menu (View profile) leaves it open over the new page.
  useEffect(() => setAnchorEl(null), [pathname]);
  const isOpen = Boolean(anchorEl);
  const menuId = useId();

  return (
    <>
      <ButtonBase
        aria-controls={isOpen ? menuId : undefined}
        aria-expanded={isOpen}
        aria-haspopup="dialog"
        data-testid="wallet-menu-button"
        onClick={(event) => setAnchorEl(event.currentTarget)}
        sx={{
          backgroundColor: "#FFFFFF",
          border: `1px solid ${ds.gray["200"]}`,
          borderRadius: `${RADIUS.pill}px`,
          gap: 0.75,
          height: 36,
          px: 1.5,
        }}
      >
        <Box
          sx={{
            alignItems: "center",
            backgroundColor: ds.success["100"],
            borderRadius: "50%",
            display: "flex",
            height: 14,
            justifyContent: "center",
            width: 14,
          }}
        >
          <Box
            sx={{
              backgroundColor: ds.success["600"],
              borderRadius: "50%",
              height: 6,
              width: 6,
            }}
          />
        </Box>
        <Box
          component="span"
          sx={{
            color: ds.gray["900"],
            fontSize: 14,
            fontWeight: 500,
            letterSpacing: "-0.09px",
            lineHeight: "20px",
            maxWidth: isMobile ? 96 : 200,
            overflow: "hidden",
            textOverflow: "ellipsis",
            whiteSpace: "nowrap",
          }}
        >
          {shortenStakeKey(stakeHexToBech32(stakeKey) ?? stakeKey)}
        </Box>
        <ChevronDown aria-hidden color={ds.gray["600"]} size={12} />
      </ButtonBase>
      <Popover
        anchorEl={anchorEl}
        anchorOrigin={{ horizontal: "right", vertical: "bottom" }}
        onClose={() => setAnchorEl(null)}
        open={isOpen}
        slotProps={{
          paper: {
            "aria-label": t("accountMenu.walletLabel"),
            id: menuId,
            role: "dialog",
            sx: {
              border: `1px solid ${ds.primary["200"]}`,
              borderRadius: `${RADIUS.lg}px`,
              borderTop: `3px solid ${ds.primary["200"]}`,
              boxShadow: SHADOWS.drop,
              mt: 1,
              p: 2,
              width: 360,
            },
          },
        }}
        transformOrigin={{ horizontal: "right", vertical: "top" }}
      >
        <ConnectedWalletCard
          onViewProfile={() => {
            setAnchorEl(null);
            if (!requestProfileMenu()) navigate(PATHS.dashboard);
          }}
        />
      </Popover>
    </>
  );
};
