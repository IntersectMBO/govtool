import { useEffect, useId, useState } from "react";
import { useLocation } from "react-router";
import { Box, ButtonBase, Popover } from "@mui/material";
import { ChevronDown } from "lucide-react";

import { useCardano } from "@context";
import { useGetVoterInfo, useScreenDimension } from "@hooks";
import { ds, RADIUS } from "@/consts/designTokens";

import { DRepInfoCard } from "./DRepInfoCard";
import { WalletInfoCard } from "./WalletInfoCard";
import { stakeHexToBech32 } from "@/utils/drepDashboard";

// stake1u9…xyz: the first 7 and last 3 characters of the stake key.
const shortenStakeKey = (stakeKey?: string) =>
  (stakeKey && stakeKey.length > 12
    ? `${stakeKey.slice(0, 7)}…${stakeKey.slice(-3)}`
    : stakeKey ?? "");

export const WalletMenu = () => {
  const { stakeKey } = useCardano();
  const { voter } = useGetVoterInfo();
  const { isMobile } = useScreenDimension();
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
            id: menuId,
            sx: {
              border: `1px solid ${ds.gray["200"]}`,
              borderRadius: `${RADIUS.lg}px`,
              mt: 1,
              width: 320,
            },
          },
        }}
        transformOrigin={{ horizontal: "right", vertical: "top" }}
      >
        <Box sx={{ display: "flex", flexDirection: "column", gap: 2, p: 2 }}>
          {voter?.isRegisteredAsDRep && <DRepInfoCard />}
          <WalletInfoCard />
        </Box>
      </Popover>
    </>
  );
};
