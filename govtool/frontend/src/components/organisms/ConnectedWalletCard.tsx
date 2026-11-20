import { ReactNode } from "react";
import { useTranslation } from "react-i18next";
import { Box, ButtonBase } from "@mui/material";
import { ArrowLeftRight, Wallet } from "lucide-react";

import { CopyButton } from "@/components/organisms/DRepDashboard/parts";
import { ds, RADIUS } from "@/consts/designTokens";
import { useCardano } from "@/context/wallet";
import { useGetAdaHolderVotingPowerQuery } from "@/hooks/queries/useGetAdaHolderVotingPowerQuery";
import { useDisconnectWallet, useWalletBalance } from "@/hooks/useConnectedWallet";
import { connectedWallet } from "@/utils/connectedWallet";
import { stakeHexToBech32 } from "@/utils/drepDashboard";

const focusRing = {
  "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
};

const short = (value: string, head: number, tail: number) =>
  (value.length > head + tail + 1 ? `${value.slice(0, head)}…${value.slice(-tail)}` : value);

/** Ada in a short form: ₳ 80.04M. */
export const compactAda = (lovelace?: number | null) => {
  if (lovelace === undefined || lovelace === null) return undefined;
  const ada = new Intl.NumberFormat("en-US", {
    maximumFractionDigits: 2,
    notation: "compact",
  }).format(lovelace / 1_000_000);
  return `₳\u00a0${ada}`;
};

const Tile = ({ label, value, testId }: { label: string; value: ReactNode; testId: string }) => (
  <Box sx={{ backgroundColor: ds.gray["50"], borderRadius: `${RADIUS.md}px`, minWidth: 0, px: 1.5, py: 1.25 }}>
    <Box sx={{ color: ds.gray["600"], fontSize: 12 }}>{label}</Box>
    <Box data-testid={testId} sx={{ color: ds.gray["900"], fontSize: 17, fontWeight: 600, mt: 0.5, whiteSpace: "nowrap" }}>
      {value}
    </Box>
  </Box>
);

/**
 * The connected wallet: name and stake address, switch wallet, balance and
 * voting power, and View Profile and Disconnect.
 */
export const ConnectedWalletCard = ({ onViewProfile }: { onViewProfile: () => void }) => {
  const { t } = useTranslation();
  const { stakeKey } = useCardano();
  const { balance } = useWalletBalance();
  const { votingPower } = useGetAdaHolderVotingPowerQuery(stakeKey);
  const { confirmDisconnect, switchWallet } = useDisconnectWallet();
  const wallet = connectedWallet();
  const address = stakeKey ? stakeHexToBech32(stakeKey) ?? stakeKey : "";
  const dash = <span aria-label={t("accountMenu.unknown")} role="img">—</span>;

  return (
    <Box
      aria-label={t("accountMenu.walletLabel")}
      component="section"
      data-testid="connected-wallet-card"
      sx={{ display: "flex", flexDirection: "column", gap: 1.5 }}
    >
      <Box sx={{ alignItems: "center", display: "flex", gap: 1.25 }}>
        <Box aria-hidden sx={{ alignItems: "center", border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.md}px`, color: ds.primary["600"], display: "flex", flexShrink: 0, height: 40, justifyContent: "center", width: 40 }}>
          {wallet?.icon ? (
            <Box alt="" component="img" src={wallet.icon} sx={{ height: 24, objectFit: "contain", width: 24 }} />
          ) : (
            <Wallet size={18} />
          )}
        </Box>
        <Box sx={{ flex: 1, minWidth: 0 }}>
          <Box sx={{ color: ds.gray["900"], fontSize: 15, fontWeight: 600 }}>
            {wallet?.name ? t("accountMenu.walletName", { name: wallet.name }) : t("accountMenu.walletFallback")}
          </Box>
          {address && (
            <Box sx={{ alignItems: "center", color: ds.gray["600"], display: "flex", fontSize: 13, gap: 0.25 }}>
              <Box component="span" title={address}>{short(address, 10, 8)}</Box>
              <CopyButton label={t("accountMenu.stakeAddress")} testId="wallet-card-copy-address" value={address} />
            </Box>
          )}
        </Box>
        <ButtonBase
          aria-label={t("accountMenu.switchWallet")}
          data-testid="switch-wallet-button"
          onClick={switchWallet}
          sx={{ backgroundColor: ds.gray["100"], borderRadius: `${RADIUS.md}px`, color: ds.gray["700"], flexShrink: 0, height: 36, width: 36, "&:hover": { backgroundColor: ds.gray["200"] }, ...focusRing }}
        >
          <ArrowLeftRight aria-hidden size={16} />
        </ButtonBase>
      </Box>
      <Box aria-hidden sx={{ borderTop: `1px dashed ${ds.gray["200"]}` }} />
      <Box sx={{ display: "grid", gap: 1, gridTemplateColumns: "repeat(2, minmax(0, 1fr))" }}>
        <Tile label={t("accountMenu.balance")} testId="wallet-card-balance" value={compactAda(balance) ?? dash} />
        <Tile label={t("accountMenu.votingPower")} testId="wallet-card-voting-power" value={compactAda(votingPower) ?? dash} />
      </Box>
      <Box sx={{ alignItems: "center", backgroundColor: ds.gray["50"], borderRadius: `${RADIUS.md}px`, display: "flex", fontSize: 13, gap: 1, px: 1.5, py: 1 }}>
        <Box sx={{ alignItems: "center", color: ds.success["700"], display: "flex", fontWeight: 500, gap: 0.75 }}>
          <Box aria-hidden sx={{ backgroundColor: ds.success["600"], borderRadius: "50%", boxShadow: `0 0 0 3px ${ds.success["100"]}`, height: 7, width: 7 }} />
          {t("accountMenu.connected")}
        </Box>
        <Box aria-hidden sx={{ alignSelf: "stretch", borderLeft: `1px solid ${ds.gray["200"]}` }} />
        <Box sx={{ color: ds.gray["800"], flex: 1, minWidth: 0, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
          {address ? short(address, 7, 4) : ""}
        </Box>
        {address && <CopyButton label={t("accountMenu.stakeAddress")} value={address} />}
      </Box>
      <Box sx={{ display: "grid", gap: 1, gridTemplateColumns: "repeat(2, minmax(0, 1fr))" }}>
        <ButtonBase
          data-testid="view-profile-button"
          onClick={onViewProfile}
          sx={{ backgroundColor: ds.primary["600"], borderRadius: `${RADIUS.md}px`, color: "#FFFFFF", fontFamily: "inherit", fontSize: 14, fontWeight: 600, height: 40, "&:hover": { backgroundColor: ds.primary["700"] }, ...focusRing }}
        >
          {t("accountMenu.viewProfile")}
        </ButtonBase>
        <ButtonBase
          data-testid="disconnect-button"
          onClick={confirmDisconnect}
          sx={{ backgroundColor: ds.error["50"], border: `1px solid ${ds.error["200"]}`, borderRadius: `${RADIUS.md}px`, color: ds.error["700"], fontFamily: "inherit", fontSize: 14, fontWeight: 600, height: 40, "&:hover": { backgroundColor: ds.error["100"] }, ...focusRing }}
        >
          {t("accountMenu.disconnect")}
        </ButtonBase>
      </Box>
    </Box>
  );
};
