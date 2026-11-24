import { useRef, useState } from "react";
import { useNavigate } from "react-router";
import { Box, ButtonBase, Skeleton } from "@mui/material";
import { useTranslation } from "react-i18next";
import { Copy, KeyRound } from "lucide-react";

import { PATHS } from "@/consts/paths";
import { ds, RADIUS } from "@/consts/designTokens";
import { useAppContext } from "@/context/appContext";
import { useModal } from "@/context/modal";
import { useSnackbar } from "@/context/snackbar";
import { useCardano } from "@/context/wallet";
import { useStakeKeyVotingPowers } from "@/hooks/queries/useStakeKeyVotingPower";
import { connectedWallet } from "@/utils/connectedWallet";
import { stakeHexToBech32 } from "@/utils/drepDashboard";
import { setItemToLocalStorage, WALLET_LS_KEY } from "@/utils/localStorage";
import { networkLabel } from "@/utils/walletConnect";

import { StatusPill } from "./WalletConnect/walletParts";

const pill = {
  borderRadius: `${RADIUS.pill}px`,
  fontSize: 15,
  fontWeight: 600,
  height: 44,
  px: 2.5,
  "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
};

/** A wallet with several stake keys picks the one GovTool uses. */
export const ChooseStakeKeyPanel = () => {
  const { t } = useTranslation();
  const navigate = useNavigate();
  const { disconnectWallet, stakeKeys, setStakeKey } = useCardano();
  const { network } = useAppContext();
  const { openModal } = useModal();
  const { addSuccessAlert } = useSnackbar();
  const [chosenKey, setChosenKey] = useState("");
  const { powers, errors } = useStakeKeyVotingPowers(stakeKeys);
  const radios = useRef<(HTMLDivElement | null)[]>([]);
  const wallet = connectedWallet();

  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 3, maxWidth: 780, mx: "auto", px: 2, py: { xxs: 4, md: 6 }, width: "100%" }}>
      <Box>
        <Box aria-hidden sx={{ alignItems: "center", backgroundColor: ds.primary["50"], borderRadius: `${RADIUS.md}px`, color: ds.primary["600"], display: "flex", height: 40, justifyContent: "center", width: 40 }}>
          <KeyRound size={18} />
        </Box>
        <Box component="h1" sx={{ color: ds.gray["900"], fontSize: 26, fontWeight: 600, m: 0, mt: 2.5 }}>{t("walletConnect.stakeKeys.title")}</Box>
        <Box component="p" sx={{ color: ds.gray["600"], fontSize: 15, m: 0, mt: 1 }}>{t("walletConnect.stakeKeys.body")}</Box>
      </Box>

      <Box sx={{ alignItems: "center", backgroundColor: ds.primary["25"], border: `1px solid ${ds.primary["100"]}`, borderRadius: `${RADIUS.lg}px`, display: "flex", gap: 1.5, px: 1.5, py: 1.25 }}>
        {wallet?.icon ? (
          <Box alt="" component="img" src={wallet.icon} sx={{ borderRadius: `${RADIUS.sm}px`, height: 40, objectFit: "contain", width: 40 }} />
        ) : (
          <Box aria-hidden sx={{ alignItems: "center", backgroundColor: ds.primary["600"], borderRadius: `${RADIUS.sm}px`, color: "#FFFFFF", display: "flex", fontWeight: 600, height: 40, justifyContent: "center", width: 40 }}>
            {(wallet?.name ?? "W").charAt(0)}
          </Box>
        )}
        <Box sx={{ flex: 1, minWidth: 0 }}>
          <Box sx={{ color: ds.gray["900"], fontSize: 15, fontWeight: 600 }}>
            {t("walletConnect.stakeKeys.connected", { wallet: t("dashboard.wallet.walletName", { name: wallet?.name ?? "" }) })}
          </Box>
          <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>{networkLabel(network)}</Box>
        </Box>
        <StatusPill label={t("walletConnect.stakeKeys.connectedPill")} tone="success" />
      </Box>

      <Box>
        <Box sx={{ alignItems: "baseline", display: "flex", justifyContent: "space-between", mb: 1.5 }}>
          <Box component="h2" id="stake-key-choice" sx={{ color: ds.gray["900"], fontSize: 15, fontWeight: 600, m: 0 }}>{t("walletConnect.stakeKeys.choose")}</Box>
          <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>{t("walletConnect.stakeKeys.available", { count: stakeKeys.length })}</Box>
        </Box>
        <Box aria-labelledby="stake-key-choice" role="radiogroup" sx={{ display: "flex", flexDirection: "column", gap: 1.5 }}>
          {stakeKeys.map((key, index) => {
            const selected = chosenKey === key;
            const address = stakeHexToBech32(key) ?? key;
            const power = powers[key];
            const move = (step: number) => {
              const next = (index + step + stakeKeys.length) % stakeKeys.length;
              setChosenKey(stakeKeys[next]);
              radios.current[next]?.focus();
            };
            return (
              <Box
                key={key}
                data-testid={`${key}-radio`}
                onClick={() => setChosenKey(key)}
                sx={{
                  backgroundColor: "#FFFFFF",
                  border: `${selected ? 2 : 1}px solid ${selected ? ds.primary["600"] : ds.gray["200"]}`,
                  borderRadius: `${RADIUS.lg}px`,
                  cursor: "pointer",
                  position: "relative",
                }}
              >
                <Box
                  ref={(node: HTMLDivElement | null) => { radios.current[index] = node; }}
                  aria-checked={selected}
                  aria-label={t("walletConnect.stakeKeys.key", { n: index + 1 })}
                  onKeyDown={(event) => {
                    const moves: Record<string, number> = {
                      ArrowDown: 1,
                      ArrowRight: 1,
                      ArrowUp: -1,
                      ArrowLeft: -1,
                    };
                    if (event.key in moves) {
                      event.preventDefault();
                      move(moves[event.key]);
                    } else if (event.key === " " || event.key === "Enter") {
                      event.preventDefault();
                      setChosenKey(key);
                    }
                  }}
                  role="radio"
                  sx={{
                    borderRadius: `${RADIUS.lg}px`,
                    display: "flex",
                    gap: 1.75,
                    p: selected ? "15px" : 2,
                    pr: 12,
                    "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
                  }}
                  tabIndex={selected || (!chosenKey && index === 0) ? 0 : -1}
                >
                  <Box aria-hidden sx={{ border: `2px solid ${selected ? ds.primary["600"] : ds.gray["400"]}`, borderRadius: "50%", flexShrink: 0, height: 20, mt: 0.25, position: "relative", width: 20, "&::after": selected ? { backgroundColor: ds.primary["600"], borderRadius: "50%", content: '""', inset: 3, position: "absolute" } : undefined }} />
                  <Box sx={{ flex: 1, minWidth: 0 }}>
                    <Box sx={{ color: ds.gray["900"], fontSize: 15, fontWeight: 600 }}>{t("walletConnect.stakeKeys.key", { n: index + 1 })}</Box>
                    <Box sx={{ color: ds.gray["600"], fontSize: 13, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }} title={address}>{address}</Box>
                    <Box sx={{ alignItems: "center", display: "flex", justifyContent: "space-between", mt: 1.25 }}>
                      <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>{t("walletConnect.stakeKeys.votingPower")}</Box>
                      <Box data-testid={`${key}-voting-power`} sx={{ color: ds.gray["900"], fontSize: 17, fontWeight: 600, mr: -10 }}>
                        {errors[key] ? "–" : power === undefined ? <Skeleton width={90} /> : `₳ ${(power / 1_000_000).toLocaleString("en-US", { maximumFractionDigits: 0 })}`}
                      </Box>
                    </Box>
                  </Box>
                </Box>
                <ButtonBase
                  aria-label={t("walletConnect.stakeKeys.copyLabel", { n: index + 1 })}
                  onClick={async (event) => {
                    event.stopPropagation();
                    try {
                      await navigator.clipboard.writeText(address);
                      addSuccessAlert(t("alerts.copiedToClipboard"));
                    } catch {
                      // Nothing was copied, so nothing is confirmed.
                    }
                  }}
                  sx={{ border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.sm}px`, color: ds.primary["600"], fontSize: 13, fontWeight: 600, gap: 0.5, position: "absolute", px: 1, py: 0.25, right: 16, top: 16, "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}` } }}
                >
                  <Copy aria-hidden size={13} />
                  {t("walletConnect.stakeKeys.copy")}
                </ButtonBase>
              </Box>
            );
          })}
        </Box>
      </Box>

      <Box sx={{ display: "flex", gap: 1.5, justifyContent: "space-between" }}>
        <ButtonBase
          data-testid="cancel-button"
          onClick={() => {
            disconnectWallet();
            navigate(PATHS.home);
          }}
          sx={{ ...pill, border: `1px solid ${ds.gray["200"]}`, color: ds.gray["800"], "&:hover": { backgroundColor: ds.gray["50"] } }}
        >
          {t("walletConnect.stakeKeys.cancel")}
        </ButtonBase>
        <ButtonBase
          data-testid="select-button"
          disabled={!chosenKey}
          onClick={() => {
            setStakeKey(chosenKey);
            setItemToLocalStorage(`${WALLET_LS_KEY}_stake_key`, chosenKey);
            navigate(PATHS.dashboard);
            openModal({ type: "walletConnection", state: { view: "checking" } });
          }}
          sx={{ ...pill, backgroundColor: ds.primary["600"], color: "#FFFFFF", "&:hover": { backgroundColor: ds.primary["700"] }, "&.Mui-disabled": { opacity: 0.5 } }}
        >
          {t("walletConnect.stakeKeys.continue")}
        </ButtonBase>
      </Box>
    </Box>
  );
};
