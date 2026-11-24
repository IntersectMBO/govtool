import { FC, useCallback } from "react";
import { To, useNavigate, useLocation } from "react-router";
import { Box, ButtonBase } from "@mui/material";
import { useTranslation } from "react-i18next";

import { PATHS } from "@/consts/paths";
import { useModal } from "@/context/modal";
import { useCardano } from "@/context/wallet";
import { ds, PILL_TONES, RADIUS } from "@/consts/designTokens";

export interface WalletOption {
  icon: string;
  label: string;
  name: string;
  cip95Available: boolean;
  dataTestId?: string;
  pathToNavigate?: To;
}

/**
 * One supported wallet in the chooser. Connecting navigates as before; then
 * the wallet connection modal shows the checks and summary, or why it failed
 * (it replaces the modal wallet.tsx opens for either).
 */
export const WalletOptionButton: FC<WalletOption> = ({
  dataTestId,
  icon,
  label,
  name,
  cip95Available,
  pathToNavigate,
}) => {
  const { t } = useTranslation();
  const { pathname, state } = useLocation();
  const { enable, isEnableLoading } = useCardano();
  const { openModal, closeModal } = useModal();
  const navigate = useNavigate();

  const enableByWalletName = useCallback(async () => {
    if (isEnableLoading) return;
    let result: Awaited<ReturnType<typeof enable>>;
    try {
      result = await enable(name);
    } catch (error) {
      openModal({
        type: "walletConnection",
        state: {
          view: "error",
          error: (error as { error?: string })?.error ?? String(error),
          walletName: label,
        },
      });
      return;
    }
    if (result?.stakeKey) {
      // Any other page stays where it is: the same URL turns into the
      // dashboard's view of it.
      if (pathToNavigate) {
        navigate(pathToNavigate, { state });
      } else if (pathname === PATHS.home) {
        navigate(PATHS.dashboard, { state });
      }
      openModal({ type: "walletConnection", state: { view: "checking" } });
      return;
    }
    closeModal();
    navigate(PATHS.stakeKeys);
  }, [
    enable,
    isEnableLoading,
    openModal,
    closeModal,
    label,
    name,
    pathToNavigate,
    pathname,
    state,
  ]);

  const isThisLoading = isEnableLoading === name;

  return (
    <ButtonBase
      aria-busy={isThisLoading}
      data-testid={dataTestId}
      disabled={!cip95Available || !!isEnableLoading}
      onClick={enableByWalletName}
      sx={{
        alignItems: "center",
        backgroundColor: isThisLoading ? ds.primary["25"] : ds.gray["25"],
        border: `1px solid ${isThisLoading ? ds.primary["600"] : ds.gray["200"]}`,
        borderRadius: `${RADIUS.lg}px`,
        display: "flex",
        gap: 1.5,
        justifyContent: "flex-start",
        px: 1.75,
        py: 1.25,
        textAlign: "left",
        width: "100%",
        "&:hover": { backgroundColor: ds.primary["25"], borderColor: ds.primary["600"] },
        "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
        "&.Mui-disabled": { opacity: isThisLoading ? 1 : 0.6 },
      }}
    >
      <Box alt="" component="img" src={icon} sx={{ flexShrink: 0, height: 28, objectFit: "contain", width: 28 }} />
      <Box component="span" sx={{ color: ds.gray["900"], flex: 1, fontSize: 15, fontWeight: 600, textTransform: "capitalize" }}>
        {label}
      </Box>
      <Box component="span" sx={{ backgroundColor: PILL_TONES.success.bg, borderRadius: `${RADIUS.pill}px`, color: PILL_TONES.success.text, fontSize: 12, fontWeight: 500, px: 1, py: 0.25 }}>
        {t("walletConnect.supported")}
      </Box>
    </ButtonBase>
  );
};
