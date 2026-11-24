import { forwardRef, useMemo, useState } from "react";
import { Box, ButtonBase, CircularProgress } from "@mui/material";
import { To } from "react-router";
import { useTranslation } from "react-i18next";
import { ArrowRight, ChevronRight, Download, MousePointerClick, RefreshCw, SearchX, Wallet } from "lucide-react";

import { ModalWrapper } from "@/components/atoms/modal/ModalWrapper";
import { WalletOptionButton, type WalletOption } from "@/components/molecules/WalletOption";
import { LINKS } from "@/consts/links";
import { ds, RADIUS } from "@/consts/designTokens";
import { useModal } from "@/context/modal";
import { useCardano } from "@/context/wallet";
import { openInNewTab } from "@/utils/openInNewTab";
import { KNOWN_WALLETS } from "@/utils/walletConnect";

import {
  Callout,
  PrimaryAction,
  SecondaryAction,
  WalletRow,
  WalletStateFrame,
} from "../WalletConnect/walletParts";

type ChooseWalletModalState = {
  pathToNavigate?: To;
};

type CardanoExtension = {
  icon?: string;
  name?: string;
  supportedExtensions?: { cip: number }[];
};

/** CIP-95 wallets in window.cardano, one per display name. */
const readWallets = (): WalletOption[] => {
  const extensions = (window as unknown as { cardano?: Record<string, CardanoExtension> }).cardano;
  if (!extensions) return [];
  const result: WalletOption[] = [];
  Object.keys(extensions).forEach((key) => {
    const { icon, name, supportedExtensions } = extensions[key] ?? {};
    if (!icon || !name || !supportedExtensions) return;
    const isDuplicate = result.some((wallet) => wallet.label === name);
    const hasCip95 = supportedExtensions.some((extension) => extension.cip === 95);
    if (!isDuplicate && hasCip95) {
      result.push({ icon, label: name, name: key, cip95Available: true });
    }
  });
  return result;
};

export const ChooseWalletModal = forwardRef<HTMLDivElement>((_, ref) => {
  const { t } = useTranslation();
  const { state, closeModal } = useModal<ChooseWalletModalState>();
  const { isEnableLoading } = useCardano();
  const [scan, setScan] = useState(0);
  const walletOptions = useMemo(readWallets, [scan]);
  const connecting = walletOptions.find((wallet) => wallet.name === isEnableLoading);

  let content;
  if (connecting) {
    const wallet = connecting.label;
    content = (
      <WalletStateFrame
        body={t("walletConnect.approve.body", { wallet })}
        footer={(
          <>
            <SecondaryAction onClick={closeModal}>{t("walletConnect.cancel")}</SecondaryAction>
            <PrimaryAction disabled testId="waiting-for-approval-button">
              <CircularProgress aria-hidden size={14} sx={{ color: "inherit" }} />
              {t("walletConnect.approve.waiting")}
            </PrimaryAction>
          </>
        )}
        icon={MousePointerClick}
        onClose={closeModal}
        status={t("walletConnect.approve.status")}
        title={t("walletConnect.approve.title", { wallet })}
        titleId="choose-wallet-title"
        tone="warning"
      >
        <WalletRow icon={connecting.icon} name={wallet} selected />
        <Box role="status" sx={{ alignItems: "center", backgroundColor: ds.primary["25"], borderRadius: `${RADIUS.md}px`, display: "flex", gap: 1.5, px: 2, py: 1.5 }}>
          <CircularProgress aria-hidden size={20} sx={{ color: ds.orange["500"] }} />
          <Box>
            <Box sx={{ color: ds.gray["900"], fontSize: 14, fontWeight: 600 }}>{t("walletConnect.approve.waitingTitle")}</Box>
            <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>{t("walletConnect.approve.waitingBody", { wallet })}</Box>
          </Box>
        </Box>
        <Callout body={t("walletConnect.approve.onlyBody")} title={t("walletConnect.approve.onlyTitle")} tone="warning" />
      </WalletStateFrame>
    );
  } else if (!walletOptions.length) {
    content = (
      <WalletStateFrame
        body={t("walletConnect.none.body")}
        footer={(
          <>
            <SecondaryAction onClick={() => setScan((count) => count + 1)} testId="rescan-wallets-button">
              <RefreshCw aria-hidden size={15} />
              {t("walletConnect.none.retry")}
            </SecondaryAction>
            <PrimaryAction onClick={() => openInNewTab(KNOWN_WALLETS[0].url)}>
              <Download aria-hidden size={15} />
              {t("walletConnect.none.install")}
            </PrimaryAction>
          </>
        )}
        icon={SearchX}
        onClose={closeModal}
        status={t("walletConnect.none.status")}
        title={t("walletConnect.none.title")}
        titleId="choose-wallet-title"
        tone="warning"
      >
        <Box component="ul" data-testid="no-wallet-detected" sx={{ display: "flex", flexDirection: "column", gap: 1, listStyle: "none", m: 0, p: 0 }}>
          {KNOWN_WALLETS.map((wallet) => (
            <Box key={wallet.name} component="li">
              <Box component="a" href={wallet.url} rel="noopener noreferrer" sx={{ color: "inherit", display: "block", textDecoration: "none", "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 } }} target="_blank">
                <WalletRow detail={t("walletConnect.none.notDetected")} name={wallet.name} trailing={<ChevronRight aria-hidden color={ds.gray["400"]} size={18} />} />
              </Box>
            </Box>
          ))}
        </Box>
        <Callout body={t("walletConnect.none.mobileBody")} title={t("walletConnect.none.mobileTitle")} tone="info" />
      </WalletStateFrame>
    );
  } else {
    content = (
      <WalletStateFrame
        body={t("walletConnect.chooser.body")}
        icon={Wallet}
        onClose={closeModal}
        status={t("walletConnect.notConnected")}
        title={t("walletConnect.chooser.title")}
        titleId="choose-wallet-title"
        tone="neutral"
      >
        <Box>
          <Box sx={{ color: ds.gray["600"], fontSize: 14, fontWeight: 500, mb: 1 }}>{t("walletConnect.supportedWallets")}</Box>
          <Box sx={{ display: "flex", flexDirection: "column", gap: 1, maxHeight: 320, overflowY: "auto", p: 0.25 }}>
            {walletOptions.map(({ icon, label, name, cip95Available }) => (
              <WalletOptionButton
                key={name}
                cip95Available={cip95Available}
                dataTestId={`${name}-wallet-button`}
                icon={icon}
                label={label}
                name={name}
                pathToNavigate={state?.pathToNavigate}
              />
            ))}
          </Box>
        </Box>
        <Callout body={t("walletConnect.chooser.connectionOnlyBody")} title={t("walletConnect.chooser.connectionOnlyTitle")} tone="info" />
        <Box sx={{ borderTop: `1px solid ${ds.gray["200"]}`, color: ds.gray["600"], fontSize: 14, pt: 2 }}>
          {t("walletConnect.chooser.more")}
          <ButtonBase
            onClick={() => openInNewTab(LINKS.COMPATIBLE_WALLETS)}
            sx={{ color: ds.primary["600"], display: "flex", fontSize: 14, fontWeight: 600, gap: 0.5, mt: 1, "&:hover": { textDecoration: "underline" } }}
          >
            {t("walletConnect.chooser.learnMore")}
            <ArrowRight aria-hidden size={14} />
          </ButtonBase>
        </Box>
      </WalletStateFrame>
    );
  }

  return (
    <ModalWrapper ref={ref} dataTestId="connect-your-wallet-modal" hideCloseButton onClose={closeModal} sx={{ maxWidth: 500, p: 3 }}>
      <Box aria-labelledby="choose-wallet-title" aria-modal="true" role="dialog">{content}</Box>
    </ModalWrapper>
  );
});
