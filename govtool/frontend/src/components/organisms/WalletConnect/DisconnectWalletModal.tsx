import { forwardRef } from "react";
import { Box } from "@mui/material";
import { useTranslation } from "react-i18next";
import { LogOut, ShieldCheck, Unlink } from "lucide-react";

import { ModalWrapper } from "@/components/atoms/modal/ModalWrapper";
import { ds, RADIUS } from "@/consts/designTokens";
import { useAppContext } from "@/context/appContext";
import { useModal } from "@/context/modal";
import { useCardano } from "@/context/wallet";
import { connectedWallet } from "@/utils/connectedWallet";
import { stakeHexToBech32 } from "@/utils/drepDashboard";
import { networkLabel } from "@/utils/walletConnect";

import { Callout, PrimaryAction, WalletRow, WalletStateFrame } from "./walletParts";

export type DisconnectWalletState = { onConfirm: () => void | Promise<void> };

/** Asks before the wallet menu disconnects the wallet. */
export const DisconnectWalletModal = forwardRef<HTMLDivElement>((_, ref) => {
  const { t } = useTranslation();
  const { state, closeModal } = useModal<DisconnectWalletState>();
  const { stakeKey } = useCardano();
  const { network } = useAppContext();
  const wallet = connectedWallet();
  const name = wallet?.name ?? "";
  const address = stakeKey ? stakeHexToBech32(stakeKey) ?? stakeKey : "";
  return (
    <ModalWrapper ref={ref} dataTestId="disconnect-wallet-modal" hideCloseButton onClose={closeModal} sx={{ maxWidth: 500, p: 3 }}>
      <Box aria-labelledby="disconnect-wallet-title" aria-modal="true" role="dialog">
        <WalletStateFrame
          body={t("walletConnect.disconnect.body")}
          footer={(
            <>
              <PrimaryAction
                danger
                onClick={async () => {
                closeModal();
                await state?.onConfirm();
              }}
                testId="confirm-disconnect-button"
              >
                <LogOut aria-hidden size={15} />
                {t("walletConnect.disconnect.confirm")}
              </PrimaryAction>
              <PrimaryAction onClick={closeModal} testId="cancel-disconnect-button">
                {t("walletConnect.disconnect.cancel")}
              </PrimaryAction>
            </>
        )}
          icon={LogOut}
          onClose={closeModal}
          status={t("walletConnect.disconnect.status")}
          title={t("walletConnect.disconnect.title", { wallet: name || t("dashboard.wallet.walletName", { name: "" }).trim() })}
          titleId="disconnect-wallet-title"
          tone="warning"
        >
          <WalletRow
            detail={address ? `${address.slice(0, 10)}…${address.slice(-6)}` : undefined}
            icon={wallet?.icon}
            name={name || t("dashboard.wallet.title")}
            tag={networkLabel(network)}
          />
          <Box component="ul" sx={{ backgroundColor: ds.gray["25"], borderRadius: `${RADIUS.md}px`, display: "flex", flexDirection: "column", gap: 1, listStyle: "none", m: 0, px: 2, py: 1.5 }}>
            <Box component="li" sx={{ alignItems: "center", color: ds.gray["800"], display: "flex", fontSize: 14, gap: 1 }}>
              <Unlink aria-hidden color={ds.error["600"]} size={16} />
              {t("walletConnect.disconnect.accessEnds")}
            </Box>
            <Box component="li" sx={{ alignItems: "center", color: ds.gray["800"], display: "flex", fontSize: 14, gap: 1 }}>
              <ShieldCheck aria-hidden color={ds.success["600"]} size={16} />
              {t("walletConnect.disconnect.chainUnchanged")}
            </Box>
          </Box>
          <Callout body={t("walletConnect.disconnect.warningBody")} title={t("walletConnect.disconnect.warningTitle")} tone="warning" />
        </WalletStateFrame>
      </Box>
    </ModalWrapper>
  );
});
