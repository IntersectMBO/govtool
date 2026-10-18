import { WALLET_LS_KEY, getItemFromLocalStorage } from "@/utils/localStorage";

export type PdfWalletStatus = "disconnected" | "connecting" | "ready";

type WalletState = {
  isEnabled: boolean;
  isEnableLoading: string | null;
  address?: string;
  stakeKey?: string;
};

/**
 * The wallet status pdf-ui acts on. `connecting` means "keep the session,
 * but do not sign or validate yet"; only `disconnected` clears it.
 * The saved wallet name is read on every call because App's extension wait
 * removes it without a state change.
 */
export const getPdfWalletStatus = ({
  isEnabled,
  isEnableLoading,
  address,
  stakeKey,
}: WalletState): PdfWalletStatus => {
  if (isEnabled && !isEnableLoading && !!address && !!stakeKey) return "ready";
  if (
    isEnableLoading ||
    isEnabled ||
    !!getItemFromLocalStorage(`${WALLET_LS_KEY}_name`)
  ) {
    return "connecting";
  }
  return "disconnected";
};
