import { useCardano } from "@context";
import { checkIsWalletConnected } from "@utils";

/**
 * Whether a page shows the dashboard layout: a wallet is enabled, or one is
 * saved and still reconnecting after a reload. A failed reconnect clears the
 * saved wallet, so the page then falls back to the public layout.
 */
export const useIsWalletConnected = () => {
  const { isEnabled } = useCardano();
  return isEnabled || checkIsWalletConnected();
};
