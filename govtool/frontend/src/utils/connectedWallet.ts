import { getItemFromLocalStorage, WALLET_LS_KEY } from "./localStorage";

type WalletExtension = { name?: unknown; icon?: unknown };

/**
 * The connected wallet's display name and icon, from the CIP-30 extension it
 * was enabled through. The icon is the extension's own; only an image data
 * URL or an https URL is used, since it ends up in an img src.
 */
export const connectedWallet = () => {
  let key: unknown;
  try {
    key = getItemFromLocalStorage(`${WALLET_LS_KEY}_name`);
  } catch {
    return undefined;
  }
  if (!key || typeof key !== "string") return undefined;
  const extensions = (window as unknown as { cardano?: Record<string, WalletExtension> }).cardano;
  const extension = extensions?.[key];
  const raw = typeof extension?.name === "string" && extension.name ? extension.name : key;
  const icon =
    typeof extension?.icon === "string" && /^(data:image\/|https:\/\/)/i.test(extension.icon)
      ? extension.icon
      : undefined;
  return { name: raw.charAt(0).toUpperCase() + raw.slice(1), icon };
};
