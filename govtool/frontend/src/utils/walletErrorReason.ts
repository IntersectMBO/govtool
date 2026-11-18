/**
 * The reason a wallet gave for refusing or failing a transaction. CIP-30
 * wallets reject with a string, an Error or an { code, info } object, and
 * wallet.tsx rethrows `info` when there is one.
 */
export const walletErrorReason = (error: unknown): string => {
  if (typeof error === "string") return error;
  if (error && typeof error === "object") {
    const { message, info } = error as { message?: unknown; info?: unknown };
    if (typeof message === "string" && message) return message;
    if (typeof info === "string" && info) return info;
  }
  return "";
};
