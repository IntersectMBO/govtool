// Helpers for the wallet connection states. wallet.tsx reports a failed
// connection as text, so the state is read from it.

export type WalletErrorKind =
  | { kind: "network"; walletNetwork: string; requiredNetwork: string }
  | { kind: "declined" }
  | { kind: "unsupported" }
  | { kind: "generic" };

const NETWORK_MISMATCH =
  /trying to connect with a wallet connected to (\w+).*?connect to (\w+)/i;

export const classifyWalletError = (error?: string | null): WalletErrorKind => {
  const text = error ?? "";
  const network = text.match(NETWORK_MISMATCH);
  if (network) {
    return { kind: "network", walletNetwork: network[1], requiredNetwork: network[2] };
  }
  // "user canceled", not any "canceled": maintenance mode says "Action canceled".
  if (/declin|reject|refus|denied|user\s+cancel/i.test(text)) return { kind: "declined" };
  if (/CIP-?(30|90|95)|does not support/i.test(text)) return { kind: "unsupported" };
  return { kind: "generic" };
};

const LABELS: Record<string, string> = {
  mainnet: "Cardano Mainnet",
  preprod: "Cardano Preprod",
  preview: "Cardano Preview",
  sanchonet: "SanchoNet",
  testnet: "Cardano Testnet",
};

/** "Cardano Mainnet" for the network names the backend and wallet.tsx use. */
export const networkLabel = (network?: string | null) =>
  LABELS[(network ?? "").toLowerCase()] ?? `Cardano ${network ?? ""}`.trim();

/** Wallets GovTool suggests when none is installed, with their official sites. */
export const KNOWN_WALLETS = [
  { name: "Lace", url: "https://www.lace.io/" },
  { name: "Eternl", url: "https://eternl.io/" },
  { name: "Vespr", url: "https://vespr.xyz/" },
];
