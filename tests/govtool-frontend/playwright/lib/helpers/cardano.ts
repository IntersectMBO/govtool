import { bech32 } from "bech32";

export function lovelaceToAda(lovelace: number) {
  if (lovelace === 0) return 0;

  return lovelace / 1e6;
}

export function generateWalletAddress() {
  const randomBytes = new Uint8Array(10);
  return bech32.encode("addr_test", randomBytes);
}
