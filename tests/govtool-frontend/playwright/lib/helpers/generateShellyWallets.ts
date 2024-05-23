import { ShelleyWallet } from "./crypto";

export default async function generateShellyWallets(
  numWallets: number = 100
): Promise<ShelleyWallet[]> {
  const wallets: ShelleyWallet[] = [];

  for (let i = 0; i < numWallets; i++) {
    const wallet = await ShelleyWallet.generate();

    wallets.push(wallet);
  }

  return wallets;
}
