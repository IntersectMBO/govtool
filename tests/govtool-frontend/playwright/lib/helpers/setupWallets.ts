import { faucetWallet } from "@constants/staticWallets";
import { ShelleyWallet } from "@helpers/crypto";
import kuberService from "@services/kuberService";
import { pollTransaction } from "./transaction";

/*
Registers stake & fund wallets
*/
export default async function setupWallets(wallets: ShelleyWallet[]) {
  if (wallets.length === 0) {
    throw new Error("No wallets to load balance");
  }

  const signingKey = faucetWallet.payment.private;
  const { txId, address } = await kuberService.initializeWallets(
    faucetWallet.address,
    signingKey,
    wallets
  );
  await pollTransaction(txId, address);

  console.debug(`[Setup Wallet] Successfully setup ${wallets.length} wallets`);
}
