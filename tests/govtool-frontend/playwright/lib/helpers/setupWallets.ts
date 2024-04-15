import kuberService from "@services/kuberService";
import { ShelleyWallet } from "@mock/cardano-test-wallet/crypto";
import pollTransaction from "./pollTransaction";
import { faucetWallet } from "@constants/staticWallets";

/*
Registers stake & fund wallets
*/
export default async function setupWallets(wallets: ShelleyWallet[]) {
  if (wallets.length === 0) {
    throw new Error("No wallets to load balance");
  }

  const signingKey = faucetWallet.payment.private;
  const { txId } = await kuberService.initializeWallets(
    faucetWallet.address,
    signingKey,
    wallets,
  );
  await pollTransaction(txId);

  console.debug(`[Setup Wallet] Successfully setup ${wallets.length} wallets`);
}
