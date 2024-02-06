import { bootstrapWallet } from "../../constants/wallet";
import { pollTxWithKuber } from "../../services/apiService";
import kuberService from "../../services/kuberService";
import { Logger } from "../logger/logger";
import { ShelleyWallet } from "./crypto";

export async function generateWallets(
  numWallets: number = 100
): Promise<ShelleyWallet[]> {
  const wallets: ShelleyWallet[] = [];

  for (let i = 0; i < numWallets; i++) {
    const wallet = await ShelleyWallet.generate();
    wallets.push(wallet);
  }

  return wallets;
}
// check if the stake is registered

export async function setupWallets(wallets: ShelleyWallet[]) {
  if (wallets.length === 0) {
    throw new Error("No wallets to load balance");
  }

  const signingKey = bootstrapWallet.payment.private;
  await kuberService.initializeMultipleWallets(
    bootstrapWallet.address,
    signingKey,
    wallets
  );
  Logger.success(`[Setup Wallet] Successfully setup ${wallets.length} wallets`);
}

export async function registerStake(wallet: ShelleyWallet) {
  const { stakeKey, paymentKey } = wallet;
  const hexPaymentObj = {
    pkh: Buffer.from(paymentKey.pkh).toString("hex"),
    private: Buffer.from(paymentKey.private).toString("hex"),
    public: Buffer.from(paymentKey.public).toString("hex"),
  };
  const hexStakeObj = {
    pkh: Buffer.from(stakeKey.pkh).toString("hex"),
    private: Buffer.from(stakeKey.private).toString("hex"),
    public: Buffer.from(stakeKey.public).toString("hex"),
  };
  const address = wallet.addressBech32(0);

  try {
    const { cbor, txId } = await kuberService
      .registerStake(
        hexStakeObj.private,
        hexStakeObj.pkh,
        hexPaymentObj.private,
        address
      )
      .catch((err) => {
        if (err.status === 400) {
          throw new Error("Stake already registered");
        }
        throw new Error(err);
      });
    await pollTxWithKuber(txId);
  } catch (err) {
    throw new Error(err);
  }
}
