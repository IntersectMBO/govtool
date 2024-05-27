import { adaHolder01Wallet } from "@constants/staticWallets";
import walletManager from "lib/walletManager";

async function globalSetup() {
  walletManager.writeWallets(Array(40).fill(adaHolder01Wallet));
}

export default globalSetup;
