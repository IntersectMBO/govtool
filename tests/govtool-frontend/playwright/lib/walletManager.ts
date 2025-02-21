import { StaticWallet } from "@types";
import * as fs from "fs";
import { LockInterceptor } from "./lockInterceptor";
const path = require("path");

const baseFilePath = path.resolve(__dirname, "./_mock");

export type Purpose =
  | "registerDRep"
  | "registeredDRep"
  | "proposalSubmission"
  | "proposalSubmissionCopy"
  | "registerDRepCopy"
  | "registeredDRepCopy";

/**
 * WalletManager class is responsible for managing a list of temporary wallets.
 * It ensures that each wallet is used only once across all tests.
 */
class WalletManager {
  private static instance: WalletManager;

  public static getInstance(): WalletManager {
    if (!WalletManager.instance) {
      WalletManager.instance = new WalletManager();
    }
    return WalletManager.instance;
  }

  async writeWallets(wallets: StaticWallet[], purpose: Purpose) {
    await new Promise<void>((resolve, reject) =>
      fs.writeFile(
        `${baseFilePath}/${purpose}Wallets.json`,
        JSON.stringify(wallets, null, 2),
        (err) => {
          if (err) {
            reject(err);
          } else {
            resolve();
          }
        }
      )
    );
  }

  async readWallets(purpose: Purpose): Promise<StaticWallet[]> {
    const data: string = await new Promise((resolve, reject) =>
      fs.readFile(
        `${baseFilePath}/${purpose}Wallets.json`,
        "utf8",
        (err, data) => {
          if (err) {
            reject(err);
          } else {
            resolve(data);
          }
        }
      )
    );
    return JSON.parse(data);
  }

  async removeCopyWallet(walletToRemove: StaticWallet, purpose: Purpose) {
    const currentWallets = await this.readWallets(purpose);
    const updatedWallets = currentWallets.filter(
      (wallet) => wallet.address !== walletToRemove.address
    );
    await this.writeWallets(updatedWallets, purpose);
  }

  async popWallet(purpose: Purpose): Promise<StaticWallet> {
    const popCb = async () => {
      const wallets = await this.readWallets(purpose);
      if (wallets.length === 0) {
        throw new Error("No more wallets available");
      }
      const wallet = wallets.pop();
      await this.writeWallets(wallets, purpose);

      await LockInterceptor.releaseLock("tempWallets");
      return wallet;
    };

    return await LockInterceptor.intercept<StaticWallet>("tempWallets", popCb);
  }
}
export default WalletManager.getInstance();
