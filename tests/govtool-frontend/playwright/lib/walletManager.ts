import { StaticWallet } from "@types";
import * as fs from "fs";
import { LockInterceptor } from "./lockInterceptor";
const path = require("path");

const filePath = path.resolve(__dirname, "./_mock/tempWallets.json");

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

  async writeWallets(wallets: StaticWallet[]) {
    await new Promise<void>((resolve, reject) =>
      fs.writeFile(filePath, JSON.stringify(wallets, null, 2), (err) => {
        if (err) {
          reject(err);
        } else {
          resolve();
        }
      })
    );
  }

  private async readWallets(): Promise<StaticWallet[]> {
    const data: string = await new Promise((resolve, reject) =>
      fs.readFile(filePath, "utf8", (err, data) => {
        if (err) {
          reject(err);
        } else {
          resolve(data);
        }
      })
    );
    return JSON.parse(data);
  }

  async popWallet(id: string): Promise<StaticWallet> {
    const popCb = async () => {
      const wallets = await this.readWallets();
      if (wallets.length === 0) {
        throw new Error("No more wallets available");
      }
      const wallet = wallets.pop();
      await this.writeWallets(wallets);

      await LockInterceptor.releaseLock("tempWallets", id);
      return wallet;
    };

    return await LockInterceptor.intercept<StaticWallet>(
      "tempWallets",
      id,
      popCb
    );
  }
}
export default WalletManager.getInstance();
