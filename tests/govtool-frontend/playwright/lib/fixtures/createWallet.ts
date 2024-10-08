import {
  CardanoTestWallet,
  CardanoTestWalletConfig,
  CardanoTestWalletJson,
} from "@cardanoapi/cardano-test-wallet/types";
import { ShelleyWallet } from "@helpers/crypto";
import { Page } from "@playwright/test";
import { StaticWallet } from "@types";

export default async function createWallet(
  page: Page,
  config?: CardanoTestWalletConfig
) {
  const wallet = (await ShelleyWallet.generate()).json();
  await updateWalletConfig(page, config, wallet);
}

export async function updateWalletConfig(
  page: Page,
  config?: CardanoTestWalletConfig,
  wallet?: StaticWallet
) {
  const initScriptArgs: {
    wallet: CardanoTestWalletJson;
    config: CardanoTestWalletConfig;
  } = {
    wallet,
    config: config,
  };

  await page.addInitScript(({ wallet, config }) => {
    window["cardanoTestWallet"] = {
      ...window["cardanoTestWallet"],
      wallet: wallet,
    } as CardanoTestWallet;
    if (config) {
      window["cardanoTestWallet"]["config"] = {
        ...window["cardanoTestWallet"]["config"],
        ...config,
      } as CardanoTestWallet;
    }
  }, initScriptArgs);
}
