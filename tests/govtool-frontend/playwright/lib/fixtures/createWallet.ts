import {
  CardanoTestWallet,
  CardanoTestWalletConfig,
} from "@cardanoapi/cardano-test-wallet/types";
import { ShelleyWallet } from "@helpers/crypto";
import { Page } from "@playwright/test";

export default async function createWallet(
  page: Page,
  config?: CardanoTestWalletConfig
) {
  const wallet = (await ShelleyWallet.generate()).json();

  const initScriptArgs: {
    wallet: CardanoTestWallet;
    config: CardanoTestWalletConfig;
  } = {
    wallet,
    config: config,
  };

  await page.addInitScript(({ wallet, config }) => {
    window["cardanoTestWallet"] = {
      ...window["cardanoTestWallet"],
      wallet: wallet,
    };
    if (config) {
      window["cardanoTestWallet"]["config"] = {
        ...window["cardanoTestWallet"]["config"],
        ...config,
      };
    }
  }, initScriptArgs);
}
