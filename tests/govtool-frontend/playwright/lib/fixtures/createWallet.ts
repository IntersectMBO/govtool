import { ShelleyWallet } from "@mock/cardano-test-wallet/crypto";
import {
  CardanoTestWallet,
  CardanoTestWalletConfig,
} from "@mock/cardano-test-wallet/types";
import { Page } from "@playwright/test";

export default async function createWallet(
  page: Page,
  config?: CardanoTestWalletConfig,
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
    // @ts-ignore
    window.cardanoTestWallet.wallet = wallet;
    //@ts-ignore
    window.cardanoTestWallet.config = config;
  }, initScriptArgs);
}
