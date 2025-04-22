import {
  CardanoTestWallet,
  CardanoTestWalletConfig,
} from "@cardanoapi/cardano-test-wallet/types";
import environments from "@constants/environments";
import { Page } from "@playwright/test";

import path = require("path");

export default async function loadDemosExtension(
  page: Page,
  enableStakeSigning?: boolean,
  enableDRepSigning?: boolean,
  supportedExtensions?: Record<string, number>[]
) {
  const demosBundleScriptPath = path.resolve(
    __dirname,
    "../../node_modules/@cardanoapi/cardano-test-wallet/script.js"
  );
  let walletConfig: CardanoTestWalletConfig = {
    enableStakeSigning,
    enableDRepSigning,
    kuberApiUrl: environments.kuber.apiUrl,
    networkId: environments.networkId,
    kuberApiKey: environments.kuber.apiKey,
    blockfrostApiKey: environments.blockfrostApiKey,
    blockfrostApiUrl: environments.blockfrostApiUrl,
  };
  await page.addInitScript(
    ({ walletConfig, supportedExtensions }) => {
      window["cardanoTestWallet"] = {
        walletName: "demos",
        supportedExtensions,
        config: walletConfig,
      } as CardanoTestWallet;
    },
    { walletConfig, supportedExtensions }
  );

  await page.addInitScript({ path: demosBundleScriptPath });
}
