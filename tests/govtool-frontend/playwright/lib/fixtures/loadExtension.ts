import { CardanoTestWalletConfig } from "@cardanoapi/cardano-test-wallet/types";
import environments from "@constants/environments";
import { Page } from "@playwright/test";

import path = require("path");

export default async function loadDemosExtension(
  page: Page,
  enableStakeSigning = false
) {
  const demosBundleScriptPath = path.resolve(
    __dirname,
    "../../node_modules/@cardanoapi/cardano-test-wallet/script.js"
  );
  let walletConfig: CardanoTestWalletConfig = {
    enableStakeSigning,
    kuberApiUrl: environments.kuber.apiUrl,
    kuberApiKey: environments.kuber.apiKey,
  };
  await page.addInitScript((walletConfig) => {
    window["cardanoTestWallet"] = {
      walletName: "demos",
      config: walletConfig,
    };
  }, walletConfig);

  await page.addInitScript({ path: demosBundleScriptPath });
}
