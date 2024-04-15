import { Page } from "@playwright/test";

import path = require("path");

export default async function loadDemosExtension(page: Page) {
  const demosBundleScriptPath = path.resolve(
    __dirname,
    "../../dist/cardano-test-wallet/index.js",
  );

  await page.addInitScript(() => {
    // @ts-ignore
    window.cardanoTestWallet = {};

    // @ts-ignore
    window.cardanoTestWallet.name = "demos";
  });

  await page.addInitScript({ path: demosBundleScriptPath });
}
