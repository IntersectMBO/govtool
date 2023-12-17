//import { cloudPlugin } from "cypress-cloud/plugin";

module.exports = {
  projectId: "wg4iq9",
  chromeWebSecurity: false,
  // experimentalModifyObstructiveThirdPartyCode: true,
  env: {
    baseUrl: "https://vva-sanchonet.cardanoapi.io",
    apiUrl: "http://vva-sanchonet.cardanoapi.io/api",
    disableSecurityTest: false, // set to true for github-ci
    networkId: "testnet",
    mobileViewportWidthBreakpoint: 414,
    sanchoNet: true,
    faucetApiUrl: "https://faucet.sanchonet.world.dev.cardano.org",
    blockfrostUrl: "https://cardano-sanchonet.blockfrost.io/api/v0",
    blockfrostApiKey: "xxxxxxxxxxxxxxxxxxxx",
    kuberApiUrl: "https://sanchonet.kuber.cardanoapi.io",
    kuberApiKey: "xxxxxxxxxxxxxx",
  },
  e2e: {
    supportFile: "./cypress/support/e2e.ts",
    setupNodeEvents(on, config) {
      on("before:browser:launch", (browser, launchOptions) => {
        if (browser.name == "chrome") {
          launchOptions.args.push("--disable-gpu");
        }
        return launchOptions;
      });
      //return cloudPlugin(on, config);
    },
  },
};