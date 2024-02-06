import { Logger } from "../lib/logger/logger";
import { ShelleyWallet } from "../lib/wallet/crypto";
import {
  convertUint8ArrayToHex,
  getWebAppNetworkId,
  isMobile,
} from "../support/utils";
import { connectWallet, openMobileDrawer } from "./actions/commonActions";

describe("Wallet Connection", () => {
  beforeEach(() => {
    cy.visit(Cypress.env("baseUrl"));
  });

  it("1A. Should connect wallet and choose stake-key to use", () => {
    cy.wrap(0).then(async () => {
      const shellyWallet1 = await ShelleyWallet.generate();
      const shellyWallet2 = await ShelleyWallet.generate();
      connectWallet({
        getRegisteredPubStakeKeys: [
          convertUint8ArrayToHex(shellyWallet1.stakeKey.public),
          convertUint8ArrayToHex(shellyWallet2.stakeKey.public),
        ],
        getRewardAddresses: [
          convertUint8ArrayToHex(shellyWallet1.rewardAddressRawBytes(0)),
          convertUint8ArrayToHex(shellyWallet2.rewardAddressRawBytes(0)),
        ],
      });
    });
  });

  it("1B. Should connect wallet with single stake key", () => {
    connectWallet({});
  });

  it("1C. Should disconnect Wallet When connected ", () => {
    connectWallet({});

    if (isMobile()) {
      openMobileDrawer();
    }
    cy.getBySel("disconnect-button").click();
    cy.getBySel("disconnect-button").should("not.exist");
  });

  it("1D. Should check correct network (Testnet/Mainnet) on connection", () => {
    const networkId = getWebAppNetworkId();
    const wrongNetwork = networkId == 0 ? 1 : 0;
    cy.on("uncaught:exception", (err, runnable) => {
      Logger.success("Network mismatch error caught");
      return false;
    });
    cy.on("fail", (err, runnable) => {
      Logger.success("Assertion error caught");
      return false;
    });
    connectWallet({ getNetworkId: wrongNetwork });
  });
});
