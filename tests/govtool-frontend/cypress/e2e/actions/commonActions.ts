import { mkWallet } from "../../lib/wallet/mockWallet";
import {
  CIP30Provider,
  CipMethodOverride,
  StaticWallet,
  WalletParams,
} from "../../lib/wallet/types";
import { validateFilter } from "../../support/validations";
import { ShelleyWallet, ShelleyWalletAddress } from "../../lib/wallet/crypto";
import { mkCardanoWalletExtension } from "../../lib/wallet/cardanoWallet";
import walletState from "../../state/walletState";
import { destructWalletArgs, isMobile } from "../../support/utils";

import { cleanUpStaticWallet } from "./cleanUpActions";
import { pollTxWithKuber } from "../../services/apiService";
import kuberService from "../../services/kuberService";
import { bootstrapWallet } from "../../constants/wallet";

export function injectWallet(wallet: CIP30Provider) {
  cy.window().then((w: any) => {
    w.cardano = {
      demos: wallet || mkWallet(),
    };
  });
}

export function injectCardanoWallet(wallet: Promise<CIP30Provider>) {
  cy.window().then(async (w: any) => {
    w.cardano = {
      demos: await wallet,
    };
  });
}

export async function initiateWalletConnection(
  overrides?: CipMethodOverride,
  walletParams?: WalletParams
): Promise<CIP30Provider> {
  const walletToInjected = mkCardanoWalletExtension(overrides, walletParams);
  injectCardanoWallet(walletToInjected);
  return await walletToInjected;
}

export async function initializeWallet(
  wallet: ShelleyWallet,
  overrides?: CipMethodOverride,
  walletParams?: WalletParams
): Promise<CIP30Provider> {
  return await mkCardanoWalletExtension(overrides, walletParams, wallet);
}

export function loadAndInjectWallet(
  wallet: ShelleyWallet,
  overrides?: CipMethodOverride,
  walletParams?: WalletParams
) {
  injectCardanoWallet(initializeWallet(wallet, overrides, walletParams));
}

export function filterGovActions(
  govActions: string[],
  onFilterCallback?: () => void,
  element = "div"
) {
  for (const govAction of govActions) {
    cy.get(element)
      .contains(govAction, { matchCase: false })
      .parent()
      .within(() => {
        cy.get("input").click();
        // cy.getBySel(`${govAction.replace(/ /g, "")}-checkbox`).click();
      });
  }
  onFilterCallback();
  for (const govAction of govActions) {
    validateFilter(govAction);
  }
  // Uncheck the filter after it's completed
  for (const govAction of govActions) {
    cy.get(element)
      .contains(govAction, { matchCase: false })
      .parent()
      .within(() => {
        cy.getBySel(`${govAction.replace(/ /g, "")}-checkbox`).uncheck();
      });
  }
}

export function sortElementByText(text: string, element = "span") {
  cy.get(element)
    .contains(text, { matchCase: false })
    .parent()
    .within(() => {
      cy.get("input").check();
    });
}

export function openMobileDrawer() {
  cy.getBySel("open-drawer-button").click({ force: true });
}

export function checkInvalidityOfDRepId(invalidDRepId: string) {
  cy.getBySel("delegate-button").click();
  cy.getBySel("delegate-to-drep-card").click();
  cy.getBySel("next-step-button").click();
  cy.get("input").type(invalidDRepId);
  cy.getBySel("delegate-button").click();
  cy.getBySel("delegation-transaction-error-modal").should("exist");
}

export function viewProposalDetails() {
  cy.get('[data-testid^="govaction-"][data-testid$="-view-detail"]')
    .first()
    .click();
}

export function disconnectAndReconnectWallet(
  walletToInject?: ShelleyWallet,
  overrides?: CipMethodOverride,
  walletParams?: WalletParams
) {
  if (isMobile()) {
    openMobileDrawer();
  }
  cy.getBySel("disconnect-button").click();
  cy.wrap(
    loadAndInjectWallet(
      walletToInject || walletState.getWallet(),
      overrides,
      walletParams
    )
  );
  cy.clearCookie(Cypress.env("baseUrl"));
  cy.clearLocalStorage();
  cy.getBySel("connect-wallet-button").wait(100).first().click();
  cy.getBySel("demos-wallet-button").eq(1).click({ force: true });
  cy.getBySel("confirm-modal-button").first().click({ force: true }); // accept the info about sancho net};
  cy.wait(6000); // Waiting for wallet initialization
}
export function waitForTxToComplete() {
  cy.wrap(0).then({ timeout: 5 * 60 * 1000 }, async () => {
    console.log("Waiting for Tx to complete");
    const txHash = walletState.getTxHash();
    if (txHash === undefined) return;

    console.debug("Waiting for tx: " + txHash);
    await pollTxWithKuber(txHash);
  });
}

export function connectWallet(args?: CipMethodOverride & WalletParams) {
  const { overrides, walletParams } = destructWalletArgs(args);
  cy.wrap(initiateWalletConnection(overrides, walletParams)).then(
    { timeout: 5 * 60 * 1000 },
    async (injectedWallet: CIP30Provider) => {
      cy.getBySel("connect-wallet-button").wait(100).first().click();
      cy.getBySel("demos-wallet-button").eq(1).click({ force: true });
      // cy.getBySel("demos-wallet-button").click();
      cy.getBySel("confirm-modal-button").first().click({ force: true }); // accept the info about sancho net
      const walletInstance = await injectedWallet.enable();

      // load amount from faucet
      if (args && walletParams.loadFundsAndRegister) {
        console.debug("Loading faucet...");
        const shellyWalletAddress = ShelleyWalletAddress.fromRawBytes(
          await walletInstance.getChangeAddress()
        );
        const { txId } = await loadAmountFromBootStrapWallet([
          shellyWalletAddress.toBech32(),
        ]);
        await pollTxWithKuber(txId);
        // await pollTx(txId);
      }
      // Handle multiple stake keys
      if ("cip95" in walletInstance) {
        const stakeKeys =
          await walletInstance.cip95.getRegisteredPubStakeKeys();
        const rewardAddress = await walletInstance.getRewardAddresses();
        if (stakeKeys.length > 1) {
          console.log("Stake Keys:", stakeKeys);
          console.log("Reward address:", rewardAddress);
          cy.getBySel(`${rewardAddress[0]}-radio`);
          cy.contains("Voting power").click();
          cy.getBySel("select-button").click();
        }
      }
      cy.getBySel("alert-success", { timeout: 6000 }).should("be.visible");
      cy.wait(6000); // Waiting for wallet initialization
    }
  );
}

export const loadWallet = (
  walletJson: StaticWallet,
  args?: CipMethodOverride & WalletParams
) => {
  cy.log(`Loading ${walletJson.type} wallet`);
  const { overrides, walletParams } = destructWalletArgs(args);
  const wallet = ShelleyWallet.fromJson(walletJson);
  cy.wrap(loadAndInjectWallet(wallet, overrides, walletParams));
  cy.log("Wallet injected");
  cy.getBySel("connect-wallet-button").wait(100).first().click();
  cy.getBySel("demos-wallet-button").eq(1).click({ force: true });
  cy.getBySel("confirm-modal-button").first().click({ force: true }); // accept the info about sancho net};
  cy.getBySel("alert-success", { timeout: 6000 }).should("be.visible");
  cy.wait(6000); // Waiting for wallet initialization
  cleanUpStaticWallet(walletJson, overrides, walletParams);
};

export async function loadAmountFromBootStrapWallet(addressList: string[]) {
  return await kuberService.transferADA(
    bootstrapWallet.address,
    addressList,
    bootstrapWallet.payment.private
  );
}
