import { ShelleyWallet } from "../../lib/wallet/crypto";
import {
  CipMethodOverride,
  StaticWallet,
  WalletParams,
} from "../../lib/wallet/types";
import { convertUint8ArrayToHex } from "../../support/utils";
import { disconnectAndReconnectWallet } from "./commonActions";
import kuberService from "../../services/kuberService";
import { pollTxWithKuber } from "../../services/apiService";

export function cleanUpStaticWallet(
  walletJson: StaticWallet,
  overrides?: CipMethodOverride,
  walletParams?: WalletParams
) {
  cy.log(`Check for ${walletJson.type} cleanup.....`);
  if (walletJson.type == "dRep") {
    cy.get("body").then((bodyElement) => {
      const generatedWallet = ShelleyWallet.fromJson(walletJson);
      if (bodyElement.find('[data-testid="dRep-id-display"]').length == 0) {
        cy.log("DRep clean up process (DRep registration).....");
        registerDRepAndReconnect(generatedWallet, overrides, walletParams);
      }
      if (bodyElement.find('[data-testid="delegated-to-drep-id"]').length > 0) {
        cy.log("AdaHolder clean up process (Abstain delegation).....");
        abstainDelegationAndReconnect(generatedWallet, overrides, walletParams);
      } else if (
        bodyElement.find(":contains('ABSTAIN')").length === 0 &&
        bodyElement.find('[data-testid="change-dRep-button"]').length > 0
      ) {
        cy.log("AdaHolder clean up process (Abstain delegation).....");
        abstainDelegationAndReconnect(generatedWallet, overrides, walletParams);
      }
    });
  } else if (walletJson.type == "adaHolder") {
    cy.get("body").then({ timeout: 2 * 60 * 1000 }, (bodyElement) => {
      const generatedWallet = ShelleyWallet.fromJson(walletJson);
      if (bodyElement.find('[data-testid="dRep-id-display"]').length > 0) {
        cy.log("AdaHolder clean up process (DRep de-registration).....");
        deRegisterDRepAndReconnect(generatedWallet, overrides, walletParams);
      }
      if (bodyElement.find('[data-testid="delegated-to-drep-id"]').length > 0) {
        cy.log("AdaHolder clean up process (Abstain delegation).....");
        abstainDelegationAndReconnect(generatedWallet, overrides, walletParams);
      }
    });
  }
}

function deRegisterDRepAndReconnect(wallet, overrides, walletParams) {
  cy.wrap(
    kuberService.dRepDeRegistration(
      wallet.addressBech32(0),
      convertUint8ArrayToHex(wallet.paymentKey.private),
      convertUint8ArrayToHex(wallet.stakeKey.private),
      convertUint8ArrayToHex(wallet.stakeKey.pkh)
    )
  ).then({ timeout: 5 * 60 * 1000 }, async (tx: any) => {
    await pollTxWithKuber(tx.txId);
    cy.wait(5000); // Wait for db sync
    disconnectAndReconnectWallet(null, overrides, walletParams);
  });
}

function registerDRepAndReconnect(wallet, overrides, walletParams) {
  cy.wrap(
    kuberService.dRepRegistration(
      wallet.addressBech32(0),
      convertUint8ArrayToHex(wallet.paymentKey.private),
      convertUint8ArrayToHex(wallet.stakeKey.pkh)
    )
  ).then({ timeout: 5 * 60 * 1000 }, async (tx: any) => {
    await pollTxWithKuber(tx.txId);
    cy.wait(5000); // Wait for db sync
    disconnectAndReconnectWallet(null, overrides, walletParams);
  });
}

function abstainDelegationAndReconnect(wallet, overrides, walletParams) {
  cy.wrap(
    kuberService.stakeDelegation(
      wallet.addressBech32(0),
      convertUint8ArrayToHex(wallet.paymentKey.private),
      convertUint8ArrayToHex(wallet.stakeKey.private),
      convertUint8ArrayToHex(wallet.stakeKey.pkh),
      "abstain"
    )
  ).then({ timeout: 5 * 60 * 1000 }, async (tx: any) => {
    await pollTxWithKuber(tx.txId);
    cy.wait(5000); // Wait for db sync
    disconnectAndReconnectWallet(null, overrides, walletParams);
  });
}
