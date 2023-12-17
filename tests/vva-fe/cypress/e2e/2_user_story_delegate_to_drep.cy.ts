import { generateDRep, getDRepIDHash, isMobile } from "../support/utils";
import {
  connectWallet,
  disconnectAndReconnectWallet,
  loadWallet,
  openMobileDrawer,
  viewProposalDetails,
} from "./actions/commonActions";
import {
  checkForDelegationConfirmation,
  clickDelegateButton,
  delegateToDRepId,
  delegateToMyself,
} from "./actions/delegateActions";
import {
  adaHolderWallets,
  dRepWallets,
  referenceWallets,
} from "../constants/wallet";
import { retireDRep } from "./actions/dRepActions";

describe("Delegation to DRep", () => {
  beforeEach(() => {
    cy.visit(Cypress.env("baseUrl"));
  });

  it("2A. Should show delegated DRepId on dashboard after delegating", () => {
    loadWallet(adaHolderWallets.adaHolder1, { withStakeSigning: true });
    clickDelegateButton();
    delegateToDRepId(referenceWallets.dRep1.dRepId);
    checkForDelegationConfirmation(referenceWallets.dRep1.dRepId);
  });

  // Given that I'm an Ada-Holder
  it("2B. Should access delegation to dRep page", () => {
    connectWallet();
    cy.getBySel("delegate-button").click();
    cy.contains("Delegate to dRep", { matchCase: false });
  });

  // Given that I'm not connected to the wallet
  // User visits the DRep delegation page
  it("2C. Verify DRep Behavior in Disconnected State", () => {
    cy.getBySel("delegate-connect-wallet-button").click();
    cy.getBySel("connect-your-wallet-modal").should("be.visible");
  });

  // Given that I'm connected to the wallet as ADA-Holder
  // User visits the DRep delegation page
  // All delegation actions should be accessible
  it("2D. Verify DRep Behavior in Connected State", () => {
    loadWallet(referenceWallets.dRep1);
    clickDelegateButton();

    // Verifying Delegation options card
    cy.getBySel("delegate-to-drep-card").should("be.visible");
    cy.getBySel("delegate-to-myself-card").should("be.visible");
    cy.contains("Other options").click();
    cy.getBySel("signal-no-confidence-card").should("be.visible");
    cy.getBySel("vote-abstain-card").should("be.visible");

    // Verifying dRepId delegation
    cy.getBySel("delegate-to-drep-card").click();
    cy.getBySel("next-step-button").click();
    cy.getBySel("dRep-id-input").should("be.visible");
    cy.getBySel("delegate-button").should("be.visible");
  });

  it("2E. Should delegate to myself", () => {
    loadWallet(dRepWallets.dRep1, { withStakeSigning: true });
    clickDelegateButton();
    delegateToMyself();
    checkForDelegationConfirmation(dRepWallets.dRep1.dRepId);
  });

  // Given that ADA-Holder has already delegated to a DRep
  it("2F. Should change delegated DRep", () => {
    loadWallet(adaHolderWallets.adaHolder2, { withStakeSigning: true });
    // delegate
    clickDelegateButton();
    delegateToDRepId(referenceWallets.dRep1.dRepId);
    checkForDelegationConfirmation(referenceWallets.dRep1.dRepId);
    // change delegation
    clickDelegateButton();
    delegateToDRepId(referenceWallets.dRep2.dRepId);
    checkForDelegationConfirmation(referenceWallets.dRep2.dRepId);
  });

  // Given I'm already registered as DRep
  // When pressing retire as dRep button, wallet asks to sign the tx
  it("2G. Should verify retire as a DRep", () => {
    loadWallet(dRepWallets.dRep1, { withStakeSigning: true });
    cy.getBySel("retire-button").should("be.enabled");
    cy.getBySel("retire-button").click();
    cy.intercept("**/tx/submit").as("submitTx");
    cy.wait("@submitTx").then((res) => {
      expect(
        res.response.statusCode,
        `[Submit Tx error]: ${res.response.body.message}`
      ).eq(200);
    });
  });

  // Given that the I'm connected to the wallet with the registered dRep account
  it("2H. Verify DRep behavior in Retired State", () => {
    loadWallet(dRepWallets.dRep2, { withStakeSigning: true });
    retireDRep();
    disconnectAndReconnectWallet(null, null, { withStakeSigning: true });
    if (isMobile()) {
      openMobileDrawer();
    }
    cy.getBySel("governance-actions-link").click();
    viewProposalDetails();
    cy.getBySel("vote-button").should("not.exist");
  });

  // Given that I'm an Ada-Holder
  it("2I. Should check validity of DRep Id", () => {
    connectWallet();
    const dRep = generateDRep();

    cy.getBySel("delegate-button").click();
    // not registered dRep id
    cy.getBySel("delegate-to-drep-card").click();
    cy.getBySel("next-step-button").click();
    cy.getBySel("dRep-id-input").type(getDRepIDHash(dRep.id));
    cy.getBySel("delegate-button").click();
    cy.contains("DrepId not found", { matchCase: false }).should("exist");
    cy.getBySel("confirm-modal-button").first().click({ force: true });
    cy.getBySel("dRep-id-input").clear();
    // registered dRep id
    cy.getBySel("dRep-id-input").type(
      getDRepIDHash(referenceWallets.dRep1.dRepId)
    );
    cy.getBySel("delegate-button").click();
    cy.contains("DrepId not found", { matchCase: false }).should("not.exist");
  });
});
