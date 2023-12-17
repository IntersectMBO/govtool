import { waitForTxToComplete } from "./commonActions";
import { getDRepIDHash } from "../../support/utils";

const enum DelegationOptions {
  delegateTodRep = "delegate-to-drep-card",
  delegateToMyself = "delegate-to-myself",
}

export function changeDRepDelegation(
  dRepId: string,
  option: DelegationOptions = DelegationOptions.delegateTodRep
) {
  cy.getBySel("change-dRep-button").click();
  delegate(dRepId, option);
  waitForTxToComplete();
}

export function delegateToDRepId(dRepId: string, convertToHash = true) {
  delegate(dRepId, DelegationOptions.delegateTodRep, convertToHash);
}

export function delegateToMyself() {
  delegate("", DelegationOptions.delegateToMyself);
}
function delegate(
  dRepId: string,
  option: DelegationOptions = DelegationOptions.delegateTodRep,
  convertToHash = true
) {
  const hashedDRepId =
    convertToHash && dRepId !== "" ? getDRepIDHash(dRepId) : dRepId;

  if (option === DelegationOptions.delegateTodRep) {
    cy.getBySel("delegate-to-drep-card").click();
    cy.getBySel("next-step-button").click();
    cy.getBySel("dRep-id-input").type(hashedDRepId);
  } else if (option === DelegationOptions.delegateToMyself) {
    cy.getBySel("delegate-to-myself-card").click();
  }
  cy.getBySel("delegate-button").click();
  cy.getBySel("delegation-transaction-submitted-modal").should("be.visible");
  cy.getBySel("confirm-modal-button").first().click({ force: true });
  waitForTxToComplete();
}

export function checkForDelegationConfirmation(dRepId: string) {
  cy.getBySel("change-dRep-button", { timeout: 30 * 1000 }).should("exist");
  cy.contains(dRepId).should("exist");
}

export function clickDelegateButton() {
  cy.get("body").then((bodyElement) => {
    if (bodyElement.find('[data-testid="delegate-button"]').length > 0) {
      cy.getBySel("delegate-button").click();
    } else {
      cy.getBySel("change-dRep-button").click();
    }
  });
}
