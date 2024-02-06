import { waitForTxToComplete } from "./commonActions";

export function registerAsDRep() {
  cy.getBySel("register-button").click();
  cy.getBySel("skip-button").click();
  cy.getBySel("register-button").click();
  cy.getBySel("registration-transaction-submitted-modal").should("be.visible");
  cy.getBySel("confirm-modal-button").first().click({ force: true });
  waitForTxToComplete();
}

export function retireDRep() {
  cy.getBySel("retire-button").click();
  cy.intercept("**/tx/submit").as("submitTx");
  cy.wait("@submitTx").then((res) => {
    expect(
      res.response.statusCode,
      `[Submit Tx error]: ${res.response.body.message}`
    ).eq(200);
  });
  cy.getBySel("retirement-transaction-submitted-modal").should("be.visible");
  cy.getBySel("confirm-modal-button").first().click({ force: true });
  waitForTxToComplete();
  cy.contains("in progress", { timeout: 30 * 1000, matchCase: false }).should(
    "not.exist"
  );
}

export function checkForDRepRegistrationConfirmation(expectedDRepId: string) {
  cy.getBySel("dRep-id-display", { timeout: 30 * 1000 }).then(($dRepIdEl) => {
    cy.wrap($dRepIdEl).should("contain", expectedDRepId);
  });
}

export function verifyVoteOnProposal() {
  cy.contains("button", "View proposal details", { matchCase: false }).click();
  cy.getBySel("abstain-radio").click();
  cy.getBySel("yes-radio").click();
  cy.getBySel("no-radio").click();
  cy.getBySel("vote-button").should("be.visible");
}
