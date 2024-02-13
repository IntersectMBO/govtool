import { governanceActions } from "../../constants/governanceActions";
import { isMobile } from "../../support/utils";
import { openMobileDrawer, waitForTxToComplete } from "./commonActions";

export function visitGovernanceActionsPage() {
  cy.intercept("**/proposal/list").as("getAllProposal");
  if (isMobile()) {
    openMobileDrawer();
  }
  cy.getBySel("governance-actions-link").click();
  cy.url().should("include", "/governance_actions");
  // Waits for proposals to be loaded for further governance actions functionality
  cy.wait("@getAllProposal", { timeout: 5000 });
}

function isViewProposalDetailsButtonMissing($slider: any) {
  return !(
    $slider.find('[data-testid^="govaction-"][data-testid$="-view-detail"]')
      .length > 0
  );
}

function isExpectedProposalTypeMissing($slide: any) {
  for (const type of governanceActions) {
    if ($slide.find(`p:contains(${type})`).length > 0) {
      return false;
    }
  }

  return true;
}

export const checkGovernanceActionsVisibility = () => {
  cy.get(".keen-slider")
    .find(".keen-slider__slide")
    .each(($slide) => {
      if ($slide.find("p:contains(Governance Action Type)").length > 0) {
        if (isExpectedProposalTypeMissing($slide)) {
          throw new Error("Expected proposals are missing");
        }
        if (isViewProposalDetailsButtonMissing($slide)) {
          throw new Error("View proposal button missing");
        }
      }
    });
};

export const castVote = (castOption: "newVote" | "changeVote" = "newVote") => {
  if (castOption === "changeVote") {
    cy.getBySel("change-vote").click();
  } else {
    cy.getBySel("vote-button").click();
  }
  cy.intercept("**/tx/submit").as("submitTx");
  cy.wait("@submitTx");
  cy.getBySel("alert-success").should("be.visible");
  waitForTxToComplete();
};
