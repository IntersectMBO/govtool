import { groupElements } from "../support/utils";
import {
  checkGovernanceActionsVisibility,
  visitGovernanceActionsPage,
} from "./actions/governanceActions";
import {
  governanceActions,
  governanceSorts,
} from "../constants/governanceActions";
import {
  connectWallet,
  filterGovActions,
  sortElementByText,
  viewProposalDetails,
} from "./actions/commonActions";
import { validateSort } from "../support/validations";
import { dRepWallets } from "../constants/wallet";

describe("Governance Actions visibility", () => {
  beforeEach(() => {
    cy.visit(Cypress.env("baseUrl"));
    cy.intercept("**/proposal/list").as("getAllProposal");
  });

  it("4A: Should access Governance Actions page with/without connecting wallet", () => {
    // without wallet connection
    cy.getBySel("move-to-governance-actions-button").click();
    cy.wait("@getAllProposal", { timeout: 50000 });
    checkGovernanceActionsVisibility();

    // with wallet connection
    cy.visit(Cypress.env("baseUrl"));
    connectWallet();
    visitGovernanceActionsPage();
    checkGovernanceActionsVisibility();
  });

  it("4B: Should restrict voting for users who are not registered as DReps", () => {
    cy.getBySel("move-to-governance-actions-button").click();
    viewProposalDetails();
    cy.getBySel("vote-button").should("not.exist");
  });

  it("4C.1: Should filter Governance Action Type on governance actions page", () => {
    cy.intercept(`**/proposal/list?type=*`).as("getFilteredProposals");
    connectWallet();
    visitGovernanceActionsPage();
    cy.getBySel("filters-button").click();
    // single filter
    governanceActions.forEach((govAction) => {
      filterGovActions([govAction], () => {
        cy.wait("@getFilteredProposals");
      });
    });

    // multiple filters
    for (let i = 2; i <= governanceActions.length; i++) {
      filterGovActions(governanceActions.slice(0, i), () => {
        cy.wait("@getFilteredProposals");
      });
    }
  });

  it("4C.2: Should sort Governance Action Type on governance actions page", () => {
    cy.intercept({ method: "GET", url: "**/proposal/list?sort=*" }).as(
      "getSortedProposals"
    );
    connectWallet();
    visitGovernanceActionsPage();
    cy.getBySel("sort-button").click();
    governanceSorts.forEach((sort) => {
      sortElementByText(sort);
      cy.wait("@getSortedProposals").then((responseData: any) => {
        if (responseData) {
          const sortedData = groupElements(responseData.response.body);
          validateSort(sortedData);
        }
      });
    });
  });

  it("4C.3: Should filter and sort Governance Action Type on governance actions page", () => {
    cy.intercept(`**/proposal/list?type=*`).as("getFilteredProposals");
    cy.intercept({ method: "GET", url: "**/proposal/list?type=*sort=*" }).as(
      "getFilteredAndSortedProposals"
    );
    connectWallet();
    visitGovernanceActionsPage();
    cy.getBySel("filters-button").click();
    // single filter
    governanceActions.forEach((govAction) => {
      filterGovActions([govAction], () => {
        cy.wait("@getFilteredProposals");
        cy.getBySel("sort-button").click();
        governanceSorts.forEach((sort) => {
          sortElementByText(sort);
          cy.wait("@getFilteredAndSortedProposals").then(
            (responseData: any) => {
              if (responseData) {
                const sortedData = groupElements(responseData.response.body);
                validateSort(sortedData);
              }
            }
          );
        });
        cy.getBySel("filters-button").click();
      });
    });
  });
  // Not implemented in FE yet.
  it.skip(
    "4.D Should only view the proposals that are accepted by the Constitutional Committee."
  );
});
