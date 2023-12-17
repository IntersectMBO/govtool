import { governanceActions } from "../../constants/governanceActions";
import {
  validateDRepMetaHashInput,
  validateDRepURLInput,
} from "../../support/validations";
import {
  connectWallet,
  disconnectAndReconnectWallet,
  viewProposalDetails,
} from "../actions/commonActions";
import * as crypto from "crypto";
import { visitGovernanceActionsPage } from "../actions/governanceActions";
import { ShelleyWallet } from "../../lib/wallet/crypto";
import { referenceWallets } from "../../constants/wallet";

describe("Test for Overflows, Valid/Invalid data and Headers", () => {
  if (Cypress.env("disableSecurityTest")) return;
  beforeEach(() => {
    cy.visit(Cypress.env("baseUrl"));
  });

  // Input Size Testing
  it("7.1.1: Should reject excessively long inputs", () => {
    // DRep registration
    connectWallet();
    cy.getBySel("register-button").click();
    const longInput = "a".repeat(1500);
    cy.getBySel("url-input").type(longInput).blur();
    cy.getBySel("hash-input").type(longInput).blur();
    cy.go("back");
    // Search input (Governance actions)
    visitGovernanceActionsPage();
    cy.getBySel("search-input").type(longInput).clear().blur();

    // Vote on Proposal form
    disconnectAndReconnectWallet(
      ShelleyWallet.fromJson(referenceWallets.dRep1)
    );
    visitGovernanceActionsPage();
    viewProposalDetails();
    cy.getBySel("context-button").click();
    cy.getBySel("url-input").type(longInput).blur();
    cy.getBySel("hash-input").type(longInput).blur();
  });

  // Valid/Invalid Data Testing
  it("7.1.2: Should accept valid data and reject the invalid data", () => {
    // DRep registration
    connectWallet();
    cy.getBySel("register-button").click();
    dRepFormValidation();

    // Search input (Governance actions)
    cy.go("back");
    // Positive data
    visitGovernanceActionsPage();
    governanceActions.forEach((action) => {
      cy.getBySel("search-input").type(action).blur();
      cy.contains(action);
      cy.getBySel("search-input").clear();
    });

    // Negative data
    cy.getBySel("search-input").type("nono").blur();
    cy.contains("There're any Governance Actions yet.").should("be.visible");
    cy.getBySel("search-input").clear();

    // Vote on Proposal form
    viewProposalDetails();
    dRepFormValidation();
  });

  it("7.1.3.a: Should contain CSP header", () => {
    const urlsToTest = ["", "governance_actions"];
    urlsToTest.map((url) => {
      cy.request({
        method: "GET",
        url: Cypress.env("baseUrl") + url,
      }).then((response) => {
        expect(response.headers).to.have.property("content-security-policy");
      });
    });
  });

  it("7.1.3.b: Should handle CORS request properly", () => {
    cy.request({
      method: "GET",
      url: Cypress.env("apiUrl") + "/proposal/list",
      headers: {
        Origin: "https://another-origin.com",
      },
    }).then((response) => {
      expect(response.headers).to.have.property("access-control-allow-origin");
      expect(response.headers).to.not.have.property(
        "access-control-allow-origin",
        "*"
      );
    });
  });

  it("7.1.3.c: Should match the actual content", () => {
    cy.visit(Cypress.env("baseUrl"));
    const endPointsContentTypes = [
      { endpoint: "/proposal/list", contentType: "application/json" },
      { endpoint: "/drep/list", contentType: "application/json" },
    ];
    endPointsContentTypes.forEach((endpoint) => {
      cy.request(Cypress.env("apiUrl") + endpoint.endpoint).then((response) => {
        expect(response.headers).to.have.property("content-type");

        const contentTypeHeader = response.headers["content-type"];
        expect(contentTypeHeader).to.include(endpoint.contentType);
      });
    });
  });
});

const dRepFormValidation = () => {
  const invalidUrls = [
    "https://ipfs.io/ipfs/" + crypto.randomBytes(32).toString("hex"),
    "abcd",
    " ",
  ];
  const invalidHashs = [
    "ipfs://" + crypto.randomBytes(32).toString("hex"),
    "abcd",
    " ",
  ];
  const validHash = crypto.randomBytes(32).toString("hex");
  const validUrl = "ipfs://QmXqBdW3t6jFwz3L4pRy7zmZGKEzj7qHpA7ZyG6Zr45nsw";

  // Negative Tests
  invalidUrls.forEach((url) => {
    validateDRepURLInput(url);
  });
  invalidHashs.forEach((url) => {
    validateDRepMetaHashInput(url);
  });

  // Positive Tests
  validateDRepURLInput(validUrl, false);
  validateDRepMetaHashInput(validHash, false);
};
