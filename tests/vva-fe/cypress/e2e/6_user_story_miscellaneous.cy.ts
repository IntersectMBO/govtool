import { injectOpen, isMobile, random32BytesHex } from "../support/utils";
import * as mockWallet from "../lib/wallet/mockWallet";
import {
  checkInvalidityOfDRepId,
  connectWallet,
  openMobileDrawer,
} from "./actions/commonActions";

describe("Miscellaneous", () => {
  beforeEach(() => {
    cy.visit(Cypress.env("baseUrl"));
  });
  // All testes can be tested for mobile
  it.skip("6A: Should be accessible from mobile", () => {});

  it("6B. Provides error for invalid format", () => {
    connectWallet();

    // invalid dRep id
    checkInvalidityOfDRepId("Random values");
    cy.getBySel("confirm-modal-button").first().click({ force: true });
    cy.go(-2);
    // invalid dRep registration
    cy.getBySel("register-button").click();
    cy.getBySel("url-input").type("abc");
    cy.getBySel("invalid-url-format-error").should("be.visible");

    cy.getBySel("hash-input").type("abc");
    cy.getBySel("hash-must-be-exactly-64-characters-long-error").should(
      "be.visible"
    );
  });

  it("6C. Navigation within the dApp ", () => {
    if (isMobile()) {
      openMobileDrawer();
    }
    cy.getBySel("governance-actions-link").click();
    cy.url().should("include", "/governance_actions");
    if (isMobile()) {
      openMobileDrawer();
    }
    const res = injectOpen();
    cy.getBySel("guides-link").click();
    cy.wait(100).then(() => {
      expect(res.args[1]).to.equal("_blank");
    });
    if (isMobile()) {
      openMobileDrawer();
    }
    cy.getBySel("faqs-link").click();
    cy.wait(100).then(() => {
      expect(res.args[1]).to.equal("_blank");
    });
    if (isMobile()) {
      openMobileDrawer();
    }
    cy.getBySel("home-link").click();
    cy.url().should("include", "/");
  });

  it("6D. Proper label and recognition of the testnet network", () => {
    connectWallet();
    cy.contains("testnet");
  });

  //Skipped: Mainnet cannot be connected from testnet
  it.skip("6E. Proper label and recognition of the mainnet network");
});
