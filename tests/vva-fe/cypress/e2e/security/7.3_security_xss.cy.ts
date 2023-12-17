import { referenceWallets } from "../../constants/wallet";
import { ShelleyWallet } from "../../lib/wallet/crypto";
import {
  connectWallet,
  disconnectAndReconnectWallet,
  viewProposalDetails,
} from "../actions/commonActions";
import { visitGovernanceActionsPage } from "../actions/governanceActions";

describe("Cross Site  Scripting", () => {
  if (Cypress.env("disableSecurityTest")) return;
  beforeEach(() => {
    cy.visit(Cypress.env("baseUrl"));
  });

  // Input Fields Testing
  it("7.3.1: Should prevent script from injection in input fields", () => {
    connectWallet();
    const script = "<script>alert('XSS')</script>";
    // DRep Registration page
    cy.getBySel("register-button").click();
    dRepFormXSS(script);

    cy.go("back");
    disconnectAndReconnectWallet(
      ShelleyWallet.fromJson(referenceWallets.dRep1)
    );
    // Positive data
    visitGovernanceActionsPage();
    cy.getBySel("search-input").type(script).blur();
    listenXSSAlert();

    cy.getBySel("search-input").clear();

    // Vote on Proposal form
    viewProposalDetails();
    cy.getBySel("context-button").click();
    dRepFormXSS(script);
  });

  // Output Rendering Testing
  it("7.3.2: Should properly encode or escape user generated content", () => {
    const maliciousContent =
      "<div>User-generated content with a <script>alert('XSS')</script></div>";
    cy.intercept("**/proposal/list", (req) => {
      req.continue((res) => {
        res.body = [...res.body, { ...res.body[0], type: maliciousContent }];
      });
    });

    cy.intercept("**/drep/get-voting-power/*", (req) => {
      req.continue((res) => {
        res.body = maliciousContent;
      });
    });
    connectWallet();
    visitGovernanceActionsPage();
    listenXSSAlert();
  });
});

const dRepFormXSS = (script: string) => {
  cy.getBySel("url-input").type(script).blur();
  cy.getBySel("hash-input").type("<img src=x onerror=alert('XSS')>").blur();
  listenXSSAlert();
};

const listenXSSAlert = () => {
  cy.on("window:alert", (alertText) => {
    throw new Error(`Unexpected alert: ${alertText}`);
  });
};
