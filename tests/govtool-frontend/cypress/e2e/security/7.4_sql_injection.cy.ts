import { referenceWallets } from "../../constants/wallet";
import { ShelleyWallet } from "../../lib/wallet/crypto";
import {
  validateDRepMetaHashInput,
  validateDRepURLInput,
} from "../../support/validations";
import {
  connectWallet,
  disconnectAndReconnectWallet,
  viewProposalDetails,
} from "../actions/commonActions";
import { visitGovernanceActionsPage } from "../actions/governanceActions";

const sqlVulnerable = "' OR '1'='1";
const hexEncodedSqlVulnerable = Buffer.from(sqlVulnerable, "utf-8").toString(
  "hex"
);

describe("SQL Injection", () => {
  if (Cypress.env("disableSecurityTest")) return;
  beforeEach(() => {
    cy.visit(Cypress.env("baseUrl"));
  });

  it("7.4.1: Should prevent SQL Injection in input fields", () => {
    connectWallet();
    // DRep registration
    cy.getBySel("register-button").click();
    validateDRepURLInput(sqlVulnerable);
    validateDRepMetaHashInput(sqlVulnerable);

    cy.go("back");

    disconnectAndReconnectWallet(
      ShelleyWallet.fromJson(referenceWallets.dRep1)
    );

    // Proposal form
    visitGovernanceActionsPage();
    viewProposalDetails();
    cy.getBySel("context-button").click();
    cy.getBySel("url-input").type(sqlVulnerable).blur();
    cy.getBySel("hash-input").type(sqlVulnerable).blur();
  });

  it("7.4.2: Should prevent SQL Injection in query and path parameters", () => {
    // Sql injection in proposals query (Governance actions)
    checkForSqlVulnerable(`/proposal/list?type=${sqlVulnerable}`);
    // Sql injection in sort query (Governance actions)
    checkForSqlVulnerable(`/proposal/list?sort=${sqlVulnerable}`);
    // Sql injection in get-current-delegation path
    checkForSqlVulnerable(
      `/ada-holder/get-current-delegation/${hexEncodedSqlVulnerable}`
    );
    // Sql injection in adaHolder get-voting-power path
    checkForSqlVulnerable(
      `/ada-holder/get-voting-delegation/${hexEncodedSqlVulnerable}`
    );
    // Sql injection in dRep get-voting-power path
    checkForSqlVulnerable(
      `/dRep/get-voting-delegation/${hexEncodedSqlVulnerable}`
    );
    // Sql injection in dRep get-votes path
    checkForSqlVulnerable(`/dRep/getVotes/${hexEncodedSqlVulnerable}`);
  });

  it("7.4.3: Should prevent error based injection", () => {
    const sqlInjectionTests = [
      "'",
      "' '",
      '"',
      "“ ”",
      "\\",
      "\\\\",
      "' OR '1",
      "' OR 1 -- -",
      '" OR "" = "',
      '" OR 1 = 1 -- -',
      "'='",
      "'LIKE'",
      "'=0 -- +",
    ];
    sqlInjectionTests.forEach((test) => {
      // Sql injection in proposals query (Governance actions)
      checkForSqlVulnerable(`/proposal/list?type=${test}`);
      // Sql injection in sort query (Governance actions)
      checkForSqlVulnerable(`/proposal/list?sort=${test}`);
      // Sql injection in get-current-delegation path
      checkForSqlVulnerable(`/ada-holder/get-current-delegation/${test}`);
      // Sql injection injection in adaHolder get-voting-power path
      checkForSqlVulnerable(`/ada-holder/get-voting-delegation/${test}`);
      // Sql injection injection in dRep get-voting-power path
      checkForSqlVulnerable(`/dRep/get-voting-delegation/${test}`);
      // Sql injection injection in dRep get-votes path
      checkForSqlVulnerable(`/dRep/getVotes/${test}`);
    });
  });
});

function checkForSqlVulnerable(path: string) {
  cy.request({
    url: Cypress.env("apiUrl") + path,
    failOnStatusCode: false,
  }).then((resp) => {
    console.log(resp);
    expect(resp.status).to.eq(
      400,
      `[SQL injection] URL:${Cypress.env("apiUrl")}${path}`
    );
  });
}
