import { getDRepIDHash, injectOpen } from "../support/utils";
import { randomBytes } from "crypto";
import {
  castVote,
  visitGovernanceActionsPage,
} from "./actions/governanceActions";
import {
  validateDRepMetaHashInput,
  validateDRepURLInput,
} from "../support/validations";
import {
  connectWallet,
  loadWallet,
  viewProposalDetails,
} from "./actions/commonActions";
import { IProposal } from "../state/type";
import { dRepWallets } from "../constants/wallet";

describe("Governance Action Functionality", () => {
  beforeEach(() => {
    cy.visit(Cypress.env("baseUrl"));
    // stubbing responses
    cy.intercept("**/proposal/list", { fixture: "notVotedProposals.json" });
    cy.intercept(
      `**/drep/getVotes/${getDRepIDHash(dRepWallets.votedDRep.dRepId)}`,
      { fixture: "votedProposals.json" }
    ).as("getVotes");
  });

  it("5A: Should show relevant details about governance action as dRep.", () => {
    loadWallet(dRepWallets.dRep1);
    visitGovernanceActionsPage();

    viewProposalDetails();
    cy.contains("Expiry").should("exist");
    cy.contains("Governance Action Type").should("exist");
    cy.get("p:contains(View other details)").should("exist");
    cy.getBySel("vote-button").should("exist");
    cy.getBySel("yes-radio").should("exist");
    cy.getBySel("no-radio").should("exist");
    cy.getBySel("abstain-radio").should("exist");
  });

  it('5B: Should view "Vote" button on governance action item on registered as DRep', () => {
    loadWallet(dRepWallets.dRep1);
    visitGovernanceActionsPage();

    viewProposalDetails();
    cy.getBySel("vote-button").should("exist");
  });

  it("5C: Should show required field in proposal voting on registered as DRep", () => {
    loadWallet(dRepWallets.dRep1);
    visitGovernanceActionsPage();

    viewProposalDetails();
    cy.getBySel("yes-radio").should("exist");
    cy.getBySel("no-radio").should("exist");
    cy.getBySel("abstain-radio").should("exist");
    cy.getBySel("context-button").click();
    cy.getBySel("url-input").should("be.visible");
    cy.getBySel("hash-input").should("be.visible");
    cy.getBySel("yes-radio").click();
    cy.getBySel("vote-button").should("be.enabled");
  });

  it("5D: Should validate proposal voting", () => {
    const invalidURLs = ["testdotcom", "https://testdotcom", "https://test.c"];
    const validURLs = ["https://test.com"];
    loadWallet(dRepWallets.dRep1);
    visitGovernanceActionsPage();

    viewProposalDetails();
    cy.getBySel("context-button").click();
    invalidURLs.forEach((url) => {
      validateDRepURLInput(url);
    });

    validURLs.forEach((url) => {
      validateDRepURLInput(url, false);
    });

    // invalid hash
    const hash20Bytes = randomBytes(20).toString("hex");
    validateDRepMetaHashInput(hash20Bytes);

    const hash32BytesString = randomBytes(32).toString();
    validateDRepMetaHashInput(hash32BytesString);

    // valid hash
    const hash32Bytes = randomBytes(32).toString("hex");
    validateDRepMetaHashInput(hash32Bytes, false);
  });

  // The proposals should include following criteria
  // The proposals must be already voted by (VotedDRep)
  // As a dRep which has already voted
  it("5E: Should re-vote with new data on a already voted governance action.", () => {
    cy.fixture("votedProposals.json").then((proposals) => {
      cy.intercept("**/proposal/list").as("getAllProposal");
      console.log({ proposals });
      const votedProposal: IProposal = proposals[0].proposal;
      const proposalID = `${votedProposal.txHash}#${votedProposal.index}`;
      loadWallet(dRepWallets.votedDRep, { withStakeSigning: true });
      visitGovernanceActionsPage();
      cy.getBySel("voted-tab").click();
      cy.wait("@getVotes");
      cy.contains("Yes", { matchCase: false });
      cy.getBySel(`govaction-${proposalID}-change-your-vote`).click();
      cy.getBySel("no-radio").click();
      cy.getBySel("context-button").click();
      cy.getBySel("url-input").clear().type("https://test.com");
      cy.getBySel("hash-input").clear().type(randomBytes(32).toString("hex"));
      castVote("changeVote");
      cy.getBySel("voted-tab").click();
      cy.wait("@getAllProposal");
      cy.contains("No", { matchCase: false });
    });
  });

  it("5F: Should show notification of casted vote after vote", () => {
    cy.fixture("notVotedProposals.json").then((proposals) => {
      const proposalToVote: IProposal = proposals[0];
      const proposalID = `${proposalToVote.txHash}#${proposalToVote.index}`;
      loadWallet(dRepWallets.dRep1, { withStakeSigning: true });
      visitGovernanceActionsPage();
      cy.getBySel(`govaction-${proposalID}-view-detail`).click();
      cy.getBySel("context-button").click();
      cy.getBySel("yes-radio").click();
      cy.getBySel("url-input").clear().type("https://test.com");
      cy.getBySel("hash-input").clear().type(randomBytes(32).toString("hex"));
      castVote();
    });
  });

  it.skip(
    "5G: Given governance action headline is clicked, JSON metadata url is opened in new tab"
  );

  it.skip(
    "5H: Given governance action headline is clicked, warn user to validate the metadata hash themselves"
  );

  it("5I: Should show warning to the users to visit the site at their own risk, when external url is opened", () => {
    connectWallet();
    visitGovernanceActionsPage();

    viewProposalDetails();
    cy.getBySel("view-other-details-button").click();
    cy.getBySel("external-link-modal").should("be.visible");
  });

  it("5J: Should open a new tab, when external URL is opened", () => {
    connectWallet();
    visitGovernanceActionsPage();

    viewProposalDetails();
    cy.getBySel("view-other-details-button").click();
    cy.getBySel("external-link-modal").should("be.visible");
    let res = injectOpen();
    cy.getBySel("continue-modal-button").click();
    cy.wait(100).then(() => {
      expect(res.args[0]).to.exist;
      expect(res.args[1]).to.equal("_blank");
    });
  });

  // Given that I am logged into the app as a dRep
  // When I view governance actions I can see which ones I have personally voted on and how I voted
  it("5K: Should view the vote details,when viewing governance action already voted by the DRep", () => {
    loadWallet(dRepWallets.votedDRep);
    visitGovernanceActionsPage();

    cy.getBySel("voted-tab").click();
    cy.contains("Yes", { matchCase: false });
  });
});
