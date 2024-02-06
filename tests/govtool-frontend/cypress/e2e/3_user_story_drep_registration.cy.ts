import * as crypto from "crypto";
import { random32BytesHex } from "../support/utils";
import {
  connectWallet,
  loadWallet,
  waitForTxToComplete,
} from "./actions/commonActions";
import {
  checkForDRepRegistrationConfirmation,
  registerAsDRep,
} from "./actions/dRepActions";
import {
  validateDRepMetaHashInput,
  validateDRepURLInput,
} from "../support/validations";
import {
  adaHolderWallets,
  dRepWallets,
  referenceWallets,
} from "../constants/wallet";

describe("DRep registration", () => {
  beforeEach(() => {
    cy.visit(Cypress.env("baseUrl"));
  });

  it("3A. Should show dRepId on dashboard after connecting registered dRep Wallet", () => {
    loadWallet(referenceWallets.dRep1);
    checkForDRepRegistrationConfirmation(referenceWallets.dRep1.dRepId);
  });

  it("3B. Should access dRep registration page", () => {
    connectWallet();
    cy.getBySel("register-button").click();
    cy.contains("Add information", { matchCase: false });
  });

  it("3C. Should open wallet connection popup, when Register as DRep from wallet unconnected state", () => {
    cy.getBySel("register-connect-wallet-button").click();
    cy.getBySel("connect-your-wallet-modal").should("be.visible");
  });

  // Given that I'm connected to the wallet
  // User visits the DRep Registration page
  // DRep registration actions should be accessible
  it("3D.Should access DRep registration page in Wallet Connected State", () => {
    connectWallet();
    cy.getBySel("register-button").click();
    cy.getBySel("url-input").should("be.visible");
    cy.getBySel("hash-input").should("be.visible");
    cy.getBySel("skip-button").should("be.visible");
  });

  it("3E. Should reject invalid data and accept valid data", () => {
    connectWallet();
    cy.getBySel("register-button").click();
    // invalid datas
    const longUrl =
      "https://ipfs.io/ipfs/" + crypto.randomBytes(32).toString("hex");
    validateDRepURLInput(longUrl);

    const invalidUrl = "abcd";
    validateDRepURLInput(invalidUrl);

    const emptyUrl = " ";
    validateDRepURLInput(emptyUrl);

    const invalidHash = "ipfs://" + crypto.randomBytes(32).toString("hex");
    validateDRepMetaHashInput(invalidHash);

    // too short text should be invalid
    const tooShortHash = "abcd";
    validateDRepMetaHashInput(tooShortHash);
    // empty should be valid
    validateDRepMetaHashInput(" ");

    // valid datas
    const validUrl = "ipfs://" + crypto.randomBytes(28).toString("hex");
    const validHash = crypto.randomBytes(32).toString("hex");
    cy.getBySel("url-input").type(validUrl);
    cy.getBySel("hash-input").type(validHash);
    cy.getBySel("confirm-button").should("be.enabled");
  });

  // Given that I'm an ADA-Holder
  // When registering for DRep, wallet asks to sign the tx
  it("3F. Should create proper DRep registration request, when registered with data", () => {
    cy.intercept("**/utxo?**").as("getUtxos");
    connectWallet();
    cy.getBySel("register-button").click();
    cy.getBySel("url-input").type("https://google.com");
    cy.getBySel("hash-input").type(random32BytesHex());
    cy.getBySel("confirm-button").click();
    cy.getBySel("register-button").click();
    cy.wait("@getUtxos").then((interception) => {
      expect(interception.response.body.length).to.equal(0);
    });
  });

  it("3G: Should show confirmation message with link to view transaction, when DRep registration txn is submitted", () => {
    loadWallet(adaHolderWallets.adaHolder2);
    cy.getBySel("register-button").click();
    cy.getBySel("url-input").type(
      "ipfs://" + crypto.randomBytes(28).toString("hex")
    );
    cy.getBySel("hash-input").type(random32BytesHex());
    cy.getBySel("confirm-button").click();
    cy.getBySel("register-button").click();
    cy.getBySel("registration-transaction-submitted-modal").should(
      "be.visible"
    );
    waitForTxToComplete();
    cy.contains("this link");
  });

  it("3H. Should be able to update metadata.", () => {
    loadWallet(dRepWallets.dRep1);
    cy.getBySel("change-metadata-button").click();
    cy.getBySel("url-input").type("https://google.com");
    cy.getBySel("hash-input").should("be.visible").type(random32BytesHex());
    cy.getBySel("confirm-button").should("be.enabled");
  });
});
