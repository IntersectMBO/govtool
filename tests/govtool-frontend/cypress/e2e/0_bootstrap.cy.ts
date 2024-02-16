import {
  adaHolderWallets,
  bootstrapWallet,
  dRepWallets,
  referenceWallets,
} from "../constants/wallet";
import { ShelleyWallet } from "../lib/wallet/crypto";
import {
  generateWallets,
  registerStake,
  setupWallets,
} from "../lib/wallet/setupWallet";
import { StaticWallet } from "../lib/wallet/types";
import { pollTx, pollTxWithKuber } from "../services/apiService";
import kuberService from "../services/kuberService";
import { loadAmountFromFaucet } from "../services/faucetService";
import { IFaucetResponse } from "../services/types";
import { IProposal, IVotedProposal } from "../state/type";
import { getDRepIDHash, getTxHash, saveWallets } from "../support/utils";
import { loadAmountFromBootStrapWallet } from "./actions/commonActions";

const staticWallets = [
  ...Object.values(dRepWallets),
  ...Object.values(adaHolderWallets),
  ...Object.values(referenceWallets),
];

const bootstrapConfig = {
  onTimeWalletSetup: Cypress.env("runOneTimeWalletSetup") || false,
  enableStakeRegistrationForStaticWallets:
    Cypress.env("enableStakeRegistrationForStaticWallets") || true,
};
// NOTE This is not for tests.
describe("Bootstrap", () => {
  it("Setup mock wallets", () => {
    if (!bootstrapConfig.onTimeWalletSetup) return;

    const generateAndRegisterWallets = async () => {
      const wallets = await generateWallets(50);
      await setupWallets(wallets);
      saveWallets(wallets);
    };
    cy.wrap(0).then({ timeout: 2 * 60 * 10000 }, async () =>
      generateAndRegisterWallets()
    );
  });

  it("Load faucet into bootstrap wallet", () => {
    cy.wrap(loadAmountFromFaucet(bootstrapWallet.address)).then(
      { timeout: 2 * 60 * 1000 },
      async (res: IFaucetResponse) => {
        await pollTxWithKuber(res.txid);
      }
    );
  });

  it("Load amounts into static wallets", () => {
    const staticWalletAddressList = Object.values(staticWallets).map(
      (wallet) => wallet.address
    );
    cy.wrap(loadAmountFromBootStrapWallet(staticWalletAddressList)).then(
      { timeout: 2 * 60 * 1000 },
      async (tx: any) => {
        await pollTxWithKuber(tx.txId);
      }
    );
  });

  Object.values(staticWallets).forEach((walletJson) => {
    it(`Register stake of the static wallet: ${walletJson.type}`, () => {
      cy.on("fail", (err) => {
        console.log(err);
        if (err.message === "Stake already registered") {
          return false;
        }
      });
      cy.wrap(registerStake(ShelleyWallet.fromJson(walletJson)), {
        timeout: 2 * 60 * 1000,
      });
    });
  });

  Object.values(referenceWallets).forEach((walletJson: StaticWallet) => {
    it(`Clean up reference wallet: ${walletJson.type}`, () => {
      cy.on("fail", (err) => {
        return false;
      });
      cy.wrap(
        kuberService.dRepRegistration(
          walletJson.address,
          walletJson.payment.private,
          walletJson.stake.pkh
        )
      ).then({ timeout: 2 * 60 * 1000 }, async (tx: any) => {
        await pollTxWithKuber(tx.txId);
      });
    });
  });

  it("Setup proposals for voting ", () => {
    let notVotedProposals: IProposal[] = [];
    let votedProposals: IVotedProposal[] = [];

    const today = new Date();
    const tomorrow = new Date(today);
    today.setDate(today.getDate() + 1);
    // Creating proposals
    cy.log("Creating not voted proposal");
    cy.wrap(0).then({ timeout: 2 * 60 * 1000 }, async () => {
      const res = await kuberService.createGovAction(1);
      const txHash = getTxHash(res.cborHex);
      await pollTxWithKuber(txHash);
      notVotedProposals.push({
        id: "1",
        txHash: txHash,
        index: 0,
        type: "Info",
        details: "Info",
        createdDate: today.toISOString(),
        expiryDate: tomorrow.toISOString(),
        abstainVotes: 0,
        noVote: 0,
        metadataHash:
          "1111111111111111111111111111111111111111111111111111111111111111",
        url: "https://bit.ly/3zCH2HL",
        yesVotes: 0,
      });
    });

    cy.log("Creating voted proposal");
    cy.wrap(0).then({ timeout: 2 * 60 * 1000 }, async () => {
      const res = await kuberService.createGovAction(1);
      const txHash = getTxHash(res.cborHex);
      await pollTxWithKuber(txHash);
      votedProposals.push({
        proposal: {
          id: "1",
          txHash: txHash,
          index: 0,
          type: "Info",
          details: "Info",
          createdDate: today.toISOString(),
          expiryDate: tomorrow.toISOString(),
          abstainVotes: 0,
          noVote: 0,
          metadataHash:
            "1111111111111111111111111111111111111111111111111111111111111111",
          url: "https://bit.ly/3zCH2HL",
          yesVotes: 1,
        },
        vote: {
          drepId: getDRepIDHash(dRepWallets.votedDRep.dRepId),
          metadataHash:
            "1111111111111111111111111111111111111111111111111111111111111111",
          url: "https://bit.ly/3zCH2HL",
          proposalId: "1",
          vote: "Yes",
        },
      });
    });
    // vote
    cy.log("Voting proposal");
    cy.wrap(0).then({ timeout: 2 * 60 * 1000 }, async () => {
      const votedDRepWallet = dRepWallets.votedDRep;
      const votedProposal = votedProposals[0];
      const res = await kuberService.voteOnProposal(
        votedDRepWallet.address,
        votedDRepWallet.payment.private,
        getDRepIDHash(votedDRepWallet.dRepId),
        votedDRepWallet.stake.private,
        `${votedProposal.proposal.txHash}#${votedProposal.proposal.index}`
      );
      await pollTxWithKuber(res.txId);
    });
    cy.writeFile("cypress/fixtures/notVotedProposals.json", notVotedProposals, {
      flag: "w",
    });
    cy.writeFile("cypress/fixtures/votedProposals.json", votedProposals, {
      flag: "w",
    });
  });
});
