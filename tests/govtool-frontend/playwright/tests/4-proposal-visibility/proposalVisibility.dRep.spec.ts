import environments from "@constants/environments";
import { dRep01Wallet } from "@constants/staticWallets";
import { createTempDRepAuth } from "@datafactory/createAuth";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { lovelaceToAda } from "@helpers/cardano";
import convertBufferToHex from "@helpers/convertBufferToHex";
import { ShelleyWallet } from "@helpers/crypto";
import { createNewPageWithWallet } from "@helpers/page";
import { pollTransaction } from "@helpers/transaction";
import GovernanceActionsPage from "@pages/governanceActionsPage";
import { Page, expect } from "@playwright/test";
import kuberService from "@services/kuberService";
import { FilterOption, IProposal } from "@types";

test.describe("Logged in DRep", () => {
  test.use({ storageState: ".auth/dRep01.json", wallet: dRep01Wallet });

  test("4E. Should display DRep's voting power in governance actions page", async ({
    page,
  }) => {
    const votingPowerPromise = page.waitForResponse("**/get-voting-power/**");
    const governanceActionsPage = new GovernanceActionsPage(page);
    await governanceActionsPage.goto();

  const res = await votingPowerPromise;
  const votingPower = await res.json();

    await expect(
      page.getByText(`₳ ${lovelaceToAda(votingPower)}`)
    ).toBeVisible();
  });

  test("4F. Should Disable DRep functionality upon wallet disconnection on governance page", async ({
    page,
  }) => {
    const governanceActionsPage = new GovernanceActionsPage(page);
    await governanceActionsPage.goto();

    await page.getByTestId("disconnect-button").click();

    const govActionDetailsPage =
      await governanceActionsPage.viewFirstProposal();
    await expect(govActionDetailsPage.voteBtn).not.toBeVisible();
  });

  test("4G. Should display correct vote counts on governance details page for DRep", async ({
    page,
  }) => {
    const responsesPromise = Object.keys(FilterOption).map((filterKey) =>
      page.waitForResponse((response) =>
        response.url().includes(`&type[]=${FilterOption[filterKey]}`)
      )
    );

    const governanceActionsPage = new GovernanceActionsPage(page);
    await governanceActionsPage.goto();
    const responses = await Promise.all(responsesPromise);
    const proposals: IProposal[] = (
      await Promise.all(
        responses.map(async (response) => {
          const data = await response.json();
          return data.elements;
        })
      )
    ).flat();

    expect(proposals.length, "No proposals found!").toBeGreaterThan(0);

    const proposalToCheck = proposals[0];
    const govActionDetailsPage =
      await governanceActionsPage.viewProposal(proposalToCheck);
    await govActionDetailsPage.showVotesBtn.click();

    await expect(
      page
        .getByText("yes₳")
        .getByText(`₳ ${lovelaceToAda(proposalToCheck.yesVotes)}`)
    ).toBeVisible();
    await expect(
      page
        .getByText("abstain₳")
        .getByText(`₳ ${lovelaceToAda(proposalToCheck.abstainVotes)}`)
    ).toBeVisible();
    await expect(
      page
        .getByText("no₳")
        .getByText(`₳ ${lovelaceToAda(proposalToCheck.noVotes)}`)
    ).toBeVisible();
  });
});

test.describe("Temporary DReps", async () => {
  let dRepPage: Page;

  test.beforeEach(async ({ page, browser }, testInfo) => {
    test.setTimeout(testInfo.timeout + 2 * environments.txTimeOut);

    const wallet = await ShelleyWallet.generate();
    const registrationRes = await kuberService.dRepRegistration(
      convertBufferToHex(wallet.stakeKey.private),
      convertBufferToHex(wallet.stakeKey.pkh)
    );
    await pollTransaction(registrationRes.txId, registrationRes.lockInfo);

    const res = await kuberService.transferADA(
      [wallet.addressBech32(environments.networkId)],
      40
    );
    await pollTransaction(res.txId, registrationRes.lockInfo);

    const tempDRepAuth = await createTempDRepAuth(page, wallet);

    dRepPage = await createNewPageWithWallet(browser, {
      storageState: tempDRepAuth,
      wallet,
      enableStakeSigning: true,
    });
  });

  test("4J. Should include metadata anchor in the vote transaction", async () => {
    const govActionsPage = new GovernanceActionsPage(dRepPage);
    await govActionsPage.goto();

    const govActionDetailsPage = await govActionsPage.viewFirstProposal();
    await govActionDetailsPage.vote(faker.lorem.sentence(200));
    await govActionsPage.votedTab.click();
    await govActionsPage.viewFirstVotedProposal();
    expect(false, "No vote context displayed").toBe(true);
  });
});

test("4G. Should display correct vote counts on governance details page for DRep", async ({
  page,
}) => {
  const responsesPromise = Object.keys(FilterOption).map((filterKey) =>
    page.waitForResponse((response) =>
      response.url().includes(`&type[]=${FilterOption[filterKey]}`)
    )
  );

  const governanceActionsPage = new GovernanceActionsPage(page);
  await governanceActionsPage.goto();
  const responses = await Promise.all(responsesPromise);
  const proposals: IProposal[] = (
    await Promise.all(
      responses.map(async (response) => {
        const data = await response.json();
        return data.elements;
      })
    )
  ).flat();

  expect(proposals.length, "No proposals found!").toBeGreaterThan(0);

  const proposalToCheck = proposals[0];
  const govActionDetailsPage =
    await governanceActionsPage.viewProposal(proposalToCheck);
  await govActionDetailsPage.showVotesBtn.click();

  await expect(
    page
      .getByText("yes₳")
      .getByText(`₳ ${lovelaceToAda(proposalToCheck.yesVotes)}`)
  ).toBeVisible();
  await expect(
    page
      .getByText("abstain₳")
      .getByText(`₳ ${lovelaceToAda(proposalToCheck.abstainVotes)}`)
  ).toBeVisible();
  await expect(
    page
      .getByText("no₳")
      .getByText(`₳ ${lovelaceToAda(proposalToCheck.noVotes)}`)
  ).toBeVisible();
});

test("4F. Should Disable DRep functionality upon wallet disconnection on governance page", async ({
  page,
}) => {
  const governanceActionsPage = new GovernanceActionsPage(page);
  await governanceActionsPage.goto();

  await page.getByTestId("disconnect-button").click();

  const govActionDetailsPage = await governanceActionsPage.viewFirstProposal();
  await expect(govActionDetailsPage.voteBtn).not.toBeVisible();
});
