import environments from "@constants/environments";
import { dRep01Wallet } from "@constants/staticWallets";
import { createTempDRepAuth } from "@datafactory/createAuth";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { lovelaceToAda } from "@helpers/cardano";
import { createNewPageWithWallet } from "@helpers/page";
import GovernanceActionDetailsPage from "@pages/governanceActionDetailsPage";
import GovernanceActionsPage from "@pages/governanceActionsPage";
import { Page, expect } from "@playwright/test";
import { FilterOption, IProposal } from "@types";
import walletManager from "lib/walletManager";

test.beforeEach(async () => {
  await setAllureEpic("4. Proposal visibility");
});

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

    await expect(page.getByTestId("voting-power-chips-value")).toHaveText(
      `₳ ${lovelaceToAda(votingPower)}`
    );
  });
});

test.describe("Temporary DReps", async () => {
  let dRepPage: Page;

  test.beforeEach(async ({ page, browser }) => {
    const wallet = await walletManager.popWallet("registeredDRep");

    const tempDRepAuth = await createTempDRepAuth(page, wallet);

    dRepPage = await createNewPageWithWallet(browser, {
      storageState: tempDRepAuth,
      wallet,
      enableStakeSigning: true,
    });
  });

  test("4J. Should include metadata anchor in the vote transaction", async ({}, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    const govActionsPage = new GovernanceActionsPage(dRepPage);
    await govActionsPage.goto();

    const govActionDetailsPage = await govActionsPage.viewFirstProposal();
    await govActionDetailsPage.vote(faker.lorem.sentence(200));

    await govActionsPage.votedTab.click();
    await govActionsPage.viewFirstVotedProposal();
    expect(false, "No vote context displayed").toBe(true);
  });

  test("4I. Should display the recent vote on same sameshot", async ({
    context,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + 2 * environments.txTimeOut);

    const govActionsPage = new GovernanceActionsPage(dRepPage);
    await govActionsPage.goto();

    const govActionDetailsPage = await govActionsPage.viewFirstProposal();
    const urlList = dRepPage.url().split("/");
    const governanceActionId = urlList[urlList.length - 1];
    await govActionDetailsPage.vote();

    await govActionsPage.votedTab.click();

    await govActionsPage.searchInput.fill(governanceActionId);

    await dRepPage
      .getByTestId(`govaction-${governanceActionId}-change-your-vote`)
      .click();
    const votedActionDetailsPage = new GovernanceActionDetailsPage(dRepPage);
    await votedActionDetailsPage.noVoteRadio.click();
    await votedActionDetailsPage.changeVoteBtn.click();

    await govActionsPage.searchInput.fill(governanceActionId);

    await expect(dRepPage.getByTestId("my-vote").getByText("No")).toBeVisible();
  });
});

test.describe("Check vote count", () => {
  test.use({ storageState: ".auth/dRep01.json", wallet: dRep01Wallet });

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

test("4F. Should Disable DRep functionality upon wallet disconnection on governance actions page", async ({
  page,
  browser,
}) => {
  const wallet = await walletManager.popWallet("registeredDRep");

  const tempDRepAuth = await createTempDRepAuth(page, wallet);

  const dRepPage = await createNewPageWithWallet(browser, {
    storageState: tempDRepAuth,
    wallet,
  });

  const governanceActionsPage = new GovernanceActionsPage(dRepPage);
  await governanceActionsPage.goto();

  await dRepPage.getByTestId("disconnect-button").click();

  await expect(dRepPage).toHaveURL("/governance_actions");

  const govActionDetailsPage = await governanceActionsPage.viewFirstProposal();
  await expect(govActionDetailsPage.voteBtn).not.toBeVisible();
});
