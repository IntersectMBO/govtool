import environments from "@constants/environments";
import { dRep01Wallet } from "@constants/staticWallets";
import { createTempDRepAuth } from "@datafactory/createAuth";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import {
  isBootStrapingPhase,
  lovelaceToAda,
  skipIfNotHardFork,
} from "@helpers/cardano";
import { createNewPageWithWallet } from "@helpers/page";
import GovernanceActionsPage from "@pages/governanceActionsPage";
import { Page, expect } from "@playwright/test";
import { invalid as mockInvalid, valid as mockValid } from "@mock/index";
import {
  BootstrapGovernanceActionType,
  GrovernanceActionType,
  IProposal,
} from "@types";
import walletManager from "lib/walletManager";
import GovernanceActionDetailsPage from "@pages/governanceActionDetailsPage";

test.beforeEach(async () => {
  await setAllureEpic("4. Proposal visibility");
  await skipIfNotHardFork();
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

  test.describe("vote context metadata anchor validation", () => {
    let govActionDetailsPage: GovernanceActionDetailsPage;
    test.beforeEach(async ({ page }) => {
      const govActionsPage = new GovernanceActionsPage(page);
      await govActionsPage.goto();

      govActionDetailsPage = (await isBootStrapingPhase())
        ? await govActionsPage.viewFirstProposalByGovernanceAction(
            GrovernanceActionType.InfoAction
          )
        : await govActionsPage.viewFirstProposal();

      await govActionDetailsPage.contextBtn.click();
      await govActionDetailsPage.contextInput.fill(faker.lorem.sentence(200));
      await govActionDetailsPage.confirmModalBtn.click();
      await page.getByRole("checkbox").click();
      await govActionDetailsPage.confirmModalBtn.click();
    });

    test("4N. Should accept valid metadata anchor on vote context", async ({
      page,
    }) => {
      for (let i = 0; i < 100; i++) {
        await govActionDetailsPage.metadataUrlInput.fill(mockValid.url());
        await expect(page.getByTestId("invalid-url-error")).toBeHidden();
      }
    });

    test("4O. Should reject invalid metadata anchor on vote context", async ({
      page,
    }) => {
      for (let i = 0; i < 100; i++) {
        const invalidUrl = mockInvalid.url(false);
        await  govActionDetailsPage.metadataUrlInput.fill(invalidUrl);
        if (invalidUrl.length <= 128) {
          await expect(page.getByTestId("invalid-url-error")).toBeVisible();
        } else {
          await expect(
            page.getByTestId("url-must-be-less-than-128-bytes-error")
          ).toBeVisible();
        }
      }
    });
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
    test.skip(); // Skipped: Vote context is not displayed in UI to validate

    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    const govActionsPage = new GovernanceActionsPage(dRepPage);
    await govActionsPage.goto();

    const govActionDetailsPage = await govActionsPage.viewFirstProposal();
    await govActionDetailsPage.vote(faker.lorem.sentence(200));

    await dRepPage.waitForTimeout(5_000);

    await govActionsPage.votedTab.click();
    await govActionsPage.viewFirstVotedProposal();
    expect(false, "No vote context displayed").toBe(true);
  });
});

test.describe("Check vote count", () => {
  test.use({ storageState: ".auth/dRep01.json", wallet: dRep01Wallet });

  test("4G. Should display correct vote counts on governance details page for DRep", async ({
    page,
  }) => {
    const voteWhiteListOption = (await isBootStrapingPhase())
      ? BootstrapGovernanceActionType
      : GrovernanceActionType;
    const responsesPromise = Object.keys(voteWhiteListOption).map((filterKey) =>
      page.waitForResponse((response) =>
        response.url().includes(`&type[]=${voteWhiteListOption[filterKey]}`)
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

    // check dRep votes
    await expect(govActionDetailsPage.dRepYesVotes).toHaveText(
      `₳ ${lovelaceToAda(proposalToCheck.dRepYesVotes)}`
    );
    await expect(govActionDetailsPage.dRepAbstainVotes).toHaveText(
      `₳ ${lovelaceToAda(proposalToCheck.dRepAbstainVotes)}`
    );
    await expect(govActionDetailsPage.dRepNoVotes).toHaveText(
      `₳ ${lovelaceToAda(proposalToCheck.dRepNoVotes)}`
    );

    // check sPos votes
    await expect(govActionDetailsPage.sPosYesVotes).toHaveText(
      `₳ ${lovelaceToAda(proposalToCheck.poolYesVotes)}`
    );
    await expect(govActionDetailsPage.sPosAbstainVotes).toHaveText(
      `₳ ${lovelaceToAda(proposalToCheck.poolAbstainVotes)}`
    );
    await expect(govActionDetailsPage.sPosNoVotes).toHaveText(
      `₳ ${lovelaceToAda(proposalToCheck.poolNoVotes)}`
    );

    // check ccCommittee votes
    await expect(govActionDetailsPage.ccCommitteeYesVotes).toHaveText(
      `${proposalToCheck.ccYesVotes}`
    );
    await expect(govActionDetailsPage.ccCommitteeAbstainVotes).toHaveText(
      `${proposalToCheck.ccAbstainVotes}`
    );
    await expect(govActionDetailsPage.ccCommitteeNoVotes).toHaveText(
      `${proposalToCheck.ccNoVotes}`
    );
  });
});

test("4F. Should Disable DRep functionality upon wallet disconnection on governance actions page", async ({
  page,
  browser,
}) => {
  test.slow(); // Due to queue in pop wallets

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
