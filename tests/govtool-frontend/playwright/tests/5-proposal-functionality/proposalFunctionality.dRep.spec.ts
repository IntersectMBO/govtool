import environments from "@constants/environments";
import { dRep01Wallet } from "@constants/staticWallets";
import { createTempDRepAuth } from "@datafactory/createAuth";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import {
  isBootStrapingPhase,
  skipIfMainnet,
  skipIfNotHardFork,
} from "@helpers/cardano";
import { encodeCIP129Identifier } from "@helpers/encodeDecode";
import { createNewPageWithWallet } from "@helpers/page";
import { waitForTxConfirmation } from "@helpers/transaction";
import GovernanceActionDetailsPage from "@pages/governanceActionDetailsPage";
import GovernanceActionsPage from "@pages/governanceActionsPage";
import { Page, expect } from "@playwright/test";
import kuberService from "@services/kuberService";
import { BootstrapGovernanceActionType, GovernanceActionType } from "@types";
import { allure } from "allure-playwright";
import walletManager from "lib/walletManager";

test.beforeEach(async () => {
  await setAllureEpic("5. Proposal functionality");
  await skipIfNotHardFork();
  await skipIfMainnet();
});

test.describe("Proposal checks", () => {
  test.use({ storageState: ".auth/dRep01.json", wallet: dRep01Wallet });

  let govActionDetailsPage: GovernanceActionDetailsPage;
  let currentPage: Page;

  test.beforeEach(async ({ page }) => {
    const govActionsPage = new GovernanceActionsPage(page);
    await govActionsPage.goto();

    // assert to wait until the loading button is hidden
    await expect(page.getByTestId("to-vote-tab")).toBeVisible({
      timeout: 60_000,
    });

    currentPage = page;
    govActionDetailsPage = (await isBootStrapingPhase())
      ? await govActionsPage.viewFirstProposalByGovernanceAction(
          GovernanceActionType.InfoAction
        )
      : await govActionsPage.viewFirstProposal();
  });

  test("5A. Should show relevant details about governance action as DRep", async () => {
    const governanceActionIdWithIndex = currentPage.url().split("/").pop();
    const governanceActionId = governanceActionIdWithIndex.substring(0, 64);

    const cip129GovActionId = encodeCIP129Identifier({
      txID: governanceActionId,
      index: governanceActionIdWithIndex
        .replace(`${governanceActionId}#`, "")
        .toString()
        .padStart(2, "0"),
      bech32Prefix: "gov_action",
    });

    await expect(govActionDetailsPage.governanceActionType).toBeVisible();
    await expect(govActionDetailsPage.submittedDate).toBeVisible();
    await expect(govActionDetailsPage.expiryDate).toBeVisible();
    await expect(
      currentPage.getByTestId(`${governanceActionIdWithIndex}-id`)
    ).toBeVisible();
    await expect(
      currentPage.getByTestId(`${cip129GovActionId}-id`)
    ).toBeVisible();

    await expect(govActionDetailsPage.contextBtn).toBeVisible();
    await expect(govActionDetailsPage.showVotesBtn).toBeVisible();

    await expect(govActionDetailsPage.voteBtn).toBeVisible();
    await expect(govActionDetailsPage.yesVoteRadio).toBeVisible();
    await expect(govActionDetailsPage.noVoteRadio).toBeVisible();
    await expect(govActionDetailsPage.abstainRadio).toBeVisible();
  });

  test("5C. Should show required field in proposal voting on registered as DRep", async () => {
    await expect(govActionDetailsPage.voteBtn).toBeVisible();
    await expect(govActionDetailsPage.yesVoteRadio).toBeVisible();
    await expect(govActionDetailsPage.noVoteRadio).toBeVisible();
    await expect(govActionDetailsPage.abstainRadio).toBeVisible();

    await govActionDetailsPage.contextBtn.click();

    await expect(govActionDetailsPage.contextInput).toBeVisible();
    await govActionDetailsPage.cancelModalBtn.click();

    await govActionDetailsPage.yesVoteRadio.click();
    await expect(govActionDetailsPage.voteBtn).toBeEnabled();
  });

  test.describe("Validate provide context about vote", () => {
    const characters =
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
    test("5D_1. Should accept valid data in provide context", async () => {
      await govActionDetailsPage.contextBtn.click();

      await expect(govActionDetailsPage.contextInput).toBeVisible();

      for (let i = 0; i < 100; i++) {
        const randomContext = faker.string.fromCharacters(characters, {
          min: 1,
          max: 9999,
        });
        await govActionDetailsPage.contextInput.fill(randomContext);
        expect(await govActionDetailsPage.contextInput.textContent()).toEqual(
          randomContext
        );

        await expect(govActionDetailsPage.confirmModalBtn).toBeVisible();
      }
    });

    test("5D_2. Should reject invalid data in provide context", async () => {
      await govActionDetailsPage.contextBtn.click();

      await expect(govActionDetailsPage.contextInput).toBeVisible();

      for (let i = 0; i < 100; i++) {
        const randomContext = faker.string.fromCharacters(characters, 10001);
        await govActionDetailsPage.contextInput.fill(randomContext);
        expect(
          await govActionDetailsPage.contextInput.textContent()
        ).not.toEqual(randomContext);
      }
    });
  });
});

test.describe("Perform voting", () => {
  let govActionDetailsPage: GovernanceActionDetailsPage;
  let dRepPage: Page;

  test.beforeEach(async ({ page, browser }) => {
    test.slow(); // Due to queue in pop wallets

    const wallet = await walletManager.popWallet("registeredDRep");

    const tempDRepAuth = await createTempDRepAuth(page, wallet);

    dRepPage = await createNewPageWithWallet(browser, {
      storageState: tempDRepAuth,
      wallet,
      enableStakeSigning: true,
    });

    const govActionsPage = new GovernanceActionsPage(dRepPage);
    await govActionsPage.goto();

    // assert to wait until the loading button is hidden
    await expect(dRepPage.getByTestId("to-vote-tab")).toBeVisible({
      timeout: 60_000,
    });

    govActionDetailsPage = (await isBootStrapingPhase())
      ? await govActionsPage.viewFirstProposalByGovernanceAction(
          GovernanceActionType.InfoAction
        )
      : await govActionsPage.viewFirstProposal();
  });

  test("5E. Should re-vote with new data on a already voted governance action", async ({}, testInfo) => {
    test.setTimeout(testInfo.timeout + 2 * environments.txTimeOut);

    await govActionDetailsPage.vote();

    const governanceActionsPage = new GovernanceActionsPage(
      govActionDetailsPage.currentPage
    );

    await dRepPage.waitForTimeout(5_000);

    await governanceActionsPage.votedTab.click();

    await govActionDetailsPage.currentPage.evaluate(() =>
      window.scrollTo(0, 500)
    );

    await expect(
      govActionDetailsPage.currentPage.getByTestId("my-vote").getByText("Yes")
    ).toBeVisible();

    govActionDetailsPage = await governanceActionsPage.viewFirstVotedProposal();
    await govActionDetailsPage.reVote();

    await dRepPage.reload();

    await governanceActionsPage.votedTab.click();

    const isNoVoteVisible = await govActionDetailsPage.currentPage
      .getByTestId("my-vote")
      .getByText("No")
      .isVisible();

    const textContent = await govActionDetailsPage.currentPage
      .getByTestId("my-vote")
      .textContent();

    await govActionDetailsPage.currentPage.evaluate(() =>
      window.scrollTo(0, 500)
    );
    await expect(
      govActionDetailsPage.currentPage.getByTestId("my-vote").getByText("No"),
      {
        message:
          !isNoVoteVisible &&
          `"No" vote not visible, current vote status: ${textContent.match(/My Vote:(Yes|No)/)[1]}`,
      }
    ).toBeVisible({ timeout: 60_000 });
  });

  test("5F. Should show notification of casted vote after vote", async ({}, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);
    await govActionDetailsPage.vote();
    await expect(govActionDetailsPage.voteSuccessModal).toBeVisible({
      timeout: 60_000,
    });
  });

  test("5I. Should view the vote details,when viewing governance action already voted by the DRep", async ({}, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    await govActionDetailsPage.vote();

    const governanceActionsPage = new GovernanceActionsPage(
      govActionDetailsPage.currentPage
    );

    await dRepPage.waitForTimeout(5_000);

    await governanceActionsPage.votedTab.click();
    await expect(
      govActionDetailsPage.currentPage.getByTestId("my-vote").getByText("Yes")
    ).toBeVisible();
  });
});

test.describe("Check voting power", () => {
  test("5K. Should return deposit on DRep retirement", async ({
    page,
    browser,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    const wallet = await walletManager.popWallet("registeredDRep");
    await walletManager.removeCopyWallet(wallet, "registeredDRepCopy");

    const tempDRepAuth = await createTempDRepAuth(page, wallet);

    const dRepPage = await createNewPageWithWallet(browser, {
      storageState: tempDRepAuth,
      wallet,
      enableStakeSigning: true,
    });

    await dRepPage.goto("/");
    await dRepPage.getByTestId("retire-button").click();
    await dRepPage.getByTestId("continue-retirement-button").click();
    await expect(
      dRepPage.getByTestId("retirement-transaction-submitted-modal")
    ).toBeVisible({ timeout: 60_000 });
    dRepPage.getByTestId("confirm-modal-button").click();
    await waitForTxConfirmation(dRepPage);

    const balance = await kuberService.getBalance(wallet.address);
    expect(balance, "Retirement deposit not returned").toBeGreaterThan(500);
  });
});
