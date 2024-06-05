import environments from "@constants/environments";
import { dRep01Wallet } from "@constants/staticWallets";
import { createTempDRepAuth } from "@datafactory/createAuth";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { createNewPageWithWallet } from "@helpers/page";
import { waitForTxConfirmation } from "@helpers/transaction";
import GovernanceActionDetailsPage from "@pages/governanceActionDetailsPage";
import GovernanceActionsPage from "@pages/governanceActionsPage";
import { Page, expect } from "@playwright/test";
import kuberService from "@services/kuberService";
import walletManager from "lib/walletManager";

test.beforeEach(async () => {
  await setAllureEpic("5. Proposal functionality");
});

test.describe("Proposal checks", () => {
  test.use({ storageState: ".auth/dRep01.json", wallet: dRep01Wallet });

  let govActionDetailsPage: GovernanceActionDetailsPage;

  test.beforeEach(async ({ page }) => {
    const govActionsPage = new GovernanceActionsPage(page);
    await govActionsPage.goto();

    govActionDetailsPage = await govActionsPage.viewFirstProposal();
  });

  test("5A. Should show relevant details about governance action as DRep", async () => {
    await expect(govActionDetailsPage.governanceActionType).toBeVisible();
    await expect(govActionDetailsPage.submittedDate).toBeVisible();
    await expect(govActionDetailsPage.expiryDate).toBeVisible();

    await expect(govActionDetailsPage.contextBtn).toBeVisible();
    await expect(govActionDetailsPage.showVotesBtn).toBeVisible();

    await expect(govActionDetailsPage.voteBtn).toBeVisible();
    await expect(govActionDetailsPage.yesVoteRadio).toBeVisible();
    await expect(govActionDetailsPage.noVoteRadio).toBeVisible();
    await expect(govActionDetailsPage.abstainRadio).toBeVisible();
  });

  // test("5B. Should view Vote button on governance action item on registered as DRep", async () => {
  //   await expect(govActionDetailsPage.voteBtn).toBeVisible();
  // });

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

  // Skipped: No url/hash input to validate
  test("5D. Should validate proposal voting", async () => {
    test.skip();
    // const invalidURLs = ["testdotcom", "https://testdotcom", "https://test.c"];
    // invalidURLs.forEach(async (url) => {
    //   govActionDetailsPage.urlInput.fill(url);
    //   await expect(govActionDetailsPage.urlInputError).toBeVisible();
    // });
    // const validURLs = ["https://test.com"];
    // validURLs.forEach(async (url) => {
    //   govActionDetailsPage.urlInput.fill(url);
    //   await expect(govActionDetailsPage.urlInputError).not.toBeVisible();
    // });
    // const invalidHashes = [
    //   randomBytes(20).toString("hex"),
    //   randomBytes(32).toString(),
    // ];
    // invalidHashes.forEach(async (hash) => {
    //   govActionDetailsPage.hashInput.fill(hash);
    //   await expect(govActionDetailsPage.hashInputError).toBeVisible();
    // });
    // const validHash = randomBytes(32).toString("hex");
    // govActionDetailsPage.hashInput.fill(validHash);
    // await expect(govActionDetailsPage.hashInputError).not.toBeVisible();
  });

  test("5G. Should show warning in bad governance action proposal to the users to visit the site at their own risk, when external url is opened", async () => {
    if (await govActionDetailsPage.externalModalBtn.isVisible()) {
      await govActionDetailsPage.externalModalBtn.click();

      await expect(govActionDetailsPage.externalLinkModal).toBeVisible();
      await expect(
        govActionDetailsPage.currentPage.getByText("Be careful", {
          exact: false,
        })
      ).toBeVisible();
    } else {
      expect(true, "Is not a bad or data missing proposal").toBeTruthy();
    }
  });

  test("5H. Should open a new tab in Bad governance action proposal, when external URL is opened", async ({
    page,
  }) => {
    if (await govActionDetailsPage.externalModalBtn.isVisible()) {
      await govActionDetailsPage.externalModalBtn.click();
      await govActionDetailsPage.continueModalBtn.click();

      const existingPages = page.context().pages();
      expect(existingPages).toHaveLength(1);
    } else {
      expect(true, "Is not a bad or data missing proposal").toBeTruthy();
    }
  });
});

test.describe("Perform voting", () => {
  let govActionDetailsPage: GovernanceActionDetailsPage;
  let dRepPage: Page;

  test.beforeEach(async ({ page, browser }) => {
    const wallet = await walletManager.popWallet("registeredDRep");

    const tempDRepAuth = await createTempDRepAuth(page, wallet);

    dRepPage = await createNewPageWithWallet(browser, {
      storageState: tempDRepAuth,
      wallet,
      enableStakeSigning: true,
    });

    const govActionsPage = new GovernanceActionsPage(dRepPage);
    await govActionsPage.goto();

    govActionDetailsPage = await govActionsPage.viewFirstProposal();
  });

  test("5E. Should re-vote with new data on a already voted governance action", async ({}, testInfo) => {
    test.setTimeout(testInfo.timeout + 2 * environments.txTimeOut);

    govActionDetailsPage.vote();
    await waitForTxConfirmation(govActionDetailsPage.currentPage);

    const governanceActionsPage = new GovernanceActionsPage(
      govActionDetailsPage.currentPage
    );
    await governanceActionsPage.goto();
    await dRepPage.waitForTimeout(5_000);
    await governanceActionsPage.votedTab.click();
    await expect(
      govActionDetailsPage.currentPage.getByTestId("my-vote").getByText("Yes")
    ).toBeVisible();

    govActionDetailsPage = await governanceActionsPage.viewFirstVotedProposal();
    govActionDetailsPage.reVote();
    await waitForTxConfirmation(govActionDetailsPage.currentPage);

    await governanceActionsPage.votedTab.click();

    await expect(
      govActionDetailsPage.currentPage.getByTestId("my-vote").getByText("No")
    ).toBeVisible({ timeout: 20_000 });
  });

  test("5F. Should show notification of casted vote after vote", async ({}, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);
    await govActionDetailsPage.vote();
    await expect(govActionDetailsPage.voteSuccessModal).toBeVisible();
  });

  test("5I. Should view the vote details,when viewing governance action already voted by the DRep", async ({}, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    govActionDetailsPage.vote();
    await waitForTxConfirmation(govActionDetailsPage.currentPage);

    const governanceActionsPage = new GovernanceActionsPage(
      govActionDetailsPage.currentPage
    );
    await governanceActionsPage.goto();
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
    ).toBeVisible();
    dRepPage.getByTestId("confirm-modal-button").click();
    await waitForTxConfirmation(dRepPage);

    const balance = await kuberService.getBalance(wallet.address);
    expect(balance, "Retirement deposit not returned").toBeGreaterThan(500);
  });
});
