import environments from "@constants/environments";
import { dRep01Wallet } from "@constants/staticWallets";
import { createTempDRepAuth } from "@datafactory/createAuth";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { isBootStrapingPhase, skipIfNotHardFork } from "@helpers/cardano";
import { encodeCIP129Identifier } from "@helpers/encodeDecode";
import { createNewPageWithWallet } from "@helpers/page";
import { waitForTxConfirmation } from "@helpers/transaction";
import GovernanceActionDetailsPage from "@pages/governanceActionDetailsPage";
import GovernanceActionsPage from "@pages/governanceActionsPage";
import { Page, expect } from "@playwright/test";
import kuberService from "@services/kuberService";
import { BootstrapGovernanceActionType, GrovernanceActionType } from "@types";
import walletManager from "lib/walletManager";

const invalidInfinityProposals = require("../../lib/_mock/invalidInfinityProposals.json");

test.beforeEach(async () => {
  await setAllureEpic("5. Proposal functionality");
  await skipIfNotHardFork();
});

test.describe("Proposal checks", () => {
  test.use({ storageState: ".auth/dRep01.json", wallet: dRep01Wallet });

  let govActionDetailsPage: GovernanceActionDetailsPage;
  let currentPage: Page;

  test.beforeEach(async ({ page }) => {
    const govActionsPage = new GovernanceActionsPage(page);
    await govActionsPage.goto();

    currentPage = page;
    govActionDetailsPage = (await isBootStrapingPhase())
      ? await govActionsPage.viewFirstProposalByGovernanceAction(
          GrovernanceActionType.InfoAction
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
    test("5D_1. Should accept valid data in provide context", async () => {
      await govActionDetailsPage.contextBtn.click();

      await expect(govActionDetailsPage.contextInput).toBeVisible();

      for (let i = 0; i < 100; i++) {
        const randomContext = faker.lorem.paragraph(2);
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
        const randomContext = faker.lorem.paragraph(40);
        await govActionDetailsPage.contextInput.fill(randomContext);
        expect(
          await govActionDetailsPage.contextInput.textContent()
        ).not.toEqual(randomContext);
      }
    });
  });
});

test.describe("Bad Proposals", () => {
  test.use({ storageState: ".auth/dRep01.json", wallet: dRep01Wallet });

  let govActionsPage: GovernanceActionsPage;

  test.beforeEach(async ({ page }) => {
    await page.route("**/proposal/list?**", async (route) =>
      route.fulfill({
        body: JSON.stringify(invalidInfinityProposals),
      })
    );

    govActionsPage = new GovernanceActionsPage(page);
    await govActionsPage.goto();
  });

  test("5G. Should show warning in bad governance action proposal to the users to visit the site at their own risk, when external url is opened", async () => {
    const govActionDetailsPage = await govActionsPage.viewFirstProposal();

    await govActionDetailsPage.externalModalBtn.click();

    await expect(govActionDetailsPage.externalLinkModal).toBeVisible();
    await expect(
      govActionDetailsPage.currentPage.getByText("Be careful", {
        exact: false,
      })
    ).toBeVisible();
  });

  test("5H. Should open a new tab in Bad governance action proposal, when external URL is opened", async ({
    page,
  }) => {
    const govActionDetailsPage = await govActionsPage.viewFirstProposal();

    await govActionDetailsPage.externalModalBtn.click();
    await govActionDetailsPage.continueModalBtn.click();
    const existingPages = page.context().pages();
    expect(existingPages).toHaveLength(1);
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

    govActionDetailsPage = (await isBootStrapingPhase())
      ? await govActionsPage.viewFirstProposalByGovernanceAction(
          GrovernanceActionType.InfoAction
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

    await expect(
      govActionDetailsPage.currentPage.getByTestId("my-vote").getByText("Yes")
    ).toBeVisible();

    govActionDetailsPage = await governanceActionsPage.viewFirstVotedProposal();
    await govActionDetailsPage.reVote();
    await governanceActionsPage.votedTab.click();

    await expect(
      govActionDetailsPage.currentPage.getByTestId("my-vote").getByText("No")
    ).toBeVisible();
  });

  test("5F. Should show notification of casted vote after vote", async ({}, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);
    await govActionDetailsPage.vote();
    await expect(govActionDetailsPage.voteSuccessModal).toBeVisible();
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
    ).toBeVisible({ timeout: 15_000 });
    dRepPage.getByTestId("confirm-modal-button").click();
    await waitForTxConfirmation(dRepPage);

    const balance = await kuberService.getBalance(wallet.address);
    expect(balance, "Retirement deposit not returned").toBeGreaterThan(500);
  });
});

test.describe("Bootstrap phase", () => {
  test.use({ storageState: ".auth/dRep01.json", wallet: dRep01Wallet });
  test("5L. Should restrict dRep votes to Info Governance actions During Bootstrapping Phase", async ({
    page,
    context,
  }) => {
    await page.route("**/epoch/params", async (route) => {
      // Fetch the original response from the server
      const response = await route.fetch();
      const json = await response.json();

      // update protocol major version
      json["protocol_major"] = 9;
      await route.fulfill({
        status: 200,
        contentType: "application/json",
        body: JSON.stringify(json),
      });
    });

    const voteBlacklistOptions = Object.keys(GrovernanceActionType).filter(
      (option) => option !== BootstrapGovernanceActionType.InfoAction
    );

    const govActionsPage = new GovernanceActionsPage(page);
    await govActionsPage.goto();

    // wait until the loading button is hidden
    await expect(
      page.getByRole("progressbar").getByRole("img")
    ).not.toBeVisible({ timeout: 10_000 });

    for (const voteBlacklistOption of voteBlacklistOptions) {
      const governanceActionDetailsPage =
        await govActionsPage.viewFirstProposalByGovernanceAction(
          voteBlacklistOption as GrovernanceActionType
        );

      if (governanceActionDetailsPage !== null) {
        // dRep vote
        await expect(governanceActionDetailsPage.dRepYesVotes).toBeVisible();
        await expect(
          governanceActionDetailsPage.dRepAbstainVotes
        ).toBeVisible();
        await expect(governanceActionDetailsPage.dRepNoVotes).toBeVisible();

        // sPos vote
        await expect(governanceActionDetailsPage.sPosYesVotes).toBeVisible();
        await expect(
          governanceActionDetailsPage.sPosAbstainVotes
        ).toBeVisible();
        await expect(governanceActionDetailsPage.sPosNoVotes).toBeVisible();

        // ccCommittee vote
        await expect(
          governanceActionDetailsPage.ccCommitteeYesVotes
        ).toBeVisible();
        await expect(
          governanceActionDetailsPage.ccCommitteeAbstainVotes
        ).toBeVisible();
        await expect(
          governanceActionDetailsPage.ccCommitteeNoVotes
        ).toBeVisible();

        await expect(
          governanceActionDetailsPage.yesVoteRadio
        ).not.toBeVisible();
        await expect(governanceActionDetailsPage.contextBtn).not.toBeVisible();
        await expect(governanceActionDetailsPage.voteBtn).not.toBeVisible();

        await governanceActionDetailsPage.backBtn.click();
      }
    }
  });
});
