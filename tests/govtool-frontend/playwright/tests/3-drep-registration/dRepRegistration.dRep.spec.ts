import environments from "@constants/environments";
import { dRep01Wallet } from "@constants/staticWallets";
import { createTempDRepAuth } from "@datafactory/createAuth";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { downloadMetadata } from "@helpers/metadata";
import { createNewPageWithWallet } from "@helpers/page";
import { waitForTxConfirmation } from "@helpers/transaction";
import DRepRegistrationPage from "@pages/dRepRegistrationPage";
import GovernanceActionsPage from "@pages/governanceActionsPage";
import { Download, expect } from "@playwright/test";
import metadataBucketService from "@services/metadataBucketService";
import walletManager from "lib/walletManager";

test.beforeEach(async () => {
  await setAllureEpic("3. DRep registration");
});

test.describe("Logged in DReps", () => {
  test.use({ storageState: ".auth/dRep01.json", wallet: dRep01Wallet });

  test("3A. Should show dRepId on dashboard after connecting registered dRep Wallet", async ({
    page,
  }) => {
    await page.goto("/");
    await expect(page.getByTestId("dRep-id-display")).toContainText(
      dRep01Wallet.dRepId
    ); // BUG: testId -> dRep-id-display-dashboard (It is taking sidebar dRep-id)
  });

  test("3H. Should Update DRep data", async ({ page }, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    await page.goto("/");

    await page.getByTestId("view-drep-details-button").click();
    await page.getByTestId("edit-drep-data-button").click();

    const newDRepName = faker.internet.userName();
    await page.getByPlaceholder("ex. JohnDRep").fill(newDRepName);
    await page.getByTestId("continue-button").click();
    await page.getByRole("checkbox").click();
    await page.getByTestId("continue-button").click();

    page.getByRole("button", { name: `${newDRepName}.jsonld` }).click();
    const download: Download = await page.waitForEvent("download");
    const dRepMetadata = await downloadMetadata(download);

    const url = await metadataBucketService.uploadMetadata(
      dRepMetadata.name,
      dRepMetadata.data
    );

    await page.getByPlaceholder("URL").fill(url);
    await page.getByTestId("continue-button").click(); // BUG -> incorrect test id
    await page.getByTestId("confirm-modal-button").click();

    await waitForTxConfirmation(page);
  });
});

test.describe("Temporary DReps", () => {
  test("3G. Should show confirmation message with link to view transaction, when DRep registration txn is submitted", async ({
    page,
    browser,
  }) => {
    const wallet = await walletManager.popWallet("registerDRep");

    const tempDRepAuth = await createTempDRepAuth(page, wallet);
    const dRepPage = await createNewPageWithWallet(browser, {
      storageState: tempDRepAuth,
      wallet,
      enableStakeSigning: true,
    });

    const dRepRegistrationPage = new DRepRegistrationPage(dRepPage);
    await dRepRegistrationPage.goto();
    await dRepRegistrationPage.register({ name: faker.person.firstName() });

    await expect(dRepRegistrationPage.registrationSuccessModal).toBeVisible();
    await expect(
      dRepRegistrationPage.registrationSuccessModal.getByText("this link")
    ).toBeVisible();
  });

  test("3I. Should verify retire as DRep", async ({ page, browser }) => {
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
      dRepPage.getByTestId("retirement-transaction-error-modal")
    ).toBeVisible();
  });

  test("3J. Verify DRep behavior in retired state", async ({
    page,
    browser,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    const wallet = await walletManager.popWallet("registeredDRep");

    const dRepAuth = await createTempDRepAuth(page, wallet);
    const dRepPage = await createNewPageWithWallet(browser, {
      storageState: dRepAuth,
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
    await expect(dRepPage.getByText("Voting power:₳")).not.toBeVisible();

    const governanceActionsPage = new GovernanceActionsPage(dRepPage);
    await governanceActionsPage.goto();
    const govActionDetailsPage =
      await governanceActionsPage.viewFirstProposal();
    await expect(govActionDetailsPage.voteBtn).not.toBeVisible();
  });

  test("3K. Should display 'In Progress' status on dashboard until blockchain confirms DRep registration", async ({
    page,
    browser,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    const wallet = await walletManager.popWallet("registerDRep");

    const dRepAuth = await createTempDRepAuth(page, wallet);
    const dRepPage = await createNewPageWithWallet(browser, {
      storageState: dRepAuth,
      wallet,
      enableStakeSigning: true,
    });

    const dRepRegistrationPage = new DRepRegistrationPage(dRepPage);
    await dRepRegistrationPage.goto();
    await dRepRegistrationPage.register({ name: faker.person.firstName() });
    dRepRegistrationPage.registrationSuccessModal
      .getByTestId("confirm-modal-button")
      .click();

    await expect(
      dRepPage.locator("span").filter({ hasText: "In Progress" })
    ).toBeVisible(); // BUG add proper testId for dRep registration card
  });
});
