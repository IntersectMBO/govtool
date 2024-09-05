import environments from "@constants/environments";
import { dRep01Wallet } from "@constants/staticWallets";
import { createTempDRepAuth } from "@datafactory/createAuth";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { ShelleyWallet } from "@helpers/crypto";
import { skipIfNotHardFork } from "@helpers/cardano";
import { createNewPageWithWallet } from "@helpers/page";
import { waitForTxConfirmation } from "@helpers/transaction";
import DRepRegistrationPage from "@pages/dRepRegistrationPage";
import GovernanceActionsPage from "@pages/governanceActionsPage";
import { expect } from "@playwright/test";
import walletManager from "lib/walletManager";
import DRepDirectoryPage from "@pages/dRepDirectoryPage";

test.beforeEach(async () => {
  await setAllureEpic("3. DRep registration");
  await skipIfNotHardFork();
});

test.describe("Logged in DReps", () => {
  test.use({ storageState: ".auth/dRep01.json", wallet: dRep01Wallet });

  test("3A. Should show dRepId on dashboard after connecting registered dRep Wallet", async ({
    page,
  }) => {
    await page.goto("/");

    await expect(page.getByTestId("voting-power-chips")).toBeVisible();

    await expect(
      page.getByTestId("dRep-id-display-card-dashboard")
    ).toContainText(dRep01Wallet.dRepId);

    const governanceActionsPage = new GovernanceActionsPage(page);

    await governanceActionsPage.goto();
    const governanceActionDetailsPage =
      await governanceActionsPage.viewFirstInfoProposal();

    await expect(governanceActionDetailsPage.voteBtn).toBeVisible();
  });

  test("3H. Should Update DRep data", async ({ page }, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    await page.goto("/");

    await page.getByTestId("view-drep-details-button").click();
    await page.getByTestId("edit-drep-data-button").click();
    const editDRepPage = new DRepRegistrationPage(page);

    const newDRepName = faker.person.firstName();

    await editDRepPage.register({
      name: newDRepName,
      objectives: faker.lorem.paragraph(2),
      motivations: faker.lorem.paragraph(2),
      qualifications: faker.lorem.paragraph(2),
      paymentAddress: (await ShelleyWallet.generate()).addressBech32(0),
      extraContentLinks: [faker.internet.url()],
    });
    await page.getByTestId("confirm-modal-button").click();
  });
});

test.describe("Temporary DReps", () => {
  test("3G. Should show confirmation message with link to view transaction, when DRep registration txn is submitted", async ({
    page,
    browser,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

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

  test("3Q Should not list dRep in the dRep directory when 'doNotList' is checked during registration", async ({
    page,
    browser,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    const wallet = await walletManager.popWallet("registerDRep");

    const tempDRepAuth = await createTempDRepAuth(page, wallet);
    const dRepPage = await createNewPageWithWallet(browser, {
      storageState: tempDRepAuth,
      wallet,
      enableStakeSigning: true,
    });

    const dRepRegistrationPage = new DRepRegistrationPage(dRepPage);
    await dRepRegistrationPage.goto();
    await dRepRegistrationPage.register({
      name: faker.person.firstName(),
      donNotList: true,
    });

    await dRepRegistrationPage.confirmBtn.click();

    await expect(dRepPage.getByTestId("d-rep-in-progress")).not.toBeVisible();

    // connected state
    const dRepDirectoryPage = new DRepDirectoryPage(page);
    await dRepDirectoryPage.verifyDRepInList(wallet.dRepId);

    // disconnected state
    await page.getByTestId("disconnect-button").click();
    await dRepDirectoryPage.verifyDRepInList(wallet.dRepId);
  });

  test("3J. Should verify retire as DRep", async ({ page, browser }) => {
    test.slow(); // Due to queue in pop wallets

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
    ).toBeVisible({ timeout: 15_000 });
  });

  test("3K. Verify DRep behavior in retired state", async ({
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
    ).toBeVisible({ timeout: 15_000 });
    dRepPage.getByTestId("confirm-modal-button").click();

    await waitForTxConfirmation(dRepPage);

    await expect(dRepPage.getByTestId("voting-power-chips")).not.toBeVisible();

    await expect(dRepPage.getByTestId("dRep-id-display")).not.toBeVisible();

    const governanceActionsPage = new GovernanceActionsPage(dRepPage);
    await governanceActionsPage.goto();
    const govActionDetailsPage =
      await governanceActionsPage.viewFirstProposal();
    await expect(govActionDetailsPage.voteBtn).not.toBeVisible();
  });

  test("3I. Should display 'In Progress' status on dashboard until blockchain confirms DRep registration", async ({
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
    await dRepRegistrationPage.registrationSuccessModal
      .getByTestId("confirm-modal-button")
      .click();

    await expect(dRepPage.getByTestId("d-rep-in-progress")).toHaveText(
      /in progress/i
    );
  });
});
