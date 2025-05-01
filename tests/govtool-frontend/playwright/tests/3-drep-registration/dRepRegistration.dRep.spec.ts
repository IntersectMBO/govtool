import environments from "@constants/environments";
import { dRep01Wallet } from "@constants/staticWallets";
import { createTempDRepAuth } from "@datafactory/createAuth";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { ShelleyWallet } from "@helpers/crypto";
import {
  skipIfBalanceIsInsufficient,
  skipIfMainnet,
  skipIfTemporyWalletIsNotAvailable,
} from "@helpers/cardano";
import { createNewPageWithWallet } from "@helpers/page";
import { waitForTxConfirmation } from "@helpers/transaction";
import DRepRegistrationPage from "@pages/dRepRegistrationPage";
import GovernanceActionsPage from "@pages/governanceActionsPage";
import { expect } from "@playwright/test";
import walletManager from "lib/walletManager";
import DRepDirectoryPage from "@pages/dRepDirectoryPage";
import { GovernanceActionType } from "@types";
import { dRep01AuthFile } from "@constants/auth";

test.beforeEach(async () => {
  await setAllureEpic("3. DRep registration");
  await skipIfMainnet();
  await skipIfTemporyWalletIsNotAvailable("registerDRepWallets.json");
});

test.describe("Logged in DReps", () => {
  test.use({
    storageState: dRep01AuthFile,
    wallet: dRep01Wallet,
    enableDRepSigning: true,
    enableStakeSigning: false,
  });

  test("3A. Should show dRepId on dashboard and enable voting on governance actions after connecting registered dRep Wallet", async ({
    page,
  }) => {
    await page.goto("/");

    await expect(page.getByTestId("voting-power-chips")).toBeVisible({
      timeout: 60_000,
    });

    await expect(
      page.getByTestId("dRep-id-display-card-dashboard")
    ).toContainText(dRep01Wallet.dRepId, { timeout: 60_000 });

    const governanceActionsPage = new GovernanceActionsPage(page);

    await governanceActionsPage.goto();

    await expect(page.getByText(/info action/i).first()).toBeVisible({
      timeout: 60_000,
    });

    const governanceActionDetailsPage =
      await governanceActionsPage.viewFirstProposalByGovernanceAction(
        GovernanceActionType.InfoAction
      );

    await expect(governanceActionDetailsPage.voteBtn).toBeVisible({
      timeout: 60_000,
    });
  });

  test("3H. Should Update DRep data", async ({ page }, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    await page.goto("/");

    // Add an assertion to prevent clicking on "View Your dRep Details".
    await expect(
      page.getByTestId("dRep-id-display-card-dashboard")
    ).toContainText(dRep01Wallet.dRepId, { timeout: 20_000 });

    await page.getByTestId("view-drep-details-button").click();
    await page.getByTestId("edit-drep-data-button").click();
    const editDRepPage = new DRepRegistrationPage(page);

    const newDRepName = faker.person.firstName();

    await editDRepPage.register({
      name: newDRepName,
      objectives: faker.lorem.paragraph(2),
      motivations: faker.lorem.paragraph(2),
      qualifications: faker.lorem.paragraph(2),
      paymentAddress: (await ShelleyWallet.generate()).addressBech32(
        environments.networkId
      ),
      linksReferenceLinks: [
        {
          url: faker.internet.url(),
          description: faker.internet.displayName(),
        },
      ],
      identityReferenceLinks: [
        {
          url: faker.internet.url(),
          description: faker.internet.displayName(),
        },
      ],
    });
    await page.getByTestId("confirm-modal-button").click();
  });

  test("3S. Should restrict dRep registration for dRep", async ({ page }) => {
    await page.goto(`${environments.frontendUrl}/register_drep`);

    await expect(page.getByText("You already are a DRep")).toBeVisible({
      timeout: 60_000,
    });
    await expect(page.getByTestId("name-input")).not.toBeVisible();
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
      enableDRepSigning: true,
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
      enableDRepSigning: true,
    });

    const dRepRegistrationPage = new DRepRegistrationPage(dRepPage);
    await dRepRegistrationPage.goto();
    await dRepRegistrationPage.register({
      name: faker.person.firstName(),
      donNotList: true,
    });

    await dRepRegistrationPage.confirmBtn.click();

    await expect(dRepPage.getByTestId("d-rep-in-progress")).not.toBeVisible({
      timeout: 60_000,
    });

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
    await walletManager.removeCopyWallet(wallet, "registeredDRepCopy");

    const tempDRepAuth = await createTempDRepAuth(page, wallet);
    const dRepPage = await createNewPageWithWallet(browser, {
      storageState: tempDRepAuth,
      wallet,
      enableDRepSigning: true,
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
    await walletManager.removeCopyWallet(wallet, "registeredDRepCopy");

    const dRepAuth = await createTempDRepAuth(page, wallet);
    const dRepPage = await createNewPageWithWallet(browser, {
      storageState: dRepAuth,
      wallet,
      enableDRepSigning: true,
    });

    await dRepPage.goto("/");
    await dRepPage.getByTestId("retire-button").click();
    await dRepPage.getByTestId("continue-retirement-button").click();
    await expect(
      dRepPage.getByTestId("retirement-transaction-submitted-modal")
    ).toBeVisible({ timeout: 15_000 });
    dRepPage.getByTestId("confirm-modal-button").click();

    await waitForTxConfirmation(dRepPage);

    await expect(dRepPage.getByTestId("voting-power-chips")).not.toBeVisible({
      timeout: 20_000,
    });

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
      enableDRepSigning: true,
    });

    const dRepRegistrationPage = new DRepRegistrationPage(dRepPage);
    await dRepRegistrationPage.goto();
    await dRepRegistrationPage.registerWithoutTxConfirmation({
      name: faker.person.firstName(),
    });
    await dRepRegistrationPage.registrationSuccessModal
      .getByTestId("confirm-modal-button")
      .click();

    await expect(dRepPage.getByTestId("d-rep-in-progress")).toHaveText(
      /in progress/i,
      { timeout: 20_000 }
    );

    await waitForTxConfirmation(dRepPage);

    await expect(dRepPage.getByTestId("d-rep-in-progress")).not.toBeVisible({
      timeout: 20_000,
    });
  });
});
