import environments from "@constants/environments";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { skipIfMainnet } from "@helpers/cardano";
import { createNewPageWithWallet } from "@helpers/page";
import { waitForTxConfirmation } from "@helpers/transaction";
import DRepRegistrationPage from "@pages/dRepRegistrationPage";
import GovernanceActionsPage from "@pages/governanceActionsPage";
import { expect } from "@playwright/test";
import DRepDirectoryPage from "@pages/dRepDirectoryPage";
import { GovernanceActionType } from "@types";
import { sharedDRep } from "lib/wallet/sharedDReps";
import { ensureFunded, randomAddress, testWallet } from "lib/wallet/testWallets";
import { registeredDRepWallet } from "lib/wallet/transactions";

test.beforeEach(async () => {
  await setAllureEpic("3. DRep registration");
  await skipIfMainnet();
});

/** A funded wallet whose DRep key is not registered yet. */
async function unregisteredDRepWallet(name: string) {
  const wallet = await testWallet(name);
  await ensureFunded(wallet, 600);
  return wallet;
}

test.describe("Logged in DReps", () => {
  test.use({ walletName: "dRep01" });

  test.beforeAll(async () => {
    test.setTimeout(2 * environments.txTimeOut);
    await sharedDRep("dRep01");
  });

  test("3A. Should show dRepId on dashboard and enable voting on governance actions after connecting registered dRep Wallet", async ({
    page,
  }) => {
    await page.goto("/");

    await expect(page.getByTestId("voting-power-chips")).toBeVisible({
      timeout: 60_000,
    });

    
    const governanceActionsPage = new GovernanceActionsPage(page);
    
    await governanceActionsPage.goto();
    
    await governanceActionsPage.getFirstProposal();
    const governanceActionDetailsPage =
      await governanceActionsPage.viewFirstProposalByGovernanceAction(
        GovernanceActionType.InfoAction
      );

    await expect(governanceActionDetailsPage.voteBtn).toBeVisible({
      timeout: 60_000,
    });
  });

  test("3H. Should Update DRep data", async ({ page, wallet }, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    await page.goto("/");

    // Add an assertion to prevent clicking on "View Your dRep Details".
    await expect(
      page.getByTestId("dRep-id-display-card-dashboard")
    ).toContainText(wallet!.dRepId, { timeout: 20_000 });

    await page.getByTestId("view-drep-details-button").click();
    await page.getByTestId("edit-drep-data-button").click();
    const editDRepPage = new DRepRegistrationPage(page);

    const newDRepName = faker.person.firstName();

    await editDRepPage.register({
      name: newDRepName,
      objectives: faker.lorem.paragraph(2),
      motivations: faker.lorem.paragraph(2),
      qualifications: faker.lorem.paragraph(2),
      paymentAddress: await randomAddress(),
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
    browser,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + 2 * environments.txTimeOut);

    const wallet = await unregisteredDRepWallet("3G:dRep");
    const dRepPage = await createNewPageWithWallet(browser, { wallet });

    const dRepRegistrationPage = new DRepRegistrationPage(dRepPage);
    await dRepRegistrationPage.goto();
    await dRepRegistrationPage.register({ name: faker.person.firstName() });

    await expect(dRepRegistrationPage.registrationSuccessModal).toBeVisible();
    await expect(
      dRepRegistrationPage.registrationSuccessModal.getByText("this link")
    ).toBeVisible();
  });

  test("3Q Should not list dRep in the dRep directory when 'doNotList' is checked during registration", async ({
    browser,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + 2 * environments.txTimeOut);

    const wallet = await unregisteredDRepWallet("3Q:dRep");
    const dRepPage = await createNewPageWithWallet(browser, { wallet });

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
    const dRepDirectoryPage = new DRepDirectoryPage(dRepPage);
    await dRepDirectoryPage.verifyDRepInList(wallet.dRepId);

    // disconnected state
    await dRepPage.getByTestId("disconnect-button").click();
    await dRepDirectoryPage.verifyDRepInList(wallet.dRepId);
  });

  test("3J. Should verify retire as DRep", async ({ browser }, testInfo) => {
    test.setTimeout(testInfo.timeout + 2 * environments.txTimeOut);

    const wallet = await registeredDRepWallet("3J:dRep");
    const dRepPage = await createNewPageWithWallet(browser, { wallet });

    await dRepPage.goto("/");
    await dRepPage.getByTestId("retire-button").click();
    await dRepPage.getByTestId("continue-retirement-button").click();

    await expect(
      dRepPage.getByTestId("retirement-transaction-submitted-modal")
    ).toBeVisible({ timeout: 15_000 });
  });

  test("3K. Verify DRep behavior in retired state", async ({
    browser,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + 3 * environments.txTimeOut);

    const wallet = await registeredDRepWallet("3K:dRep");
    const dRepPage = await createNewPageWithWallet(browser, { wallet });

    await dRepPage.goto("/");
    await dRepPage.getByTestId("retire-button").click();
    await dRepPage.getByTestId("continue-retirement-button").click();
    await expect(
      dRepPage.getByTestId("retirement-transaction-submitted-modal")
    ).toBeVisible({ timeout: 60_000 });
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
    browser,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + 2 * environments.txTimeOut);

    const wallet = await unregisteredDRepWallet("3I:dRep");
    const dRepPage = await createNewPageWithWallet(browser, { wallet });

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
