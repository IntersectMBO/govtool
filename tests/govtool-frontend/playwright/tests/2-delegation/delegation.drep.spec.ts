import environments from "@constants/environments";
import { dRep01Wallet, user01Wallet } from "@constants/staticWallets";
import { createTempDRepAuth } from "@datafactory/createAuth";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { skipIfNotHardFork } from "@helpers/cardano";
import { ShelleyWallet } from "@helpers/crypto";
import { isMobile, openDrawer } from "@helpers/mobile";
import { createNewPageWithWallet } from "@helpers/page";
import extractDRepFromWallet from "@helpers/shellyWallet";
import DRepDetailsPage from "@pages/dRepDetailsPage";
import DRepDirectoryPage from "@pages/dRepDirectoryPage";
import DRepRegistrationPage from "@pages/dRepRegistrationPage";
import { expect } from "@playwright/test";
import { LinkType } from "@types";
import walletManager from "lib/walletManager";

test.beforeEach(async () => {
  await setAllureEpic("2. Delegation");
  await skipIfNotHardFork();
});

test("2C. Should open wallet connection popup on delegate in disconnected state", async ({
  page,
}) => {
  await page.goto("/");
  if (isMobile(page)) {
    openDrawer(page);
  }

  await page.getByTestId("view-drep-directory-button").click();
  await page.getByTestId("search-input").fill(dRep01Wallet.dRepId);
  await page
    .getByTestId(`${dRep01Wallet.dRepId}-connect-to-delegate-button`)
    .click();
  await expect(page.getByTestId("connect-your-wallet-modal")).toBeVisible();
});

test("2N. Should show DRep information on details page", async ({
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

  const name = faker.person.firstName();
  const objectives = faker.lorem.paragraph(2);
  const motivations = faker.lorem.paragraph(2);
  const qualifications = faker.lorem.paragraph(2);
  const paymentAddress = ShelleyWallet.fromJson(wallet).rewardAddressBech32(0);
  const linksReferenceLinks: LinkType[] = [
    {
      url: faker.internet.url(),
      description: faker.internet.displayName(),
    },
  ];

  const identityReferenceLinks: LinkType[] = [
    {
      url: faker.internet.url(),
      description: faker.internet.displayName(),
    },
  ];

  await dRepRegistrationPage.register({
    name,
    objectives,
    motivations,
    qualifications,
    paymentAddress,
    linksReferenceLinks,
    identityReferenceLinks,
  });

  await dRepRegistrationPage.confirmBtn.click();

  // Add an assertion to prevent clicking on "View Your dRep Details".
  await expect(
    dRepPage.getByTestId("dRep-id-display-card-dashboard")
  ).toContainText(wallet.dRepId, { timeout: 10_000 });
  await dRepPage.getByTestId("view-drep-details-button").click();

  // Verification
  await expect(dRepPage.getByTestId("copy-drep-id-button")).toHaveText(
    wallet.dRepId
  );
  await expect(dRepPage.getByTestId("copy-payment-address-button")).toHaveText(
    paymentAddress
  );
  await expect(dRepPage.getByTestId("Active-pill")).toHaveText("Active");
  await expect(dRepPage.getByTestId("voting-power")).toHaveText("₳ 0");

  await expect(
    dRepPage.getByTestId("objectives-info-item-description")
  ).toHaveText(objectives);
  await expect(
    dRepPage.getByTestId("motivations-info-item-description")
  ).toHaveText(motivations);
  await expect(
    dRepPage.getByTestId("qualifications-info-item-description")
  ).toHaveText(qualifications);

  for (const link of linksReferenceLinks) {
    await expect(
      dRepPage.getByTestId(`${link.description.toLowerCase()}-link`)
    ).toHaveText(link.url);
  }

  for (const link of identityReferenceLinks) {
    await expect(
      dRepPage.getByTestId(`${link.description.toLowerCase()}-link`)
    ).toHaveText(link.url);
  }
});

test("2P. Should enable sharing of DRep details", async ({ page, context }) => {
  await context.grantPermissions(["clipboard-read", "clipboard-write"]);

  const dRepDetailsPage = new DRepDetailsPage(page);
  await dRepDetailsPage.goto(dRep01Wallet.dRepId);

  await dRepDetailsPage.shareLink();
  await expect(page.getByText("Copied to clipboard")).toBeVisible();

  const copiedText = await page.evaluate(() => navigator.clipboard.readText());
  expect(copiedText).toEqual(
    `${environments.frontendUrl}/drep_directory/${dRep01Wallet.dRepId}`
  );
});

test("2Q. Should include DRep status and voting power on the DRep card", async ({
  page,
}) => {
  const dRepDirectory = new DRepDirectoryPage(page);
  await dRepDirectory.goto();

  await dRepDirectory.searchInput.fill(dRep01Wallet.dRepId);
  const dRepCard = dRepDirectory.getDRepCard(dRep01Wallet.dRepId);

  await expect(
    dRepCard.getByTestId(`${dRep01Wallet.dRepId}-voting-power`)
  ).toBeVisible();
  await expect(
    dRepCard.getByTestId(`${dRep01Wallet.dRepId}-Active-pill`)
  ).toBeVisible();
});

test.describe("Insufficient funds", () => {
  test.use({ storageState: ".auth/user01.json", wallet: user01Wallet });

  test("2T. Should show warning message on delegation when insufficient funds", async ({
    page,
  }) => {
    const dRepDirectoryPage = new DRepDirectoryPage(page);
    await dRepDirectoryPage.goto();

    await dRepDirectoryPage.searchInput.fill(dRep01Wallet.dRepId);
    const delegateBtn = page.getByTestId(
      `${dRep01Wallet.dRepId}-delegate-button`
    );
    await expect(delegateBtn).toBeVisible();
    await page.getByTestId(`${dRep01Wallet.dRepId}-delegate-button`).click();

    await expect(dRepDirectoryPage.delegationErrorModal).toBeVisible({
      timeout: 10_000,
    });
  });
});

test("2I. Should check validity of DRep Id", async ({ page }) => {
  const dRepDirectory = new DRepDirectoryPage(page);
  await dRepDirectory.goto();

  await dRepDirectory.searchInput.fill(dRep01Wallet.dRepId);
  await expect(dRepDirectory.getDRepCard(dRep01Wallet.dRepId)).toBeVisible();

  const wallet = await ShelleyWallet.generate();
  const invalidDRepId = extractDRepFromWallet(wallet);

  await dRepDirectory.searchInput.fill(invalidDRepId);
  await expect(dRepDirectory.getDRepCard(invalidDRepId)).not.toBeVisible();
});

test("2J. Should search by DRep id", async ({ page }) => {
  const dRepDirectory = new DRepDirectoryPage(page);
  await dRepDirectory.goto();

  await dRepDirectory.searchInput.fill(dRep01Wallet.dRepId);
  await expect(dRepDirectory.getDRepCard(dRep01Wallet.dRepId)).toBeVisible();
});

test("2M. Should access dRep directory page on disconnected state", async ({
  page,
}) => {
  const dRepDirectoryPage = new DRepDirectoryPage(page);
  await dRepDirectoryPage.goto();

  const dRepCards = await dRepDirectoryPage.getAllListedDReps();
  expect(dRepCards.length).toBeGreaterThan(1);
});
