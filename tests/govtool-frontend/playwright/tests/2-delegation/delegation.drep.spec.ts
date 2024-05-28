import environments from "@constants/environments";
import { dRep01Wallet, user01Wallet } from "@constants/staticWallets";
import { createTempDRepAuth } from "@datafactory/createAuth";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { ShelleyWallet } from "@helpers/crypto";
import { isMobile, openDrawer } from "@helpers/mobile";
import { createNewPageWithWallet } from "@helpers/page";
import extractDRepFromWallet from "@helpers/shellyWallet";
import DRepDetailsPage from "@pages/dRepDetailsPage";
import DRepDirectoryPage from "@pages/dRepDirectoryPage";
import DRepRegistrationPage from "@pages/dRepRegistrationPage";
import { expect } from "@playwright/test";
import walletManager from "lib/walletManager";

test.beforeEach(async () => {
  await setAllureEpic("2. Delegation");
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

test("2L. Should copy DRepId", async ({ page, context }) => {
  await context.grantPermissions(["clipboard-read", "clipboard-write"]);

  const dRepDirectory = new DRepDirectoryPage(page);
  await dRepDirectory.goto();

  await dRepDirectory.searchInput.fill(dRep01Wallet.dRepId);
  await page.getByTestId(`${dRep01Wallet.dRepId}-copy-id-button`).click();
  await expect(page.getByText("Copied to clipboard")).toBeVisible();

  const copiedText = await page.evaluate(() => navigator.clipboard.readText());
  expect(copiedText).toEqual(dRep01Wallet.dRepId);
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
  const email = faker.internet.email({ firstName: name });
  const bio = faker.person.bio();
  const links = [
    faker.internet.url({ appendSlash: true }),
    faker.internet.url(),
  ];

  await dRepRegistrationPage.register({
    name,
    email,
    bio,
    extraContentLinks: links,
  });

  await dRepRegistrationPage.confirmBtn.click();

  const dRepDirectory = new DRepDirectoryPage(dRepPage);
  await dRepDirectory.goto();

  await dRepDirectory.searchInput.fill(wallet.dRepId);
  await dRepPage.getByTestId(`${wallet.dRepId}-view-details-button`).click();

  // Verification
  await expect(dRepPage.getByTestId("copy-drep-id-button")).toHaveText(wallet.dRepId);
  await expect(dRepPage.getByText("Active", { exact: true })).toBeVisible();
  await expect(dRepPage.locator("dl").getByText("₳ 0")).toBeVisible();
  await expect(dRepPage.getByText(email, { exact: true })).toBeVisible();

  for (const link of links) {
    await expect(dRepPage.getByText(link, { exact: true })).toBeVisible();
  }
  await expect(dRepPage.getByText(bio, { exact: true })).toBeVisible();
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
  test.skip(); // Cannot access dRep card

  const dRepDirectory = new DRepDirectoryPage(page);
  await dRepDirectory.goto();

  const dRepCard = dRepDirectory.getDRepCard(dRep01Wallet.dRepId);
  await expect(dRepCard).toHaveText("20");
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

    await expect(dRepDirectoryPage.delegationErrorModal).toBeVisible();
  });
});

test("2I. Should check validity of DRep Id", async ({ page }) => {
  const dRepDirectory = new DRepDirectoryPage(page);
  await dRepDirectory.goto();

  await dRepDirectory.searchInput.fill(dRep01Wallet.dRepId);
  await expect(dRepDirectory.getDRepCard(dRep01Wallet.dRepId)).toHaveText(
    dRep01Wallet.dRepId
  );

  const wallet = await ShelleyWallet.generate();
  const invalidDRepId = extractDRepFromWallet(wallet);

  await dRepDirectory.searchInput.fill(invalidDRepId);
  await expect(dRepDirectory.getDRepCard(invalidDRepId)).not.toBeVisible();
});

test("2J. Should search by DRep id", async ({ page }) => {
  const dRepDirectory = new DRepDirectoryPage(page);
  await dRepDirectory.goto();

  await dRepDirectory.searchInput.fill(dRep01Wallet.dRepId);
  await expect(dRepDirectory.getDRepCard(dRep01Wallet.dRepId)).toHaveText(
    dRep01Wallet.dRepId
  );
});
