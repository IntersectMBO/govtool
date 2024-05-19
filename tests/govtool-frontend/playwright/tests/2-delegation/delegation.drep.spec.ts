import environments from "@constants/environments";
import { dRep01Wallet } from "@constants/staticWallets";
import { createTempDRepAuth } from "@datafactory/createAuth";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { ShelleyWallet } from "@helpers/crypto";
import { isMobile, openDrawer } from "@helpers/mobile";
import { createNewPageWithWallet } from "@helpers/page";
import extractDRepFromWallet from "@helpers/shellyWallet";
import { transferAdaForWallet } from "@helpers/transaction";
import DRepDirectoryPage from "@pages/dRepDirectoryPage";
import DRepRegistrationPage from "@pages/dRepRegistrationPage";
import { expect } from "@playwright/test";

test("2C. Should open wallet connection popup on delegate in disconnected state", async ({
  page,
}) => {
  await page.goto("/");
  if (isMobile(page)) {
    openDrawer(page);
  }

  await page.getByTestId("view-drep-directory-button").click();
  await page
    .locator('[data-testid$="-connect-to-delegate-button"]')
    .first()
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
  test.setTimeout(testInfo.timeout + 2 * environments.txTimeOut);

  const wallet = await ShelleyWallet.generate();

  await transferAdaForWallet(wallet, 600);

  const tempDRepAuth = await createTempDRepAuth(page, wallet);
  const dRepPage = await createNewPageWithWallet(browser, {
    storageState: tempDRepAuth,
    wallet,
    enableStakeSigning: true,
  });

  const dRepRegistrationPage = new DRepRegistrationPage(dRepPage);
  await dRepRegistrationPage.goto();

  const dRepId = extractDRepFromWallet(wallet);
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

  await dRepDirectory.searchInput.fill(dRepId);
  await dRepPage.getByTestId(`${dRepId}-view-details-button`).click();

  // Verification
  await expect(dRepPage.getByTestId("copy-drep-id-button")).toHaveText(dRepId);
  await expect(dRepPage.getByText("Active", { exact: true })).toBeVisible();
  await expect(dRepPage.locator("dl").getByText("₳ 0")).toBeVisible();
  await expect(dRepPage.getByText(email, { exact: true })).toBeVisible();

  for (const link of links) {
    await expect(dRepPage.getByText(link, { exact: true })).toBeVisible();
  }
  await expect(dRepPage.getByText(bio, { exact: true })).toBeVisible();
});
