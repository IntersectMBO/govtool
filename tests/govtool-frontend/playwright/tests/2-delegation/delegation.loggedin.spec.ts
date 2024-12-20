import { user01Wallet } from "@constants/staticWallets";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { skipIfNotHardFork } from "@helpers/cardano";
import { ShelleyWallet } from "@helpers/crypto";
import { fetchFirstActiveDRepDetails } from "@helpers/dRep";
import { isMobile } from "@helpers/mobile";
import extractDRepFromWallet from "@helpers/shellyWallet";
import DRepDirectoryPage from "@pages/dRepDirectoryPage";
import { expect } from "@playwright/test";

test.use({ storageState: ".auth/user01.json", wallet: user01Wallet });

test.beforeEach(async () => {
  await setAllureEpic("2. Delegation");
  await skipIfNotHardFork();
});

test("2B. Should access DRep Directory page", async ({ page }) => {
  await page.goto("/");

  await page.getByTestId("view-drep-directory-button").click();
  if (isMobile(page)) {
    await expect(page.getByText("DRep Directory")).toBeVisible();
  } else {
    await expect(
      page.getByRole("navigation").getByText("DRep Directory")
    ).toBeVisible();
  }
});

test("2D. Should show delegation options in connected state", async ({
  page,
}) => {
  const dRepDirectoryPage = new DRepDirectoryPage(page);
  await dRepDirectoryPage.goto();

  // Verifying automatic delegation options
  await dRepDirectoryPage.automaticDelegationOptionsDropdown.click();
  await expect(dRepDirectoryPage.abstainDelegationCard).toBeVisible();
  await expect(dRepDirectoryPage.signalNoConfidenceCard).toBeVisible();

  expect(await dRepDirectoryPage.delegateBtns.count()).toBeGreaterThanOrEqual(
    2
  );
});

test("2X_1. Should include info button and voting power on the Abstain card", async ({
  page,
}) => {
  const dRepDirectoryPage = new DRepDirectoryPage(page);
  await dRepDirectoryPage.goto();

  await dRepDirectoryPage.automaticDelegationOptionsDropdown.click();

  await expect(
    dRepDirectoryPage.abstainDelegationCard.getByText("₳")
  ).toBeVisible();

  await expect(dRepDirectoryPage.abstainInfoButton).toBeVisible();
});

test("2X_2. Should include info button and voting power on the Signal-No-Confidence card", async ({
  page,
}) => {
  const dRepDirectoryPage = new DRepDirectoryPage(page);
  await dRepDirectoryPage.goto();

  await dRepDirectoryPage.automaticDelegationOptionsDropdown.click();

  await expect(
    dRepDirectoryPage.signalNoConfidenceCard.getByText("₳")
  ).toBeVisible();

  await expect(dRepDirectoryPage.signalNoConfidenceInfoButton).toBeVisible();
});

test.describe("DRep dependent tests", () => {
  let dRepGivenName = "test";
  let dRepId = "drep1ef7uslcjhjqrn4vv2y39c3yn345gzjsg7yufn76zye3v6fkz23q";
  let dRepDirectoryPage: DRepDirectoryPage;

  test.beforeEach(async ({ page }) => {
    ({ dRepDirectoryPage, dRepId, dRepGivenName } =
      await fetchFirstActiveDRepDetails(page));
  });

  test("2T. Should show warning message on delegation when insufficient funds", async ({
    page,
  }) => {
    await dRepDirectoryPage.searchInput.fill(dRepId);
    await page.getByTestId(`${dRepId}-delegate-button`).click();

    await expect(dRepDirectoryPage.delegationErrorModal).toBeVisible({
      timeout: 10_000,
    });
    await expect(dRepDirectoryPage.delegationErrorModal).toHaveText(
      /UTxO Balance Insufficient/
    );
  });

  test("2I. Should check validity of DRep Id", async () => {
    await dRepDirectoryPage.searchInput.fill(dRepId);
    await expect(dRepDirectoryPage.getDRepCard(dRepId)).toBeVisible();

    const wallet = await ShelleyWallet.generate();
    const invalidDRepId = extractDRepFromWallet(wallet);

    await dRepDirectoryPage.searchInput.fill(invalidDRepId);
    await expect(
      dRepDirectoryPage.getDRepCard(invalidDRepId)
    ).not.toBeVisible();
  });

  test("2J. Should search by DRep id and DRep givenname", async () => {
    // search by dRep Id
    await dRepDirectoryPage.searchInput.fill(dRepId);
    await expect(dRepDirectoryPage.getDRepCard(dRepId)).toBeVisible();

    // search by dRep givenname
    await dRepDirectoryPage.searchInput.fill(dRepGivenName);
    const searchDRepCards = await dRepDirectoryPage.getAllListedDReps();

    for (const dRepCard of searchDRepCards) {
      expect((await dRepCard.innerText()).includes(dRepGivenName));
    }
  });
});
