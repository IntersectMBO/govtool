import environments from "@constants/environments";
import { createTempDRepAuth } from "@datafactory/createAuth";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { skipIfMainnet, skipIfNotHardFork } from "@helpers/cardano";
import { ShelleyWallet } from "@helpers/crypto";
import { createNewPageWithWallet } from "@helpers/page";
import DRepRegistrationPage from "@pages/dRepRegistrationPage";
import { expect } from "@playwright/test";
import { LinkType } from "@types";
import walletManager from "lib/walletManager";

test.beforeEach(async () => {
  await setAllureEpic("2. Delegation");
  await skipIfNotHardFork();
  await skipIfMainnet();
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
  const paymentAddress = ShelleyWallet.fromJson(wallet).addressBech32(
    environments.networkId
  );
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

  await expect(dRepPage.getByTestId("alert-success")).not.toBeVisible();

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
  ).toContainText(wallet.dRepId, { timeout: 60_000 });
  await dRepPage.getByTestId("view-drep-details-button").click();

  // Verification
  await expect(dRepPage.getByTestId("copy-drep-id-button")).toHaveText(
    wallet.dRepId
  );
  await expect(dRepPage.getByTestId("copy-payment-address-button")).toHaveText(
    paymentAddress,
    {
      timeout: 60_000,
    }
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
