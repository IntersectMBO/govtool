import environments from "@constants/environments";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { skipIfMainnet } from "@helpers/cardano";
import { createNewPageWithWallet } from "@helpers/page";
import DRepRegistrationPage from "@pages/dRepRegistrationPage";
import { expect } from "@playwright/test";
import { LinkType } from "@types";
import { adaBalance, singleUseWalletName } from "lib/wallet/testWallets";
import { stakeRegisteredWallet } from "lib/wallet/transactions";

test.beforeEach(async () => {
  await setAllureEpic("2. Delegation");
  await skipIfMainnet();
});

test("2N. Should show DRep information on details page", async ({
  browser,
}, testInfo) => {
  test.setTimeout(testInfo.timeout + 2 * environments.txTimeOut);

  // Registers as a DRep: single use, with a registered stake key as the old
  // registerDRep wallets had.
  const wallet = await stakeRegisteredWallet(
    singleUseWalletName("2N:dRep"),
    600
  );

  const dRepPage = await createNewPageWithWallet(browser, { wallet });

  const dRepRegistrationPage = new DRepRegistrationPage(dRepPage);
  // The app reads /network/info on load; its epochNo dates the registration.
  const networkInfo = dRepPage.waitForResponse("**/network/info");
  await dRepRegistrationPage.goto();
  const networkInfoUrl = (await networkInfo).url();
  const currentEpoch = async (): Promise<number> =>
    (await (await dRepPage.request.get(networkInfoUrl)).json()).epochNo;

  const name = faker.person.firstName();
  const objectives = faker.lorem.paragraph(2);
  const motivations = faker.lorem.paragraph(2);
  const qualifications = faker.lorem.paragraph(2);
  const paymentAddress = wallet.address;
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

  const registrationEpoch = await currentEpoch();
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
  // The card shows once the registration is on chain and the frontend's
  // 15 s status check has read it back, which on preview takes longer than
  // a minute.
  await expect(
    dRepPage.getByTestId("dRep-id-display-card-dashboard")
  ).toContainText(wallet.dRepId, { timeout: environments.txTimeOut });
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
  // Registering also delegates the wallet's own stake to the new DRep, which
  // the ledger counts from the next epoch boundary. On preview's long epochs
  // that is after this check, so it reads 0. Devnet's 60 s epochs can cross
  // a boundary mid-test; then only the wallet's own stake is right, and only
  // in a later epoch than the registration.
  const votingPower = dRepPage.getByTestId("voting-power");
  await expect(votingPower).toHaveText(/^₳ [\d,]+$/);
  if ((await votingPower.textContent()) !== "₳ 0") {
    expect(await currentEpoch()).toBeGreaterThan(registrationEpoch);
    const ownStake = Number(
      (await adaBalance(wallet)).toFixed(0)
    ).toLocaleString("en-US");
    await expect(votingPower).toHaveText(`₳ ${ownStake}`);
  }

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
