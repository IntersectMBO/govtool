import environments from "@constants/environments";
import { dRep01Wallet } from "@constants/staticWallets";
import { createTempDRepAuth } from "@datafactory/createAuth";
import { test } from "@fixtures/walletExtension";
import convertBufferToHex from "@helpers/convertBufferToHex";
import { ShelleyWallet } from "@helpers/crypto";
import { createNewPageWithWallet } from "@helpers/page";
import { pollTransaction, waitForTxConfirmation } from "@helpers/transaction";
import DRepRegistrationPage from "@pages/dRepRegistrationPage";
import GovernanceActionsPage from "@pages/governanceActionsPage";
import { expect } from "@playwright/test";
import kuberService from "@services/kuberService";

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
});

test.describe("Temporary DReps", () => {
  test("3G. Should show confirmation message with link to view transaction, when DRep registration txn is submitted @slow ", async ({
    page,
    browser,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    const wallet = await ShelleyWallet.generate();
    const res = await kuberService.transferADA(
      [wallet.addressBech32(environments.networkId)],
      600
    );
    await pollTransaction(res.txId, res.address);

    const tempDRepAuth = await createTempDRepAuth(page, wallet);
    const dRepPage = await createNewPageWithWallet(browser, {
      storageState: tempDRepAuth,
      wallet,
      enableStakeSigning: true,
    });

    const dRepRegistrationPage = new DRepRegistrationPage(dRepPage);
    await dRepRegistrationPage.goto();
    await dRepRegistrationPage.register({ name: "Test_dRep" });

    await expect(dRepRegistrationPage.registrationSuccessModal).toBeVisible();
    await expect(
      dRepRegistrationPage.registrationSuccessModal.getByText("this link")
    ).toBeVisible();
  });

  test("3I. Should verify retire as DRep @slow", async ({
    page,
    browser,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    const wallet = await ShelleyWallet.generate();
    const registrationRes = await kuberService.dRepRegistration(
      convertBufferToHex(wallet.stakeKey.private),
      convertBufferToHex(wallet.stakeKey.pkh)
    );
    await pollTransaction(registrationRes.txId, registrationRes.address);

    const tempDRepAuth = await createTempDRepAuth(page, wallet);
    const dRepPage = await createNewPageWithWallet(browser, {
      storageState: tempDRepAuth,
      wallet,
      enableStakeSigning: true,
    });

    await dRepPage.goto("/");
    await dRepPage.getByTestId("retire-button").click();
    await dRepPage.getByTestId("retire-button").click(); // BUG testId -> continue-retire-button

    await expect(
      dRepPage.getByTestId("retirement-transaction-error-modal")
    ).toBeVisible();
  });

  test("3J. Verify DRep behavior in retired state", async ({
    page,
    browser,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + 3 * environments.txTimeOut);

    const wallet = await ShelleyWallet.generate();
    const registrationRes = await kuberService.dRepRegistration(
      convertBufferToHex(wallet.stakeKey.private),
      convertBufferToHex(wallet.stakeKey.pkh)
    );
    await pollTransaction(registrationRes.txId, registrationRes.address);

    const res = await kuberService.transferADA([
      wallet.addressBech32(environments.networkId),
    ]);
    await pollTransaction(res.txId, res.address);

    const dRepAuth = await createTempDRepAuth(page, wallet);
    const dRepPage = await createNewPageWithWallet(browser, {
      storageState: dRepAuth,
      wallet,
      enableStakeSigning: true,
    });

    await dRepPage.goto("/");
    await dRepPage.getByTestId("retire-button").click();
    await dRepPage.getByTestId("retire-button").click(); // BUG: testId -> continue-retire-button
    await expect(
      dRepPage.getByTestId("retirement-transaction-submitted-modal")
    ).toBeVisible();
    dRepPage.getByTestId("confirm-modal-button").click();
    await waitForTxConfirmation(dRepPage);

    const governanceActionsPage = new GovernanceActionsPage(dRepPage);
    await governanceActionsPage.goto();
    const govActionDetailsPage =
      await governanceActionsPage.viewFirstProposal();
    await expect(govActionDetailsPage.voteBtn).not.toBeVisible();
  });
});
