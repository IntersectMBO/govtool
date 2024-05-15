import createWallet from "@fixtures/createWallet";
import { test } from "@fixtures/walletExtension";
import convertBufferToHex from "@helpers/convertBufferToHex";
import { ShelleyWallet } from "@helpers/crypto";
import LoginPage from "@pages/loginPage";
import { expect } from "@playwright/test";

test("1A. Should connect wallet and choose stake-key to use @smoke @fast", async ({
  page,
}) => {
  const shellyWallet = await ShelleyWallet.generate();
  const extraPubStakeKey = convertBufferToHex(shellyWallet.stakeKey.public);
  const extraRewardAddress = convertBufferToHex(
    shellyWallet.rewardAddressRawBytes(0),
  );

  await createWallet(page, {
    extraRegisteredPubStakeKeys: [extraPubStakeKey],
    extraRewardAddresses: [extraRewardAddress],
  });

  const loginPage = new LoginPage(page);
  await loginPage.login();
});

test("1C: Should disconnect Wallet When connected @smoke @fast", async ({
  page,
}) => {
  await createWallet(page);

  const loginPage = new LoginPage(page);
  await loginPage.login();

  await loginPage.logout();
});

test("1D. Should check correct network (Testnet/Mainnet) on connection @smoke @fast", async ({
  page,
}) => {
  const wrongNetworkId = 1; // mainnet network
  await createWallet(page, { networkId: wrongNetworkId });

  const errors: Array<Error> = [];
  page.on("pageerror", (error) => {
    errors.push(error);
  });

  const loginPage = new LoginPage(page);
  await loginPage.login();

  expect(errors).not.toHaveLength(0);
});
