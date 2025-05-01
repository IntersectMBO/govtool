import environments from "@constants/environments";
import { dRepWallets } from "@constants/staticWallets";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { skipIfBalanceIsInsufficient, skipIfMainnet } from "@helpers/cardano";
import { uploadMetadataAndGetJsonHash } from "@helpers/metadata";
import { generateWallets } from "@helpers/shellyWallet";
import { pollTransaction } from "@helpers/transaction";
import { expect } from "@playwright/test";
import { test as setup } from "@fixtures/walletExtension";

import kuberService from "@services/kuberService";
import walletManager from "lib/walletManager";
import { functionWaitedAssert } from "@helpers/waitedLoop";
import { StaticWallet } from "@types";

const REGISTER_DREP_WALLETS_COUNT = 6;
const DREP_WALLETS_COUNT = 10;

let dRepDeposit: number;

setup.beforeAll(async () => {
  await functionWaitedAssert(
    async () => {
      const res = await kuberService.queryProtocolParams();
      dRepDeposit = res.dRepDeposit;
    },
    { name: "queryProtocolParams" }
  );
});

setup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("Register DRep");
  await skipIfMainnet();
});

setup("Register DRep of static wallets", async () => {
  const totalRequiredBalanceForDRepSetup = dRepWallets.length * dRepDeposit + 2;
  await skipIfBalanceIsInsufficient(totalRequiredBalanceForDRepSetup);
  setup.setTimeout(environments.txTimeOut);

  try {
    // Submit metadata to obtain a URL and generate hash value.
    const metadataPromises = dRepWallets.map(async (dRepWallet) => {
      const metadataResponse = await uploadMetadataAndGetJsonHash();
      const givenName = metadataResponse.givenName;
      const index = dRepWallets.indexOf(dRepWallet);
      dRepWallets[index] = {
        ...dRepWallet,
        givenName,
      };
      return {
        ...metadataResponse,
        wallet: dRepWallet,
      };
    });

    const metadataAndDRepWallets = await Promise.all(metadataPromises);
    const res = await kuberService.multipleDRepRegistration(
      metadataAndDRepWallets
    );

    await pollTransaction(res.txId, res.lockInfo);
  } catch (err) {
    if (
      err.status === 400 &&
      err.message.includes("ConwayDRepAlreadyRegistered")
    ) {
      expect(true, "DRep already registered").toBeTruthy();
    } else {
      throw err;
    }
  }
});

setup("Setup temporary DRep wallets", async () => {
  const totalRequiredBalanceForDRepSetup =
    (DREP_WALLETS_COUNT + REGISTER_DREP_WALLETS_COUNT) * dRepDeposit +
    22 * DREP_WALLETS_COUNT;
  await skipIfBalanceIsInsufficient(totalRequiredBalanceForDRepSetup);
  setup.setTimeout(3 * environments.txTimeOut);

  const dRepWallets: StaticWallet[] = await generateWallets(DREP_WALLETS_COUNT);
  const registerDRepWallets = await generateWallets(
    REGISTER_DREP_WALLETS_COUNT
  );

  // initialize wallets
  const initializeRes = await kuberService.initializeWallets([
    ...dRepWallets,
    ...registerDRepWallets,
  ]);
  await pollTransaction(initializeRes.txId, initializeRes.lockInfo);

  // Submit metadata to obtain a URL and generate hash value.
  const metadataPromises = dRepWallets.map(async (dRepWallet) => {
    const metadataResponse = await uploadMetadataAndGetJsonHash();
    const givenName = metadataResponse.givenName;
    const index = dRepWallets.indexOf(dRepWallet);
    dRepWallets[index] = {
      ...dRepWallet,
      givenName,
    };
    return {
      ...metadataResponse,
      wallet: dRepWallet,
    };
  });

  const metadatasAndDRepWallets = await Promise.all(metadataPromises);
  // register dRep
  const registrationRes = await kuberService.multipleDRepRegistration(
    metadatasAndDRepWallets
  );
  await pollTransaction(registrationRes.txId, registrationRes.lockInfo);

  // transfer 600 ADA for dRep registration
  const amountOutputs = registerDRepWallets.map((wallet) => {
    return { address: wallet.address, value: dRepDeposit };
  });
  const transferRes = await kuberService.multipleTransferADA(amountOutputs);
  await pollTransaction(transferRes.txId, transferRes.lockInfo);

  // save to file
  await walletManager.writeWallets(dRepWallets, "registeredDRep");
  await walletManager.writeWallets(registerDRepWallets, "registerDRep");
  await walletManager.writeWallets(dRepWallets, "registeredDRepCopy");
  await walletManager.writeWallets(registerDRepWallets, "registerDRepCopy");
});
