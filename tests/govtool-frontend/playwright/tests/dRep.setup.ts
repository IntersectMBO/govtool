import environments from "@constants/environments";
import { dRepWallets } from "@constants/staticWallets";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { ShelleyWallet } from "@helpers/crypto";
import { pollTransaction } from "@helpers/transaction";
import { expect, test as setup } from "@playwright/test";
import kuberService from "@services/kuberService";
import walletManager from "lib/walletManager";

const REGISTER_DREP_WALLETS = 9;
const DREP_WALLETS = 9;

setup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("Register DRep");
});

async function generateWallets(num: number) {
  return await Promise.all(
    Array.from({ length: num }, () =>
      ShelleyWallet.generate().then((wallet) => wallet.json())
    )
  );
}

setup("Register DRep of static wallets", async () => {
  setup.setTimeout(environments.txTimeOut);

  try {
    const res = await kuberService.multipleDRepRegistration(dRepWallets);

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
  setup.setTimeout(3 * environments.txTimeOut);

  const dRepWallets = await generateWallets(DREP_WALLETS);
  const registerDRepWallets = await generateWallets(REGISTER_DREP_WALLETS);

  // initialize wallets
  const initializeRes = await kuberService.initializeWallets([
    ...dRepWallets,
    ...registerDRepWallets,
  ]);
  await pollTransaction(initializeRes.txId, initializeRes.lockInfo);

  // register dRep
  const registrationRes =
    await kuberService.multipleDRepRegistration(dRepWallets);
  await pollTransaction(registrationRes.txId, registrationRes.lockInfo);

  // transfer 600 ADA for dRep registration
  const amountOutputs = registerDRepWallets.map((wallet) => {
    return { address: wallet.address, value: `${600}A` };
  });
  const transferRes = await kuberService.multipleTransferADA(amountOutputs);
  await pollTransaction(transferRes.txId, transferRes.lockInfo);

  // save to file
  await walletManager.writeWallets(dRepWallets, "registeredDRep");
  await walletManager.writeWallets(registerDRepWallets, "registerDRep");
});
