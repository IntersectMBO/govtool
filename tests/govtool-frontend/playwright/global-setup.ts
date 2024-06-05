import { faucetWallet } from "@constants/staticWallets";
import { ShelleyWallet } from "@helpers/crypto";
import { pollTransaction } from "@helpers/transaction";
import { loadAmountFromFaucet } from "@services/faucetService";
import kuberService from "@services/kuberService";
import walletManager from "lib/walletManager";

const REGISTER_DREP_WALLETS_COUNT = 9;
const DREP_WALLETS_COUNT = 9;

async function generateWallets(num: number) {
  return await Promise.all(
    Array.from({ length: num }, () =>
      ShelleyWallet.generate().then((wallet) => wallet.json())
    )
  );
}

async function globalSetup() {
  const dRepWallets = await generateWallets(DREP_WALLETS_COUNT);
  const registerDRepWallets = await generateWallets(
    REGISTER_DREP_WALLETS_COUNT
  );

  // faucet setup
  const res = await loadAmountFromFaucet(faucetWallet.address);
  await pollTransaction(res.txid);

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
}

export default globalSetup;
