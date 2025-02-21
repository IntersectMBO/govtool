import environments from "@constants/environments";
import { faucetWallet } from "@constants/staticWallets";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { skipIfMainnet } from "@helpers/cardano";
import { pollTransaction } from "@helpers/transaction";
import { test as setup } from "@fixtures/walletExtension";
import { loadAmountFromFaucet } from "@services/faucetService";
import kuberService from "@services/kuberService";

setup.describe.configure({ timeout: environments.txTimeOut });

setup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("Faucet");
  await skipIfMainnet();
});

setup("Faucet setup", async () => {
  const balance = await kuberService.getBalance(faucetWallet.address);
  if (balance > 100_000) return;

  const res = await loadAmountFromFaucet(faucetWallet.address);
  await pollTransaction(res.txid);
});
