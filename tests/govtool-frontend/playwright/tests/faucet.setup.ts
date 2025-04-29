import environments from "@constants/environments";
import { faucetWallet } from "@constants/staticWallets";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { getWalletBalance, skipIfMainnet } from "@helpers/cardano";
import { pollTransaction } from "@helpers/transaction";
import { test as setup } from "@fixtures/walletExtension";
import { loadAmountFromFaucet } from "@services/faucetService";

setup.describe.configure({ timeout: environments.txTimeOut });

setup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("Faucet");
  await skipIfMainnet();
});

setup("Faucet setup", async () => {
  const balance = await getWalletBalance(faucetWallet.address);
  if (balance > 450_000) return;
  if (environments.faucet.apiKey === "" || environments.faucet.apiKey === undefined) {
    console.log("Faucet API key is not set");
    console.log("please fund 10_000 Ada to the wallet", faucetWallet.address);
  } else {
    try {
      const res = await loadAmountFromFaucet(faucetWallet.address);
      await pollTransaction(res.txid);
    } catch (e) {
      console.log("Error loading amount from faucet", e);
    }
  }
});
