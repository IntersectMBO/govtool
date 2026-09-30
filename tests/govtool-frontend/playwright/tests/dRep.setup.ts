import environments from "@constants/environments";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { skipIfBalanceIsInsufficient, skipIfMainnet } from "@helpers/cardano";
import { test as setup } from "@fixtures/walletExtension";
import { SharedDRepName, sharedDRep } from "lib/wallet/sharedDReps";

const SHARED_DREPS: SharedDRepName[] = ["dRep01", "dRep02", "dRep03"];

setup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("Register DRep");
  await skipIfMainnet();
  await skipIfBalanceIsInsufficient(600 * SHARED_DREPS.length);
});

setup("Register shared DReps", async () => {
  // Funding goes through the faucet one at a time; registrations then run in
  // parallel from each DRep's own wallet.
  setup.setTimeout(2 * SHARED_DREPS.length * environments.txTimeOut);

  await Promise.all(SHARED_DREPS.map((name) => sharedDRep(name)));
});
