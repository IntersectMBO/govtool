import environments from "@constants/environments";
import {
  budgetProposalDRepWallets,
  dRepWallets,
} from "@constants/staticWallets";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { skipIfBalanceIsInsufficient, skipIfMainnet } from "@helpers/cardano";
import { uploadMetadataAndGetJsonHash } from "@helpers/metadata";
import { pollTransaction } from "@helpers/transaction";
import { expect } from "@playwright/test";
import { test as setup } from "@fixtures/walletExtension";

import kuberService from "@services/kuberService";

setup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("Register DRep");
  await skipIfMainnet();
  await skipIfBalanceIsInsufficient(502 * budgetProposalDRepWallets.length);
});

setup("Register DRep of proposal budget static wallets", async () => {
  setup.setTimeout(environments.txTimeOut);

  try {
    // Submit metadata to obtain a URL and generate hash value.
    const metadataPromises = budgetProposalDRepWallets.map(
      async (dRepWallet) => {
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
      }
    );

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
