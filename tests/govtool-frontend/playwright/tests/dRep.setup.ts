import environments from "@constants/environments";
import { dRepWallets } from "@constants/staticWallets";
import { pollTransaction } from "@helpers/transaction";
import { expect, test as setup } from "@playwright/test";
import kuberService from "@services/kuberService";
import { Logger } from "../../cypress/lib/logger/logger";
import fetch = require("node-fetch");

const dRepInfo = require("../lib/_mock/dRepInfo.json");

setup.describe.configure({ timeout: environments.txTimeOut });

dRepWallets.forEach((wallet) => {
  setup(`Register DRep of wallet: ${wallet.address}`, async () => {
    try {
      const res = await kuberService.dRepRegistration(
        wallet.stake.private,
        wallet.stake.pkh
      );

      await pollTransaction(res.txId, res.address);
    } catch (err) {
      if (err.status === 400) {
        expect(true, "DRep already registered").toBeTruthy();
      } else {
        throw err;
      }
    }
  });
});

setup("Setup dRep metadata", async () => {
  try {
    const res = await fetch(`${environments.metadataBucketUrl}/Test_dRep`, {
      method: "PUT",
      body: JSON.stringify(dRepInfo),
    });
    Logger.success("Uploaded dRep metadata to bucket");
  } catch (e) {
    Logger.fail(`Failed to upload dRep metadata: ${e}`);
  }
});
