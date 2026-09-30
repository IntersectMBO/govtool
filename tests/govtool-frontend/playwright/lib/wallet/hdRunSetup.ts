import { randomBytes } from "crypto";
import * as fs from "fs";
import path = require("path");
import { HD_RUN_FILE } from "./testWallets";

/**
 * Global setup: picks this run's id, which with each wallet's name picks its
 * HD account, so the run's wallets have no chain history. HD_RUN_ID pins it,
 * for example to reuse or sweep a past run's wallets.
 */
export default async function hdRunSetup() {
  const runId = process.env.HD_RUN_ID ?? randomBytes(8).toString("hex");
  fs.writeFileSync(
    HD_RUN_FILE,
    JSON.stringify({ runId, startedAt: new Date().toISOString() }, null, 2)
  );
  // Protocol parameters are cached per run (see helpers/cardano.ts).
  fs.rmSync(path.resolve(__dirname, "../_mock/protocolParameter.json"), {
    force: true,
  });
}
